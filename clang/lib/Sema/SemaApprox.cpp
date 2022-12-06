//===--- SemaApprox.cpp - Semantic Analysis for Approx constructs ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file implements semantic analysis for Approx directives and clauses.
///
//===----------------------------------------------------------------------===//

#include "TreeTransform.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTFwd.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclApprox.h"
#include "clang/AST/StmtApprox.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Basic/Approx.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/OpenMPKinds.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Frontend/OpenMP/OMP.h.inc"
#include "llvm/Support/Debug.h"

using namespace clang;
using namespace llvm;
using namespace approx;

ExprResult Sema::PerformApproxImplicitIntegerConversion(SourceLocation Loc,
                                                        Expr *Op) {
  if (!Op)
    return ExprError();

  class IntConvertDiagnoser : public ICEConvertDiagnoser {
  public:
    IntConvertDiagnoser()
        : ICEConvertDiagnoser(/*AllowScopedEnumerations*/ false, false, true) {}
    SemaDiagnosticBuilder diagnoseNotInt(Sema &S, SourceLocation Loc,
                                         QualType T) override {
      return S.Diag(Loc, diag::err_approx_not_integral) << T;
    }
    SemaDiagnosticBuilder diagnoseIncomplete(Sema &S, SourceLocation Loc,
                                             QualType T) override {
      return S.Diag(Loc, diag::err_approx_incomplete_type) << T;
    }
    SemaDiagnosticBuilder diagnoseExplicitConv(Sema &S, SourceLocation Loc,
                                               QualType T,
                                               QualType ConvTy) override {
      return S.Diag(Loc, diag::err_approx_explicit_conversion) << T << ConvTy;
    }
    SemaDiagnosticBuilder noteExplicitConv(Sema &S, CXXConversionDecl *Conv,
                                           QualType ConvTy) override {
      return S.Diag(Conv->getLocation(), diag::note_approx_conversion_here)
             << ConvTy->isEnumeralType() << ConvTy;
    }
    SemaDiagnosticBuilder diagnoseAmbiguous(Sema &S, SourceLocation Loc,
                                            QualType T) override {
      return S.Diag(Loc, diag::err_approx_ambiguous_conversion) << T;
    }
    SemaDiagnosticBuilder noteAmbiguous(Sema &S, CXXConversionDecl *Conv,
                                        QualType ConvTy) override {
      return S.Diag(Conv->getLocation(), diag::note_approx_conversion_here)
             << ConvTy->isEnumeralType() << ConvTy;
    }
    SemaDiagnosticBuilder diagnoseConversion(Sema &, SourceLocation, QualType,
                                             QualType) override {
      llvm_unreachable("conversion functions are permitted");
    }
  } ConvertDiagnoser;
  return PerformContextualImplicitConversion(Loc, Op, ConvertDiagnoser);
}

static Stmt *buildPreInits(ASTContext &Context,
                                 MutableArrayRef<Decl *> PreInits) {
  if (!PreInits.empty()) {
    return new (Context) DeclStmt(
        DeclGroupRef::Create(Context, PreInits.begin(), PreInits.size()),
        SourceLocation(), SourceLocation());
  }
  return nullptr;
}

static Stmt *buildPreInits(
    ASTContext &Context,
    const llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) {
  if (!Captures.empty()) {
    SmallVector<Decl *, 16> PreInits;
    for (const auto &Pair : Captures)
      PreInits.push_back(Pair.second->getDecl());
    return buildPreInits(Context, PreInits);
  }
  return nullptr;
}

static ApproxCapturedExprDecl *buildCaptureDecl(Sema &S, IdentifierInfo *Id,
                                                Expr *CaptureExpr,
                                                bool WithInit,
                                                bool AsExpression) {
  assert(CaptureExpr);
  ASTContext &C = S.getASTContext();
  Expr *Init = AsExpression ? CaptureExpr : CaptureExpr->IgnoreImpCasts();
  QualType Ty = Init->getType();
  if (CaptureExpr->getObjectKind() == OK_Ordinary && CaptureExpr->isGLValue()) {
    if (S.getLangOpts().CPlusPlus) {
      Ty = C.getLValueReferenceType(Ty);
    } else {
      Ty = C.getPointerType(Ty);
      ExprResult Res =
          S.CreateBuiltinUnaryOp(CaptureExpr->getExprLoc(), UO_AddrOf, Init);
      if (!Res.isUsable())
        return nullptr;
      Init = Res.get();
    }
    WithInit = true;
  }

  auto *CED = ApproxCapturedExprDecl::Create(C, S.CurContext, Id, Ty,
                                             CaptureExpr->getBeginLoc());
  S.CurContext->addHiddenDecl(CED);
  S.AddInitializerToDecl(CED, Init, /*DirectInit=*/false);
  return CED;
}

static DeclRefExpr *buildDeclRefExpr(Sema &S, VarDecl *D, QualType Ty,
                                     SourceLocation Loc,
                                     bool RefersToCapture = false) {
  D->setReferenced();
  D->markUsed(S.Context);
  return DeclRefExpr::Create(S.getASTContext(), NestedNameSpecifierLoc(),
                             SourceLocation(), D, RefersToCapture, Loc, Ty,
                             VK_LValue);
}

static ExprResult buildCapture(Sema &S, Expr *CaptureExpr, DeclRefExpr *&Ref) {
  CaptureExpr = S.DefaultLvalueConversion(CaptureExpr).get();
  if (!Ref) {
    ApproxCapturedExprDecl *CD = buildCaptureDecl(
        S, &S.getASTContext().Idents.get(".approx_capture_expr."), CaptureExpr,
        /*WithInit=*/true, /*AsExpression=*/true);
    Ref = buildDeclRefExpr(S, CD, CD->getType().getNonReferenceType(),
                           CaptureExpr->getExprLoc());
  }
  ExprResult Res = Ref;
  if (!S.getLangOpts().CPlusPlus &&
      CaptureExpr->getObjectKind() == OK_Ordinary && CaptureExpr->isGLValue() &&
      Ref->getType()->isPointerType()) {
    Res = S.CreateBuiltinUnaryOp(CaptureExpr->getExprLoc(), UO_Deref, Ref);
    if (!Res.isUsable())
      return ExprError();
  }
  return S.DefaultLvalueConversion(Res.get());
}

static ExprResult
tryBuildCapture(Sema &SemaRef, Expr *Capture,
                      llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) {
  if (SemaRef.CurContext->isDependentContext() || Capture->containsErrors())
    return Capture;
  if (Capture->isEvaluatable(SemaRef.Context, Expr::SE_AllowSideEffects))
    return SemaRef.PerformImplicitConversion(Capture->IgnoreImpCasts(),
                                             Capture->getType(),
                                             Sema::AA_Converting, true);
  auto I = Captures.find(Capture);
  if (I != Captures.end())
    return buildCapture(SemaRef, Capture, I->second);
  DeclRefExpr *Ref = nullptr;
  ExprResult Res = buildCapture(SemaRef, Capture, Ref);
  Captures[Capture] = Ref;
  return Res;
}

/* =========== Sema analysis for perforated loop ================ */

static const Expr *getExprAsWritten(const Expr *E) {
  if (const auto *FE = dyn_cast<FullExpr>(E))
    E = FE->getSubExpr();

  if (const auto *MTE = dyn_cast<MaterializeTemporaryExpr>(E))
    E = MTE->getSubExpr();

  while (const auto *Binder = dyn_cast<CXXBindTemporaryExpr>(E))
    E = Binder->getSubExpr();

  if (const auto *ICE = dyn_cast<ImplicitCastExpr>(E))
    E = ICE->getSubExprAsWritten();
  return E->IgnoreParens();
}

static Expr *getExprAsWritten(Expr *E) {
  return const_cast<Expr *>(getExprAsWritten(const_cast<const Expr *>(E)));
}
/// Iteration space of a single for loop.
struct LoopIterationSpace final {
  /// True if the condition operator is the strict compare operator (<, > or
  /// !=).
  bool IsStrictCompare = false;
  /// Condition of the loop.
  Expr *PreCond = nullptr;
  /// This expression calculates the number of iterations in the loop.
  /// It is always possible to calculate it before starting the loop.
  Expr *NumIterations = nullptr;
  /// The loop counter variable.
  Expr *CounterVar = nullptr;
  /// Private loop counter variable.
  //Expr *PrivateCounterVar = nullptr;
  /// This is initializer for the initial value of #CounterVar.
  Expr *CounterInit = nullptr;
  /// This is step for the #CounterVar used to generate its update:
  /// #CounterVar = #CounterInit + #CounterStep * CurrentIteration.
  Expr *CounterStep = nullptr;
  /// Should step be subtracted?
  bool Subtract = false;
  /// Source range of the loop init.
  SourceRange InitSrcRange;
  /// Source range of the loop condition.
  SourceRange CondSrcRange;
  /// Source range of the loop increment.
  SourceRange IncSrcRange;
  /// Minimum value that can have the loop control variable. Used to support
  /// non-rectangular loops. Applied only for LCV with the non-iterator types,
  /// since only such variables can be used in non-loop invariant expressions.
  Expr *MinValue = nullptr;
  /// Maximum value that can have the loop control variable. Used to support
  /// non-rectangular loops. Applied only for LCV with the non-iterator type,
  /// since only such variables can be used in non-loop invariant expressions.
  Expr *MaxValue = nullptr;
  /// true, if the lower bound depends on the outer loop control var.
  bool IsNonRectangularLB = false;
  /// true, if the upper bound depends on the outer loop control var.
  bool IsNonRectangularUB = false;
  /// Index of the loop this loop depends on and forms non-rectangular loop
  /// nest.
  unsigned LoopDependentIdx = 0;
  /// Final condition for the non-rectangular loop nest support. It is used to
  /// check that the number of iterations for this particular counter must be
  /// finished.
  Expr *FinalCondition = nullptr;
};

/// Helper class for checking canonical form of the Approx perforated loops
/// (similar to OpenMP) and extracting iteration space, to be used
class ApproxIterationSpaceChecker {
  /// Reference to Sema.
  Sema &SemaRef;
  /// A location for diagnostics (when there is no some better location).
  SourceLocation DefaultLoc;
  /// A location for diagnostics (when increment is not compatible).
  SourceLocation ConditionLoc;
  /// A source location for referring to loop init later.
  SourceRange InitSrcRange;
  /// A source location for referring to condition later.
  SourceRange ConditionSrcRange;
  /// A source location for referring to increment later.
  SourceRange IncrementSrcRange;
  /// Loop variable.
  ValueDecl *LCDecl = nullptr;
  /// Reference to loop variable.
  Expr *LCRef = nullptr;
  /// Lower bound (initializer for the var).
  Expr *LB = nullptr;
  /// Upper bound.
  Expr *UB = nullptr;
  /// Loop step (increment).
  Expr *Step = nullptr;
  /// This flag is true when condition is one of:
  ///   Var <  UB
  ///   Var <= UB
  ///   UB  >  Var
  ///   UB  >= Var
  /// This will have no value when the condition is !=
  llvm::Optional<bool> TestIsLessOp;
  /// This flag is true when condition is strict ( < or > ).
  bool TestIsStrictOp = false;
  /// This flag is true when step is subtracted on each iteration.
  bool SubtractStep = false;
  /// The outer loop counter this loop depends on (if any).
  const ValueDecl *DepDecl = nullptr;
  /// Contains number of loop (starts from 1) on which loop counter init
  /// expression of this loop depends on.
  Optional<unsigned> InitDependOnLC;
  /// Contains number of loop (starts from 1) on which loop counter condition
  /// expression of this loop depends on.
  Optional<unsigned> CondDependOnLC;
  /// Checks if the provide statement depends on the loop counter.
  // GG: is this needed since there aren't nested loops?
  //Optional<unsigned> doesDependOnLoopCounter(const Stmt *S, bool IsInitializer);
  /// Original condition required for checking of the exit condition for
  /// non-rectangular loop.
  Expr *Condition = nullptr;

public:
  ApproxIterationSpaceChecker(Sema &SemaRef,
                              SourceLocation DefaultLoc)
      : SemaRef(SemaRef), DefaultLoc(DefaultLoc),
        ConditionLoc(DefaultLoc) {}
  /// Check init-expr for canonical loop form and save loop counter
  /// variable - #Var and its initialization value - #LB.
  bool checkAndSetInit(Stmt *S, bool EmitDiags = true);
  /// Check test-expr for canonical form, save upper-bound (#UB), flags
  /// for less/greater and for strict/non-strict comparison.
  bool checkAndSetCond(Expr *S);
  /// Check incr-expr for canonical loop form and return true if it
  /// does not conform, otherwise save loop step (#Step).
  bool checkAndSetInc(Expr *S);
  /// Return the loop counter variable.
  ValueDecl *getLoopDecl() const { return LCDecl; }
  /// Return the reference expression to loop counter variable.
  Expr *getLoopDeclRefExpr() const { return LCRef; }
  /// Source range of the loop init.
  SourceRange getInitSrcRange() const { return InitSrcRange; }
  /// Source range of the loop condition.
  SourceRange getConditionSrcRange() const { return ConditionSrcRange; }
  /// Source range of the loop increment.
  SourceRange getIncrementSrcRange() const { return IncrementSrcRange; }
  /// True if the step should be subtracted.
  bool shouldSubtractStep() const { return SubtractStep; }
  /// True, if the compare operator is strict (<, > or !=).
  bool isStrictTestOp() const { return TestIsStrictOp; }
  /// Build the expression to calculate the number of iterations.
  Expr *buildNumIterations(
      Scope *S, LoopIterationSpace &ResultIterSpace, bool LimitedType,
      llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const;
  /// Build the precondition expression for the loops.
  Expr *
  buildPreCond(Scope *S, Expr *Cond,
               llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const;
  /// Build reference expression to the counter be used for codegen.
  DeclRefExpr *buildCounterVar(
      llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const;
  /// Build reference expression to the private counter be used for
  /// codegen.
  /// Build initialization of the counter be used for codegen.
  Expr *buildCounterInit() const;
  /// Build step of the counter be used for codegen.
  Expr *buildCounterStep() const;
  /// Builds the minimum value for the loop counter.
  std::pair<Expr *, Expr *> buildMinMaxValues(
      Scope *S, llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const;
  /// Builds final condition for the non-rectangular loops.
  /// Return true if any expression is dependent.
  bool dependent() const;
  /// Returns true if the initializer forms non-rectangular loop.
  bool doesInitDependOnLC() const { return InitDependOnLC.hasValue(); }
  /// Returns true if the condition forms non-rectangular loop.
  bool doesCondDependOnLC() const { return CondDependOnLC.hasValue(); }
  /// Returns index of the loop we depend on (starting from 1), or 0 otherwise.
  unsigned getLoopDependentIdx() const {
    return InitDependOnLC.getValueOr(CondDependOnLC.getValueOr(0));
  }

private:
  /// Check the right-hand side of an assignment in the increment
  /// expression.
  bool checkAndSetIncRHS(Expr *RHS);
  /// Helper to set loop counter variable and its initializer.
  bool setLCDeclAndLB(ValueDecl *NewLCDecl, Expr *NewDeclRefExpr, Expr *NewLB,
                      bool EmitDiags);
  /// Helper to set upper bound.
  bool setUB(Expr *NewUB, llvm::Optional<bool> LessOp, bool StrictOp,
             SourceRange SR, SourceLocation SL);
  /// Helper to set loop increment.
  bool setStep(Expr *NewStep, bool Subtract);
};

static const ValueDecl *getCanonicalDecl(const ValueDecl *D) {
  //if (const auto *CED = dyn_cast<OMPCapturedExprDecl>(D))
  if (const auto *CED = dyn_cast<ApproxCapturedExprDecl>(D))
  //if (const auto *CED = dyn_cast<VarDecl>(D))
    if (const auto *ME = dyn_cast<MemberExpr>(getExprAsWritten(CED->getInit())))
      D = ME->getMemberDecl();
  const auto *VD = dyn_cast<VarDecl>(D);
  const auto *FD = dyn_cast<FieldDecl>(D);
  if (VD != nullptr) {
    VD = VD->getCanonicalDecl();
    D = VD;
  } else {
    assert(FD);
    FD = FD->getCanonicalDecl();
    D = FD;
  }
  return D;
}

static ValueDecl *getCanonicalDecl(ValueDecl *D) {
  return const_cast<ValueDecl *>(
      getCanonicalDecl(const_cast<const ValueDecl *>(D)));
}

bool ApproxIterationSpaceChecker::dependent() const {
  if (!LCDecl) {
    assert(!LB && !UB && !Step);
    return false;
  }
  return LCDecl->getType()->isDependentType() ||
         (LB && LB->isValueDependent()) || (UB && UB->isValueDependent()) ||
         (Step && Step->isValueDependent());
}

bool ApproxIterationSpaceChecker::setUB(Expr *NewUB,
                                        llvm::Optional<bool> LessOp,
                                        bool StrictOp, SourceRange SR,
                                        SourceLocation SL) {
  // State consistency checking to ensure correct usage.
  assert(LCDecl != nullptr && LB != nullptr && UB == nullptr &&
         Step == nullptr && !TestIsLessOp && !TestIsStrictOp);
  if (!NewUB)
    return true;
  UB = NewUB;
  if (LessOp)
    TestIsLessOp = LessOp;
  TestIsStrictOp = StrictOp;
  ConditionSrcRange = SR;
  ConditionLoc = SL;
  //CondDependOnLC = doesDependOnLoopCounter(UB, /*IsInitializer=*/false);
  return false;
}

bool ApproxIterationSpaceChecker::setStep(Expr *NewStep, bool Subtract) {
  // State consistency checking to ensure correct usage.
  assert(LCDecl != nullptr && LB != nullptr && Step == nullptr);
  if (!NewStep)
    return true;
  if (!NewStep->isValueDependent()) {
    // Check that the step is integer expression.
    SourceLocation StepLoc = NewStep->getBeginLoc();
    ExprResult Val = SemaRef.PerformOpenMPImplicitIntegerConversion(
        StepLoc, getExprAsWritten(NewStep));
    if (Val.isInvalid())
      return true;
    NewStep = Val.get();

    // OpenMP [2.6, Canonical Loop Form, Restrictions]
    //  If test-expr is of form var relational-op b and relational-op is < or
    //  <= then incr-expr must cause var to increase on each iteration of the
    //  loop. If test-expr is of form var relational-op b and relational-op is
    //  > or >= then incr-expr must cause var to decrease on each iteration of
    //  the loop.
    //  If test-expr is of form b relational-op var and relational-op is < or
    //  <= then incr-expr must cause var to decrease on each iteration of the
    //  loop. If test-expr is of form b relational-op var and relational-op is
    //  > or >= then incr-expr must cause var to increase on each iteration of
    //  the loop.
    llvm::APSInt Result;
    bool IsConstant = NewStep->isIntegerConstantExpr(SemaRef.Context);
    if(IsConstant)
      Result = NewStep->getIntegerConstantExpr(SemaRef.Context).getValue();
    bool IsUnsigned = !NewStep->getType()->hasSignedIntegerRepresentation();
    bool IsConstNeg =
        IsConstant && Result.isSigned() && (Subtract != Result.isNegative());
    bool IsConstPos =
        IsConstant && Result.isSigned() && (Subtract == Result.isNegative());
    bool IsConstZero = IsConstant && !Result.getBoolValue();

    // != with increment is treated as <; != with decrement is treated as >
    if (!TestIsLessOp.hasValue())
      TestIsLessOp = IsConstPos || (IsUnsigned && !Subtract);
    if (UB && (IsConstZero ||
               (TestIsLessOp.getValue() ?
                  (IsConstNeg || (IsUnsigned && Subtract)) :
                  (IsConstPos || (IsUnsigned && !Subtract))))) {
      SemaRef.Diag(NewStep->getExprLoc(),
                   diag::err_omp_loop_incr_not_compatible)
          << LCDecl << TestIsLessOp.getValue() << NewStep->getSourceRange();
      SemaRef.Diag(ConditionLoc,
                   diag::note_omp_loop_cond_requres_compatible_incr)
          << TestIsLessOp.getValue() << ConditionSrcRange;
      return true;
    }
    if (TestIsLessOp.getValue() == Subtract) {
      NewStep =
          SemaRef.CreateBuiltinUnaryOp(NewStep->getExprLoc(), UO_Minus, NewStep)
              .get();
      Subtract = !Subtract;
    }
  }

  Step = NewStep;
  SubtractStep = Subtract;
  return false;
}

bool ApproxIterationSpaceChecker::setLCDeclAndLB(ValueDecl *NewLCDecl,
                                                 Expr *NewLCRefExpr,
                                                 Expr *NewLB, bool EmitDiags) {
  // State consistency checking to ensure correct usage.
  assert(LCDecl == nullptr && LB == nullptr && LCRef == nullptr &&
         UB == nullptr && Step == nullptr && !TestIsLessOp && !TestIsStrictOp);
  if (!NewLCDecl || !NewLB)
    return true;
  LCDecl = getCanonicalDecl(NewLCDecl);
  LCRef = NewLCRefExpr;
  if (auto *CE = dyn_cast_or_null<CXXConstructExpr>(NewLB))
    if (const CXXConstructorDecl *Ctor = CE->getConstructor())
      if ((Ctor->isCopyOrMoveConstructor() ||
           Ctor->isConvertingConstructor(/*AllowExplicit=*/false)) &&
          CE->getNumArgs() > 0 && CE->getArg(0) != nullptr)
        NewLB = CE->getArg(0)->IgnoreParenImpCasts();
  LB = NewLB;
  return false;
}

/// Ignore parenthesizes, implicit casts, copy constructor and return the
/// variable (which may be the loop variable) if possible.
static const ValueDecl *getInitLCDecl(const Expr *E) {
  if (!E)
    return nullptr;
  E = getExprAsWritten(E);
  if (const auto *CE = dyn_cast_or_null<CXXConstructExpr>(E))
    if (const CXXConstructorDecl *Ctor = CE->getConstructor())
      if ((Ctor->isCopyOrMoveConstructor() ||
           Ctor->isConvertingConstructor(/*AllowExplicit=*/false)) &&
          CE->getNumArgs() > 0 && CE->getArg(0) != nullptr)
        E = CE->getArg(0)->IgnoreParenImpCasts();
  if (const auto *DRE = dyn_cast_or_null<DeclRefExpr>(E)) {
    if (const auto *VD = dyn_cast<VarDecl>(DRE->getDecl()))
      return getCanonicalDecl(VD);
  }
  if (const auto *ME = dyn_cast_or_null<MemberExpr>(E))
    if (ME->isArrow() && isa<CXXThisExpr>(ME->getBase()->IgnoreParenImpCasts()))
      return getCanonicalDecl(ME->getMemberDecl());
  return nullptr;
}

bool ApproxIterationSpaceChecker::checkAndSetCond(Expr *S) {
  // Check test-expr for canonical form, save upper-bound UB, flags for
  // less/greater and for strict/non-strict comparison.
  // OpenMP [2.9] Canonical loop form. Test-expr may be one of the following:
  //   var relational-op b
  //   b relational-op var
  //
  bool IneqCondIsCanonical = SemaRef.getLangOpts().OpenMP >= 50;
  if (!S) {
    SemaRef.Diag(DefaultLoc, diag::err_omp_loop_not_canonical_cond)
        << (IneqCondIsCanonical ? 1 : 0) << LCDecl;
    return true;
  }
  Condition = S;
  S = getExprAsWritten(S);
  SourceLocation CondLoc = S->getBeginLoc();
  if (auto *BO = dyn_cast<BinaryOperator>(S)) {
    if (BO->isRelationalOp()) {
      if (getInitLCDecl(BO->getLHS()) == LCDecl)
        return setUB(BO->getRHS(),
                     (BO->getOpcode() == BO_LT || BO->getOpcode() == BO_LE),
                     (BO->getOpcode() == BO_LT || BO->getOpcode() == BO_GT),
                     BO->getSourceRange(), BO->getOperatorLoc());
      if (getInitLCDecl(BO->getRHS()) == LCDecl)
        return setUB(BO->getLHS(),
                     (BO->getOpcode() == BO_GT || BO->getOpcode() == BO_GE),
                     (BO->getOpcode() == BO_LT || BO->getOpcode() == BO_GT),
                     BO->getSourceRange(), BO->getOperatorLoc());
    } else if (IneqCondIsCanonical && BO->getOpcode() == BO_NE)
      return setUB(
          getInitLCDecl(BO->getLHS()) == LCDecl ? BO->getRHS() : BO->getLHS(),
          /*LessOp=*/llvm::None,
          /*StrictOp=*/true, BO->getSourceRange(), BO->getOperatorLoc());
  } else if (auto *CE = dyn_cast<CXXOperatorCallExpr>(S)) {
    if (CE->getNumArgs() == 2) {
      auto Op = CE->getOperator();
      switch (Op) {
      case OO_Greater:
      case OO_GreaterEqual:
      case OO_Less:
      case OO_LessEqual:
        if (getInitLCDecl(CE->getArg(0)) == LCDecl)
          return setUB(CE->getArg(1), Op == OO_Less || Op == OO_LessEqual,
                       Op == OO_Less || Op == OO_Greater, CE->getSourceRange(),
                       CE->getOperatorLoc());
        if (getInitLCDecl(CE->getArg(1)) == LCDecl)
          return setUB(CE->getArg(0), Op == OO_Greater || Op == OO_GreaterEqual,
                       Op == OO_Less || Op == OO_Greater, CE->getSourceRange(),
                       CE->getOperatorLoc());
        break;
      case OO_ExclaimEqual:
        if (IneqCondIsCanonical)
          return setUB(getInitLCDecl(CE->getArg(0)) == LCDecl ? CE->getArg(1)
                                                              : CE->getArg(0),
                       /*LessOp=*/llvm::None,
                       /*StrictOp=*/true, CE->getSourceRange(),
                       CE->getOperatorLoc());
        break;
      default:
        break;
      }
    }
  }
  if (dependent() || SemaRef.CurContext->isDependentContext())
    return false;
  SemaRef.Diag(CondLoc, diag::err_omp_loop_not_canonical_cond)
      << (IneqCondIsCanonical ? 1 : 0) << S->getSourceRange() << LCDecl;
  return true;
}

bool ApproxIterationSpaceChecker::checkAndSetInit(Stmt *S, bool EmitDiags) {
  // Check init-expr for canonical loop form and save loop counter
  // variable - #Var and its initialization value - #LB.
  // OpenMP [2.6] Canonical loop form. init-expr may be one of the following:
  //   var = lb
  //   integer-type var = lb
  //   random-access-iterator-type var = lb
  //   pointer-type var = lb
  //
  if (!S) {
    if (EmitDiags) {
      SemaRef.Diag(DefaultLoc, diag::err_omp_loop_not_canonical_init);
    }
    return true;
  }
  if (auto *ExprTemp = dyn_cast<ExprWithCleanups>(S))
    if (!ExprTemp->cleanupsHaveSideEffects())
      S = ExprTemp->getSubExpr();

  InitSrcRange = S->getSourceRange();
  if (Expr *E = dyn_cast<Expr>(S))
    S = E->IgnoreParens();
  if (auto *BO = dyn_cast<BinaryOperator>(S)) {
    if (BO->getOpcode() == BO_Assign) {
      Expr *LHS = BO->getLHS()->IgnoreParens();
      if (auto *DRE = dyn_cast<DeclRefExpr>(LHS)) {
        //if (auto *CED = dyn_cast<OMPCapturedExprDecl>(D))
        if (auto *CED = dyn_cast<ApproxCapturedExprDecl>(DRE->getDecl()))
        //if (auto *CED = dyn_cast<VarDecl>(DRE->getDecl()))
          if(auto *ME = dyn_cast<MemberExpr>(getExprAsWritten(CED->getInit())))
            return setLCDeclAndLB(ME->getMemberDecl(), ME, BO->getRHS(),
                                  EmitDiags);
        return setLCDeclAndLB(DRE->getDecl(), DRE, BO->getRHS(), EmitDiags);
      }
      if (auto *ME = dyn_cast<MemberExpr>(LHS)) {
        if (ME->isArrow() &&
            isa<CXXThisExpr>(ME->getBase()->IgnoreParenImpCasts()))
          return setLCDeclAndLB(ME->getMemberDecl(), ME, BO->getRHS(),
                                EmitDiags);
      }
    }
  } else if (auto *DS = dyn_cast<DeclStmt>(S)) {
    if (DS->isSingleDecl()) {
      if (auto *Var = dyn_cast_or_null<VarDecl>(DS->getSingleDecl())) {
        if (Var->hasInit() && !Var->getType()->isReferenceType()) {
          // Accept non-canonical init form here but emit ext. warning.
          if (Var->getInitStyle() != VarDecl::CInit && EmitDiags)
            SemaRef.Diag(S->getBeginLoc(),
                         diag::ext_omp_loop_not_canonical_init)
                << S->getSourceRange();
          return setLCDeclAndLB(
              Var,
              buildDeclRefExpr(SemaRef, Var,
                               Var->getType().getNonReferenceType(),
                               DS->getBeginLoc()),
              Var->getInit(), EmitDiags);
        }
      }
    }
  } else if (auto *CE = dyn_cast<CXXOperatorCallExpr>(S)) {
    if (CE->getOperator() == OO_Equal) {
      Expr *LHS = CE->getArg(0);
      if (auto *DRE = dyn_cast<DeclRefExpr>(LHS)) {
        //if (auto *CED = dyn_cast<OMPCapturedExprDecl>(DRE->getDecl()))
        if (auto *CED = dyn_cast<ApproxCapturedExprDecl>(DRE->getDecl()))
        //if (auto *CED = dyn_cast<VarDecl>(DRE->getDecl()))
          if (auto *ME = dyn_cast<MemberExpr>(getExprAsWritten(CED->getInit())))
            return setLCDeclAndLB(ME->getMemberDecl(), ME, BO->getRHS(),
                                  EmitDiags);
        return setLCDeclAndLB(DRE->getDecl(), DRE, CE->getArg(1), EmitDiags);
      }
      if (auto *ME = dyn_cast<MemberExpr>(LHS)) {
        if (ME->isArrow() &&
            isa<CXXThisExpr>(ME->getBase()->IgnoreParenImpCasts()))
          return setLCDeclAndLB(ME->getMemberDecl(), ME, BO->getRHS(),
                                EmitDiags);
      }
    }
  }

  if (dependent() || SemaRef.CurContext->isDependentContext())
    return false;
  if (EmitDiags) {
    SemaRef.Diag(S->getBeginLoc(), diag::err_omp_loop_not_canonical_init)
        << S->getSourceRange();
  }
  return true;
}

/// Build a variable declaration for loop variables.
static VarDecl *buildVarDecl(Sema &SemaRef, SourceLocation Loc, QualType Type,
                             StringRef Name, const AttrVec *Attrs = nullptr,
                             DeclRefExpr *OrigRef = nullptr) {
  DeclContext *DC = SemaRef.CurContext;
  IdentifierInfo *II = &SemaRef.PP.getIdentifierTable().get(Name);
  TypeSourceInfo *TInfo = SemaRef.Context.getTrivialTypeSourceInfo(Type, Loc);
  auto *Decl =
      VarDecl::Create(SemaRef.Context, DC, Loc, Loc, II, Type, TInfo, SC_None);
  if (Attrs) {
    for (specific_attr_iterator<AlignedAttr> I(Attrs->begin()), E(Attrs->end());
         I != E; ++I)
      Decl->addAttr(*I);
  }
  Decl->setImplicit();

  return Decl;
}

bool ApproxIterationSpaceChecker::checkAndSetIncRHS(Expr *RHS) {
  // RHS of canonical loop form increment can be:
  //   var + incr
  //   incr + var
  //   var - incr
  //
  RHS = RHS->IgnoreParenImpCasts();
  if (auto *BO = dyn_cast<BinaryOperator>(RHS)) {
    if (BO->isAdditiveOp()) {
      bool IsAdd = BO->getOpcode() == BO_Add;
      if (getInitLCDecl(BO->getLHS()) == LCDecl)
        return setStep(BO->getRHS(), !IsAdd);
      if (IsAdd && getInitLCDecl(BO->getRHS()) == LCDecl)
        return setStep(BO->getLHS(), /*Subtract=*/false);
    }
  } else if (auto *CE = dyn_cast<CXXOperatorCallExpr>(RHS)) {
    bool IsAdd = CE->getOperator() == OO_Plus;
    if ((IsAdd || CE->getOperator() == OO_Minus) && CE->getNumArgs() == 2) {
      if (getInitLCDecl(CE->getArg(0)) == LCDecl)
        return setStep(CE->getArg(1), !IsAdd);
      if (IsAdd && getInitLCDecl(CE->getArg(1)) == LCDecl)
        return setStep(CE->getArg(0), /*Subtract=*/false);
    }
  }
  if (dependent() || SemaRef.CurContext->isDependentContext())
    return false;
  SemaRef.Diag(RHS->getBeginLoc(), diag::err_omp_loop_not_canonical_incr)
      << RHS->getSourceRange() << LCDecl;
  return true;
}

bool ApproxIterationSpaceChecker::checkAndSetInc(Expr *S) {
  // Check incr-expr for canonical loop form and return true if it
  // does not conform.
  // OpenMP [2.6] Canonical loop form. Test-expr may be one of the following:
  //   ++var
  //   var++
  //   --var
  //   var--
  //   var += incr
  //   var -= incr
  //   var = var + incr
  //   var = incr + var
  //   var = var - incr
  //
  if (!S) {
    SemaRef.Diag(DefaultLoc, diag::err_omp_loop_not_canonical_incr) << LCDecl;
    return true;
  }
  if (auto *ExprTemp = dyn_cast<ExprWithCleanups>(S))
    if (!ExprTemp->cleanupsHaveSideEffects())
      S = ExprTemp->getSubExpr();

  IncrementSrcRange = S->getSourceRange();
  S = S->IgnoreParens();
  if (auto *UO = dyn_cast<UnaryOperator>(S)) {
    if (UO->isIncrementDecrementOp() &&
        getInitLCDecl(UO->getSubExpr()) == LCDecl)
      return setStep(SemaRef
                         .ActOnIntegerConstant(UO->getBeginLoc(),
                                               (UO->isDecrementOp() ? -1 : 1))
                         .get(),
                     /*Subtract=*/false);
  } else if (auto *BO = dyn_cast<BinaryOperator>(S)) {
    switch (BO->getOpcode()) {
    case BO_AddAssign:
    case BO_SubAssign:
      if (getInitLCDecl(BO->getLHS()) == LCDecl)
        return setStep(BO->getRHS(), BO->getOpcode() == BO_SubAssign);
      break;
    case BO_Assign:
      if (getInitLCDecl(BO->getLHS()) == LCDecl)
        return checkAndSetIncRHS(BO->getRHS());
      break;
    default:
      break;
    }
  } else if (auto *CE = dyn_cast<CXXOperatorCallExpr>(S)) {
    switch (CE->getOperator()) {
    case OO_PlusPlus:
    case OO_MinusMinus:
      if (getInitLCDecl(CE->getArg(0)) == LCDecl)
        return setStep(SemaRef
                           .ActOnIntegerConstant(
                               CE->getBeginLoc(),
                               ((CE->getOperator() == OO_MinusMinus) ? -1 : 1))
                           .get(),
                       /*Subtract=*/false);
      break;
    case OO_PlusEqual:
    case OO_MinusEqual:
      if (getInitLCDecl(CE->getArg(0)) == LCDecl)
        return setStep(CE->getArg(1), CE->getOperator() == OO_MinusEqual);
      break;
    case OO_Equal:
      if (getInitLCDecl(CE->getArg(0)) == LCDecl)
        return checkAndSetIncRHS(CE->getArg(1));
      break;
    default:
      break;
    }
  }
  if (dependent() || SemaRef.CurContext->isDependentContext())
    return false;
  SemaRef.Diag(S->getBeginLoc(), diag::err_omp_loop_not_canonical_incr)
      << S->getSourceRange() << LCDecl;
  return true;
}

/// Build reference expression to the counter be used for codegen.
DeclRefExpr *ApproxIterationSpaceChecker::buildCounterVar(
    llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const {
  auto *VD = dyn_cast<VarDecl>(LCDecl);
  if (!VD) {
    errs() << "Counter variable must be declared\n";
    abort();
  }

  return cast<DeclRefExpr>(LCRef);
}

/// Build 'VarRef = Start.
static ExprResult
buildCounterInit(Sema &SemaRef, Scope *S, SourceLocation Loc, ExprResult VarRef,
                 ExprResult Start, bool IsNonRectangularLB,
                 llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) {
  // Build 'VarRef = Start.
  ExprResult NewStart = IsNonRectangularLB
                            ? Start.get()
                            : tryBuildCapture(SemaRef, Start.get(), Captures);
  if (!NewStart.isUsable())
    return ExprError();
  if (!SemaRef.Context.hasSameType(NewStart.get()->getType(),
                                   VarRef.get()->getType())) {
    NewStart = SemaRef.PerformImplicitConversion(
        NewStart.get(), VarRef.get()->getType(), Sema::AA_Converting,
        /*AllowExplicit=*/true);
    if (!NewStart.isUsable())
      return ExprError();
  }

  ExprResult Init =
      SemaRef.BuildBinOp(S, Loc, BO_Assign, VarRef.get(), NewStart.get());
  return Init;
}

/// Build 'VarRef = Start + Iter * Step'.
static ExprResult buildCounterUpdate(
    Sema &SemaRef, Scope *S, SourceLocation Loc, ExprResult VarRef,
    ExprResult Start, ExprResult Iter, ExprResult Step, bool Subtract,
    bool IsNonRectangularLB,
    llvm::MapVector<const Expr *, DeclRefExpr *> *Captures = nullptr) {
  // Add parentheses (for debugging purposes only).
  Iter = SemaRef.ActOnParenExpr(Loc, Loc, Iter.get());
  if (!VarRef.isUsable() || !Start.isUsable() || !Iter.isUsable() ||
      !Step.isUsable())
    return ExprError();

  ExprResult NewStep = Step;
  if (Captures)
    NewStep = tryBuildCapture(SemaRef, Step.get(), *Captures);
  if (NewStep.isInvalid())
    return ExprError();
  ExprResult Update =
      SemaRef.BuildBinOp(S, Loc, BO_Mul, Iter.get(), NewStep.get());
  if (!Update.isUsable())
    return ExprError();

  // Try to build 'VarRef = Start, VarRef (+|-)= Iter * Step' or
  // 'VarRef = Start (+|-) Iter * Step'.
  if (!Start.isUsable())
    return ExprError();
  ExprResult NewStart = SemaRef.ActOnParenExpr(Loc, Loc, Start.get());
  if (!NewStart.isUsable())
    return ExprError();
  if (Captures && !IsNonRectangularLB)
    NewStart = tryBuildCapture(SemaRef, Start.get(), *Captures);
  if (NewStart.isInvalid())
    return ExprError();

  // First attempt: try to build 'VarRef = Start, VarRef += Iter * Step'.
  ExprResult SavedUpdate = Update;
  ExprResult UpdateVal;
  if (VarRef.get()->getType()->isOverloadableType() ||
      NewStart.get()->getType()->isOverloadableType() ||
      Update.get()->getType()->isOverloadableType()) {
    Sema::TentativeAnalysisScope Trap(SemaRef);

    Update =
        SemaRef.BuildBinOp(S, Loc, BO_Assign, VarRef.get(), NewStart.get());
    if (Update.isUsable()) {
      UpdateVal =
          SemaRef.BuildBinOp(S, Loc, Subtract ? BO_SubAssign : BO_AddAssign,
                             VarRef.get(), SavedUpdate.get());
      if (UpdateVal.isUsable()) {
        Update = SemaRef.CreateBuiltinBinOp(Loc, BO_Comma, Update.get(),
                                            UpdateVal.get());
      }
    }
  }

  // Second attempt: try to build 'VarRef = Start (+|-) Iter * Step'.
  if (!Update.isUsable() || !UpdateVal.isUsable()) {
    Update = SemaRef.BuildBinOp(S, Loc, Subtract ? BO_Sub : BO_Add,
                                NewStart.get(), SavedUpdate.get());
    if (!Update.isUsable())
      return ExprError();

    if (!SemaRef.Context.hasSameType(Update.get()->getType(),
                                     VarRef.get()->getType())) {
      Update = SemaRef.PerformImplicitConversion(
          Update.get(), VarRef.get()->getType(), Sema::AA_Converting, true);
      if (!Update.isUsable())
        return ExprError();
    }

    Update = SemaRef.BuildBinOp(S, Loc, BO_Assign, VarRef.get(), Update.get());
  }
  return Update;
}

Expr *ApproxIterationSpaceChecker::buildPreCond(
    Scope *S, Expr *Cond,
    llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const {
  // Do not build a precondition when the condition/initialization is dependent
  // to prevent pessimistic early loop exit.
  // TODO: this can be improved by calculating min/max values but not sure that
  // it will be very effective.
  if (CondDependOnLC || InitDependOnLC)
    return SemaRef.PerformImplicitConversion(
        SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get(),
        SemaRef.Context.BoolTy, /*Action=*/Sema::AA_Casting,
        /*AllowExplicit=*/true).get();

  // Try to build LB <op> UB, where <op> is <, >, <=, or >=.
  Sema::TentativeAnalysisScope Trap(SemaRef);

  ExprResult NewLB = tryBuildCapture(SemaRef, LB, Captures);
  ExprResult NewUB = tryBuildCapture(SemaRef, UB, Captures);
  if (!NewLB.isUsable() || !NewUB.isUsable())
    return nullptr;

  ExprResult CondExpr =
      SemaRef.BuildBinOp(S, DefaultLoc,
                         TestIsLessOp.getValue() ?
                           (TestIsStrictOp ? BO_LT : BO_LE) :
                           (TestIsStrictOp ? BO_GT : BO_GE),
                         NewLB.get(), NewUB.get());
  if (CondExpr.isUsable()) {
    if (!SemaRef.Context.hasSameUnqualifiedType(CondExpr.get()->getType(),
                                                SemaRef.Context.BoolTy))
      CondExpr = SemaRef.PerformImplicitConversion(
          CondExpr.get(), SemaRef.Context.BoolTy, /*Action=*/Sema::AA_Casting,
          /*AllowExplicit=*/true);
  }

  // Otherwise use original loop condition and evaluate it in runtime.
  return CondExpr.isUsable() ? CondExpr.get() : Cond;
}
/// Build initialization of the counter to be used for codegen.
Expr *ApproxIterationSpaceChecker::buildCounterInit() const { return LB; }

/// Build step of the counter be used for codegen.
Expr *ApproxIterationSpaceChecker::buildCounterStep() const { return Step; }

/// Convert integer expression \a E to make it have at least \a Bits
/// bits.
static ExprResult widenIterationCount(unsigned Bits, Expr *E, Sema &SemaRef) {
  if (E == nullptr)
    return ExprError();
  ASTContext &C = SemaRef.Context;
  QualType OldType = E->getType();
  unsigned HasBits = C.getTypeSize(OldType);
  if (HasBits >= Bits)
    return ExprResult(E);
  // OK to convert to signed, because new type has more bits than old.
  QualType NewType = C.getIntTypeForBitwidth(Bits, /* Signed */ true);
  return SemaRef.PerformImplicitConversion(E, NewType, Sema::AA_Converting,
                                           true);
}

/// Check if the given expression \a E is a constant integer that fits
/// into \a Bits bits.
static bool fitsInto(unsigned Bits, bool Signed, const Expr *E, Sema &SemaRef) {
  if (E == nullptr)
    return false;
  llvm::APSInt Result;
  if (E->isIntegerConstantExpr(SemaRef.Context))
    {
      Result = E->getIntegerConstantExpr(SemaRef.Context).getValue();
      return Signed ? Result.isSignedIntN(Bits) : Result.isIntN(Bits);
    }
  return false;
}

/// Calculate number of iterations, transforming to unsigned, if number of
/// iterations may be larger than the original type.
static Expr *
calculateNumIters(Sema &SemaRef, Scope *S, SourceLocation DefaultLoc,
                  Expr *Lower, Expr *Upper, Expr *Step, QualType LCTy,
                  bool TestIsStrictOp, bool RoundToStep,
                  llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) {
  ExprResult NewStep = tryBuildCapture(SemaRef, Step, Captures);
  if (!NewStep.isUsable())
    return nullptr;
  llvm::APSInt LRes, URes, SRes;
  bool IsLowerConst = Lower->isIntegerConstantExpr(SemaRef.Context);
  if(IsLowerConst)
    LRes = Lower->getIntegerConstantExpr(SemaRef.Context).getValue();
  bool IsStepConst = Step->isIntegerConstantExpr(SemaRef.Context);
  if(IsStepConst)
    SRes = Step->getIntegerConstantExpr(SemaRef.Context).getValue();
  bool NoNeedToConvert = IsLowerConst && !RoundToStep &&
                         ((!TestIsStrictOp && LRes.isNonNegative()) ||
                          (TestIsStrictOp && LRes.isStrictlyPositive()));
  bool NeedToReorganize = false;
  // Check if any subexpressions in Lower -Step [+ 1] lead to overflow.
  if (!NoNeedToConvert && IsLowerConst &&
      (TestIsStrictOp || (RoundToStep && IsStepConst))) {
    NoNeedToConvert = true;
    if (RoundToStep) {
      unsigned BW = LRes.getBitWidth() > SRes.getBitWidth()
                        ? LRes.getBitWidth()
                        : SRes.getBitWidth();
      LRes = LRes.extend(BW + 1);
      LRes.setIsSigned(true);
      SRes = SRes.extend(BW + 1);
      SRes.setIsSigned(true);
      LRes -= SRes;
      NoNeedToConvert = LRes.trunc(BW).extend(BW + 1) == LRes;
      LRes = LRes.trunc(BW);
    }
    if (TestIsStrictOp) {
      unsigned BW = LRes.getBitWidth();
      LRes = LRes.extend(BW + 1);
      LRes.setIsSigned(true);
      ++LRes;
      NoNeedToConvert =
          NoNeedToConvert && LRes.trunc(BW).extend(BW + 1) == LRes;
      // truncate to the original bitwidth.
      LRes = LRes.trunc(BW);
    }
    NeedToReorganize = NoNeedToConvert;
  }
  bool IsUpperConst = Upper->isIntegerConstantExpr(SemaRef.Context);
  if(IsUpperConst)
    URes = Upper->getIntegerConstantExpr(SemaRef.Context).getValue();
  if (NoNeedToConvert && IsLowerConst && IsUpperConst &&
      (!RoundToStep || IsStepConst)) {
    unsigned BW = LRes.getBitWidth() > URes.getBitWidth() ? LRes.getBitWidth()
                                                          : URes.getBitWidth();
    LRes = LRes.extend(BW + 1);
    LRes.setIsSigned(true);
    URes = URes.extend(BW + 1);
    URes.setIsSigned(true);
    URes -= LRes;
    NoNeedToConvert = URes.trunc(BW).extend(BW + 1) == URes;
    NeedToReorganize = NoNeedToConvert;
  }
  // If the boundaries are not constant or (Lower - Step [+ 1]) is not constant
  // or less than zero (Upper - (Lower - Step [+ 1]) may overflow) - promote to
  // unsigned.
  if ((!NoNeedToConvert || (LRes.isNegative() && !IsUpperConst)) &&
      !LCTy->isDependentType() && LCTy->isIntegerType()) {
    QualType LowerTy = Lower->getType();
    QualType UpperTy = Upper->getType();
    uint64_t LowerSize = SemaRef.Context.getTypeSize(LowerTy);
    uint64_t UpperSize = SemaRef.Context.getTypeSize(UpperTy);
    if ((LowerSize <= UpperSize && UpperTy->hasSignedIntegerRepresentation()) ||
        (LowerSize > UpperSize && LowerTy->hasSignedIntegerRepresentation())) {
      QualType CastType = SemaRef.Context.getIntTypeForBitwidth(
          LowerSize > UpperSize ? LowerSize : UpperSize, /*Signed=*/0);
      Upper =
          SemaRef
              .PerformImplicitConversion(
                  SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Upper).get(),
                  CastType, Sema::AA_Converting)
              .get();
      Lower = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Lower).get();
      NewStep = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, NewStep.get());
    }
  }
  if (!Lower || !Upper || NewStep.isInvalid())
    return nullptr;

  ExprResult Diff;
  // If need to reorganize, then calculate the form as Upper - (Lower - Step [+
  // 1]).
  if (NeedToReorganize) {
    Diff = Lower;

    if (RoundToStep) {
      // Lower - Step
      Diff =
          SemaRef.BuildBinOp(S, DefaultLoc, BO_Sub, Diff.get(), NewStep.get());
      if (!Diff.isUsable())
        return nullptr;
    }

    // Lower - Step [+ 1]
    if (TestIsStrictOp)
      Diff = SemaRef.BuildBinOp(
          S, DefaultLoc, BO_Add, Diff.get(),
          SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get());
    if (!Diff.isUsable())
      return nullptr;

    Diff = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Diff.get());
    if (!Diff.isUsable())
      return nullptr;

    // Upper - (Lower - Step [+ 1]).
    Diff = SemaRef.BuildBinOp(S, DefaultLoc, BO_Sub, Upper, Diff.get());
    if (!Diff.isUsable())
      return nullptr;
  } else {
    Diff = SemaRef.BuildBinOp(S, DefaultLoc, BO_Sub, Upper, Lower);

    if (!Diff.isUsable() && LCTy->getAsCXXRecordDecl()) {
      // BuildBinOp already emitted error, this one is to point user to upper
      // and lower bound, and to tell what is passed to 'operator-'.
      SemaRef.Diag(Upper->getBeginLoc(), diag::err_omp_loop_diff_cxx)
          << Upper->getSourceRange() << Lower->getSourceRange();
      return nullptr;
    }

    if (!Diff.isUsable())
      return nullptr;

    // Upper - Lower [- 1]
    if (TestIsStrictOp)
      Diff = SemaRef.BuildBinOp(
          S, DefaultLoc, BO_Sub, Diff.get(),
          SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get());
    if (!Diff.isUsable())
      return nullptr;

    if (RoundToStep) {
      // Upper - Lower [- 1] + Step
      Diff =
          SemaRef.BuildBinOp(S, DefaultLoc, BO_Add, Diff.get(), NewStep.get());
      if (!Diff.isUsable())
        return nullptr;
    }
  }

  // Parentheses (for dumping/debugging purposes only).
  Diff = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Diff.get());
  if (!Diff.isUsable())
    return nullptr;

  // (Upper - Lower [- 1] + Step) / Step or (Upper - Lower) / Step
  Diff = SemaRef.BuildBinOp(S, DefaultLoc, BO_Div, Diff.get(), NewStep.get());
  if (!Diff.isUsable())
    return nullptr;

  return Diff.get();
}

/// Build the expression to calculate the number of iterations.
Expr *ApproxIterationSpaceChecker::buildNumIterations(
    Scope *S, LoopIterationSpace &ResultIterSpace, bool LimitedType,
    llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const {
  QualType VarType = LCDecl->getType().getNonReferenceType();
  if (!VarType->isIntegerType() && !VarType->isPointerType() &&
      !SemaRef.getLangOpts().CPlusPlus)
    return nullptr;
  Expr *LBVal = LB;
  Expr *UBVal = UB;
  // LB = TestIsLessOp.getValue() ? min(LB(MinVal), LB(MaxVal)) :
  // max(LB(MinVal), LB(MaxVal))
  if (InitDependOnLC) {
    const LoopIterationSpace &IS = ResultIterSpace;
    if (!IS.MinValue || !IS.MaxValue)
      return nullptr;
    // OuterVar = Min
    ExprResult MinValue =
        SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, IS.MinValue);
    if (!MinValue.isUsable())
      return nullptr;

    ExprResult LBMinVal = SemaRef.BuildBinOp(S, DefaultLoc, BO_Assign,
                                             IS.CounterVar, MinValue.get());
    if (!LBMinVal.isUsable())
      return nullptr;
    // OuterVar = Min, LBVal
    LBMinVal =
        SemaRef.BuildBinOp(S, DefaultLoc, BO_Comma, LBMinVal.get(), LBVal);
    if (!LBMinVal.isUsable())
      return nullptr;
    // (OuterVar = Min, LBVal)
    LBMinVal = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, LBMinVal.get());
    if (!LBMinVal.isUsable())
      return nullptr;

    // OuterVar = Max
    ExprResult MaxValue =
        SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, IS.MaxValue);
    if (!MaxValue.isUsable())
      return nullptr;

    ExprResult LBMaxVal = SemaRef.BuildBinOp(S, DefaultLoc, BO_Assign,
                                             IS.CounterVar, MaxValue.get());
    if (!LBMaxVal.isUsable())
      return nullptr;
    // OuterVar = Max, LBVal
    LBMaxVal =
        SemaRef.BuildBinOp(S, DefaultLoc, BO_Comma, LBMaxVal.get(), LBVal);
    if (!LBMaxVal.isUsable())
      return nullptr;
    // (OuterVar = Max, LBVal)
    LBMaxVal = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, LBMaxVal.get());
    if (!LBMaxVal.isUsable())
      return nullptr;

    Expr *LBMin = tryBuildCapture(SemaRef, LBMinVal.get(), Captures).get();
    Expr *LBMax = tryBuildCapture(SemaRef, LBMaxVal.get(), Captures).get();
    if (!LBMin || !LBMax)
      return nullptr;
    // LB(MinVal) < LB(MaxVal)
    ExprResult MinLessMaxRes =
        SemaRef.BuildBinOp(S, DefaultLoc, BO_LT, LBMin, LBMax);
    if (!MinLessMaxRes.isUsable())
      return nullptr;
    Expr *MinLessMax =
        tryBuildCapture(SemaRef, MinLessMaxRes.get(), Captures).get();
    if (!MinLessMax)
      return nullptr;
    if (TestIsLessOp.getValue()) {
      // LB(MinVal) < LB(MaxVal) ? LB(MinVal) : LB(MaxVal) - min(LB(MinVal),
      // LB(MaxVal))
      ExprResult MinLB = SemaRef.ActOnConditionalOp(DefaultLoc, DefaultLoc,
                                                    MinLessMax, LBMin, LBMax);
      if (!MinLB.isUsable())
        return nullptr;
      LBVal = MinLB.get();
    } else {
      // LB(MinVal) < LB(MaxVal) ? LB(MaxVal) : LB(MinVal) - max(LB(MinVal),
      // LB(MaxVal))
      ExprResult MaxLB = SemaRef.ActOnConditionalOp(DefaultLoc, DefaultLoc,
                                                    MinLessMax, LBMax, LBMin);
      if (!MaxLB.isUsable())
        return nullptr;
      LBVal = MaxLB.get();
    }
  }

  Expr *UBExpr = TestIsLessOp.getValue() ? UBVal : LBVal;
  Expr *LBExpr = TestIsLessOp.getValue() ? LBVal : UBVal;
  Expr *Upper = tryBuildCapture(SemaRef, UBExpr, Captures).get();
  Expr *Lower = tryBuildCapture(SemaRef, LBExpr, Captures).get();
  if (!Upper || !Lower)
    return nullptr;

  ExprResult Diff =
      calculateNumIters(SemaRef, S, DefaultLoc, Lower, Upper, Step, VarType,
                        TestIsStrictOp, /*RoundToStep=*/true, Captures);
  if (!Diff.isUsable())
    return nullptr;

  // OpenMP runtime requires 32-bit or 64-bit loop variables.
  QualType Type = Diff.get()->getType();
  ASTContext &C = SemaRef.Context;
  bool UseVarType = VarType->hasIntegerRepresentation() &&
                    C.getTypeSize(Type) > C.getTypeSize(VarType);
  if (!Type->isIntegerType() || UseVarType) {
    unsigned NewSize =
        UseVarType ? C.getTypeSize(VarType) : C.getTypeSize(Type);
    bool IsSigned = UseVarType ? VarType->hasSignedIntegerRepresentation()
                               : Type->hasSignedIntegerRepresentation();
    Type = C.getIntTypeForBitwidth(NewSize, IsSigned);
    if (!SemaRef.Context.hasSameType(Diff.get()->getType(), Type)) {
      Diff = SemaRef.PerformImplicitConversion(
          Diff.get(), Type, Sema::AA_Converting, /*AllowExplicit=*/true);
      if (!Diff.isUsable())
        return nullptr;
    }
  }
  if (LimitedType) {
    unsigned NewSize = (C.getTypeSize(Type) > 32) ? 64 : 32;
    if (NewSize != C.getTypeSize(Type)) {
      if (NewSize < C.getTypeSize(Type)) {
        assert(NewSize == 64 && "incorrect loop var size");
        SemaRef.Diag(DefaultLoc, diag::warn_omp_loop_64_bit_var)
            << InitSrcRange << ConditionSrcRange;
      }
      QualType NewType = C.getIntTypeForBitwidth(
          NewSize, Type->hasSignedIntegerRepresentation() ||
                       C.getTypeSize(Type) < NewSize);
      if (!SemaRef.Context.hasSameType(Diff.get()->getType(), NewType)) {
        Diff = SemaRef.PerformImplicitConversion(Diff.get(), NewType,
                                                 Sema::AA_Converting, true);
        if (!Diff.isUsable())
          return nullptr;
      }
    }
  }

  return Diff.get();
}

std::pair<Expr *, Expr *> ApproxIterationSpaceChecker::buildMinMaxValues(
    Scope *S, llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) const {
  // Do not build for iterators, they cannot be used in non-rectangular loop
  // nests.
  if (LCDecl->getType()->isRecordType())
    return std::make_pair(nullptr, nullptr);
  // If we subtract, the min is in the condition, otherwise the min is in the
  // init value.
  Expr *MinExpr = nullptr;
  Expr *MaxExpr = nullptr;
  Expr *LBExpr = TestIsLessOp.getValue() ? LB : UB;
  Expr *UBExpr = TestIsLessOp.getValue() ? UB : LB;
  bool LBNonRect = TestIsLessOp.getValue() ? InitDependOnLC.hasValue()
                                           : CondDependOnLC.hasValue();
  bool UBNonRect = TestIsLessOp.getValue() ? CondDependOnLC.hasValue()
                                           : InitDependOnLC.hasValue();
  Expr *Lower =
      LBNonRect ? LBExpr : tryBuildCapture(SemaRef, LBExpr, Captures).get();
  Expr *Upper =
      UBNonRect ? UBExpr : tryBuildCapture(SemaRef, UBExpr, Captures).get();
  if (!Upper || !Lower)
    return std::make_pair(nullptr, nullptr);

  if (TestIsLessOp.getValue())
    MinExpr = Lower;
  else
    MaxExpr = Upper;

  // Build minimum/maximum value based on number of iterations.
  QualType VarType = LCDecl->getType().getNonReferenceType();

  ExprResult Diff =
      calculateNumIters(SemaRef, S, DefaultLoc, Lower, Upper, Step, VarType,
                        TestIsStrictOp, /*RoundToStep=*/false, Captures);
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  // ((Upper - Lower [- 1]) / Step) * Step
  // Parentheses (for dumping/debugging purposes only).
  Diff = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Diff.get());
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  ExprResult NewStep = tryBuildCapture(SemaRef, Step, Captures);
  if (!NewStep.isUsable())
    return std::make_pair(nullptr, nullptr);
  Diff = SemaRef.BuildBinOp(S, DefaultLoc, BO_Mul, Diff.get(), NewStep.get());
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  // Parentheses (for dumping/debugging purposes only).
  Diff = SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Diff.get());
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  // Convert to the ptrdiff_t, if original type is pointer.
  if (VarType->isAnyPointerType() &&
      !SemaRef.Context.hasSameType(
          Diff.get()->getType(),
          SemaRef.Context.getUnsignedPointerDiffType())) {
    Diff = SemaRef.PerformImplicitConversion(
        Diff.get(), SemaRef.Context.getUnsignedPointerDiffType(),
        Sema::AA_Converting, /*AllowExplicit=*/true);
  }
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  if (TestIsLessOp.getValue()) {
    // MinExpr = Lower;
    // MaxExpr = Lower + (((Upper - Lower [- 1]) / Step) * Step)
    Diff = SemaRef.BuildBinOp(
        S, DefaultLoc, BO_Add,
        SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Lower).get(),
        Diff.get());
    if (!Diff.isUsable())
      return std::make_pair(nullptr, nullptr);
  } else {
    // MaxExpr = Upper;
    // MinExpr = Upper - (((Upper - Lower [- 1]) / Step) * Step)
    Diff = SemaRef.BuildBinOp(
        S, DefaultLoc, BO_Sub,
        SemaRef.ActOnParenExpr(DefaultLoc, DefaultLoc, Upper).get(),
        Diff.get());
    if (!Diff.isUsable())
      return std::make_pair(nullptr, nullptr);
  }

  // Convert to the original type.
  if (SemaRef.Context.hasSameType(Diff.get()->getType(), VarType))
    Diff = SemaRef.PerformImplicitConversion(Diff.get(), VarType,
                                             Sema::AA_Converting,
                                             /*AllowExplicit=*/true);
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  Diff = SemaRef.ActOnFinishFullExpr(Diff.get(), /*DiscardedValue=*/false);
  if (!Diff.isUsable())
    return std::make_pair(nullptr, nullptr);

  if (TestIsLessOp.getValue())
    MaxExpr = Diff.get();
  else
    MinExpr = Diff.get();

  return std::make_pair(MinExpr, MaxExpr);
}

/// Called on a for stmt to check and extract its iteration space
/// for further processing (such as collapsing).
static bool checkApproxIterationSpace(
    Stmt *S, Sema &SemaRef, LoopIterationSpace &ResultIterSpace,
    llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) {
  // OpenMP [2.9.1, Canonical Loop Form]
  //   for (init-expr; test-expr; incr-expr) structured-block
  //   for (range-decl: range-expr) structured-block
  auto *For = dyn_cast_or_null<ForStmt>(S);
  auto *CXXFor = dyn_cast_or_null<CXXForRangeStmt>(S);

  assert(((For && For->getBody()) || (CXXFor && CXXFor->getBody())) &&
         "No loop body.");

  ApproxIterationSpaceChecker ISC(SemaRef,
                                  For ? For->getForLoc() : CXXFor->getForLoc());

  // Check init.
  Stmt *Init = For ? For->getInit() : CXXFor->getBeginStmt();
  if (ISC.checkAndSetInit(Init))
    return true;

  bool HasErrors = false;

  // Check loop variable's type.
  if (ValueDecl *LCDecl = ISC.getLoopDecl()) {
    // OpenMP [2.6, Canonical Loop Form]
    // Var is one of the following:
    //   A variable of signed or unsigned integer type.
    //   For C++, a variable of a random access iterator type.
    //   For C, a variable of a pointer type.
    QualType VarType = LCDecl->getType().getNonReferenceType();
    if (!VarType->isDependentType() && !VarType->isIntegerType() &&
        !VarType->isPointerType() &&
        !(SemaRef.getLangOpts().CPlusPlus && VarType->isOverloadableType())) {
      SemaRef.Diag(Init->getBeginLoc(), diag::err_omp_loop_variable_type)
          << SemaRef.getLangOpts().CPlusPlus;
      HasErrors = true;
    }

    // OpenMP, 2.14.1.1 Data-sharing Attribute Rules for Variables Referenced in
    // a Construct
    // The loop iteration variable(s) in the associated for-loop(s) of a for or
    // parallel for construct is (are) private.
    // The loop iteration variable in the associated for-loop of a simd
    // construct with just one associated for-loop is linear with a
    // constant-linear-step that is the increment of the associated for-loop.
    // Exclude loop var from the list of variables with implicitly defined data
    // sharing attributes.
    //VarsWithImplicitDSA.erase(LCDecl);

    // Check test-expr.
    HasErrors |= ISC.checkAndSetCond(For ? For->getCond() : CXXFor->getCond());

    // Check incr-expr.
    HasErrors |= ISC.checkAndSetInc(For ? For->getInc() : CXXFor->getInc());
  }

  if (ISC.dependent() || SemaRef.CurContext->isDependentContext() || HasErrors)
    return HasErrors;

  Scope *CurScope = SemaRef.getCurScope();

  // Build the loop's iteration space representation.
  ResultIterSpace.PreCond = ISC.buildPreCond(
      CurScope, For ? For->getCond() : CXXFor->getCond(), Captures);
  ResultIterSpace.NumIterations =
      ISC.buildNumIterations(CurScope, ResultIterSpace,
                             /* LimitedType */ true, Captures);
  ResultIterSpace.CounterVar = ISC.buildCounterVar(Captures);
  // ResultIterSpace.PrivateCounterVar = ISC.buildPrivateCounterVar();
  ResultIterSpace.CounterInit = ISC.buildCounterInit();
  ResultIterSpace.CounterStep = ISC.buildCounterStep();
  ResultIterSpace.InitSrcRange = ISC.getInitSrcRange();
  ResultIterSpace.CondSrcRange = ISC.getConditionSrcRange();
  ResultIterSpace.IncSrcRange = ISC.getIncrementSrcRange();
  ResultIterSpace.Subtract = ISC.shouldSubtractStep();
  ResultIterSpace.IsStrictCompare = ISC.isStrictTestOp();
  std::tie(ResultIterSpace.MinValue, ResultIterSpace.MaxValue) =
      ISC.buildMinMaxValues(CurScope, Captures);
  //ResultIterSpace.FinalCondition = ISC.buildFinalCondition(CurScope);
  ResultIterSpace.IsNonRectangularLB = ISC.doesInitDependOnLC();
  ResultIterSpace.IsNonRectangularUB = ISC.doesCondDependOnLC();
  ResultIterSpace.LoopDependentIdx = ISC.getLoopDependentIdx();

  HasErrors |=
      (ResultIterSpace.PreCond == nullptr ||
       ResultIterSpace.NumIterations == nullptr ||
       ResultIterSpace.CounterVar == nullptr ||
       ResultIterSpace.CounterInit == nullptr ||
       ResultIterSpace.CounterStep == nullptr);

  return HasErrors;
}

/// Called on a for stmt to check itself.
/// \return Returns 0 if it is not canonical for loop, else 1.
static unsigned checkApproxLoop(Stmt *AStmt, Sema &SemaRef,
                            ApproxLoopHelperExprs &Built) {
  // LoopCount must be 1, 0 indicated there was an error.
  bool LoopCount = 1;
  // This is helper routine for loop directives (e.g., 'for', 'simd',
  // 'for simd', etc.).
  llvm::MapVector<const Expr *, DeclRefExpr *> Captures;
  LoopIterationSpace IterSpace;
  Stmt *CurStmt = AStmt->IgnoreContainers(/* IgnoreCaptured */ true);
  if (checkApproxIterationSpace(CurStmt, SemaRef, IterSpace, Captures))
    return 0;
  // Move on to the next nested for loop, or to the loop body.
  // OpenMP [2.8.1, simd construct, Restrictions]
  // All loops associated with the construct must be perfectly nested; that
  // is, there must be no intervening code nor any OpenMP directive between
  // any two loops.
  if (auto *For = dyn_cast<ForStmt>(CurStmt)) {
    CurStmt = For->getBody();
    } else {
      assert(isa<CXXForRangeStmt>(CurStmt) &&
             "Expected canonical for or range-based for loops.");
      CurStmt = cast<CXXForRangeStmt>(CurStmt)->getBody();
    }

  if (SemaRef.CurContext->isDependentContext())
    return LoopCount;

  // An example of what is generated for the following code:
  //
  //   #pragma omp simd collapse(2) ordered(2)
  //   for (i = 0; i < NI; ++i)
  //     for (k = 0; k < NK; ++k)
  //       for (j = J0; j < NJ; j+=2) {
  //         <loop body>
  //       }
  //
  // We generate the code below.
  // Note: the loop body may be outlined in CodeGen.
  // Note: some counters may be C++ classes, operator- is used to find number of
  // iterations and operator+= to calculate counter value.
  // Note: decltype(NumIterations) must be integer type (in 'omp for', only i32
  // or i64 is currently supported).
  //
  //   #define NumIterations (NI * ((NJ - J0 - 1 + 2) / 2))
  //   for (int[32|64]_t IV = 0; IV < NumIterations; ++IV ) {
  //     .local.i = IV / ((NJ - J0 - 1 + 2) / 2);
  //     .local.j = J0 + (IV % ((NJ - J0 - 1 + 2) / 2)) * 2;
  //     // similar updates for vars in clauses (e.g. 'linear')
  //     <loop body (using local i and j)>
  //   }
  //   i = NI; // assign final values of counters
  //   j = NJ;
  //

  // Last iteration number is (I1 * I2 * ... In) - 1, where I1, I2 ... In are
  // the iteration counts of the collapsed for loops.
  // Precondition tests if there is at least one iteration (all conditions are
  // true).
  auto PreCond = ExprResult(IterSpace.PreCond);
  Expr *N0 = IterSpace.NumIterations;
  ExprResult LastIteration32 =
      widenIterationCount(/*Bits=*/32,
                          SemaRef
                              .PerformImplicitConversion(
                                  N0->IgnoreImpCasts(), N0->getType(),
                                  Sema::AA_Converting, /*AllowExplicit=*/true)
                              .get(),
                          SemaRef);
  ExprResult LastIteration64 = widenIterationCount(
      /*Bits=*/64,
      SemaRef
          .PerformImplicitConversion(N0->IgnoreImpCasts(), N0->getType(),
                                     Sema::AA_Converting,
                                     /*AllowExplicit=*/true)
          .get(),
      SemaRef);

  if (!LastIteration32.isUsable() || !LastIteration64.isUsable())
    return LoopCount;

  ASTContext &C = SemaRef.Context;
  bool AllCountsNeedLessThan32Bits = C.getTypeSize(N0->getType()) < 32;

  Scope *CurScope = SemaRef.getCurScope();
  if (PreCond.isUsable()) {
    PreCond = SemaRef.BuildBinOp(CurScope, PreCond.get()->getExprLoc(), BO_LAnd,
                                 PreCond.get(), IterSpace.PreCond);
  }

  // Choose either the 32-bit or 64-bit version.
  ExprResult LastIteration = LastIteration64;
  if (SemaRef.getLangOpts().OpenMPOptimisticCollapse ||
      (LastIteration32.isUsable() &&
       C.getTypeSize(LastIteration32.get()->getType()) == 32 &&
       (AllCountsNeedLessThan32Bits || LoopCount == 1 ||
        fitsInto(
            /*Bits=*/32,
            LastIteration32.get()->getType()->hasSignedIntegerRepresentation(),
            LastIteration64.get(), SemaRef))))
    LastIteration = LastIteration32;
  QualType VType = LastIteration.get()->getType();
  QualType RealVType = VType;
  QualType StrideVType = VType;

  if (!LastIteration.isUsable())
    return 0;

  // Save the number of iterations.
  ExprResult NumIterations = LastIteration;
  {
    LastIteration = SemaRef.BuildBinOp(
        CurScope, LastIteration.get()->getExprLoc(), BO_Sub,
        LastIteration.get(),
        SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get());
    if (!LastIteration.isUsable())
      return 0;
  }

  // Calculate the last iteration number beforehand instead of doing this on
  // each iteration. Do not do this if the number of iterations may be kfold-ed.
  llvm::APSInt Result;
  bool IsConstant =
      LastIteration.get()->isIntegerConstantExpr(SemaRef.Context);
  if(IsConstant)
    Result = LastIteration.get()->getIntegerConstantExpr(SemaRef.Context).getValue();
  ExprResult CalcLastIteration;
  if (!IsConstant) {
    ExprResult SaveRef =
        tryBuildCapture(SemaRef, LastIteration.get(), Captures);
    LastIteration = SaveRef;

    // Prepare SaveRef + 1.
    NumIterations = SemaRef.BuildBinOp(
        CurScope, SaveRef.get()->getExprLoc(), BO_Add, SaveRef.get(),
        SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get());
    if (!NumIterations.isUsable())
      return 0;
  }

  SourceLocation InitLoc = IterSpace.InitSrcRange.getBegin();

  // Build variables passed into runtime, necessary for worksharing directives.
  ExprResult LB, UB, IL, ST, EUB;
  {
    // Lower bound variable, initialized with zero.
    VarDecl *LBDecl = buildVarDecl(SemaRef, InitLoc, VType, ".approx.lb");
    LB = buildDeclRefExpr(SemaRef, LBDecl, VType, InitLoc);
    SemaRef.AddInitializerToDecl(LBDecl,
                                 SemaRef.ActOnIntegerConstant(InitLoc, 0).get(),
                                 /*DirectInit*/ false);

    // Upper bound variable, initialized with last iteration number.
    VarDecl *UBDecl = buildVarDecl(SemaRef, InitLoc, VType, ".approx.ub");
    UB = buildDeclRefExpr(SemaRef, UBDecl, VType, InitLoc);
    SemaRef.AddInitializerToDecl(UBDecl, LastIteration.get(),
                                 /*DirectInit*/ false);

    // Stride variable returned by runtime (we initialize it to 1 by default).
    VarDecl *STDecl =
        buildVarDecl(SemaRef, InitLoc, StrideVType, ".approx.stride");
    ST = buildDeclRefExpr(SemaRef, STDecl, StrideVType, InitLoc);
    SemaRef.AddInitializerToDecl(STDecl,
                                 SemaRef.ActOnIntegerConstant(InitLoc, 1).get(),
                                 /*DirectInit*/ false);

    // Build expression: UB = min(UB, LastIteration)
    // It is necessary for CodeGen of directives with static scheduling.
    ExprResult IsUBGreater = SemaRef.BuildBinOp(CurScope, InitLoc, BO_GT,
                                                UB.get(), LastIteration.get());
    ExprResult CondOp = SemaRef.ActOnConditionalOp(
        LastIteration.get()->getExprLoc(), InitLoc, IsUBGreater.get(),
        LastIteration.get(), UB.get());
    EUB = SemaRef.BuildBinOp(CurScope, InitLoc, BO_Assign, UB.get(),
                             CondOp.get());
    EUB = SemaRef.ActOnFinishFullExpr(EUB.get(), /*DiscardedValue*/ false);
  }

  // Build the iteration variable and its initialization before loop.
  ExprResult IV;
  ExprResult Init;
  {
    VarDecl *IVDecl = buildVarDecl(SemaRef, InitLoc, RealVType, ".approx.iv");
    IV = buildDeclRefExpr(SemaRef, IVDecl, RealVType, InitLoc);
    Expr *RHS = LB.get();
    Init = SemaRef.BuildBinOp(CurScope, InitLoc, BO_Assign, IV.get(), RHS);
    Init = SemaRef.ActOnFinishFullExpr(Init.get(), /*DiscardedValue*/ false);
  }

  bool UseStrictCompare = RealVType->hasUnsignedIntegerRepresentation() &&
                          IterSpace.IsStrictCompare;
  // Loop condition (IV < NumIterations) or (IV <= UB or IV < UB + 1 (for
  // unsigned IV)) for worksharing loops.
  SourceLocation CondLoc = AStmt->getBeginLoc();
  Expr *BoundUB = UB.get();
  if (UseStrictCompare) {
    BoundUB =
        SemaRef
            .BuildBinOp(CurScope, CondLoc, BO_Add, BoundUB,
                        SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get())
            .get();
    BoundUB =
        SemaRef.ActOnFinishFullExpr(BoundUB, /*DiscardedValue*/ false).get();
  }
  ExprResult Cond = SemaRef.BuildBinOp(
      CurScope, CondLoc, UseStrictCompare ? BO_LT : BO_LE, IV.get(), BoundUB);

  // Loop increment (IV = IV + 1)
  SourceLocation IncLoc = AStmt->getBeginLoc();
  ExprResult Inc =
      SemaRef.BuildBinOp(CurScope, IncLoc, BO_Add, IV.get(),
                         SemaRef.ActOnIntegerConstant(IncLoc, 1).get());
  if (!Inc.isUsable())
    return 0;
  Inc = SemaRef.BuildBinOp(CurScope, IncLoc, BO_Assign, IV.get(), Inc.get());
  Inc = SemaRef.ActOnFinishFullExpr(Inc.get(), /*DiscardedValue*/ false);
  if (!Inc.isUsable())
    return 0;

  // Build updates and final values of the loop counters.
  bool HasErrors = false;
  {
    // We implement the following algorithm for obtaining the
    // original loop iteration variable values based on the
    // value of the collapsed loop iteration variable IV.
    //
    // Let n+1 be the number of collapsed loops in the nest.
    // Iteration variables (I0, I1, .... In)
    // Iteration counts (N0, N1, ... Nn)
    //
    // Acc = IV;
    //
    // To compute Ik for loop k, 0 <= k <= n, generate:
    //    Prod = N(k+1) * N(k+2) * ... * Nn;
    //    Ik = Acc / Prod;
    //    Acc -= Ik * Prod;
    //
    ExprResult Acc = IV;
    {
      LoopIterationSpace &IS = IterSpace;
      SourceLocation UpdLoc = IS.IncSrcRange.getBegin();
      ExprResult Iter;

      // Compute prod
      ExprResult Prod =
          SemaRef.ActOnIntegerConstant(SourceLocation(), 1).get();

      // Iter = Acc / Prod
      // If there is at least one more inner loop to avoid
      // multiplication by 1.
      Iter = Acc;
      if (!Iter.isUsable())
        return 0;

      // Update Acc:
      // Acc -= Iter * Prod
      // Check if there is at least one more inner loop to avoid
      // multiplication by 1.
      Prod = Iter;
      Acc = SemaRef.BuildBinOp(CurScope, UpdLoc, BO_Sub,
                               Acc.get(), Prod.get());

      // Build update: IS.CounterVar(Private) = IS.Start + Iter * IS.Step
      auto *VD = cast<VarDecl>(cast<DeclRefExpr>(IS.CounterVar)->getDecl());
      DeclRefExpr *CounterVar = buildDeclRefExpr(
          SemaRef, VD, IS.CounterVar->getType(), IS.CounterVar->getExprLoc(),
          /*RefersToCapture=*/true);
      ExprResult Init =
          buildCounterInit(SemaRef, CurScope, UpdLoc, CounterVar,
                           IS.CounterInit, IS.IsNonRectangularLB, Captures);
      if (!Init.isUsable())
        return 0;

      ExprResult Update = buildCounterUpdate(
          SemaRef, CurScope, UpdLoc, CounterVar, IS.CounterInit, Iter,
          IS.CounterStep, IS.Subtract, IS.IsNonRectangularLB, &Captures);
      if (!Update.isUsable())
        return 0;

      // Build final: IS.CounterVar = IS.Start + IS.NumIters * IS.Step
      ExprResult Final =
          buildCounterUpdate(SemaRef, CurScope, UpdLoc, CounterVar,
                             IS.CounterInit, IS.NumIterations, IS.CounterStep,
                             IS.Subtract, IS.IsNonRectangularLB, &Captures);
      if (!Final.isUsable())
        return 0;

      if (!Update.isUsable() || !Final.isUsable())
        return 0;

      // Save results.
      Built.Counter = IS.CounterVar;
      Built.CounterInit = Init.get();
      Built.CounterUpdate = Update.get();
    }
  }

  if (HasErrors)
    return 0;

  // Save results
  Built.IterationVarRef = IV.get();
  Built.LastIteration = LastIteration.get();
  Built.NumIterations = NumIterations.get();
  Built.CalcLastIteration = SemaRef
                                .ActOnFinishFullExpr(CalcLastIteration.get(),
                                                     /*DiscardedValue=*/false)
                                .get();
  Built.PreCond = PreCond.get();
  Built.PreInits = buildPreInits(C, Captures);
  Built.Cond = Cond.get();
  Built.Init = Init.get();
  Built.Inc = Inc.get();
  Built.LB = LB.get();
  Built.UB = UB.get();
  Built.IL = IL.get();
  Built.ST = ST.get();
  Built.EUB = EUB.get();
  Built.PerfoInc = nullptr;
  Built.PerfoSkip = nullptr;
  Built.OMPParallelForDir = nullptr;

  return LoopCount;
}
/* =========== End of Sema analysis for perforated loop ============= */

StmtResult Sema::ActOnApproxDirective(Stmt *AssociatedStmt,
                                      ArrayRef<ApproxClause *> Clauses,
                                      ApproxVarListLocTy &Locs) {
  if(!AssociatedStmt)
    return StmtError();

  CapturedStmt *CS = nullptr;
  ApproxLoopHelperExprs B;
  OMPLoopDirective *OMPLoopDir = nullptr;

  for(const auto &AC : Clauses) {
    if(AC->getClauseKind() == CK_PERFO) {
      Stmt *LoopStmt = nullptr;
      if ((OMPLoopDir = dyn_cast<OMPLoopDirective>(AssociatedStmt))) {
        LoopStmt = OMPLoopDir->getInnermostCapturedStmt()->IgnoreContainers(true);
        llvm::dbgs() << "LoopStmt class name is: " << LoopStmt->getStmtClassName() << "\n";
      } else {
        LoopStmt = AssociatedStmt;
        B.OMPParallelForDir = nullptr;
      }
      assert(LoopStmt && "Expected non-null LoopStmt");
      if(checkApproxLoop(LoopStmt, *this, B) == 0) {
        errs() << "Perforated loop is not canonical!\n";
        abort();
      }

      ApproxPerfoClause *PC = cast<ApproxPerfoClause>(AC);

      // Capture PerfoStep if a DRE or use directly the expression.
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(PC->getStep())) {
        ExprResult CapturedDRE =
            BuildDeclRefExpr(DRE->getDecl(), DRE->getDecl()->getType(),
                                   VK_LValue, SourceLocation());
        B.PerfoStep = CapturedDRE.get();
      } else
        B.PerfoStep = PC->getStep();

      switch(PC->getPerfoType()) {
        case PT_SMALL: {
          // Create the perforation conditional add expression.
          ExprResult ModOp = BuildBinOp(getCurScope(), SourceLocation(),
                                        BO_Rem, B.IterationVarRef, B.PerfoStep);
          ExprResult PerfoCondMod = BuildBinOp(
              getCurScope(), SourceLocation(), BO_EQ, ModOp.get(),
              ActOnIntegerConstant(SourceLocation(), 0).get());
          ExprResult PerfoCondNotFirst = BuildBinOp(
              getCurScope(), SourceLocation(), BO_GT, B.IterationVarRef,
              ActOnIntegerConstant(SourceLocation(), 0).get());
          ExprResult PerfoCond = BuildBinOp(getCurScope(), SourceLocation(),
                                              BO_LAnd, PerfoCondNotFirst.get(), PerfoCondMod.get());
          PerfoCond = PerformImplicitConversion(
              PerfoCond.get(), Context.BoolTy,
              /*Action=*/Sema::AA_Casting,
              /*AllowExplicit=*/true);
#if 0
          ExprResult PerfoCondAdd = ActOnConditionalOp(
              SourceLocation(), SourceLocation(), PerfoCond.get(),
              this->ActOnIntegerConstant(SourceLocation(), 1).get(),
              this->ActOnIntegerConstant(SourceLocation(), 0).get());

          // Update the canonical loop increment with the perforation addition.
          BinaryOperator *BO = dyn_cast<BinaryOperator>(B.Inc);
          ExprResult AddOp =
              this->BuildBinOp(this->getCurScope(), SourceLocation(), BO_Add,
                               ActOnIntegerConstant(SourceLocation(), 1).get(),
                               PerfoCondAdd.get());
          //BO->setRHS(AddOp.get());
          DeclRefExpr *Ref = nullptr;
          buildCapture(*this, AddOp.get(), Ref);

          auto *PreInits = cast<DeclStmt>(B.PreInits);
          ASTContext &Context = getASTContext();
          SmallVector<Decl *, 8> PreInitsList(PreInits->decl_begin(),
                                              PreInits->decl_end());
          PreInitsList.push_back(Ref->getDecl());
          B.PreInits = buildPreInits(Context, PreInitsList);

          B.PerfoInc = BuildBinOp(getCurScope(), SourceLocation(), BO_Assign,
                                  Ref, AddOp.get())
                           .get();

          B.Inc = BuildBinOp(getCurScope(), SourceLocation(), BO_AddAssign,
                             BO->getLHS(), Ref)
                      .get();
#endif

          // FIXME: How do we get l/r paren of if statememt?
          B.PerfoSkip = ActOnIfStmt(SourceLocation(), IfStatementKind::Ordinary,
                                    PC->getBeginLoc(), nullptr,
                                    ConditionResult(*this, nullptr, FullExprArg(PerfoCond.get()), false),
                                    PC->getEndLoc(), new (Context) ContinueStmt(SourceLocation()),
                                    SourceLocation(), nullptr).get();
          break;
        }
        case PT_LARGE: {
#if 0
          // Change the canonical loop increment.
          BinaryOperator *BO = dyn_cast<BinaryOperator>(B.Inc);

          DeclRefExpr *Ref = nullptr;
          buildCapture(*this, B.PerfoStep, Ref);
          auto *PreInits = cast<DeclStmt>(B.PreInits);
          ASTContext &Context = getASTContext();
          SmallVector<Decl *, 8> PreInitsList(PreInits->decl_begin(), PreInits->decl_end());
          PreInitsList.push_back(Ref->getDecl());
          B.PreInits = buildPreInits(Context, PreInitsList);

          B.Inc = BuildBinOp(getCurScope(), SourceLocation(), BO_AddAssign,
                             BO->getLHS(), Ref)
                      .get();
#endif
          ExprResult ModOp = BuildBinOp(getCurScope(), SourceLocation(), BO_Rem,
                                        B.IterationVarRef, B.PerfoStep);
          ExprResult PerfoCondMod =
              BuildBinOp(getCurScope(), SourceLocation(), BO_NE, ModOp.get(),
                         ActOnIntegerConstant(SourceLocation(), 0).get());
          ExprResult PerfoCondNotFirst = BuildBinOp(
              getCurScope(), SourceLocation(), BO_GT, B.IterationVarRef,
              ActOnIntegerConstant(SourceLocation(), 0).get());
          ExprResult PerfoCond =
              BuildBinOp(getCurScope(), SourceLocation(), BO_LAnd,
                         PerfoCondNotFirst.get(), PerfoCondMod.get());
          PerfoCond = PerformImplicitConversion(
              PerfoCond.get(), Context.BoolTy,
              /*Action=*/Sema::AA_Casting,
              /*AllowExplicit=*/true);

          // FIXME: How do we get l/r paren of if statememt?
          B.PerfoSkip = ActOnIfStmt(SourceLocation(), IfStatementKind::Ordinary,
                                    PC->getBeginLoc(), nullptr,
                                    ConditionResult(*this, nullptr, FullExprArg(PerfoCond.get()), false),
                                    PC->getEndLoc(), new (Context) ContinueStmt(SourceLocation()),
                                    SourceLocation(), nullptr).get();


          break;
        }
        case PT_SINIT: {
          // Update canonical loop IV init.
          BinaryOperator *BO = dyn_cast<BinaryOperator>(B.Init);
          ExprResult MaxValue =
              BuildBinOp(getCurScope(), SourceLocation(), BO_Add,
                               B.UB, ActOnIntegerConstant(SourceLocation(), 1).get());
          ExprResult MulOp =
              BuildBinOp(getCurScope(), SourceLocation(), BO_Mul,
                               MaxValue.get(), B.PerfoStep);
          MulOp = PerformImplicitConversion(MulOp.get(),
                                                  BO->getLHS()->getType(),
                                                  /*Action=*/Sema::AA_Casting,
                                                  /*AllowExplicit=*/true);
          BO->setRHS(MulOp.get());

          break;
        }
        case PT_SFINAL: {
          // Update canonical loop condition.
          BinaryOperator *BO = dyn_cast<BinaryOperator>(B.Cond);
          ExprResult SubOp = BuildBinOp(
              getCurScope(), SourceLocation(), BO_Sub,
              ActOnIntegerConstant(SourceLocation(), 1).get(),
              B.PerfoStep);
          ExprResult MulOp =
              BuildBinOp(getCurScope(), SourceLocation(), BO_Mul,
                               SubOp.get(), BO->getRHS());
          MulOp = PerformImplicitConversion(MulOp.get(),
                                                  BO->getRHS()->getType(),
                                                  /*Action=*/Sema::AA_Casting,
                                                  /*AllowExplicit=*/true);
          BO->setRHS(MulOp.get());

          break;
        }
        case PT_RAND: {
          // Codegen is performed in CGApproxRuntime.
          SmallVector<QualType, 2> ArgTys(
              {Context.UnsignedIntTy, Context.FloatTy});

          QualType FunctionTy =
              Context.getFunctionType(Context.BoolTy, ArgTys, {});

          DeclContext *Parent = Context.getTranslationUnitDecl();
          if (getLangOpts().CPlusPlus) {
            LinkageSpecDecl *CLinkageDecl = LinkageSpecDecl::Create(
                Context, Parent, SourceLocation(), SourceLocation(),
                LinkageSpecDecl::lang_c, false);
            CLinkageDecl->setImplicit();
            Parent->addDecl(CLinkageDecl);
            Parent = CLinkageDecl;
          }

          FunctionDecl *RandSkipF = FunctionDecl::Create(
              Context, Parent, SourceLocation(), SourceLocation(),
              &Context.Idents.get("__approx_skip_iteration"), FunctionTy,
              nullptr, SC_Extern);
          RandSkipF->setImplicit();

          SmallVector<ParmVarDecl *, 2> Params;
          Params.push_back(ParmVarDecl::Create(
              Context, RandSkipF, SourceLocation(), SourceLocation(), nullptr,
              ArgTys[0], nullptr, SC_None, nullptr));
          Params.push_back(ParmVarDecl::Create(
              Context, RandSkipF, SourceLocation(), SourceLocation(), nullptr,
              ArgTys[1], nullptr, SC_None, nullptr));
          RandSkipF->setParams(Params);

          //auto *DRE = buildDeclRefExpr(*this, RandSkipF, Context.BoolTy, SourceLocation());
          auto *DRE = BuildDeclRefExpr(RandSkipF, FunctionTy, VK_LValue, SourceLocation());

          SmallVector<Expr *, 2> ArgExprs({ B.IterationVarRef, B.PerfoStep });
          ExprResult CallRes = ActOnCallExpr(getCurScope(), DRE, SourceLocation(), ArgExprs, SourceLocation());

          B.PerfoSkip = ActOnIfStmt(SourceLocation(), IfStatementKind::Ordinary,
                                    PC->getBeginLoc(), nullptr,
                                    ConditionResult(*this, nullptr, FullExprArg(CallRes.get()), false),
                                    PC->getEndLoc(), new (Context) ContinueStmt(SourceLocation()),
                                    SourceLocation(), nullptr).get();
          break;
        }
        default:
          errs() << "Unknown perforation type " << PC->getPerfoType() << "\n";
          abort();
        }

        break;
    }
  }

  if (OMPLoopDir) {
    ASTContext &Context = getASTContext();
    SmallVector<clang::OMPClause *, 8> OMPClauses;
    for (unsigned i = 0; i < OMPLoopDir->getNumClauses(); i++)
      {
      OMPClauses.push_back(OMPLoopDir->getClause(i));
      }

    const CapturedStmt *OMPCap = cast<CapturedStmt>(OMPLoopDir->getInnermostCapturedStmt());
    auto BuildOMPParallelFor = [&]() {
      auto *DRE = cast<DeclRefExpr>(B.IterationVarRef);
      BuildDeclRefExpr(DRE->getDecl(), DRE->getDecl()->getType(),
                             VK_LValue, SourceLocation());

      DRE = cast<DeclRefExpr>(B.LB);
      BuildDeclRefExpr(DRE->getDecl(), DRE->getDecl()->getType(),
                             VK_LValue, SourceLocation());

      DRE = cast<DeclRefExpr>(B.UB);
      BuildDeclRefExpr(DRE->getDecl(), DRE->getDecl()->getType(),
                             VK_LValue, SourceLocation());

      DRE = cast<DeclRefExpr>(B.Counter);
      BuildDeclRefExpr(DRE->getDecl(), DRE->getDecl()->getType(),
                             VK_LValue, SourceLocation());
      clang::OMPClause *PrivCounterClause =
          ActOnOpenMPPrivateClause( { DRE }, SourceLocation(), SourceLocation(),
                                   SourceLocation());
      OMPClauses.push_back(PrivCounterClause);

      if((DRE = dyn_cast<DeclRefExpr>(B.PerfoStep)))
        BuildDeclRefExpr(DRE->getDecl(), DRE->getDecl()->getType(),
                               VK_LValue, SourceLocation());

      if (const auto *PreInits = cast_or_null<DeclStmt>(B.PreInits)) {
        for (auto *I : PreInits->decls()) {
          VarDecl *VD = cast<VarDecl>(I);
          BuildDeclRefExpr(VD, VD->getType(), VK_LValue,
                                 SourceLocation());
        }
      }

      for (CapturedStmt::Capture C : OMPCap->captures()) {
        if (C.capturesVariable()) {
          VarDecl *VD = C.getCapturedVar();
          BuildDeclRefExpr(VD, VD->getType().getNonReferenceType(), VK_LValue,
                           SourceLocation());
        }
      }

      SmallVector<Stmt *, 8> StmtList;
      Stmt *LoopStmt = OMPLoopDir->getInnermostCapturedStmt()->IgnoreContainers(true);
      Stmt *LoopBody = nullptr;
      if (auto *For = dyn_cast<ForStmt>(LoopStmt)) {
        LoopBody = For->getBody();
      } else {
        assert(isa<CXXForRangeStmt>(LoopStmt) &&
               "Expected canonical for loop or range-based for loop.");
        auto *CXXFor = cast<CXXForRangeStmt>(LoopStmt);
        StmtList.push_back(CXXFor->getLoopVarStmt());
        LoopBody = CXXFor->getBody();
      }

      assert(LoopBody && "Expected non-null LoopBody");

      if (B.PerfoSkip)
        StmtList.push_back(B.PerfoSkip);
      StmtList.push_back(B.CounterUpdate);
      for(Stmt *S : cast<CompoundStmt>(LoopBody)->body())
        StmtList.push_back(S);

      CompoundStmt *ExtLoopBody = CompoundStmt::Create(
                                Context, StmtList, FPOptionsOverride(),
                                SourceLocation(), SourceLocation());

      StmtResult NewForStmtRes =
          ActOnForStmt(SourceLocation(), SourceLocation(), B.Init,
                       ConditionResult(*this, nullptr, FullExprArg(B.Cond),
                                       /* isConstExpr */ false),
                       FullExprArg(B.Inc), SourceLocation(), ExtLoopBody);

      return NewForStmtRes;
    };

    StartOpenMPDSABlock(OMPLoopDir->getDirectiveKind(), DeclarationNameInfo(),
                        getCurScope(), SourceLocation());

    ActOnOpenMPRegionStart(OMPLoopDir->getDirectiveKind(), getCurScope());
    StmtResult CompStmtRes = BuildOMPParallelFor();
    StmtResult OMPCSRes = ActOnOpenMPRegionEnd(CompStmtRes, OMPClauses);
    StmtResult OMPParForDirRes = ActOnOpenMPExecutableDirective(
        OMPLoopDir->getDirectiveKind(), DeclarationNameInfo(), OMPD_unknown,
        OMPClauses, OMPCSRes.get(), Locs.StartLoc, Locs.EndLoc);

    EndOpenMPDSABlock(OMPParForDirRes.get());

    B.OMPParallelForDir = OMPParForDirRes.get();
  }

  CS = dyn_cast<CapturedStmt>(ActOnCapturedRegionEnd(AssociatedStmt).get());
  assert(CS && "Expected non-null CS");
  ApproxDirective *Stmt = ApproxDirective::Create(Context, Locs.StartLoc,
                                                  Locs.EndLoc, CS, Clauses, B);
  Stmt->printPretty(dbgs(), nullptr, this->getPrintingPolicy());
  return Stmt;
}

ApproxClause *Sema::ActOnApproxPerfoClause(ClauseKind Kind, PerfoType PType,
                                           ApproxVarListLocTy &Locs, Expr *Step) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation LParenLoc = Locs.LParenLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  Expr *stepExpr = Step;
  Stmt *PreInitStmt = nullptr;
  stepExpr = MakeFullExpr(stepExpr).get();
  llvm::MapVector<const Expr *, DeclRefExpr *> Captures;
  tryBuildCapture(*this, stepExpr, Captures).get();
  PreInitStmt = buildPreInits(Context, Captures);
  return new (Context) ApproxPerfoClause(PType, StartLoc, EndLoc, LParenLoc, PreInitStmt, Step);
}

ApproxClause *Sema::ActOnApproxLabelClause(ClauseKind Kind, ApproxVarListLocTy &Locs, Expr *Label){
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation LParenLoc =Locs.LParenLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  Expr *labelExpr = Label;
  Stmt *PreInitStmt;

  labelExpr = MakeFullExpr(labelExpr).get();
  llvm::MapVector<const Expr *, DeclRefExpr *> Captures;
  tryBuildCapture(*this, labelExpr, Captures);
  PreInitStmt = buildPreInits(Context, Captures);
  return new (Context) ApproxLabelClause(StartLoc, EndLoc, LParenLoc, PreInitStmt, Label);
}

ApproxClause *Sema::ActOnApproxMemoClause(ClauseKind Kind,
                                          MemoType MType,
                                          ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation LParenLoc = Locs.LParenLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxMemoClause(MType, StartLoc, EndLoc, LParenLoc);
}

ApproxClause *Sema::ActOnApproxDTClause(ClauseKind Kind,
                                        ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxDTClause(StartLoc, EndLoc);
}

ApproxClause *Sema::ActOnApproxNNClause(ClauseKind Kind,
                                        ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxNNClause(StartLoc, EndLoc);
}

ApproxClause *Sema::ActOnApproxUserClause(ClauseKind Kind,
                                          ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxUserClause(StartLoc, EndLoc);
}

ApproxClause *Sema::ActOnApproxIfClause(ClauseKind Kind,
                                        ApproxVarListLocTy &Locs, Expr *Cond) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation LParenLoc = Locs.LParenLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  Expr *VarExpr = Cond;
  Stmt *PreInitStmt = nullptr;
  if (!Cond->isValueDependent() && !Cond->isTypeDependent() &&
      !Cond->isInstantiationDependent() &&
      !Cond->containsUnexpandedParameterPack()) {
    ExprResult Val = CheckBooleanCondition(StartLoc, Cond);
    if (Val.isInvalid())
      return nullptr;
    VarExpr = Val.get();
    VarExpr = MakeFullExpr(VarExpr).get();
    llvm::MapVector<const Expr *, DeclRefExpr *> Captures;
    VarExpr = tryBuildCapture(*this, VarExpr, Captures).get();
    PreInitStmt = buildPreInits(Context, Captures);
  }
  return new (Context)
      ApproxIfClause(StartLoc, EndLoc, LParenLoc, PreInitStmt, Cond);
}


ApproxClause *Sema::ActOnApproxVarList(ClauseKind Kind,
                                       ArrayRef<Expr *> VarList,
                                       ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation LParenLoc = Locs.LParenLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  SmallVector<Expr *, 8> Vars;
  for (Expr *RefExpr : VarList) {
    assert(RefExpr && "Null Expr in Approx in/out/inout clause.");
    SourceLocation ELoc = RefExpr->getExprLoc();
    /// For template types
    if (isa<DependentScopeDeclRefExpr>(RefExpr)) {
      // It will be analyzed later.
      Vars.push_back(RefExpr);
      continue;
    }
    Expr *SimpleExpr = RefExpr->IgnoreParenCasts();
    QualType ExprTy = RefExpr->getType().getNonReferenceType();
    if (const auto *AASE = dyn_cast<ApproxArraySectionExpr>(SimpleExpr)) {
      QualType BaseType = ApproxArraySectionExpr::getBaseOriginalType(AASE->getBase());
      if (const auto *ATy = BaseType->getAsArrayTypeUnsafe())
        ExprTy = ATy->getElementType();
      else
        ExprTy = BaseType->getPointeeType();

      if(ExprTy->isPointerType()){
        Diag(ELoc, diag::err_approx_expected_scalar_var_item);
      }
      const Expr *Length = AASE->getLength();
      Expr::EvalResult Result;
      if (Length && !Length->isValueDependent() &&
          Length->EvaluateAsInt(Result, Context) &&
          Result.Val.getInt().isNullValue()) {
        Diag(ELoc,
              diag::err_approx_depend_zero_length_array_section_not_allowed)
            << SimpleExpr->getSourceRange();
        continue;
      }
    }
    else if ( auto *ASE = dyn_cast<ArraySubscriptExpr>(SimpleExpr) ){
      if (!RefExpr->IgnoreParenImpCasts()->isLValue() ||
        (ASE && !ASE->getBase()->isTypeDependent() &&
          !ASE->getBase()
              ->getType()
              .getNonReferenceType()
              ->isPointerType() &&
          !ASE->getBase()->getType().getNonReferenceType()->isArrayType())) {
            Diag(ELoc, diag::err_approx_expected_addressable_lvalue_or_array_item);
            continue;
      }
    }
    else{
      if(SimpleExpr->getType()->isPointerType()){
        Diag(ELoc, diag::err_approx_expected_scalar_var_item);
      }
    }

    ExprResult Res;
    {
      Sema::TentativeAnalysisScope Trap(*this);
      Res = CreateBuiltinUnaryOp(ELoc, UO_AddrOf,
                                  RefExpr->IgnoreParenImpCasts());
    }
    if (!Res.isUsable() && !isa<ApproxArraySectionExpr>(SimpleExpr)) {
      Diag(ELoc, diag::err_approx_expected_addressable_lvalue_or_array_item);
      continue;
    }
    Vars.push_back(RefExpr->IgnoreParenImpCasts());
  }

  if (Vars.empty()) {
    return nullptr;
  } else if (Kind == CK_IN) {
    return ApproxInClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars);
  } else if (Kind == CK_OUT) {
    return ApproxOutClause::Create(Context, StartLoc, LParenLoc, EndLoc, Vars);
  } else if (Kind == CK_INOUT) {
    return ApproxInOutClause::Create(Context, StartLoc, LParenLoc, EndLoc,
                                     Vars);
  } else {
    dbgs() << "Should Never Reach This point (TODO RAISE ERROR CODE)\n";
    return nullptr;
  }
}
