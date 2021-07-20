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

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclApprox.h"
#include "clang/AST/StmtApprox.h"
#include "clang/Basic/Approx.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/STLExtras.h"
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

static Stmt *buildApproxPreInits(ASTContext &Context,
                                 MutableArrayRef<Decl *> PreInits) {
  if (!PreInits.empty()) {
    return new (Context) DeclStmt(
        DeclGroupRef::Create(Context, PreInits.begin(), PreInits.size()),
        SourceLocation(), SourceLocation());
  }
  return nullptr;
}

static Stmt *buildApproxPreInits(
    ASTContext &Context,
    const llvm::MapVector<const Expr *, DeclRefExpr *> &Captures) {
  if (!Captures.empty()) {
    SmallVector<Decl *, 16> PreInits;
    for (const auto &Pair : Captures)
      PreInits.push_back(Pair.second->getDecl());
    return buildApproxPreInits(Context, PreInits);
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
tryBuildApproxCapture(Sema &SemaRef, Expr *Capture,
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

StmtResult Sema::ActOnApproxDirective(Stmt *AssociatedStmt,
                                      ArrayRef<ApproxClause *> Clauses,
                                      ApproxVarListLocTy &Locs) {
  if(!AssociatedStmt)
    return StmtError();

  ApproxDirective *Stmt = ApproxDirective::Create(
      Context, Locs.StartLoc, Locs.EndLoc, AssociatedStmt, Clauses);
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
  tryBuildApproxCapture(*this, stepExpr, Captures).get();
  PreInitStmt = buildApproxPreInits(Context, Captures);
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
  tryBuildApproxCapture(*this, labelExpr, Captures);
  PreInitStmt = buildApproxPreInits(Context, Captures);
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
    VarExpr = tryBuildApproxCapture(*this, VarExpr, Captures).get();
    PreInitStmt = buildApproxPreInits(Context, Captures);
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
