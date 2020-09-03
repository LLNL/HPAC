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
#include "llvm/Support/Debug.h"

using namespace clang;
using namespace llvm;
using namespace approx;

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

static const ValueDecl *getCanonicalDecl(const ValueDecl *D) {
  const auto *VD = dyn_cast<VarDecl>(D);
  const auto *FD = dyn_cast<FieldDecl>(D);
  if (VD != nullptr) {
    VD = VD->getCanonicalDecl();
    D = FD;
  } else {
    assert(FD);
    D = FD;
  }
  return D;
}

static ValueDecl *getCanonicalDecl(ValueDecl *D) {
  return const_cast<ValueDecl *>(
      getCanonicalDecl(const_cast<const ValueDecl *>(D)));
}

static ValueDecl *getNextVariable(Sema &S, Expr *&RefExpr, SourceLocation &ELoc,
                                  SourceRange &ERange) {
  /// TODO: This function needs to be extended to handle
  /// more complicated cases such as subarrays and
  /// range of arrays
  RefExpr = RefExpr->IgnoreParens();
  ELoc = RefExpr->getExprLoc();
  ERange = RefExpr->getSourceRange();
  RefExpr = RefExpr->IgnoreParenImpCasts();
  auto *DE = dyn_cast_or_null<DeclRefExpr>(RefExpr);
  if (!DE)
    return nullptr;
  return getCanonicalDecl(DE->getDecl());
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

ApproxClause *Sema::ActOnApproxMemoClause(ClauseKind Kind,
                                          ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxMemoClause(StartLoc, EndLoc);
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
    SourceLocation ELoc;
    SourceRange ERange;
    Expr *SimpleRefExpr = RefExpr;
    auto Res = getNextVariable(*this, SimpleRefExpr, ELoc, ERange);
    if (!Res) {
      Vars.push_back(RefExpr);
    } else {
      ValueDecl *D = Res;
      if (!D)
        continue;
      Vars.push_back(RefExpr->IgnoreParens());
    }
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
