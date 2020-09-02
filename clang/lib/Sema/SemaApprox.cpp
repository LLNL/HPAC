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
#include "clang/AST/StmtApprox.h"
#include "clang/Basic/Approx.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/Debug.h"

using namespace clang;
using namespace llvm;
using namespace approx;

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

ApproxClause *Sema::ActOnApproxPerfoClause(ClauseKind Kind,
                                           ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxPerfoClause(StartLoc, EndLoc);
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
                                        ApproxVarListLocTy &Locs) {
  SourceLocation StartLoc = Locs.StartLoc;
  SourceLocation EndLoc = Locs.EndLoc;
  return new (Context) ApproxIfClause(StartLoc, EndLoc);
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
