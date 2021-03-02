//===--- StmtApprox.cpp - Classes for Approx directives -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Stmt class declared in StmtApprox.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ApproxClause.h"
#include "clang/AST/StmtApprox.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Approx.h"
#include "llvm/Support/Debug.h"

using namespace clang;
using namespace llvm;


void ApproxDirective::setClauses(ArrayRef<ApproxClause *> Clauses) {
  assert(
      Clauses.size() == getNumClauses() &&
      "Approx:: Number of clauses is not the same as the preallocated buffer");
  std::copy(Clauses.begin(), Clauses.end(), getClauses().begin());
}

ApproxDirective *ApproxDirective::Create(const ASTContext &C,
                                         SourceLocation StartLoc,
                                         SourceLocation EndLoc,
                                         Stmt *AssociatedStmt,
                                         ArrayRef<ApproxClause *> Clauses,
                                         const ApproxLoopHelperExprs &B) {
  unsigned Size =
      llvm::alignTo(sizeof(ApproxDirective), alignof(ApproxClause *));
  void *Mem = C.Allocate(Size + sizeof(ApproxClause *) * Clauses.size() +
                         sizeof(Stmt *));
  ApproxDirective *AD = new (Mem)
      ApproxDirective(ApproxDirectiveClass, StartLoc, EndLoc, Clauses.size());
  AD->setClauses(Clauses);
  AD->setAssociatedStmt(AssociatedStmt);
  AD->LoopExprs = B;
  return AD;
}
