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
#include "clang/Sema/Sema.h"

using namespace clang;
using namespace approx;

StmtResult Sema::ActOnApproxDirective(Stmt *AssociatedStmt) {
  if(!AssociatedStmt)
    return StmtError();

  return ApproxDirective::Create(Context, AssociatedStmt);
}
