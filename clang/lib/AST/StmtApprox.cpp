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

#include "clang/AST/StmtApprox.h"
#include "clang/AST/ASTContext.h"

using namespace clang;
using namespace llvm;

ApproxDirective *ApproxDirective::Create(ASTContext &C) {
  // TODO: what should be the alignof value?
  unsigned Size = llvm::alignTo(sizeof(ApproxDirective), alignof(void *));
  void *Mem = C.Allocate(Size + sizeof(Stmt *));
  ApproxDirective *AD = new (Mem) ApproxDirective(ApproxDirectiveClass);

  return AD;
}
