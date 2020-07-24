//===- StmtApprox.h - Classes for Approx directives  ------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file defines Approx AST classes for directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTAPPROX_H
#define LLVM_CLANG_AST_STMTAPPROX_H

#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/SourceLocation.h"

namespace clang {

class ApproxDirective : public Stmt {
  friend class ASTStmtReader;

  /// Starting location of the directive (directive keyword).
  SourceLocation StartLoc;
  /// Ending location of the directive.
  SourceLocation EndLoc;

  /// Associated captured Stmt
  Stmt *AssociatedStmt;

protected:
  ApproxDirective(StmtClass SC) : Stmt(SC) {}

public:
  static ApproxDirective *Create(ASTContext &C, Stmt *AssociatedStmt);
  /// Returns starting location of directive kind.
  SourceLocation getBeginLoc() const { return StartLoc; }
  /// Returns ending location of directive.
  SourceLocation getEndLoc() const { return EndLoc; }

  /// Set starting location of directive kind.
  ///
  /// \param Loc New starting location of directive.
  ///
  void setLocStart(SourceLocation Loc) { StartLoc = Loc; }
  /// Set ending location of directive.
  ///
  /// \param Loc New ending location of directive.
  ///
  void setLocEnd(SourceLocation Loc) { EndLoc = Loc; }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  void setAssociatedStmt(Stmt *S) { AssociatedStmt = S; }

  Stmt *getAssociatedStmt() const { return AssociatedStmt; }

  static bool classof(const Stmt *S) {
    return S->getStmtClass() == ApproxDirectiveClass;
  }
};

} // end namespace clang

#endif
