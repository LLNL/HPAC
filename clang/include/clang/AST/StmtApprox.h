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
#include "clang/AST/ApproxClause.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/Approx.h"
#include "clang/Basic/SourceLocation.h"

namespace clang {

class ApproxDirective : public Stmt {
  friend class ASTStmtReader;

  /// Starting location of the directive (directive keyword).
  SourceLocation StartLoc;
  /// Ending location of the directive.
  SourceLocation EndLoc;

  const unsigned NumClauses;
  /// Associated captured Stmt
  Stmt *AssociatedStmt;

  const unsigned ClausesOffset;

  MutableArrayRef<ApproxClause*> getClauses(){
    ApproxClause **ClauseStorage = reinterpret_cast<ApproxClause **>(
      reinterpret_cast<char *>(this) + ClausesOffset);
      return MutableArrayRef<ApproxClause *> (ClauseStorage, NumClauses);
  }

protected:
  ApproxDirective(StmtClass SC,
                         SourceLocation StartLoc, SourceLocation EndLoc,
                         unsigned NumClauses)
      : Stmt(SC), StartLoc(std::move(StartLoc)),
        EndLoc(std::move(EndLoc)), NumClauses(NumClauses),
        ClausesOffset(llvm::alignTo(sizeof(ApproxDirective), alignof(ApproxClause*))) {}
public:
  static ApproxDirective *Create(const ASTContext &C, SourceLocation StartLoc,
                                SourceLocation EndLoc, Stmt *AssociatedStmt,
                                ArrayRef<ApproxClause *> Clauses);
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


  unsigned getNumClauses() const { return NumClauses; }

  unsigned getClausesOffset() const { return ClausesOffset; }


  ApproxClause *getClause(unsigned i) const { return clauses()[i]; }

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

  ArrayRef<ApproxClause *> clauses() { return getClauses(); }

  ArrayRef<ApproxClause *> clauses() const {
    return const_cast<ApproxDirective*>(this)->getClauses();
  }

  void setClauses(ArrayRef<ApproxClause*> Clauses);

};

} // end namespace clang

#endif
