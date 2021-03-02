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

struct ApproxLoopHelperExprs {
  /// Loop iteration variable.
  Expr *IterationVarRef;
  /// Loop last iteration number.
  Expr *LastIteration;
  /// Loop number of iterations.
  Expr *NumIterations;
  /// Calculation of last iteration.
  Expr *CalcLastIteration;
  /// Loop pre-condition.
  Expr *PreCond;
  /// Loop condition.
  Expr *Cond;
  /// Loop iteration variable init.
  Expr *Init;
  /// Loop increment.
  Expr *Inc;
  /// IsLastIteration - local flag variable passed to runtime.
  Expr *IL;
  /// LowerBound - local variable passed to runtime.
  Expr *LB;
  /// UpperBound - local variable passed to runtime.
  Expr *UB;
  /// Stride - local variable passed to runtime.
  Expr *ST;
  /// EnsureUpperBound -- expression UB = min(UB, NumIterations).
  Expr *EUB;
  /// Counters Loop counters.
  //SmallVector<Expr *, 4> Counters;
  Expr *Counter;
  /// Expressions for loop counter init for CodeGen.
  Expr *CounterInit;
  /// Expressions for loop counter update for CodeGen.
  Expr *CounterUpdate;
  /// Final loop counter value for CodeGen.
  //Expr *CounterFinal;
  // Random perforation condition for CodeGen.
  // TODO: Remove, if codegen builds call to __approx_skip_iteration instead of sema.
  Expr *PerfoRandCond;
  /// Init statement for all captured expressions.
  Stmt *PreInits;
};

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
                                ArrayRef<ApproxClause *> Clauses, const ApproxLoopHelperExprs &B);
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

  /// The expressions built for the approx perfo codegen.
  ApproxLoopHelperExprs LoopExprs;
};

} // end namespace clang

#endif
