//===--- ExprApprox.h - Classes for representing Approx ----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ApproxExpr interface and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_EXPRAPPROX_H
#define LLVM_CLANG_AST_EXPRAPPROX_H

#include "clang/AST/ComputeDependence.h"
#include "clang/AST/Expr.h"

namespace clang {

class ApproxArraySectionExpr : public Expr {
  enum { BASE, LOWER_BOUND, LENGTH, STRIDE, END_EXPR };
  Stmt *SubExprs[END_EXPR];
  SourceLocation ColonLocFirst;
  SourceLocation ColonLocSecond;
  SourceLocation RBracketLoc;


public:
  ApproxArraySectionExpr(Expr *Base, Expr *LowerBound, Expr *Length, Expr *Stride,
                         QualType Type, ExprValueKind VK, ExprObjectKind OK,
                         SourceLocation ColonLocFirst, SourceLocation ColonLocSecond,
                         SourceLocation RBracketLoc)
      : Expr(ApproxArraySectionExprClass, Type, VK, OK), ColonLocFirst(ColonLocFirst),
        ColonLocSecond(ColonLocSecond),
        RBracketLoc(RBracketLoc) {
    SubExprs[BASE] = Base;
    SubExprs[LOWER_BOUND] = LowerBound;
    SubExprs[LENGTH] = Length;
    SubExprs[STRIDE] = Stride;
    setDependence(computeDependence(this));
  }

  /// Create an empty array section expression.
  explicit ApproxArraySectionExpr(EmptyShell Shell)
      : Expr(ApproxArraySectionExprClass, Shell) {}

  /// An array section can be written only as Base[LowerBound:Length].

  /// Get base of the array section.
  Expr *getBase() { return cast<Expr>(SubExprs[BASE]); }
  const Expr *getBase() const { return cast<Expr>(SubExprs[BASE]); }
  /// Set base of the array section.
  void setBase(Expr *E) { SubExprs[BASE] = E; }

  /// Return original type of the base expression for array section.
  static QualType getBaseOriginalType(const Expr *Base);

  /// Get lower bound of array section.
  Expr *getLowerBound() { return cast_or_null<Expr>(SubExprs[LOWER_BOUND]); }
  const Expr *getLowerBound() const {
    return cast_or_null<Expr>(SubExprs[LOWER_BOUND]);
  }
  /// Set lower bound of the array section.
  void setLowerBound(Expr *E) { SubExprs[LOWER_BOUND] = E; }

  /// Get length of array section.
  Expr *getLength() { return cast_or_null<Expr>(SubExprs[LENGTH]); }
  const Expr *getLength() const { return cast_or_null<Expr>(SubExprs[LENGTH]); }
  /// Set length of the array section.
  void setLength(Expr *E) { SubExprs[LENGTH] = E; }

  /// Get stride of array section.
  Expr *getStride() { return cast_or_null<Expr>(SubExprs[STRIDE]); }
  const Expr *getStride() const { return cast_or_null<Expr>(SubExprs[STRIDE]); }
  /// Set stride of the array section.
  void setStride(Expr *E) { SubExprs[STRIDE] = E; }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    return getBase()->getBeginLoc();
  }
  SourceLocation getEndLoc() const LLVM_READONLY { return RBracketLoc; }

  SourceLocation getColonLocFirst() const { return ColonLocFirst; }
  void setColonLocFirst(SourceLocation L) { ColonLocFirst = L; }

  SourceLocation getColonLocSecond() const { return ColonLocSecond; }
  void setColonLocSecond(SourceLocation L) { ColonLocSecond = L; }

  SourceLocation getRBracketLoc() const { return RBracketLoc; }
  void setRBracketLoc(SourceLocation L) { RBracketLoc = L; }

  SourceLocation getExprLoc() const LLVM_READONLY {
    return getBase()->getExprLoc();
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == ApproxArraySectionExprClass;
  }

  child_range children() {
    return child_range(&SubExprs[BASE], &SubExprs[END_EXPR]);
  }

  const_child_range children() const {
    return const_child_range(&SubExprs[BASE], &SubExprs[END_EXPR]);
  }
};

} // namespace clang

#endif
