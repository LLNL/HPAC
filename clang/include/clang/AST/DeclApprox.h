//===--- DeclApprox.h - Approx Declarations---------------------------------*-
// C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines some Approx-specific declarative directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_DECLAPPROX_H
#define LLVM_CLANG_AST_DECLAPPROX_H

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExternalASTSource.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/TrailingObjects.h"

namespace clang {

class ApproxCapturedExprDecl final : public VarDecl {
  friend class ASTDeclReader;
  void anchor() override;

  ApproxCapturedExprDecl(ASTContext &C, DeclContext *DC, IdentifierInfo *Id,
                         QualType Type, TypeSourceInfo *TInfo,
                         SourceLocation StartLoc)
      : VarDecl(ApproxCapturedExpr, C, DC, StartLoc, StartLoc, Id, Type, TInfo,
                SC_None) {
    setImplicit();
  }

public:
  static ApproxCapturedExprDecl *Create(ASTContext &C, DeclContext *DC,
                                        IdentifierInfo *Id, QualType T,
                                        SourceLocation StartLoc);

  static ApproxCapturedExprDecl *CreateDeserialized(ASTContext &C, unsigned ID);

  SourceRange getSourceRange() const override LLVM_READONLY;

  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == ApproxCapturedExpr; }
};

} // namespace clang

#endif
