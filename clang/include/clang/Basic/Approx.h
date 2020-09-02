//===--- Approx.h - Approx enums ---------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines some Approx-specific enums and functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_BASIC_APPROX_H
#define LLVM_CLANG_BASIC_APPROX_H

#include "clang/Basic/SourceLocation.h"

namespace clang {
namespace approx {

enum ClauseKind : uint {
  CK_PERFO = 0,
  CK_MEMO,
  CK_DT,
  CK_NN,
  CK_USER,
  CK_IF,
  CK_IN,
  CK_OUT,
  CK_INOUT
};

const unsigned CK_START = CK_PERFO;
const unsigned CK_END = CK_INOUT + 1;

struct ApproxVarListLocTy {
  SourceLocation StartLoc;
  SourceLocation LParenLoc;
  SourceLocation EndLoc;
  ApproxVarListLocTy() = default;
  ApproxVarListLocTy(SourceLocation StartLoc, SourceLocation LParenLoc,
                     SourceLocation EndLoc)
      : StartLoc(StartLoc), LParenLoc(LParenLoc), EndLoc(EndLoc) {}
};

} // namespace approx
} // namespace clang

#endif // LLVM_CLANG_BASIC_APPROX_H
