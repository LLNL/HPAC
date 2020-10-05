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
  CK_INOUT,
  CK_END
};

const unsigned CK_START = CK_PERFO;

enum PerfoType : uint {
  PT_SMALL = 0,
  PT_LARGE,
  PT_RAND,
  PT_SINIT,
  PT_SFINAL,
  PT_END
};

const unsigned PT_START = PT_SMALL;

enum MemoType : uint {
  MT_IN = 0,
  MT_OUT,
  MT_END
};

const unsigned MT_START = MT_IN;

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
