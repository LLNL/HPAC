//===----- CGApproxRuntime.h - Interface to Approx Runtimes -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This provides a class for Approx runtime code generation.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_CODEGEN_CGAPPROXRUNTIME_H
#define LLVM_CLANG_LIB_CODEGEN_CGAPPROXRUNTIME_H

#include "CGValue.h"
#include "clang/AST/ApproxClause.h"
#include "clang/AST/DeclApprox.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/SmallVector.h"

namespace clang {
namespace CodeGen {
class CodeGenModule;

enum ApproxRTArgsIndex : uint {
  AccurateFn = 0,
  PerfoFn,
  CapDataPtr,
  Cond,
  PerfoDesc
};

const unsigned ARG_START = AccurateFn;
const unsigned ARG_END = PerfoDesc + 1;

class CGApproxRuntime {
private:
  CodeGenModule &CGM;
  /// PerfoInfoTy is a struct containing infor about the perforation.
  ///  typedef struct approx_perfo_info_t{
  ///    int type;
  ///    int region;
  ///    int step;
  ///    float rate;
  /// } approx_perfo_info_t;
  QualType PerfoInfoTy;
  llvm::SmallVector<llvm::Value *, ARG_END> approxRTParams;
  llvm::SmallVector<llvm::Type *, ARG_END> approxRTTypes;
  int approxRegions;

private:
  void CGApproxRuntimeEmitPerfoFn(CapturedStmt &CS);

public:
  CGApproxRuntime(CodeGenModule &CGM);
  void CGApproxRuntimeEnterRegion(CodeGenFunction &CGF, CapturedStmt &CS);
  void CGApproxRuntimeEmitPerfoInit(CodeGenFunction &CGF, CapturedStmt &CS,
                                    ApproxPerfoClause &PerfoClause);
  void CGApproxRuntimeEmitIfInit(CodeGenFunction &CGF,
                                 ApproxIfClause &IfClause);
  void CGApproxRuntimeExitRegion(CodeGenFunction &CGF);
};

} // namespace CodeGen
} // namespace clang

#endif
