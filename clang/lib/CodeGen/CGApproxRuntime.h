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
#include "clang/AST/StmtApprox.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/SmallVector.h"

namespace clang {
namespace CodeGen {
class CodeGenModule;

enum ApproxType : int8_t {
#define APPROX_TYPE(Id, type, name) Id,
#include "clang/Basic/approxTypes.def"
  INVALID
};

enum ApproxRTArgsIndex : uint {
  AccurateFn = 0,
  PerfoFn,
  CapDataPtr,
  Cond,
  Label,
  PerfoDesc,
  MemoDescr,
  DataDescIn,
  DataSizeIn,
  DataDescOut,
  DataSizeOut,
  ARG_END
};

enum Directionality : int { Input = 1, Output = 2, InputOuput = 4 };

const unsigned ARG_START = AccurateFn;

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

  /// VarInfoTy is a struct containing info about the in/out/inout variables
  /// of this region.
  ///    typedef struct approx_var_info_t{
  ///        void* ptr;         // Ptr to data
  ///        size_t num_elem;   // Number of elements
  ///        size_t sz_elem;    // Size of elements in bytes
  ///        int8_t data_type; // Type of data float/double/int etc.
  ///        uint8_t dir;       // Direction of data: in/out/inout
  ///    } approx_var_info_t;
  QualType VarInfoTy;
  llvm::Value *approxRTParams[ARG_END];
  llvm::SmallVector<std::pair<Expr *, Directionality>, 16> Inputs;
  llvm::SmallVector<std::pair<Expr *, Directionality>, 16> Outputs;
  // Function type of callback functions.
  llvm::FunctionType *CallbackFnTy;
  // Function type of the runtime interface call.
  llvm::FunctionType *RTFnTy;
  int approxRegions;
  SourceLocation StartLoc;
  SourceLocation EndLoc;
  bool requiresData;
  bool requiresInputs;

private:
  void CGApproxRuntimeEmitPerfoFn(CapturedStmt &CS, const ApproxLoopHelperExprs &LoopExprs, const ApproxPerfoClause &PC);
  std::pair<llvm::Value *, llvm::Value *> CGApproxRuntimeEmitData(CodeGenFunction &CGF, llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *arrayName);

public:
  CGApproxRuntime(CodeGenModule &CGM);
  void CGApproxRuntimeEnterRegion(CodeGenFunction &CGF, CapturedStmt &CS);
  void CGApproxRuntimeEmitPerfoInit(CodeGenFunction &CGF, CapturedStmt &CS,
                                    ApproxPerfoClause &PerfoClause, const ApproxLoopHelperExprs &LoopExprs);
  void CGApproxRuntimeEmitMemoInit(CodeGenFunction &CGF,
                                   ApproxMemoClause &MemoClause);
  void CGApproxRuntimeEmitIfInit(CodeGenFunction &CGF,
                                 ApproxIfClause &IfClause);
  void CGApproxRuntimeEmitLabelInit(CodeGenFunction &CGF, ApproxLabelClause &LabelCluse);
  void CGApproxRuntimeExitRegion(CodeGenFunction &CGF);
  void CGApproxRuntimeRegisterInputs(ApproxInClause &InClause);
  void CGApproxRuntimeRegisterOutputs(ApproxOutClause &OutClause);
  void CGApproxRuntimeRegisterInputsOutputs(ApproxInOutClause &InOutClause);
  void CGApproxRuntimeEmitDataValues(CodeGenFunction &CG);
};

} // namespace CodeGen
} // namespace clang

#endif
