//===----- CGApproxRuntimeGPU::.cpp - Interface to Approx Runtimes -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This provides a class for Approx runtime code generation for offloading to device.
//
//===----------------------------------------------------------------------===//

#include "CGApproxRuntime.h"
#include "CodeGenFunction.h"
#include "clang/AST/ApproxClause.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtApprox.h"
#include "clang/Basic/Approx.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

using namespace llvm;
using namespace clang;
using namespace CodeGen;
// FIXME: find out how to re-integrate convertToApproxType

CGApproxRuntimeGPU::CGApproxRuntimeGPU(CodeGenModule &CGM)
  : CGApproxRuntime(CGM), CGM(CGM), RTFnTy(nullptr)  {

  // This is the runtime call function type information, which mirrors the
  // types provided in the argument parameters.
  RTFnTy = llvm::FunctionType::get(CGM.VoidTy,
                                         {
                                           /* Orig. fn ptr*/ llvm::PointerType::getUnqual(CallbackFnTy),
                                           /* Captured data ptr*/ CGM.VoidPtrTy,
                                           /* Memoization Type */ CGM.Int32Ty,
                                           /* Input Data Descr. */ CGM.VoidPtrTy,
                                           /* Input Data Num Elements */ CGM.Int32Ty,
                                           /* Output Data Descr. */ CGM.VoidPtrTy,
                                           /* Output Data Num Elements */ CGM.Int32Ty
                                         },
                                         false);
}

void CGApproxRuntimeGPU::CGApproxRuntimeEnterRegion(CodeGenFunction &CGF,
                                                 CapturedStmt &CS) {
  // This two values (requiresInputs, requiredData) should be false.
  // currently though the compiler is forwarding everything to
  // the runtime system
  requiresInputs = true;
  requiresData = true;

  ASTContext &C = CGM.getContext();
  CodeGen::CodeGenTypes &Types = CGM.getTypes();
  llvm::PointerType *CharPtrTy =
      llvm::PointerType::getUnqual(Types.ConvertType(C.CharTy));
  /// Reset All info of the Runtime "state machine"
  getVarInfoType(C, VarInfoTy);
  Inputs.clear();
  Outputs.clear();
  for (unsigned i = DEV_ARG_START; i < DEV_ARG_END; i++)
    approxRTParams[i] = nullptr;

  Address CapStructAddr = CGF.GenerateCapturedStmtArgument(CS);
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction localCGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(localCGF, &CGSI);
  llvm::Function *Fn = localCGF.GenerateCapturedStmtFunction(CS);

  /// Fill in parameters of runtime function call
  /// Put default values on everything.
  /// EmitClause* Will replace as necessary
  approxRTParams[DevAccurateFn] =
    CGF.Builder.CreatePointerCast(Fn, CallbackFnTy->getPointerTo());
  approxRTParams[DevCapDataPtr] =
      CGF.Builder.CreatePointerCast(CapStructAddr.getPointer(), CGM.VoidPtrTy);
  approxRTParams[DevDataDescIn] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevDataSizeIn] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);
  approxRTParams[DevDataDescOut] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevDataSizeOut] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);
  approxRTParams[DevMemoDescr] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);

  StartLoc = CS.getBeginLoc();
  EndLoc = CS.getEndLoc();
  return;
}

void CGApproxRuntimeGPU::CGApproxRuntimeEmitMemoInit(
    CodeGenFunction &CGF, ApproxMemoClause &MemoClause) {
  requiresData = true;
  switch(MemoClause.getMemoType()) {
    case approx::MT_IN:
      requiresInputs = true;
      approxRTParams[DevMemoDescr] =
        llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 1);
      break;
    case approx::MT_OUT:
    approxRTParams[DevMemoDescr] =
        llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 2);
      break;
    }
}



void CGApproxRuntimeGPU::CGApproxRuntimeExitRegion(CodeGenFunction &CGF) {
  Function *RTFnDev = nullptr;
  StringRef RTFnName("__approx_device_memo");
  RTFnDev = CGM.getModule().getFunction(RTFnName);

  assert(RTFnTy != nullptr);
  if (!RTFnDev)
    RTFnDev = Function::Create(RTFnTy, GlobalValue::ExternalLinkage, RTFnName,
                               CGM.getModule());

  llvm::FunctionCallee RTFnCallee({RTFnTy, RTFnDev});
  CGF.EmitRuntimeCall(RTFnCallee, ArrayRef<llvm::Value *>(approxRTParams));
}

void CGApproxRuntimeGPU::CGApproxRuntimeEmitDataValues(CodeGenFunction &CGF) {
  /// No Dependencies so exit.
  static int input_arrays = 0;
  static int output_arrays = 0;
  char name[100];
  if (!requiresData)
    return;

  llvm::Value *NumOfElements, *ArrayAddress;
  if (requiresInputs && Inputs.size() > 0) {
    sprintf(name, ".dep.approx_inputs.arr.addr_%d", input_arrays++);
    std::tie(NumOfElements, ArrayAddress) =
        CGApproxRuntimeEmitData(CGF, Inputs, name);
    approxRTParams[DevDataDescIn] = ArrayAddress;
    approxRTParams[DevDataSizeIn] = NumOfElements;
  }

  // All approximation techniques require the output
  sprintf(name, ".dep.approx_outputs.arr.addr_%d", output_arrays++);
  std::tie(NumOfElements, ArrayAddress) =
      CGApproxRuntimeEmitData(CGF, Outputs, name);
  approxRTParams[DevDataDescOut] = ArrayAddress;
  approxRTParams[DevDataSizeOut] = NumOfElements;
}
