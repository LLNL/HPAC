//===--- CGStmtApprox.cpp - Emit LLVM Code from Statements ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit Approx nodes as LLVM code.
//
//===----------------------------------------------------------------------===//

#include "CodeGenFunction.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtApprox.h"
#include "llvm/Support/Debug.h"

using namespace clang;
using namespace CodeGen;
using namespace llvm;

void CodeGenFunction::EmitApproxDirective(const ApproxDirective &AD) {
  Function *OutlinedFn = nullptr;
  Value *CapStruct = nullptr;

  auto GenerateOutlinedFunction = [&](const CapturedStmt &CS) {
    Address CapStructAddr = GenerateCapturedStmtArgument(CS);
    CapStruct = CapStructAddr.getPointer();
    CGCapturedStmtInfo CGSI(CS);
    CodeGenFunction CGF(CGM, true);
    CGCapturedStmtRAII CapInfoRAII(CGF, &CGSI);
    OutlinedFn = CGF.GenerateCapturedStmtFunction(CS);
  };

  const CapturedStmt *CS = cast<CapturedStmt>(AD.getAssociatedStmt());
  GenerateOutlinedFunction(*CS);
  assert(OutlinedFn && "OutlinedFn should not be null");
  assert(CapStruct && "CapturedStruct should not be null");
  llvm::PointerType *OutlinedFnPtrTy =
      llvm::PointerType::getUnqual(OutlinedFn->getFunctionType());
  llvm::FunctionType *RTFnTy = llvm::FunctionType::get(
      llvm::Type::getVoidTy(getLLVMContext()),
      ArrayRef<llvm::Type *>{OutlinedFnPtrTy, CapStruct->getType()},
      /* isVarArg = */ false);
  llvm::Function *RTFn =
      Function::Create(RTFnTy, GlobalValue::ExternalLinkage,
                       "__approx_exec_call", CGM.getModule());
  EmitCallOrInvoke(RTFn, ArrayRef<llvm::Value *>{OutlinedFn, CapStruct});
}
