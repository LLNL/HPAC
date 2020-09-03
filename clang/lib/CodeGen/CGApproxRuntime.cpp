//===----- CGApproxRuntime.cpp - Interface to Approx Runtimes -------------===//
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

static FieldDecl *addFieldToRecordDecl(ASTContext &C, DeclContext *DC,
                                       QualType FieldTy) {
  auto *Field = FieldDecl::Create(
      C, DC, SourceLocation(), SourceLocation(), /*Id=*/nullptr, FieldTy,
      C.getTrivialTypeSourceInfo(FieldTy, SourceLocation()),
      /*BW=*/nullptr, /*Mutable=*/false, /*InitStyle=*/ICIS_NoInit);
  Field->setAccess(AS_public);
  DC->addDecl(Field);
  return Field;
}

static void getPerfoInfoType(ASTContext &C, QualType &perfoInfoTy) {
  if (perfoInfoTy.isNull()) {
    RecordDecl *perfoInfoRD = C.buildImplicitRecord("approx_perfo_info_t");
    perfoInfoRD->startDefinition();
    /// The Perfo Flags Field
    addFieldToRecordDecl(C, perfoInfoRD, C.getIntTypeForBitwidth(32, false));
    /// The approx region Id
    addFieldToRecordDecl(C, perfoInfoRD, C.getIntTypeForBitwidth(32, false));
    /// The approx step
    addFieldToRecordDecl(C, perfoInfoRD, C.getIntTypeForBitwidth(32, false));
    /// The percentage of loops to skip
    addFieldToRecordDecl(C, perfoInfoRD, C.getRealTypeForBitwidth(32, false));
    perfoInfoRD->completeDefinition();
    perfoInfoTy = C.getRecordType(perfoInfoRD);
  }
  return;
}

CGApproxRuntime::CGApproxRuntime(CodeGenModule &CGM)
    : CGM(CGM), approxRegions(0) {
  ASTContext &C = CGM.getContext();
  for (unsigned i = ARG_START; i < ARG_END; i++) {
    approxRTParams.push_back(nullptr);
    approxRTTypes.push_back(nullptr);
  }
  getPerfoInfoType(C, PerfoInfoTy);
}

void CGApproxRuntime::CGApproxRuntimeEnterRegion(CodeGenFunction &CGF,
                                                 CapturedStmt &CS) {
  /// Rest All info of the Runtime "state machine"
  for (unsigned i = ARG_START; i < ARG_END; i++) {
    approxRTParams[i] = nullptr;
    approxRTTypes[i] = nullptr;
  }

  Address CapStructAddr = CGF.GenerateCapturedStmtArgument(CS);
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction localCGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(localCGF, &CGSI);
  llvm::Function *Fn = localCGF.GenerateCapturedStmtFunction(CS);

  /// Set Types of the function call
  approxRTTypes[AccurateFn] =
      llvm::PointerType::getUnqual(Fn->getFunctionType());
  /// The type of the perforated function is the same with the accurate one
  approxRTTypes[PerfoFn] = approxRTTypes[AccurateFn];
  approxRTTypes[CapDataPtr] = CapStructAddr.getType();
  approxRTTypes[Cond] = CGF.Builder.getInt1Ty();
  approxRTTypes[PerfoDesc] = CGM.VoidPtrTy;

  /// Fill in parameters of runtime function call
  /// Put default values on everything.
  /// EmitClause* Will replace as necessary
  approxRTParams[AccurateFn] = Fn;
  approxRTParams[PerfoFn] = llvm::ConstantPointerNull::get(
      llvm::PointerType::getUnqual(Fn->getFunctionType()));
  approxRTParams[CapDataPtr] = CapStructAddr.getPointer();
  approxRTParams[Cond] = llvm::ConstantInt::get(CGF.Builder.getInt1Ty(), true);
  approxRTParams[PerfoDesc] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  return;
}

void CGApproxRuntime::CGApproxRuntimeEmitPerfoInit(
    CodeGenFunction &CGF, CapturedStmt &CS, ApproxPerfoClause &PerfoClause) {
  enum PerfoInfoFieldID { FlagsId, ApproxRegionId, StepId, RateId };
  Value *StepVal = nullptr;
  Expr *Step = nullptr;
  ASTContext &C = CGM.getContext();
  const auto *PerfoInfoRecord = PerfoInfoTy->getAsRecordDecl();
  auto *PD =
      ImplicitParamDecl::Create(C, PerfoInfoTy, ImplicitParamDecl::Other);
  CGF.EmitVarDecl(*PD);
  Address PerfoStructAddress = CGF.GetAddrOfLocalVar(PD);
  Step = PerfoClause.getStep();
  if (const auto *PreInit = cast_or_null<DeclStmt>(PerfoClause.getPreInit())) {
    for (const auto *D : PreInit->decls()) {
      CGF.EmitVarDecl(cast<VarDecl>(*D));
    }
  }
  StepVal = CGF.EmitScalarExpr(Step);
  Value *PerfoType =
      llvm::ConstantInt::get(CGM.Int32Ty, PerfoClause.getPerfoType(), false);
  Value *RGId = llvm::ConstantInt::get(CGM.Int32Ty, approxRegions, false);
  LValue BaseAddr = CGF.MakeAddrLValue(PerfoStructAddress, PerfoInfoTy);

  LValue FieldAddr = CGF.EmitLValueForField(
      BaseAddr, *std::next(PerfoInfoRecord->field_begin(), FlagsId));
  CGF.EmitStoreOfScalar(PerfoType, FieldAddr);

  FieldAddr = CGF.EmitLValueForField(
      BaseAddr, *std::next(PerfoInfoRecord->field_begin(), ApproxRegionId));
  CGF.EmitStoreOfScalar(RGId, FieldAddr);

  if (PerfoClause.getPerfoType() == approx::PT_SMALL ||
      PerfoClause.getPerfoType() == approx::PT_LARGE) {
    FieldAddr = CGF.EmitLValueForField(
        BaseAddr, *std::next(PerfoInfoRecord->field_begin(), StepId));
    CGF.EmitStoreOfScalar(StepVal, FieldAddr);
  } else {
    FieldAddr = CGF.EmitLValueForField(
        BaseAddr, *std::next(PerfoInfoRecord->field_begin(), RateId));
    CGF.EmitStoreOfScalar(StepVal, FieldAddr);
  }
  /// Cast ptr to void* and assign to respective parameter
  approxRTParams[PerfoDesc] = CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
      PerfoStructAddress.getPointer(), CGM.VoidPtrTy);
  /// Emit Function which needs to be perforated.
  CGApproxRuntimeEmitPerfoFn(CS);
}

void CGApproxRuntime::CGApproxRuntimeEmitIfInit(CodeGenFunction &CGF,
                                                ApproxIfClause &IfClause) {
  if (const auto *PreInit = cast_or_null<DeclStmt>(IfClause.getPreInit())) {
    for (const auto *D : PreInit->decls()) {
      CGF.EmitVarDecl(cast<VarDecl>(*D));
    }
  }
  approxRTParams[Cond] = CGF.EvaluateExprAsBool(IfClause.getCondition());
}

void CGApproxRuntime::CGApproxRuntimeEmitPerfoFn(CapturedStmt &CS) {
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction CGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(CGF, &CGSI);
  llvm::Function *Fn = CGF.GenerateCapturedStmtFunction(CS);
  /// When we decide how the Interaction of the Attributes will work with LLVM
  /// we will add extra information here.
  Fn->addFnAttr("Perforate");
  approxRTParams[PerfoFn] = Fn;
  return;
}

void CGApproxRuntime::CGApproxRuntimeExitRegion(CodeGenFunction &CGF) {
  /// Create Runtime function type
  dbgs() << "Size is " << approxRTParams.capacity() << "\n";
  llvm::FunctionType *RTFnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(CGF.getLLVMContext()),
                              ArrayRef<llvm::Type *>(approxRTTypes), false);
  llvm::Function *RTFn =
      Function::Create(RTFnTy, GlobalValue::ExternalLinkage,
                       "__approx_exec_call", CGM.getModule());
  CGF.EmitCallOrInvoke(RTFn, ArrayRef<llvm::Value *>(approxRTParams));
}
