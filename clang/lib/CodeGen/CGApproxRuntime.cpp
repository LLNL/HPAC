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

int8_t convertToApproxType(const BuiltinType *T) {
  ApproxType approxType;
  switch (T->getKind()) {
  case BuiltinType::Kind::Bool:
    approxType = BOOL;
    break;
  case BuiltinType::Kind::Char_U:
  case BuiltinType::Kind::UChar:
  case BuiltinType::Kind::WChar_U:
    approxType = UCHAR;
    break;
  case BuiltinType::Kind::UShort:
    approxType = USHORT;
    break;
  case BuiltinType::Kind::UInt:
    approxType = UINT;
    break;
  case BuiltinType::Kind::ULong:
    approxType = ULONG;
    break;
  case BuiltinType::Kind::ULongLong:
    approxType = ULONGLONG;
    break;
  case BuiltinType::Kind::Char_S:
  case BuiltinType::Kind::SChar:
  case BuiltinType::Kind::WChar_S:
    approxType = SCHAR;
    break;
  case BuiltinType::Kind::Short:
    approxType = SHORT;
    break;
  case BuiltinType::Kind::Int:
    approxType = INT;
    break;
  case BuiltinType::Kind::Long:
    approxType = LONG;
    break;
  case BuiltinType::Kind::LongLong:
    approxType = LONGLONG;
    break;
  case BuiltinType::Kind::Float:
    approxType = FLOAT;
    break;
  case BuiltinType::Kind::Double:
    approxType = DOUBLE;
    break;
  case BuiltinType::Kind::LongDouble:
    approxType = LDOUBLE;
    break;
  default:
    approxType = ApproxType::INVALID;
    break;
  }
  return approxType;
}

static std::tuple<llvm::Value *, llvm::Value *, llvm::Value *, llvm::Value *>
getPointerAndSize(CodeGenFunction &CGF, const Expr *E) {
  // Address of first Element.
  llvm::Value *Addr;
  // Total Size in Bytes.
  llvm::Value *Size;
  // Number of elements
  llvm::Value *NumElements;
  // Data Type
  llvm::Value *TypeOfElement;
  // This is actually required only for
  // user defined types. Everything else
  // should already be known by the RT system.
  llvm::Value *SizeOfElement;

  Addr = CGF.EmitLValue(E).getPointer(CGF);

  if (const auto *ASE =
          dyn_cast<ApproxArraySectionExpr>(E->IgnoreParenImpCasts())) {
    QualType BaseTy =
        ApproxArraySectionExpr::getBaseOriginalType(ASE->getBase());
    QualType ResultExprTy = BaseTy;
    int8_t TyKind = -1;

    // Drill down to find the scalar type we are point to.
    do {
      if (auto *AT = CGF.getContext().getAsArrayType(ResultExprTy))
        ResultExprTy = AT->getElementType();
      else
        ResultExprTy = ResultExprTy->getPointeeType();
    } while (ResultExprTy->isPointerType() || ResultExprTy->isReferenceType() ||
             ResultExprTy->isArrayType());

    if (const BuiltinType *T = ResultExprTy->getAs<BuiltinType>()) {
      TyKind = convertToApproxType(T);
    }

    // The array slicer does not yet support multi-dimensional slicing
    // Example:
    // int a[100];
    // #pragma omp in(a[0:10]) ---> will work correclty.
    // int a[100][100];
    // #pragma omp in(a[0:10][:]) ---> will work correclty.
    // int a[100][100];
    // #pragma omp in(a[4:2][5:4]) ---> will NOT work correclty.
    // Solution 1:
    // Describe access  dimensions with some kind of struct
    // with an access pattern, strides etc.
    // Solution 2:
    // Create code that iterats through all the outer indexes
    // and points to the inner continues memeory array and pass
    // to the runtime system multiple in/out/inout parameters.
    LValue UpAddrLVal =
        CGF.EmitApproxArraySectionExpr(ASE, /*IsLowerBound=*/false);
    llvm::Value *UpAddr =
        CGF.Builder.CreateConstGEP1_32(UpAddrLVal.getPointer(CGF), /*Idx0=*/1);

    llvm::Value *LowIntPtr = CGF.Builder.CreatePtrToInt(Addr, CGF.SizeTy);
    llvm::Value *UpIntPtr = CGF.Builder.CreatePtrToInt(UpAddr, CGF.SizeTy);
    Size = CGF.Builder.CreateNUWSub(UpIntPtr, LowIntPtr);
    SizeOfElement = CGF.getTypeSize(ResultExprTy);
    NumElements = CGF.Builder.CreateUDiv(Size, SizeOfElement);
    TypeOfElement = llvm::ConstantInt::get(CGF.Builder.getInt8Ty(), TyKind);
  } else {
    QualType Ty = E->getType();
    int TyKind = -1;
    if (const BuiltinType *T = Ty->getAs<BuiltinType>()) {
      TyKind = convertToApproxType(T);
    }

    SizeOfElement = CGF.getTypeSize(Ty);
    Size = SizeOfElement;
    QualType SizeOfType = CGF.getContext().getSizeType();
    NumElements = llvm::ConstantInt::get(CGF.ConvertType(SizeOfType), 1);
    TypeOfElement = llvm::ConstantInt::get(CGF.Builder.getInt8Ty(), TyKind);
  }
  return std::make_tuple(Addr, NumElements, SizeOfElement, TypeOfElement);
}

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

static void getVarInfoType(ASTContext &C, QualType &VarInfoTy) {
  if (VarInfoTy.isNull()) {
    RecordDecl *VarInfoRD = C.buildImplicitRecord("approx_var_info_t");
    VarInfoRD->startDefinition();
    /// Void pointer pointing to data values
    addFieldToRecordDecl(C, VarInfoRD, C.getIntPtrType());
    QualType SizeOfType = C.getSizeType();
    SizeOfType = C.getCanonicalType(SizeOfType);
    /// number of elements
    addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    /// Sizeof(type)
    addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    /// Data Type can be negative.
    /// The bitwidth will depend on the way we support
    /// user types/ primary types. Keep it 8 atm.
    addFieldToRecordDecl(C, VarInfoRD, C.getIntTypeForBitwidth(8, true));
    /// The directionality of this region in/out/inout
    addFieldToRecordDecl(C, VarInfoRD, C.getIntTypeForBitwidth(8, false));
    VarInfoRD->completeDefinition();
    VarInfoTy = C.getRecordType(VarInfoRD);
  }
  return;
}

CGApproxRuntime::CGApproxRuntime(CodeGenModule &CGM)
    : CGM(CGM), CallbackFnTy(nullptr), RTFnTy(nullptr), approxRegions(0),
      StartLoc(SourceLocation()), EndLoc(SourceLocation()), requiresData(false),
      requiresInputs(false) {
  ASTContext &C = CGM.getContext();
  getPerfoInfoType(C, PerfoInfoTy);
  getVarInfoType(C, VarInfoTy);

  CallbackFnTy = llvm::FunctionType::get(CGM.VoidTy, {CGM.VoidPtrTy}, false);

  // This is the runtime call function type information, which mirrors the
  // types provided in the argument parameters.
  RTFnTy = llvm::FunctionType::get(
      CGM.VoidTy, {/* Orig. fn ptr*/ llvm::PointerType::getUnqual(CallbackFnTy),
                   /* Perfo fn ptr*/ llvm::PointerType::getUnqual(CallbackFnTy),
                   /* Captured data ptr*/ CGM.VoidPtrTy,
                   /* Cond Value*/ llvm::Type::getInt1Ty(CGM.getLLVMContext()),
                   /* Perfo Description */ CGM.VoidPtrTy,
                   /* Memoization Type*/ CGM.Int32Ty,
                   /* Input Data Descr*/ CGM.VoidPtrTy,
                   /* Input Data Num Elements*/ CGM.Int32Ty,
                   /* Ouput Data Descr. */ CGM.VoidPtrTy,
                   /* Output Data Num Elements*/ CGM.Int32Ty},false);
}

void CGApproxRuntime::CGApproxRuntimeEnterRegion(CodeGenFunction &CGF,
                                                 CapturedStmt &CS) {
  requiresInputs = false;
  requiresData = false;
  /// Reset All info of the Runtime "state machine"
  for (unsigned i = ARG_START; i < ARG_END; i++)
    approxRTParams[i] = nullptr;

  Address CapStructAddr = CGF.GenerateCapturedStmtArgument(CS);
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction localCGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(localCGF, &CGSI);
  llvm::Function *Fn = localCGF.GenerateCapturedStmtFunction(CS);

  /// Fill in parameters of runtime function call
  /// Put default values on everything.
  /// EmitClause* Will replace as necessary
  approxRTParams[AccurateFn] =
      CGF.Builder.CreatePointerCast(Fn, CallbackFnTy->getPointerTo());
  approxRTParams[PerfoFn] = llvm::ConstantPointerNull::get(
      llvm::PointerType::getUnqual(CallbackFnTy));
  approxRTParams[CapDataPtr] =
      CGF.Builder.CreatePointerCast(CapStructAddr.getPointer(), CGM.VoidPtrTy);
  approxRTParams[Cond] = llvm::ConstantInt::get(CGF.Builder.getInt1Ty(), true);
  approxRTParams[PerfoDesc] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DataDescIn] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DataSizeIn] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);
  approxRTParams[DataDescOut] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DataSizeOut] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);
  approxRTParams[MemoDescr] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);

  StartLoc = CS.getBeginLoc();
  EndLoc = CS.getEndLoc();
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

void CGApproxRuntime::CGApproxRuntimeEmitMemoInit(
    CodeGenFunction &CGF, ApproxMemoClause &MemoClause) {
  requiresData = true;
  if (MemoClause.getMemoType() == approx::MT_IN) {
    requiresInputs = true;
    approxRTParams[MemoDescr] =
        llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 1);
  } else if (MemoClause.getMemoType() == approx::MT_OUT) {
    approxRTParams[MemoDescr] =
        llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 2);
  }
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
  approxRTParams[AccurateFn] =
      CGF.Builder.CreatePointerCast(Fn, CallbackFnTy->getPointerTo());
  return;
}

void CGApproxRuntime::CGApproxRuntimeExitRegion(CodeGenFunction &CGF) {
  Function *RTFn = nullptr;
  StringRef RTFnName("__approx_exec_call");
  RTFn = CGM.getModule().getFunction(RTFnName);

  assert(RTFnTy != nullptr);
  if (!RTFn)
    RTFn = Function::Create(RTFnTy, GlobalValue::ExternalLinkage, RTFnName,
                            CGM.getModule());

  llvm::FunctionCallee RTFnCallee({RTFnTy, RTFn});
  CGF.EmitRuntimeCall(RTFnCallee, ArrayRef<llvm::Value *>(approxRTParams));
}

void CGApproxRuntime::CGApproxRuntimeRegisterInputs(ApproxInClause &InClause) {
  for (auto *V : InClause.varlist()) {
    Inputs.push_back(std::make_pair(V, Input));
  }
}

void CGApproxRuntime::CGApproxRuntimeRegisterOutputs(
    ApproxOutClause &OutClause) {
  for (auto *V : OutClause.varlist()) {
    Outputs.push_back(std::make_pair(V, Output));
  }
}

void CGApproxRuntime::CGApproxRuntimeRegisterInputsOutputs(
    ApproxInOutClause &InOutClause) {
  for (auto *V : InOutClause.varlist()) {
    Inputs.push_back(std::make_pair(V, InputOuput));
    Outputs.push_back(std::make_pair(V, InputOuput));
  }
}

std::pair<llvm::Value *, llvm::Value *>
CGApproxRuntime::CGApproxRuntimeEmitData(
    CodeGenFunction &CGF,
    llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data,
    const char *arrayName) {
  int numVars = Data.size();
  ASTContext &C = CGM.getContext();
  QualType VarInfoArrayTy;
  llvm::Value *NumOfElements =
      llvm::ConstantInt::get(CGM.Int32Ty, numVars, false);

  VarInfoArrayTy = C.getConstantArrayType(VarInfoTy, llvm::APInt(64, numVars),
                                          nullptr, ArrayType::Normal, 0);

  Address VarInfoArray = CGF.CreateMemTemp(VarInfoArrayTy, arrayName);
  VarInfoArray = CGF.Builder.CreateConstArrayGEP(VarInfoArray, 0);

  const auto *VarInfoRecord = VarInfoTy->getAsRecordDecl();
  unsigned Pos = 0;
  enum VarInfoFieldID { PTR, NUM_ELEM, SZ_ELEM, DATA_TYPE, DIR };

  for (auto P : Data) {
    llvm::Value *Addr;
    llvm::Value *NumElements;
    llvm::Value *TypeOfElement;
    llvm::Value *SizeOfElement;
    Expr *E = P.first;
    Directionality Dir = P.second;
    std::tie(Addr, NumElements, SizeOfElement, TypeOfElement) =
        getPointerAndSize(CGF, E);
    // Store Addr
    LValue Base = CGF.MakeAddrLValue(
        CGF.Builder.CreateConstGEP(VarInfoArray, Pos), VarInfoTy);
    auto *FieldT = *std::next(VarInfoRecord->field_begin(), PTR);
    LValue BaseAddrLVal = CGF.EmitLValueForField(Base, FieldT);
    CGF.EmitStoreOfScalar(CGF.Builder.CreatePtrToInt(Addr, CGF.IntPtrTy),
                          BaseAddrLVal);

    // Store NUM_ELEMENTS
    LValue nElemLVal = CGF.EmitLValueForField(
        Base, *std::next(VarInfoRecord->field_begin(), NUM_ELEM));
    CGF.EmitStoreOfScalar(NumElements, nElemLVal);

    // Store SZ_ELEM
    LValue sElemLVal = CGF.EmitLValueForField(
        Base, *std::next(VarInfoRecord->field_begin(), SZ_ELEM));
    CGF.EmitStoreOfScalar(SizeOfElement, sElemLVal);

    // Store DATA_TYPE
    LValue typeLVal = CGF.EmitLValueForField(
        Base, *std::next(VarInfoRecord->field_begin(), DATA_TYPE));
    CGF.EmitStoreOfScalar(TypeOfElement, typeLVal);

    // Store Dir
    Value *direction = llvm::ConstantInt::get(CGM.Int8Ty, Dir, false);
    LValue DirLVal = CGF.EmitLValueForField(
        Base, *std::next(VarInfoRecord->field_begin(), DIR));
    CGF.EmitStoreOfScalar(direction, DirLVal);
    Pos++;
  }
  return std::make_pair(NumOfElements,
                        CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
                            VarInfoArray.getPointer(), CGF.VoidPtrTy));
}

void CGApproxRuntime::CGApproxRuntimeEmitDataValues(CodeGenFunction &CGF) {
  dbgs()<<"I am trying to emit this mofo\n";
  /// No Dependencies so exit.
  if (!requiresData)
    return;
  dbgs()<<"I am trying to emit this mofo\n";

  llvm::Value *NumOfElements, *ArrayAddress;
  if (requiresInputs && Inputs.size() > 0) {
    std::tie(NumOfElements, ArrayAddress) =
        CGApproxRuntimeEmitData(CGF, Inputs, ".dep.approx_inputs.arr.addr");
    approxRTParams[DataDescIn] = ArrayAddress;
    approxRTParams[DataSizeIn] = NumOfElements;
  }

  // All approximation techniques require the output
  std::tie(NumOfElements, ArrayAddress) =
      CGApproxRuntimeEmitData(CGF, Outputs, ".dep.approx_outputs.arr.addr");
  approxRTParams[DataDescOut] = ArrayAddress;
  approxRTParams[DataSizeOut] = NumOfElements;
}
