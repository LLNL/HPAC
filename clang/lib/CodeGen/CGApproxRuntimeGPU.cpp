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


static int8_t convertToApproxType(const BuiltinType *T) {
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
  // Long double not available for nvptx64 target
  // case BuiltinType::Kind::LongDouble:
  //   approxType = LDOUBLE;
  //   break;
  default:
    approxType = ApproxType::INVALID;
    break;
  }
  return approxType;
}

static std::tuple<llvm::Value *, llvm::Value *, llvm::Value *, llvm::Value *, llvm::Value *>
getPointerAndSize(CodeGenFunction &CGF, const Expr *E) {
  // Address of first Element.
  llvm::Value *Addr;
  // Total Size in Bytes.
  llvm::Value *Size;
  // Stride number of items between access
  llvm::Value *Stride;
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
    Address UpAddrAddress = UpAddrLVal.getAddress(CGF);
    llvm::Value *UpAddr =
      CGF.Builder.CreateConstGEP1_32(UpAddrAddress.getElementType(), UpAddrAddress.getPointer(), /*Idx0=*/1);

    llvm::Value *LowIntPtr = CGF.Builder.CreatePtrToInt(Addr, CGF.SizeTy);
    llvm::Value *UpIntPtr = CGF.Builder.CreatePtrToInt(UpAddr, CGF.SizeTy);
    Size = CGF.Builder.CreateNUWSub(UpIntPtr, LowIntPtr);
    SizeOfElement = CGF.getTypeSize(ResultExprTy);
    NumElements = CGF.Builder.CreateUDiv(Size, SizeOfElement);
    TypeOfElement = llvm::ConstantInt::get(CGF.Builder.getInt8Ty(), TyKind);

    // Get the stride integer value.
    const Expr *StrideExpr = ASE->getStride();
    Expr::EvalResult StrideResult;
    if(!StrideExpr)
      {
        Stride = llvm::ConstantInt::get(CGF.Int64Ty, 1);
      }
    else {
    Stride = CGF.Builder.CreateIntCast(CGF.EmitScalarExpr(StrideExpr),
                                       CGF.Int64Ty, /*isSigned=*/false);
    }

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
    Stride = llvm::ConstantInt::get(CGF.Int64Ty, 1);
    TypeOfElement = llvm::ConstantInt::get(CGF.Builder.getInt8Ty(), TyKind);
  }
  return std::make_tuple(Addr, NumElements, SizeOfElement, Stride, TypeOfElement);
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

void CGApproxRuntimeGPU::getVarInfoType(ASTContext &C, QualType &VarInfoTy) {
  if (VarInfoTy.isNull()) {
    RecordDecl *VarInfoRD = C.buildImplicitRecord("approx_region_specification");
    VarInfoRD->startDefinition();
    QualType SizeOfType = C.getSizeType();
    SizeOfType = C.getCanonicalType(SizeOfType).withConst();
    // sizeof(type)
    addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    /// number of elements
    addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    /// stride of access
    addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    /// Data Type can be negative.
    /// The bitwidth will depend on the way we support
    /// user types/ primary types. Keep it 8 atm.
    addFieldToRecordDecl(C, VarInfoRD, C.getIntTypeForBitwidth(8, true).withConst());
    /// The directionality of this region in/out/inout
    addFieldToRecordDecl(C, VarInfoRD, C.getIntTypeForBitwidth(8, false).withConst());
    VarInfoRD->completeDefinition();
    VarInfoTy = C.getRecordType(VarInfoRD);
  }
  return;
}

void CGApproxRuntimeGPU::getVarPtrType(ASTContext &C, QualType &VarInfoTy) {
  if (VarInfoTy.isNull()) {
    RecordDecl *VarPtrRD = C.buildImplicitRecord("approx_var_ptr_t");
    VarPtrRD->startDefinition();
    /// Void pointer pointing to data values
    addFieldToRecordDecl(C, VarPtrRD, C.getIntPtrType());
    VarPtrRD->completeDefinition();
    VarPtrTy = C.getRecordType(VarInfoRD);
  }
  return;
}


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
                                           /* Input Data Pointers. */ CGM.VoidPtrTy,
                                           /* Input Data Num Elements */ CGM.Int32Ty,
                                           /* Output Data Descr. */ CGM.VoidPtrTy,
                                           /* Output Data Pointers. */ CGM.VoidPtrTy,
                                           /* Output Data Num Elements */ CGM.Int32Ty
                                         },
                                         false);
}

CGApproxRuntimeGPU::~CGApproxRuntimeGPU(){ delete RegionInfo };
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
  getVarInfoType(C, VarRegionSpecTy);
  getVarInfoType(C, VarPtrTy);
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

std::pair<llvm::Value *, llvm::Value *>
CGApproxRuntimeGPU::CGApproxRuntimeEmitData(
    CodeGenFunction &CGF,
    llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data,
    const char *arrayName) {
  int numVars = Data.size();
  ASTContext &C = CGM.getContext();
  QualType VarSpecArrayTy;
  QualType VarPtrArrayTy;
  llvm::Value *NumOfElements =
      llvm::ConstantInt::get(CGM.Int32Ty, numVars, false);

  // this should be just one
  VarSpecArrayTy = C.getConstantArrayType(VarRegionSpecTy, llvm::APInt(64, numVars),
                                         nullptr, ArrayType::Normal, 0);
  VarPtrArrayTy = C.getConstantArrayType(VarPtTry, llvm::APInt(64, numVars),
                                         nullptr, ArrayType::Normal, 0);

  std::vector<llvm::SmallVector<llvm::Constant*,5>> RegionSpecInit;
  // Address VarRegionArray = CGF.CreateMemTemp(VarSpecArrayTy, arrayName);
  // VarRegionArray = CGF.Builder.CreateConstArrayGEP(VarRegionArray, 0);
  // TODO: these have the same name
  Address VarPtrArray = CGF.CreateMemTemp(VarPtrArrayTy, arrayName);
  VarPtrArray = CGF.Builder.CreateConstArrayGEP(VarPtrArray, 0);
  // Address VarPtrArray = CGF.Builder.CreateConstArrayGep(VarPtrArray, 0);

  const auto *VarInfoRecord = VarRegionSpecTy->getAsRecordDecl();
  const auto *VarPtrRecord = VarPtrTy->getAsRecordDecl();
  unsigned Pos = 0;
  enum VarRegionFieldID { SZ_ELEM, NUM_ELEM, STRIDE, DATA_TYPE, DIR };
  enum VarPtrFieldID { PTR };

  for (auto P : Data) {

    llvm::SmallVector<llvm::Constant*> SpecInit;
    llvm::Value *Addr;
    llvm::Value *NumElements;
    llvm::Value *TypeOfElement;
    llvm::Value *AccessStride;
    llvm::Value *SizeOfElement;
    Expr *E = P.first;
    Directionality Dir = P.second;
    std::tie(Addr, NumElements, SizeOfElement, AccessStride, TypeOfElement) =
        getPointerAndSize(CGF, E);
    // Store Addr
    LValue Base = CGF.MakeAddrLValue(
        CGF.Builder.CreateConstGEP(VarPtrArray, Pos), VarPtrTy);
    auto *FieldT = *std::next(VarPtrRecord->field_begin(), PTR);
    LValue BaseAddrLVal = CGF.EmitLValueForField(Base, FieldT);
    CGF.EmitStoreOfScalar(CGF.Builder.CreatePtrToInt(Addr, CGF.IntPtrTy),
                          BaseAddrLVal);

    // Store NUM_ELEMENTS
    SpecInit.push_back(llvm::ConstantInt::get(CGF.Int64Ty, NumElements, false) );
    // LValue nElemLVal = CGF.EmitLValueForField(
    //     Base, *std::next(VarPtrRecord->field_begin(), NUM_ELEM));
    // CGF.EmitStoreOfScalar(NumElements, nElemLVal);

    // Store SZ_ELEM
    SpecInit.push_back(llvm::ConstantInt::get(CGF.Int64Ty, SizeOfElement, false) );
    // LValue sElemLVal = CGF.EmitLValueForField(
    //     Base, *std::next(VarInfoRecord->field_begin(), SZ_ELEM));
    // CGF.EmitStoreOfScalar(SizeOfElement, sElemLVal);

    // Store STRIDE
    // LValue strideElemLVal = CGF.EmitLValueForField(
    //     Base, *std::next(VarInfoRecord->field_begin(), STRIDE));
    // CGF.EmitStoreOfScalar(AccessStride, strideElemLVal);
    SpecInit.push_back(llvm::ConstantInt::get(CGF.Int64Ty, AccessStride, false) );

    // Store DATA_TYPE
    SpecInit.push_back(llvm::ConstantInt::get(CGF.Int8ty, TypeOfElement, false) );
    // LValue typeLVal = CGF.EmitLValueForField(
    //     Base, *std::next(VarInfoRecord->field_begin(), DATA_TYPE));
    // CGF.EmitStoreOfScalar(TypeOfElement, typeLVal);

    // Store Dir
    SpecInit.push_back(llvm::ConstantInt::get(CGF.Int8ty, Dir, false) );
    // Value *direction = llvm::ConstantInt::get(CGM.Int8Ty, Dir, false);
    // LValue DirLVal = CGF.EmitLValueForField(
    //     Base, *std::next(VarInfoRecord->field_begin(), DIR));
    // CGF.EmitStoreOfScalar(direction, DirLVal);

    RegionSpecInit.push_back(SpecInit);
    Pos++;
  }

  llvm::StructType *VarSpecStructType;
  std::vector<llvm::Constant *> InitStructs;
  // Create each struct individually
  for(auto S : RegionSpecInit) {
    // InitStructs.push_back(new llvm::GlobalVariable());
  }

  // RegionInfo = new GlobalVariable(CGF.getModule(), VarSpecArrayTy, true, GlobalValue::ExternalLinkage,
  //                                 InitStructs?,
  //                                 infoName
  //                                 );

  return std::make_pair(NumOfElements,
                        CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
                            VarInfoArray.getPointer(), CGF.VoidPtrTy));
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
