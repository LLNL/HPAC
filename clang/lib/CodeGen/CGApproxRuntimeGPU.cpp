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
#include "clang/Basic/AddressSpaces.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/Debug.h"
#include "CGOpenMPRuntime.h"
#include <tuple>
#include "CGOpenMPRuntimeGPU.h"

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

static llvm::Value *getPointer(CodeGenFunction &CGF, const Expr *E)
{
  llvm::Value *Addr = CGF.EmitLValue(E).getPointer(CGF);
  return Addr;
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
    if(!StrideExpr) {
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

void CGApproxRuntimeGPU::getVarInfoType(ASTContext &C, QualType &VarRegionTy) {
  if (VarRegionTy.isNull()) {
    RecordDecl *VarInfoRD = C.buildImplicitRecord("approx_region_specification");
    VarInfoRD->startDefinition();
    QualType SizeOfType = C.getSizeType();
    SizeOfType = C.getCanonicalType(SizeOfType).withConst();
    // sizeof(type)
    addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    // /// number of elements
    // addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    // /// stride of access
    // addFieldToRecordDecl(C, VarInfoRD, SizeOfType);
    /// Data Type can be negative.
    /// The bitwidth will depend on the way we support
    /// user types/ primary types. Keep it 8 atm.
    addFieldToRecordDecl(C, VarInfoRD, C.getIntTypeForBitwidth(8, true).withConst());
    /// The directionality of this region in/out/inout
    addFieldToRecordDecl(C, VarInfoRD, C.getIntTypeForBitwidth(8, false).withConst());
    VarInfoRD->completeDefinition();
    VarRegionTy = C.getRecordType(VarInfoRD);
  }
  return;
}

void CGApproxRuntimeGPU::getVarAccessType(ASTContext &C, QualType &VarInfoTy) {
  if (VarInfoTy.isNull()) {
    RecordDecl *VarAccessRD = C.buildImplicitRecord("approx_var_access_t");
    QualType SizeOfType = C.getSizeType();
    SizeOfType = C.getCanonicalType(SizeOfType).withConst();
    VarAccessRD->startDefinition();
    /// Void pointer pointing to data values
    // addFieldToRecordDecl(C, VarAccessRD, C.getIntPtrType());
    // /// number of elements
    addFieldToRecordDecl(C, VarAccessRD, SizeOfType);
    // /// stride of access
    addFieldToRecordDecl(C, VarAccessRD, SizeOfType);
    VarAccessRD->completeDefinition();
    VarInfoTy = C.getRecordType(VarAccessRD);
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
                                           /* Perfo fn ptr*/ llvm::PointerType::getUnqual(CallbackFnTy),
                                           /* Captured data ptr*/ CGM.VoidPtrTy,
                                           /* Memoization Type */ CGM.Int32Ty,
                                           /* Input Data Descr. */ CGM.VoidPtrTy,
                                           /* Input Access Descr. */ CGM.VoidPtrTy,
                                           /* Input Data Pointers. */ CGM.VoidPtrTy,
                                           /* Input Data Num Elements */ CGM.Int32Ty,
                                           /* Output Data Descr. */ CGM.VoidPtrTy,
                                           /* Output Access Descr. */ CGM.VoidPtrTy,
                                           /* Output Data Pointers. */ CGM.VoidPtrTy,
                                           /* Output Data Num Elements */ CGM.Int32Ty,
                                           /* Initialization done */ CGM.Int8Ty
                                         },
                                         false);
}

// CGApproxRuntimeGPU::~CGApproxRuntimeGPU(){ delete RegionInfo };
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
  getVarAccessType(C, VarAccessTy);
  Inputs.clear();
  Outputs.clear();
  for (unsigned i = DEV_ARG_START; i < DEV_ARG_END; i++)
    approxRTParams[i] = nullptr;

  Address CapStructAddr = CGF.GenerateCapturedStmtArgument(CS);
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction localCGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(localCGF, &CGSI);
  llvm::Function *Fn = localCGF.GenerateCapturedStmtFunction(CS);
  Fn->addFnAttr(Attribute::AttrKind::AlwaysInline);
  Fn->addFnAttr(Attribute::AttrKind::ArgMemOnly);

  /// Fill in parameters of runtime function call
  /// Put default values on everything.
  /// EmitClause* Will replace as necessary
  approxRTParams[DevAccurateFn] =
    CGF.Builder.CreatePointerCast(Fn, CallbackFnTy->getPointerTo());
  approxRTParams[DevPerfoFn] = llvm::ConstantPointerNull::get(
      llvm::PointerType::getUnqual(CallbackFnTy));
  approxRTParams[DevCapDataPtr] =
      CGF.Builder.CreatePointerCast(CapStructAddr.getPointer(), CGM.VoidPtrTy);
  approxRTParams[DevDataDescIn] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevAccessDescIn] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevDataPtrIn] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevDataSizeIn] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);
  approxRTParams[DevDataDescOut] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevAccessDescOut] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevDataPtrOut] = llvm::ConstantPointerNull::get(CGM.VoidPtrTy);
  approxRTParams[DevDataSizeOut] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);
  approxRTParams[DevMemoDescr] =
      llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 0);

  StartLoc = CS.getBeginLoc();
  EndLoc = CS.getEndLoc();
  return;
}

// TODO: Should we add LoopExprs to the PerfoClause?
void CGApproxRuntimeGPU::CGApproxRuntimeEmitPerfoInit(
    CodeGenFunction &CGF, CapturedStmt &CS, ApproxPerfoClause &PerfoClause, const ApproxLoopHelperExprs &LoopExprs) {
  // we don't use any perfo metadata at this point -- can be added later as needed
  /// Emit Function which needs to be perforated.
  CGApproxRuntimeEmitPerfoFn(CS, LoopExprs, PerfoClause);
}

void CGApproxRuntimeGPU::CGApproxRuntimeEmitPerfoFn(
    CapturedStmt &CS, const ApproxLoopHelperExprs &LoopExprs,
    const ApproxPerfoClause &PC) {
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction CGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(CGF, &CGSI);

  llvm::Function *Fn = CGF.GeneratePerfoCapturedStmtFunction(CS, LoopExprs, PC);
  approxRTParams[DevPerfoFn] =
      CGF.Builder.CreatePointerCast(Fn, CallbackFnTy->getPointerTo());
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
  StringRef RTFnName("__approx_device_exec_call");
  RTFnDev = CGM.getModule().getFunction(RTFnName);

  assert(RTFnTy != nullptr);
  if (!RTFnDev)
    {
      RTFnDev = Function::Create(RTFnTy, GlobalValue::ExternalLinkage, RTFnName,
                                 CGM.getModule());
      RTFnDev->addFnAttr(Attribute::AttrKind::AlwaysInline);
    }

  llvm::FunctionCallee RTFnCallee({RTFnTy, RTFnDev});
  CGF.EmitRuntimeCall(RTFnCallee, ArrayRef<llvm::Value *>(approxRTParams));

  // Signal now that we have initialized any runtime state -- allows us to check this value in the runtime function call
  //TODO: clean this up
  QualType BoolTy = CGF.getContext().getIntTypeForBitwidth(8, false);
  CGF.EmitStoreOfScalar(llvm::ConstantInt::get(CGF.Int8Ty, 1, false), *ApproxInitAddress, false, BoolTy, AlignmentSource::Type, false, false);
}

void CGApproxRuntimeGPU::CGApproxRuntimeEmitInitData(
  CodeGenFunction &CGF,
  llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, Address BasePtr) {

  int numVars = Data.size();
  ASTContext &C = CGM.getContext();

  // Address VarPtrArray = CGF.Builder.CreateConstArrayGEP(BasePtr, 0);
  unsigned Pos = 0;
  enum VarPtrFieldID { NUM_ELEM, STRIDE };

  for (auto P : Data) {
    llvm::Value *Addr;
    Expr *E = P.first;
    Directionality Dir = P.second;
    Addr = getPointer(CGF, E);
    // Store Addr
    LValue Base = CGF.MakeAddrLValue(
        CGF.Builder.CreateConstArrayGEP(BasePtr, Pos), C.VoidPtrTy);
    CGF.EmitStoreOfScalar(CGF.Builder.CreatePtrToInt(Addr, CGF.IntPtrTy),
                          Base);
    Pos++;
  }
}

void CGApproxRuntimeGPU::declareApproxInit(CodeGenFunction& CGF)
{
  QualType BoolTy = CGF.getContext().getIntTypeForBitwidth(8, false);
  llvm::Type *BoolMemTy = CGF.ConvertTypeForMem(BoolTy);
  std::string name = "approx_init";


  // Leak, but who cares
  ApproxInit = new GlobalVariable(CGM.getModule(), BoolMemTy, false, GlobalValue::InternalLinkage,
                               llvm::Constant::getNullValue(BoolMemTy),
                               name,
                               /*InsertBefore=*/ nullptr,
                               /*ThreadLocalMode=*/ GlobalValue::NotThreadLocal,
                               // how do we do this better?
                                  CGM.getContext().getTargetAddressSpace(clang::LangAS::cuda_shared)
                               );
  ApproxInitAddress = std::make_unique<Address>(this->getAddressofVarInAddressSpace(CGF, ApproxInit, BoolTy, clang::LangAS::cuda_shared));
  CGF.EmitStoreOfScalar(llvm::ConstantInt::get(CGF.Int8Ty, 0, false), *ApproxInitAddress, false, BoolTy, AlignmentSource::Type, false, false);

}

Address CGApproxRuntimeGPU::getAddressofVarInAddressSpace(CodeGenFunction &CGF, llvm::Value *V, QualType T, clang::LangAS AS)
{
  llvm::Type *MemType = CGF.ConvertTypeForMem(T);

  return Address(
                 CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
                 V, MemType->getPointerTo(CGM.getContext().getTargetAddressSpace(AS))),
                 MemType, CGM.getContext().getPreferredTypeAlignInChars(T)
                 );
}

std::unique_ptr<Address> CGApproxRuntimeGPU::declareAccessArrays(CodeGenFunction &CGF,
    llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *name) {

  int numVars = Data.size();
  ASTContext &C = CGM.getContext();
  llvm::SmallVector<llvm::Type*, 3> AITypes{CGF.SizeTy, CGF.SizeTy};
  llvm::StructType *AccessInfoStructTy = llvm::StructType::create(CGM.getLLVMContext(),
                                                               AITypes);
  AccessInfoStructTy->setName("approx_var_access_t");
  llvm::ArrayType *AccessStArrTy = llvm::ArrayType::get(AccessInfoStructTy, numVars);

  QualType VarPtrArrayTy = C.getConstantArrayType(VarAccessTy, llvm::APInt(64, numVars),
                                          nullptr, ArrayType::Normal, 0);
  llvm::Type *MemType = CGF.ConvertTypeForMem(VarPtrArrayTy);
  auto *RD = VarPtrArrayTy->getAsRecordDecl();

  // Leak, but who cares
  AccessInfo = new GlobalVariable(CGM.getModule(), MemType, false, GlobalValue::InternalLinkage,
                                  llvm::Constant::getNullValue(MemType),
                                  name,
                                  /*InsertBefore=*/ nullptr,
                                  /*ThreadLocalMode=*/ GlobalValue::NotThreadLocal,
                                  // how do we do this better?
                                  CGM.getContext().getTargetAddressSpace(clang::LangAS::cuda_shared)
                                  );

  return std::make_unique<Address>(
                 CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
                 AccessInfo, MemType->getPointerTo(CGM.getContext().getTargetAddressSpace(clang::LangAS::cuda_shared))),
                 MemType, CGM.getContext().getPreferredTypeAlignInChars(VarPtrArrayTy));
}

std::unique_ptr<Address> CGApproxRuntimeGPU::declarePtrArrays(CodeGenFunction &CGF,
    llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *name) {

  int numVars = Data.size();
  QualType VarPtrArrayTy;
  ASTContext &C = CGM.getContext();

  VarPtrArrayTy = C.getConstantArrayType(C.VoidPtrTy, llvm::APInt(64, numVars),
                                         nullptr, ArrayType::Normal, 0);

  Address VarPtrArray = CGF.CreateMemTemp(VarPtrArrayTy, name);
  return std::make_unique<Address>(VarPtrArray);
}


std::tuple<llvm::Value *, llvm::Value*>
CGApproxRuntimeGPU::CGApproxRuntimeGPUEmitData(
    CodeGenFunction &CGF,
    llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data,
    Address VarPtrArray, const char *infoName) {
  int numVars = Data.size();
  ASTContext &C = CGM.getContext();
  llvm::Value *NumOfElements =
      llvm::ConstantInt::get(CGM.Int32Ty, numVars, false);

  std::vector<llvm::SmallVector<llvm::Constant*,3>> RegionSpecInit;
  const auto *VarInfoRecord = VarRegionSpecTy->getAsRecordDecl();
  const auto *VarPtrRecord = VarAccessTy->getAsRecordDecl();
  unsigned Pos = 0;
  enum VarPtrFieldID { NUM_ELEM, STRIDE };

  Address VarPtrArrGEP = CGF.Builder.CreateConstArrayGEP(VarPtrArray, 0);


  for (auto P : Data) {

    llvm::SmallVector<llvm::Constant*, 3> SpecInit;
    llvm::Value *Addr;
    llvm::Value *NumElements;
    llvm::Value *TypeOfElement;
    llvm::Value *AccessStride;
    llvm::Value *SizeOfElement;
    Expr *E = P.first;
    Directionality Dir = P.second;
    std::tie(Addr, NumElements, SizeOfElement, AccessStride, TypeOfElement) =
        getPointerAndSize(CGF, E);
    LValue Base = CGF.MakeAddrLValue(
        CGF.Builder.CreateConstGEP(VarPtrArrGEP, Pos), VarAccessTy);
    // Store NUM_ELEMENTS
    LValue nElemLVal = CGF.EmitLValueForField(
        Base, *std::next(VarPtrRecord->field_begin(), NUM_ELEM));
    CGF.EmitStoreOfScalar(NumElements, nElemLVal);

    // Store SZ_ELEM
    SpecInit.push_back(static_cast<llvm::Constant*>(SizeOfElement));

    // Store STRIDE
    LValue strideElemLVal = CGF.EmitLValueForField(
        Base, *std::next(VarPtrRecord->field_begin(), STRIDE));
    // CGF.EmitStoreOfScalar(AccessStride, strideElemLVal);

    // Store DATA_TYPE
    SpecInit.push_back(static_cast<llvm::Constant*>(TypeOfElement));

    // Store Dir
    SpecInit.push_back(llvm::ConstantInt::get(CGF.Int8Ty, Dir, false) );

    RegionSpecInit.push_back(SpecInit);
    Pos++;
  }

  llvm::SmallVector<llvm::Type*, 3> STTypes{CGF.SizeTy, CGF.Int8Ty, CGF.Int8Ty};
  llvm::StructType *VarSpecStructType = llvm::StructType::create(CGM.getLLVMContext(),
                                                              STTypes);
  VarSpecStructType->setName("approx_region_specification");
  llvm::ArrayType *SpecStructArrTy = llvm::ArrayType::get(VarSpecStructType, numVars);
  std::vector<llvm::Constant *> InitStructs;
  // Create each struct individually
  for(auto S : RegionSpecInit) {

    InitStructs.push_back(llvm::ConstantStruct::get(VarSpecStructType, S));
  }
  llvm::Constant *InitArray = llvm::ConstantArray::get(SpecStructArrTy, InitStructs);

  // Leak, but who cares
  RegionInfo = new GlobalVariable(CGM.getModule(), SpecStructArrTy, true, GlobalValue::InternalLinkage,
                                  InitArray,
                                  infoName,
                                  /*InsertBefore=*/ nullptr,
                                  /*ThreadLocalMode=*/ GlobalValue::NotThreadLocal,
                                  // how do we do this better?
                                  Optional<unsigned>((unsigned) 4)
                                  );

  // todo: address space cast for info type
  return std::make_tuple(NumOfElements,
                         CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
                         RegionInfo, CGF.VoidPtrTy)
                         );
}

void CGApproxRuntimeGPU::CGApproxRuntimeEmitDataValues(CodeGenFunction &CGF) {
  /// No Dependencies so exit.
  static int input_arrays = 0;
  static int output_arrays = 0;
  char nameInfo[100];
  char namePtr[100];
  if (!requiresData)
    return;


  ASTContext &C = CGM.getContext();
  QualType BoolTy = C.getIntTypeForBitwidth(8, false);
  // any way to have actual bool?
  auto &OMPRT = static_cast<CGOpenMPRuntimeGPU &>(CGF.CGM.getOpenMPRuntime());

  llvm::BasicBlock *CheckBody = CGF.createBasicBlock("approx.check_init");
  llvm::BasicBlock *StoreBody = CGF.createBasicBlock("approx.init_vars");
  llvm::BasicBlock *ContinueBody = CGF.createBasicBlock("approx.continue");
  llvm::BasicBlock *StoreVals = CGF.createBasicBlock("approx.store_access_vars");
  llvm::BasicBlock *Synchronize = CGF.createBasicBlock("approx.sync_after_store");
  CGF.EmitBranch(CheckBody);

  std::unique_ptr<Address> ArrayIptBase;
  std::unique_ptr<Address> ArrayOptBase;

  std::unique_ptr<Address> ArrayIptPtrBase;
  std::unique_ptr<Address> ArrayOptPtrBase;

 CGF.EmitBlock(CheckBody);

  if(requiresInputs && Inputs.size() > 0)
    {
      sprintf(namePtr, ".dep.approx_ipt_access.arr.addr_%d", input_arrays);
      ArrayIptBase = declareAccessArrays(CGF, Inputs, namePtr);

      sprintf(namePtr, ".dep.approx_ipt_ptr.arr.addr_%d", input_arrays);
      ArrayIptPtrBase = declarePtrArrays(CGF, Inputs, namePtr);
    }

  sprintf(namePtr, ".dep.approx_opt_access.arr.addr_%d", output_arrays);
  ArrayOptBase = declareAccessArrays(CGF, Outputs, namePtr);

  sprintf(namePtr, ".dep.approx_opt_ptr.arr.addr_%d", output_arrays);
  ArrayOptPtrBase = declarePtrArrays(CGF, Outputs, namePtr);

  llvm::Value *InitCheckValue = CGF.EmitLoadOfScalar(*ApproxInitAddress,
                                                     false, BoolTy, SourceLocation()
                                                     );
  approxRTParams[DevInitDone] = InitCheckValue;

  llvm::Value *InitDone = CGF.Builder.CreateICmpEQ(InitCheckValue, llvm::ConstantInt::get(CGF.Int8Ty, 1));
  CGF.Builder.CreateCondBr(InitDone, ContinueBody, StoreBody);

  CGF.EmitBlock(StoreBody);

  llvm::Value *ThreadID = OMPRT.getGPUThreadID(CGF);
  llvm::Value *AmFirstThreadInBlock = CGF.Builder.CreateICmpEQ(ThreadID, llvm::ConstantInt::get(CGF.IntTy, 0));
  CGF.Builder.CreateCondBr(AmFirstThreadInBlock, StoreVals, Synchronize);

  CGF.EmitBlock(StoreVals);

  llvm::Value *NumOfElements, *InfoAddress, *PtrAddress;
  if (requiresInputs && Inputs.size() > 0) {
    sprintf(nameInfo, ".dep.approx_inputs.arr.addr_%d", input_arrays++);
    std::tie(NumOfElements, InfoAddress) =
      CGApproxRuntimeGPUEmitData(CGF, Inputs, *ArrayIptBase, nameInfo);
    approxRTParams[DevDataDescIn] = InfoAddress;
    approxRTParams[DevDataPtrIn] = CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(ArrayIptPtrBase->getPointer(), CGF.VoidPtrTy);
    approxRTParams[DevAccessDescIn] = CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(
                            ArrayIptBase->getPointer(), CGF.VoidPtrTy);
    approxRTParams[DevDataSizeIn] = NumOfElements;
  }

  // // All approximation techniques require the output
  sprintf(nameInfo, ".dep.approx_outputs.arr.addr_%d", output_arrays++);
  std::tie(NumOfElements, InfoAddress) =
    CGApproxRuntimeGPUEmitData(CGF, Outputs, *ArrayOptBase, nameInfo);
  approxRTParams[DevDataDescOut] = InfoAddress;
  approxRTParams[DevAccessDescOut] = CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(ArrayOptBase->getPointer(), CGF.VoidPtrTy);
  approxRTParams[DevDataPtrOut] = CGF.Builder.CreatePointerBitCastOrAddrSpaceCast(ArrayOptPtrBase->getPointer(), CGF.VoidPtrTy);
  approxRTParams[DevDataSizeOut] = NumOfElements;

  CGF.EmitBranch(Synchronize);
  CGF.EmitBlock(Synchronize);
  OMPRT.syncCTAThreads(CGF);

  // now the metadata have been written once, but we need to write the
  // pointer in the ContinueBody because it will be different each iteration
  CGF.EmitBranch(ContinueBody);
  CGF.EmitBlock(ContinueBody);

  if(requiresInputs && Inputs.size() > 0) {
    CGApproxRuntimeEmitInitData(CGF, Inputs, *ArrayIptPtrBase);
  }
  CGApproxRuntimeEmitInitData(CGF, Outputs, *ArrayOptPtrBase);
}
