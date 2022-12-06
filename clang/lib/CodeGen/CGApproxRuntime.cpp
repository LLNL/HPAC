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
#include "clang/AST/ASTFwd.h"
#include "clang/AST/ApproxClause.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtApprox.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/Basic/Approx.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

using namespace llvm;
using namespace clang;
using namespace CodeGen;

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
    addFieldToRecordDecl(C, perfoInfoRD, C.getRealTypeForBitwidth(32, FloatModeKind::Float));
    perfoInfoRD->completeDefinition();
    perfoInfoTy = C.getRecordType(perfoInfoRD);
  }
  return;
}

void CGApproxRuntime::getVarInfoType(ASTContext &C, QualType &VarInfoTy) {
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
    /// stride of access
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
  : CGM(CGM), CallbackFnTy(nullptr), RTFnTy(nullptr),
    approxRegions(0),
      StartLoc(SourceLocation()), EndLoc(SourceLocation()), requiresData(false),
      requiresInputs(false) {
  ASTContext &C = CGM.getContext();
  CodeGen::CodeGenTypes &Types = CGM.getTypes();
  llvm::PointerType *CharPtrTy =
      llvm::PointerType::getUnqual(Types.ConvertType(C.CharTy));
  getPerfoInfoType(C, PerfoInfoTy);
  getVarInfoType(C, VarInfoTy);

  CallbackFnTy = llvm::FunctionType::get(CGM.VoidTy, {CGM.VoidPtrTy}, false);

  // This is the runtime call function type information, which mirrors the
  // types provided in the argument parameters.
  RTFnTy = llvm::FunctionType::get(
      CGM.VoidTy,
      {/* Orig. fn ptr*/ llvm::PointerType::getUnqual(CallbackFnTy),
       /* Perfo fn ptr*/ llvm::PointerType::getUnqual(CallbackFnTy),
       /* Captured data ptr*/ CGM.VoidPtrTy,
       /* Cond Value*/ llvm::Type::getInt1Ty(CGM.getLLVMContext()),
       /* Label Name */ CharPtrTy,
       /* Perfo Description */ CGM.VoidPtrTy,
       /* Memoization Type*/ CGM.Int32Ty,
       /* Input Data Descr*/ CGM.VoidPtrTy,
       /* Input Data Num Elements*/ CGM.Int32Ty,
       /* Ouput Data Descr. */ CGM.VoidPtrTy,
       /* Output Data Num Elements*/ CGM.Int32Ty},
      false);

}

void CGApproxRuntime::CGApproxRuntimeEnterRegion(CodeGenFunction &CGF,
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
  Inputs.clear();
  Outputs.clear();
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
  approxRTParams[Label] = llvm::ConstantPointerNull::get(CharPtrTy);
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


// TODO: Should we add LoopExprs to the PerfoClause?
void CGApproxRuntime::CGApproxRuntimeEmitPerfoInit(
    CodeGenFunction &CGF, CapturedStmt &CS, ApproxPerfoClause &PerfoClause, const ApproxLoopHelperExprs &LoopExprs) {
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
  CGApproxRuntimeEmitPerfoFn(CS, LoopExprs, PerfoClause);
}

void CGApproxRuntime::CGApproxRuntimeEmitMemoInit(
    CodeGenFunction &CGF, ApproxMemoClause &MemoClause) {
  requiresData = true;
  switch(MemoClause.getMemoType()) {
    case approx::MT_IN:
      requiresInputs = true;
      approxRTParams[MemoDescr] =
        llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 1);
      break;
    case approx::MT_OUT:
    approxRTParams[MemoDescr] =
        llvm::ConstantInt::get(CGF.Builder.getInt32Ty(), 2);
      break;
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

/// Creates the outlined function for a perforated loop.
llvm::Function *CodeGenFunction::GeneratePerfoCapturedStmtFunction(
    const CapturedStmt &CS, const ApproxLoopHelperExprs &LoopExprs,
    const ApproxPerfoClause &PC) {
  assert(CapturedStmtInfo &&
    "CapturedStmtInfo should be set when generating the captured function");
  const CapturedDecl *CD = CS.getCapturedDecl();
  const RecordDecl *RD = CS.getCapturedRecordDecl();
  SourceLocation Loc = CS.getBeginLoc();
  assert(CD->hasBody() && "missing CapturedDecl body");

  // Build the argument list.
  ASTContext &Ctx = CGM.getContext();
  FunctionArgList Args;
  Args.append(CD->param_begin(), CD->param_end());

  // Create the function declaration.
  const CGFunctionInfo &FuncInfo =
    CGM.getTypes().arrangeBuiltinFunctionDeclaration(Ctx.VoidTy, Args);
  llvm::FunctionType *FuncLLVMTy = CGM.getTypes().GetFunctionType(FuncInfo);

  llvm::Function *F =
    llvm::Function::Create(FuncLLVMTy, llvm::GlobalValue::InternalLinkage,
                           CapturedStmtInfo->getHelperName(), &CGM.getModule());
  CGM.SetInternalFunctionAttributes(CD, F, FuncInfo);
  if (CD->isNothrow())
    F->addFnAttr(llvm::Attribute::NoUnwind);

  // Generate the function.
  StartFunction(CD, Ctx.VoidTy, F, FuncInfo, Args, CD->getLocation(),
                CD->getBody()->getBeginLoc());
  // Set the context parameter in CapturedStmtInfo.
  Address DeclPtr = GetAddrOfLocalVar(CD->getContextParam());
  CapturedStmtInfo->setContextValue(Builder.CreateLoad(DeclPtr));

  // Initialize variable-length arrays.
  LValue Base = MakeNaturalAlignAddrLValue(CapturedStmtInfo->getContextValue(),
                                           Ctx.getTagDeclType(RD));
  for (auto *FD : RD->fields()) {
    if (FD->hasCapturedVLAType()) {
      auto *ExprArg =
          EmitLoadOfLValue(EmitLValueForField(Base, FD), CS.getBeginLoc())
              .getScalarVal();
      auto VAT = FD->getCapturedVLAType();
      VLASizeMap[VAT->getSizeExpr()] = ExprArg;
    }
  }

  // If 'this' is captured, load it into CXXThisValue.
  if (CapturedStmtInfo->isCXXThisExprCaptured()) {
    FieldDecl *FD = CapturedStmtInfo->getThisFieldDecl();
    LValue ThisLValue = EmitLValueForField(Base, FD);
    CXXThisValue = EmitLoadOfLValue(ThisLValue, Loc).getScalarVal();
  }

  PGO.assignRegionCounters(GlobalDecl(CD), F);

  // Declare IV, LastIteration, LB, UB variables.
  const auto *IVExpr = cast<DeclRefExpr>(LoopExprs.IterationVarRef);
  const auto *IVDecl = cast<VarDecl>(IVExpr->getDecl());
  EmitVarDecl(*IVDecl);

  if (const auto *LIExpr = dyn_cast<DeclRefExpr>(LoopExprs.LastIteration)) {
    EmitVarDecl(*cast<VarDecl>(LIExpr->getDecl()));
    // Emit calculation of the iterations count.
    EmitIgnoredExpr(LoopExprs.CalcLastIteration);
  }

  const auto *LBExpr = cast<DeclRefExpr>(LoopExprs.LB);
  const auto *LBDecl = cast<VarDecl>(LBExpr->getDecl());
  EmitVarDecl(*LBDecl);

  // Emit variable declarations of PreInits.
  if (const auto *PreInits = cast_or_null<DeclStmt>(LoopExprs.PreInits)) {
    for (const auto *I : PreInits->decls())
      EmitVarDecl(cast<VarDecl>(*I));
  }

  const auto *UBExpr = cast<DeclRefExpr>(LoopExprs.UB);
  const auto *UBDecl = cast<VarDecl>(UBExpr->getDecl());
  EmitVarDecl(*UBDecl);

  // EmitIgnoredExpr(LoopExprs.EUB);
  // IV = LB;
  EmitIgnoredExpr(LoopExprs.Init);

  const auto *CounterExpr = cast<DeclRefExpr>(LoopExprs.Counter);
  const auto *CounterDecl = cast<VarDecl>(CounterExpr->getDecl());
  // Emit counter declaration and init if it is not captured.
  if (!CS.capturesVariable(CounterDecl)) {
    EmitVarDecl(*CounterDecl);
    EmitIgnoredExpr(LoopExprs.CounterInit);
  }

  if(LoopExprs.OMPParallelForDir) {
    EmitStmt(LoopExprs.OMPParallelForDir);
  }
  else {
    // Create BBs for end of the loop and condition check.
    auto LoopExit = getJumpDestInCurrentScope("approx.perfo.for.end");
    auto CondBlock = createBasicBlock("approx.perfo.for.cond");
    EmitBlock(CondBlock);
    const SourceRange R = CS.getSourceRange();

    LoopStack.push(CondBlock, SourceLocToDebugLoc(R.getBegin()),
                   SourceLocToDebugLoc(R.getEnd()));

    llvm::BasicBlock *ExitBlock = LoopExit.getBlock();
    llvm::BasicBlock *LoopBody = createBasicBlock("approx.perfo.for.body");
    llvm::BasicBlock *PerfRandCondBlock =
        createBasicBlock("approx.perfo.for.rand.cond");
    auto *LoopCond = LoopExprs.Cond;
    // Emit condition.
    EmitBranchOnBoolExpr(LoopCond, PerfRandCondBlock, ExitBlock,
                         getProfileCount(&CS));
    if (ExitBlock != LoopExit.getBlock()) {
      EmitBlock(ExitBlock);
      EmitBranchThroughCleanup(LoopExit);
    }

    // Create a block for the increment.
    JumpDest Continue = getJumpDestInCurrentScope("approx.perfo.for.inc");
    BreakContinueStack.push_back(BreakContinue(LoopExit, Continue));

    EmitBlock(PerfRandCondBlock);
    // Emit perfo rand cond basic block.
    #if 0
    if (PC.getPerfoType() == approx::PT_RAND) {
      // Skip iteration if true.
      StringRef FnName("__approx_skip_iteration");
      llvm::Function *Fn = CGM.getModule().getFunction(FnName);
      llvm::FunctionType *FnTy =
          llvm::FunctionType::get(llvm::Type::getInt1Ty(CGM.getLLVMContext()),
                                  {CGM.Int32Ty, CGM.FloatTy},
                                  /* VarArgs */ false);
      if (!Fn) {
        Fn = Function::Create(FnTy, GlobalValue::ExternalLinkage, FnName,
                              CGM.getModule());
      }

      llvm::Value *IV = EmitLoadOfScalar(EmitLValue(LoopExprs.IterationVarRef),
                                         SourceLocation());
      llvm::Value *Pr = nullptr;

      // Emit Pr expression, either loading from a captured DRE or evaluating
      // it.
      if (dyn_cast<DeclRefExpr>(LoopExprs.PerfoStep)) {
        Pr =
            EmitLoadOfScalar(EmitLValue(LoopExprs.PerfoStep), SourceLocation());
      } else
        Pr = EmitScalarExpr(LoopExprs.PerfoStep);

      assert(Pr != nullptr && "Expected a non-null Pr value");

      llvm::FunctionCallee FnCallee({FnTy, Fn});
      llvm::Value *Ret = EmitRuntimeCall(FnCallee, {IV, Pr});
      Builder.CreateCondBr(Ret, Continue.getBlock(), LoopBody);
    } else {
      EmitBranch(LoopBody);
    }
    #endif

    EmitBlock(LoopBody);
    incrementProfileCounter(&CS);

    // Emit counter update.
    EmitIgnoredExpr(LoopExprs.CounterUpdate);

    auto emitBody = [&](auto &&emitBody, const Stmt *S,
                        const Stmt *LoopS) -> void {
      const Stmt *SimplifiedS = S->IgnoreContainers();
      if (const auto *CompS = dyn_cast<CompoundStmt>(SimplifiedS)) {
        // Keep track of the current cleanup stack depth, including debug
        // scopes.
        // CodeGenFunction::LexicalScope Scope(CGF, S->getSourceRange());
        for (const Stmt *CurStmt : CompS->body())
          emitBody(emitBody, CurStmt, LoopS);
        return;
      }

      // Emit only the body of the loop statement.
      if (S == LoopS) {
        if (const auto *For = dyn_cast<ForStmt>(S)) {
          S = For->getBody();
        } else {
          assert(isa<CXXForRangeStmt>(S) &&
                 "Expected canonical for loop or range-based for loop.");
          const auto *CXXFor = cast<CXXForRangeStmt>(S);
          EmitStmt(CXXFor->getLoopVarStmt());
          S = CXXFor->getBody();
        }
      }

      EmitStmt(S);
    };

    if (LoopExprs.PerfoSkip)
      EmitStmt(LoopExprs.PerfoSkip);
    Stmt *S = const_cast<Stmt *>(CS.getCapturedStmt());
    Stmt *LoopS = nullptr;
    OMPLoopDirective *OMPFD = nullptr;
    if ((OMPFD = dyn_cast<OMPLoopDirective>(S)))
      LoopS = OMPFD->getInnermostCapturedStmt()->IgnoreContainers(true);
    else
      LoopS = S->IgnoreContainers();
    emitBody(emitBody, S, LoopS);

    // Emit "IV = IV + 1" and a back-edge to the condition block.
    EmitBlock(Continue.getBlock());
    if (LoopExprs.PerfoInc)
      EmitIgnoredExpr(LoopExprs.PerfoInc);
    auto *IncExpr = LoopExprs.Inc;
    EmitIgnoredExpr(IncExpr);
    BreakContinueStack.pop_back();
    EmitBranch(CondBlock);

    LoopStack.pop();
    // Emit the fall-through block.
    EmitBlock(LoopExit.getBlock());
  }

  FinishFunction(CD->getBodyRBrace());

  return F;
}

void CGApproxRuntime::CGApproxRuntimeEmitPerfoFn(
    CapturedStmt &CS, const ApproxLoopHelperExprs &LoopExprs,
    const ApproxPerfoClause &PC) {
  CodeGenFunction::CGCapturedStmtInfo CGSI(CS);
  CodeGenFunction CGF(CGM, true);
  CodeGenFunction::CGCapturedStmtRAII CapInfoRAII(CGF, &CGSI);

  llvm::Function *Fn = CGF.GeneratePerfoCapturedStmtFunction(CS, LoopExprs, PC);
  approxRTParams[PerfoFn] =
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
  enum VarInfoFieldID { PTR, NUM_ELEM, SZ_ELEM, STRIDE, DATA_TYPE, DIR };

  for (auto P : Data) {
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

    // Store STRIDE
    LValue strideElemLVal = CGF.EmitLValueForField(
        Base, *std::next(VarInfoRecord->field_begin(), STRIDE));
    CGF.EmitStoreOfScalar(AccessStride, strideElemLVal);

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
    approxRTParams[DataDescIn] = ArrayAddress;
    approxRTParams[DataSizeIn] = NumOfElements;
  }

  // All approximation techniques require the output
  sprintf(name, ".dep.approx_outputs.arr.addr_%d", output_arrays++);
  std::tie(NumOfElements, ArrayAddress) =
      CGApproxRuntimeEmitData(CGF, Outputs, name);
  approxRTParams[DataDescOut] = ArrayAddress;
  approxRTParams[DataSizeOut] = NumOfElements;
}

void CGApproxRuntime::CGApproxRuntimeEmitLabelInit(
    CodeGenFunction &CGF, ApproxLabelClause &LabelClause) {
  ASTContext &C = CGM.getContext();
  CodeGen::CodeGenTypes &Types = CGM.getTypes();
  llvm::PointerType *CharPtrTy =
      llvm::PointerType::getUnqual(Types.ConvertType(C.CharTy));

  LValue label;
  llvm::Value *Addr;
  if (StringLiteral *LiteralExpr = dyn_cast_or_null<StringLiteral>(LabelClause.getLabel())) {
      label =
          CGF.EmitStringLiteralLValue(cast<StringLiteral>(LabelClause.getLabel()));
    Addr = label.getPointer(CGF);
  }else{
    Addr = CGF.EmitLValue(LabelClause.getLabel()).getPointer(CGF);
  }
  Addr = CGF.Builder.CreatePointerCast(Addr, CharPtrTy);
  approxRTParams[Label] = Addr;
}
