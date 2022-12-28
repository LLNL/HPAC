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
#include "llvm/IR/GlobalVariable.h"
#include <memory>

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

enum DevApproxRTArgsIndex : uint {
  DevAccurateFn = 0,
  DevPerfoFn,
  DevCapDataPtr,
  DevMemoDescr,
  DevDataDescIn,
  DevAccessDescIn,
  DevDataPtrIn,
  DevDataSizeIn,
  DevDataDescOut,
  DevAccessDescOut,
  DevDataPtrOut,
  DevDataSizeOut,
  DevInitDone,
  DEV_ARG_END
};


enum Directionality : int { Input = 1, Output = 2, InputOuput = 4 };

const unsigned ARG_START = AccurateFn;
const unsigned DEV_ARG_START = DevAccurateFn;


class CGApproxRuntime {
protected:
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
  ///        size_t stride;    // Stride distance between subsequent values
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

protected:
  /// VarRegionSpecTy is a struct containing info about the in/out/inout variables
  /// of this region.
  /// typedef struct approx_region_specification {
  ///   const size_t sz_elem; // Size of elements in bytes
  ///   const size_t num_elem; // Number of elements
  ///   const size_t stride; // Stride distance between subsequent values
  ///   const int8_t data_type; // Type of data float/double/int etc.
  ///   const int8_t dir; // direction of data: in/out/inout
  /// } approx_region_specification;
  QualType VarRegionSpecTy;


  /// VarAccessTy is a struct containing the pointer to a thread's input
  /// typedef struct approx_var_ptr_t {
  ///   void *ptr;
  /// } approx_var_ptr_t;
  QualType VarAccessTy;

  virtual void CGApproxRuntimeEmitPerfoFn(CapturedStmt &CS, const ApproxLoopHelperExprs &LoopExprs, const ApproxPerfoClause &PC);
  virtual std::pair<llvm::Value *, llvm::Value *> CGApproxRuntimeEmitData(CodeGenFunction &CGF, llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *arrayName);
  virtual void getVarInfoType(ASTContext &C, QualType &VarInfoTy);

public:
  CGApproxRuntime(CodeGenModule &CGM);
  virtual void CGApproxRuntimeEnterRegion(CodeGenFunction &CGF, CapturedStmt &CS);
  virtual void CGApproxRuntimeEmitPerfoInit(CodeGenFunction &CGF, CapturedStmt &CS,
                                            ApproxPerfoClause &PerfoClause, const ApproxLoopHelperExprs &LoopExprs);
  virtual void CGApproxRuntimeEmitMemoInit(CodeGenFunction &CGF,
                                   ApproxMemoClause &MemoClause);
  void CGApproxRuntimeEmitIfInit(CodeGenFunction &CGF,
                                 ApproxIfClause &IfClause);
  void CGApproxRuntimeEmitLabelInit(CodeGenFunction &CGF, ApproxLabelClause &LabelCluse);
  virtual void CGApproxRuntimeExitRegion(CodeGenFunction &CGF);
  void CGApproxRuntimeRegisterInputs(ApproxInClause &InClause);
  void CGApproxRuntimeRegisterOutputs(ApproxOutClause &OutClause);
  void CGApproxRuntimeRegisterInputsOutputs(ApproxInOutClause &InOutClause);
  virtual void CGApproxRuntimeEmitDataValues(CodeGenFunction &CG);
};

class CGApproxRuntimeGPU : public CGApproxRuntime {
private:
  CodeGenModule &CGM;

  llvm::Value *approxRTParams[DEV_ARG_END];
  llvm::GlobalVariable *ApproxInit = nullptr;
  std::unique_ptr<Address> ApproxInitAddress;

  // Function type of the runtime interface call.
  llvm::FunctionType *RTFnTy;

  llvm::GlobalVariable *RegionInfo;
  llvm::GlobalVariable *AccessInfo;

protected:
  using CGApproxRuntime::CGApproxRuntimeEmitData;
  std::tuple<llvm::Value *, llvm::Value *, llvm::Value *> CGApproxRuntimeEmitData(CodeGenFunction &CGF, llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *infoName, const char *ptrName);
void CGApproxRuntimeEmitInitData(
  CodeGenFunction &CGF,
  llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, Address Base);
  std::unique_ptr<Address> declareAccessArrays(CodeGenFunction &CGF,
                           llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *name);
  std::unique_ptr<Address> declarePtrArrays(CodeGenFunction &CGF,
                           llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data, const char *name);

  Address getAddressofVarInAddressSpace(CodeGenFunction &CGF, llvm::Value *V, QualType T, clang::LangAS AS);


std::tuple<llvm::Value *, llvm::Value*>
CGApproxRuntimeGPUEmitData(
    CodeGenFunction &CGF,
    llvm::SmallVector<std::pair<Expr *, Directionality>, 16> &Data,
    Address VarPtrArray, const char *infoName);


  void getVarInfoType(ASTContext &C, QualType &VarInfoTy) override;
  void getVarAccessType(ASTContext &C, QualType &VarInfoTy);
public:
  CGApproxRuntimeGPU(CodeGenModule &CGM);
  ~CGApproxRuntimeGPU() = default;
  void CGApproxRuntimeEnterRegion(CodeGenFunction &CGF, CapturedStmt &CS) override;
  void CGApproxRuntimeEmitPerfoInit(CodeGenFunction &CGF, CapturedStmt &CS, ApproxPerfoClause &PerfoClause,
                                    const ApproxLoopHelperExprs &LoopExprs
                                    ) override;
  void CGApproxRuntimeEmitMemoInit(CodeGenFunction &CGF,
                                   ApproxMemoClause &MemoClause) override;
void CGApproxRuntimeEmitPerfoFn(CapturedStmt &CS, const ApproxLoopHelperExprs &LoopExprs,
                                const ApproxPerfoClause &PC
                                ) override;

  void CGApproxRuntimeExitRegion(CodeGenFunction &CGF) override;
  void CGApproxRuntimeEmitDataValues(CodeGenFunction &CG) override;
  void declareApproxInit(CodeGenFunction& CGF);
};
} // namespace CodeGen
} // namespace clang

#endif
