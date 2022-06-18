//===--- ApproxClause.h - Approx Clauses ---------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines some Approx-specific Clauses and functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_APPROXCLAUSE_H
#define LLVM_CLANG_AST_APPROXCLAUSE_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtIterator.h"
#include "clang/Basic/Approx.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/TrailingObjects.h"



namespace clang {

/// This is a basic class for representing a
/// single Approx Clause.
class ApproxClause {
  /// Starting Location of the clause (Clause Keyword)
  SourceLocation StartLoc;

  /// Ending Location of the clause.
  SourceLocation EndLoc;

  /// Kind of the clause.
  approx::ClauseKind Kind;

protected:
  ApproxClause(approx::ClauseKind Kind, SourceLocation StartLoc, SourceLocation EndLoc)
      : StartLoc(StartLoc), EndLoc(EndLoc), Kind(Kind) {}

public:
  static const std::string Name[approx::CK_END];

  SourceLocation getBeginLoc() const { return StartLoc; }

  SourceLocation getEndLoc() const { return EndLoc; }

  void setLocStart(SourceLocation Loc) { StartLoc = Loc; }

  void setLocEnd(SourceLocation Loc) { EndLoc = Loc; }

  approx::ClauseKind getClauseKind() const { return Kind; }

  std::string getAsString() const { return Name[Kind]; }

  bool isImplicit() const { return StartLoc.isInvalid(); }

  using child_iterator = StmtIterator;
  using const_child_iterator = ConstStmtIterator;
  using child_range = llvm::iterator_range<child_iterator>;
  using const_child_range = llvm::iterator_range<const_child_iterator>;

  child_range children();
  const_child_range children() const {
    auto Children = const_cast<ApproxClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  /// Get the iterator range for the expressions used in the clauses. Used
  /// expressions include only the children that must be evaluated at the
  /// runtime before entering the construct.
  child_range used_children();
  const_child_range used_children() const {
    auto Children = const_cast<ApproxClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  static bool classof(const ApproxClause *) { return true; }
};

class ApproxLabelClause final: public ApproxClause{
  SourceLocation LParenLoc;
  Stmt *Label = nullptr;
  Stmt *PreInit = nullptr;

  public:
  ApproxLabelClause(SourceLocation StartLoc, SourceLocation EndLoc, SourceLocation LParenLoc, Stmt *PreInit, Expr *Label) :
  ApproxClause(approx::CK_LABEL, StartLoc, EndLoc), LParenLoc(LParenLoc), Label(Label), PreInit(PreInit){}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_LABEL;
  }

  Expr* getLabel() {return cast_or_null<Expr>(Label);}

  const Stmt *getPreInit() const { return PreInit; }
  Stmt *getPreInit() { return PreInit; }
};

class ApproxPerfoClause final : public ApproxClause {
  approx::PerfoType Type;
  SourceLocation LParenLoc;
  Stmt *Step = nullptr;
  Stmt *PreInit = nullptr;

  void setStep(Expr *S) { Step = S; }
  void setPreInit(Expr *Init) { PreInit = Init; }


public:
  static const std::string PerfoName[approx::PT_END];
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ApproxPerfoClause(approx::PerfoType PT, SourceLocation StartLoc,
                    SourceLocation EndLoc, SourceLocation LParenLoc, Stmt *PreInit, Expr *Step)
      : ApproxClause(approx::CK_PERFO, StartLoc, EndLoc), Type(PT),
        LParenLoc(LParenLoc), Step(Step), PreInit(PreInit) {}

  /// Build an empty clause.
  ApproxPerfoClause()
      : ApproxClause(approx::CK_PERFO, SourceLocation(), SourceLocation()) {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_PERFO;
  }

  std::string getPerfoTypeAsString() const { return PerfoName[Type]; }
  approx::PerfoType getPerfoType() const { return Type; }
  SourceLocation getLParenLoc() const { return LParenLoc; }
  Expr *getStep() const { return cast_or_null<Expr>(Step); }
  const Stmt *getPreInit() const { return PreInit; }
  Stmt *getPreInit() { return PreInit; }
};

class ApproxMemoClause final : public ApproxClause {
  approx::MemoType Type;
  SourceLocation LParenLoc;
public:
  static const std::string MemoName[approx::MT_END];
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ApproxMemoClause(approx::MemoType MT, SourceLocation StartLoc,
                    SourceLocation EndLoc, SourceLocation LParenLoc)
      :ApproxClause(approx::CK_MEMO, StartLoc, EndLoc), Type(MT), LParenLoc(LParenLoc){}

  /// Build an empty clause.
  ApproxMemoClause()
      : ApproxClause(approx::CK_MEMO, SourceLocation(), SourceLocation()) {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_MEMO;
  }

  std::string getMemoTypeAsString() const {return MemoName[Type];}
  approx::MemoType getMemoType() const {return Type;}
  void setMemoType(approx::MemoType T) {Type = T;}
};

class ApproxDTClause final : public ApproxClause {
public:
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ApproxDTClause(SourceLocation StartLoc, SourceLocation EndLoc)
      : ApproxClause(approx::CK_DT, StartLoc, EndLoc) {}

  /// Build an empty clause.
  ApproxDTClause() : ApproxClause(approx::CK_DT, SourceLocation(), SourceLocation()) {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_DT;
  }
};

class ApproxNNClause final : public ApproxClause {
public:
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ApproxNNClause(SourceLocation StartLoc, SourceLocation EndLoc)
      : ApproxClause(approx::CK_NN, StartLoc, EndLoc) {}

  /// Build an empty clause.
  ApproxNNClause() : ApproxClause(approx::CK_NN, SourceLocation(), SourceLocation()) {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_NN;
  }
};

class ApproxUserClause final : public ApproxClause {
public:
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ApproxUserClause(SourceLocation StartLoc, SourceLocation EndLoc)
      : ApproxClause(approx::CK_USER, StartLoc, EndLoc) {}

  /// Build an empty clause.
  ApproxUserClause()
      : ApproxClause(approx::CK_USER, SourceLocation(), SourceLocation()) {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_USER;
  }
};

class ApproxIfClause final : public ApproxClause {

  ///Location of '('.
  SourceLocation LParenLoc;
  /// Stmt That contains declaration of variables
  Stmt *PreInit = nullptr;

  /// Condition of if Stmt
  Stmt *Condition = nullptr;

  void setCondition(Expr *Cond) { Condition = Cond; }
  void setPreInit(Expr *Init) { PreInit = Init; }


public:
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ApproxIfClause(SourceLocation StartLoc, SourceLocation EndLoc,
                 SourceLocation LParenLoc, Stmt *PreInit, Expr *Cond)
      : ApproxClause(approx::CK_IF, StartLoc, EndLoc), LParenLoc(LParenLoc),
        PreInit(PreInit), Condition(Cond) {}

  /// Build an empty clause.
  ApproxIfClause() : ApproxClause(approx::CK_IF, SourceLocation(), SourceLocation()) {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_IF;
  }

  SourceLocation getLParenLoc() const { return LParenLoc; }

  Expr *getCondition() const { return cast_or_null<Expr>(Condition); }

  const Stmt *getPreInit() const { return PreInit; }

  Stmt *getPreInit() { return PreInit; }

};

template <class T> class ApproxVarListClause : public ApproxClause {
  /// Location of opening '('.
  SourceLocation LParenLoc;

  /// Number of variables in this clause
  unsigned NumVars;

  /// Are the variables input or output or both

protected:
  ApproxVarListClause(approx::ClauseKind K, SourceLocation StartLoc,
                      SourceLocation LParenLoc, SourceLocation EndLoc,
                      unsigned N)
      : ApproxClause(K, StartLoc, EndLoc), LParenLoc(LParenLoc), NumVars(N) {}

  MutableArrayRef<Expr *> getVarRefs() {
    return MutableArrayRef<Expr *>(
        static_cast<T *>(this)->template getTrailingObjects<Expr *>(), NumVars);
  }

  void setVarRefs(ArrayRef<Expr *> VL) {
    assert(VL.size() == NumVars &&
           "Number of variables is not the same as the preallocated buffer");
    std::copy(VL.begin(), VL.end(),
              static_cast<T *>(this)->template getTrailingObjects<Expr *>());
  }

public:
  using varlist_iterator = MutableArrayRef<Expr *>::iterator;
  using varlist_const_iterator = ArrayRef<const Expr *>::iterator;
  using varlist_range = llvm::iterator_range<varlist_iterator>;
  using varlist_const_range = llvm::iterator_range<varlist_const_iterator>;

  unsigned varlist_size() const { return NumVars; }
  bool varlist_empty() const { return NumVars == 0; }

  varlist_range varlist() {
    return varlist_range(varlist_begin(), varlist_end());
  }

  varlist_iterator varlist_begin() { return getVarRefs().begin(); }
  varlist_iterator varlist_end() { return getVarRefs().end(); }
  varlist_const_iterator varlist_begin() const { return getVarRefs().begin(); }
  varlist_const_iterator varrlist_end() const { return getVarRefs().end(); }

  void setLParenLoc(SourceLocation Loc) { LParenLoc = Loc; }

  SourceLocation getLParenLoc() const { return LParenLoc; }

  ArrayRef<const Expr *> getVarRefs() const {
    return llvm::makeArrayRef(
        static_cast<const T *>(this)->template getTrailingObjects<Expr *>(),
        NumVars);
  }
};

class ApproxInClause final
    : public ApproxVarListClause<ApproxInClause>,
      private llvm::TrailingObjects<ApproxInClause, Expr *> {
  friend ApproxVarListClause;
  friend TrailingObjects;

  ApproxInClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                 SourceLocation EndLoc, unsigned N)
      : ApproxVarListClause<ApproxInClause>(approx::CK_IN, StartLoc, LParenLoc, EndLoc,
                                            N) {}
  explicit ApproxInClause(unsigned N)
      : ApproxVarListClause<ApproxInClause>(
            approx::CK_IN, SourceLocation(), SourceLocation(), SourceLocation(), N) {}

public:
  static ApproxInClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                SourceLocation LParenLoc, SourceLocation EndLoc,
                                ArrayRef<Expr *> VL);

  static ApproxInClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<ApproxInClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_IN;
  }
};

class ApproxOutClause final
    : public ApproxVarListClause<ApproxOutClause>,
      private llvm::TrailingObjects<ApproxOutClause, Expr *> {
  friend ApproxVarListClause;
  friend TrailingObjects;

  ApproxOutClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                  SourceLocation EndLoc, unsigned N)
      : ApproxVarListClause<ApproxOutClause>(approx::CK_OUT, StartLoc, LParenLoc,
                                             EndLoc, N) {}
  explicit ApproxOutClause(unsigned N)
      : ApproxVarListClause<ApproxOutClause>(
            approx::CK_OUT, SourceLocation(), SourceLocation(), SourceLocation(), N) {}

public:
  static ApproxOutClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                 SourceLocation LParenLoc,
                                 SourceLocation EndLoc, ArrayRef<Expr *> VL);

  static ApproxOutClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<ApproxOutClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_OUT;
  }
};

class ApproxInOutClause final
    : public ApproxVarListClause<ApproxInOutClause>,
      private llvm::TrailingObjects<ApproxInOutClause, Expr *> {
  friend ApproxVarListClause;
  friend TrailingObjects;

  ApproxInOutClause(SourceLocation StartLoc, SourceLocation LParenLoc,
                    SourceLocation EndLoc, unsigned N)
      : ApproxVarListClause<ApproxInOutClause>(approx::CK_INOUT, StartLoc, LParenLoc,
                                               EndLoc, N) {}
  explicit ApproxInOutClause(unsigned N)
      : ApproxVarListClause<ApproxInOutClause>(
            approx::CK_INOUT, SourceLocation(), SourceLocation(), SourceLocation(), N) {
  }

public:
  static ApproxInOutClause *Create(const ASTContext &C, SourceLocation StartLoc,
                                   SourceLocation LParenLoc,
                                   SourceLocation EndLoc, ArrayRef<Expr *> VL);

  static ApproxInOutClause *CreateEmpty(const ASTContext &C, unsigned N);

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(varlist_begin()),
                       reinterpret_cast<Stmt **>(varlist_end()));
  }

  const_child_range children() const {
    auto Children = const_cast<ApproxInOutClause *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  child_range used_children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range used_children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const ApproxClause *T) {
    return T->getClauseKind() == approx::CK_INOUT;
  }
};

template<class ImplClass, template<typename> class Ptr, typename RetTy>
class ApproxClauseVisitorBase{
  public:
  #define PTR(CLASS) Ptr<CLASS>
  #define DISPATCH(CLASS) \
    return static_cast<ImplClass*>(this)->Visit##CLASS(static_cast<PTR(CLASS)>(S))

  RetTy VisitApproxPerfoClause(PTR(ApproxPerfoClause) S) {DISPATCH(ApproxPerfoClause);}
  RetTy VisitApproxMemoClause(PTR(ApproxMemoClause) S) {DISPATCH(ApproxMemoClause);}
  RetTy VisitApproxDTClause(PTR(ApproxDTClause) S) {DISPATCH(ApproxDTClause);}
  RetTy VisitApproxNNClause(PTR(ApproxNNClause) S) {DISPATCH(ApproxNNClause);}
  RetTy VisitApproxUserClause(PTR(ApproxUserClause) S) {DISPATCH(ApproxUserClause);}
  RetTy VisitApproxIfClause(PTR(ApproxIfClause) S) {DISPATCH(ApproxIfClause);}
  RetTy VisitApproxInClause(PTR(ApproxInClause) S) {DISPATCH(ApproxInClause);}
  RetTy VisitApproxOutClause(PTR(ApproxOutClause) S) {DISPATCH(ApproxOutClause);}
  RetTy VisitApproxInOutClause(PTR(ApproxInOutClause) S) {DISPATCH(ApproxInOutClause);}
  RetTy VisitApproxLabelClause(PTR(ApproxLabelClause) S) {DISPATCH(ApproxLabelClause);}

  RetTy Visit(PTR(ApproxClause) S){
    switch (S->getClauseKind()){
      case approx::CK_PERFO:
        return VisitApproxPerfoClause(static_cast<PTR(ApproxPerfoClause)>(S));
      case approx::CK_MEMO:
        return VisitApproxMemoClause(static_cast<PTR(ApproxMemoClause)>(S));
      case approx::CK_DT:
        return VisitApproxDTClause(static_cast<PTR(ApproxDTClause)>(S));
      case approx::CK_NN:
        return VisitApproxNNClause(static_cast<PTR(ApproxNNClause)>(S));
      case approx::CK_USER:
        return VisitApproxUserClause(static_cast<PTR(ApproxUserClause)>(S));
      case approx::CK_IF:
        return VisitApproxIfClause(static_cast<PTR(ApproxIfClause)>(S));
      case approx::CK_IN:
        return VisitApproxInClause(static_cast<PTR(ApproxInClause)>(S));
      case approx::CK_OUT:
        return VisitApproxOutClause(static_cast<PTR(ApproxOutClause)>(S));
      case approx::CK_INOUT:
        return VisitApproxInOutClause(static_cast<PTR(ApproxInOutClause)>(S));
      case approx::CK_LABEL:
        return VisitApproxLabelClause(static_cast<PTR(ApproxLabelClause)>(S));
    }
  }

  RetTy VisitApproxClause(PTR(ApproxClause) Node) { return RetTy(); }
  #undef PTR
  #undef DISPATCH
};

template <typename T> using const_ptr = std::add_pointer_t<std::add_const_t<T>>;

template <class ImplClass, typename RetTy = void>
class ApproxClauseVisitor
    : public ApproxClauseVisitorBase<ImplClass, std::add_pointer_t, RetTy> {};
template<class ImplClass, typename RetTy = void>
class ConstApproxClauseVisitor :
      public ApproxClauseVisitorBase <ImplClass, const_ptr, RetTy> {};

class ApproxClausePrinter final : public ApproxClauseVisitor<ApproxClausePrinter> {
  raw_ostream &OS;
  const PrintingPolicy &Policy;

  /// Process clauses with list of variables.
  template <typename T> void VisitApproxClauseList(T *Node, char StartSym);

  public:
    ApproxClausePrinter(raw_ostream &OS, const PrintingPolicy &Policy):
      OS(OS), Policy(Policy){}

    template <typename T> void VisitApproxVarList(T *Node);

    void VisitApproxPerfoClause(ApproxPerfoClause *S);
    void VisitApproxMemoClause(ApproxMemoClause *S);
    void VisitApproxDTClause(ApproxDTClause *S);
    void VisitApproxNNClause(ApproxNNClause *S);
    void VisitApproxUserClause(ApproxUserClause *S);
    void VisitApproxIfClause(ApproxIfClause *S);
    void VisitApproxInClause(ApproxInClause *S);
    void VisitApproxOutClause(ApproxOutClause *S);
    void VisitApproxInOutClause(ApproxInOutClause *S);
    void VisitApproxLabelClause(ApproxLabelClause *S);
};

} // namespace clang

#endif // LLVM_CLANG_AST_APPROXCLAUSE_H
