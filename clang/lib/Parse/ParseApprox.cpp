//===--- ParseApprox.cpp - Approx directives parsing ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file implements parsing of all Approx directives and clauses.
///
//===----------------------------------------------------------------------===//

#include "clang/AST/ApproxClause.h"
#include "clang/Basic/Approx.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "llvm/Support/Debug.h"

#include <iostream>

using namespace clang;
using namespace llvm;
using namespace approx;

static bool isPerfoType(Token &Tok, PerfoType &Kind) {
  for (unsigned i = PT_START; i < PT_END; i++) {
    enum PerfoType PT = (enum PerfoType)i;
    if (Tok.getIdentifierInfo()->getName().equals(ApproxPerfoClause::PerfoName[PT])) {
      Kind = PT;
      return true;
    }
  }
  return false;
}

static bool isMemoType(Token &Tok, MemoType &Kind) {
  for (unsigned i = MT_START; i < MT_END; i++) {
    enum MemoType MT = (enum MemoType)i;
    if (Tok.getIdentifierInfo()->getName().equals(ApproxMemoClause::MemoName[MT])) {
      Kind = MT;
      return true;
    }
  }
  return false;
}

static bool getDecisionHierarchy(Token &Tok, DecisionHierarchyType &Kind) {
  for (unsigned i = DTH_START; i < DTH_END; i++) {
    enum DecisionHierarchyType DHT = (enum DecisionHierarchyType)i;
    Kind = DHT;
    if (Tok.getIdentifierInfo()->getName().equals(ApproxClause::ApproxDecisionHierarchy[DHT])) {
      llvm::dbgs() << ApproxClause::ApproxDecisionHierarchy[DHT] << "\n";
      return true;
    }
  }
  return false;

}

bool Parser::ParseApproxVarList(SmallVectorImpl<Expr *> &Vars,
                                SourceLocation &ELoc) {
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_approx_end);
  if (T.expectAndConsume(diag::err_expected_lparen_after))
    return true;

  while (Tok.isNot(tok::r_paren) && Tok.isNot(tok::colon) &&
         Tok.isNot(tok::annot_pragma_approx_end)) {
    ExprResult VarExpr =
        Actions.CorrectDelayedTyposInExpr(ParseAssignmentExpression());
    if (VarExpr.isUsable()) {
      Vars.push_back(VarExpr.get());
    } else {
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_approx_end,
                StopBeforeMatch);
      return false;
    }
    bool isComma = Tok.is(tok::comma);
    if (isComma)
      ConsumeToken();
    else if (Tok.isNot(tok::r_paren) &&
             Tok.isNot(tok::annot_pragma_approx_end) && Tok.isNot(tok::colon)) {
      Diag(Tok, diag::err_pragma_approx_expected_punc);
      SkipUntil(tok::annot_pragma_approx_end, StopBeforeMatch);
      return false;
    }
  }
  ELoc = Tok.getLocation();
  if (!T.consumeClose())
    ELoc = T.getCloseLocation();
  return true;
}

ApproxClause *Parser::ParseApproxPerfoClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LParenLoc = ConsumeAnyToken();
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_approx_end);
  if (T.expectAndConsume(diag::err_expected_lparen_after, ApproxPerfoClause::PerfoName[CK].c_str()))
    return nullptr;

  PerfoType PT;
  if (!isPerfoType(Tok, PT)){
    return nullptr;
  }
  /// Consume Perf Type
  ConsumeAnyToken();

  ///Parse ':'
  if (Tok.isNot(tok::colon)){
    return nullptr;
  }
  /// Consuming ':'
  ConsumeAnyToken();
  SourceLocation ExprLoc = Tok.getLocation();
  ExprResult Val(ParseExpression());
  Val = Actions.ActOnFinishFullExpr(Val.get(), ExprLoc, false);
  SourceLocation ELoc = Tok.getLocation();
  if (!T.consumeClose())
    ELoc = T.getCloseLocation();
  ApproxVarListLocTy Locs(Loc, LParenLoc, ELoc);

  return Actions.ActOnApproxPerfoClause(CK, PT, Locs, Val.get());
}

ApproxClause *Parser::ParseApproxMemoClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LParenLoc = ConsumeAnyToken();
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_approx_end);
  if (T.expectAndConsume(diag::err_expected_lparen_after, ApproxClause::Name[CK].c_str()))
    return nullptr;

  MemoType MT;
  if (!isMemoType(Tok, MT)){
    return nullptr;
  }
  /// Consume Memo Type
  ConsumeAnyToken();

  DecisionHierarchyType DHT = DecisionHierarchyType::DTH_THREAD;
  if(Tok.is(tok::colon))
    {
      // consume the colon
      ConsumeAnyToken();

      if(!getDecisionHierarchy(Tok, DHT)){
        return nullptr;
      }

      ConsumeAnyToken();
    }

  SourceLocation ELoc = Tok.getLocation();
  if (!T.consumeClose())
    ELoc = T.getCloseLocation();
  ApproxVarListLocTy Locs(Loc, LParenLoc, ELoc);
  return Actions.ActOnApproxMemoClause(CK, MT, DHT, Locs);
}

ApproxClause *Parser::ParseApproxDTClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation ELoc = ConsumeAnyToken();
  ApproxVarListLocTy Locs(Loc, SourceLocation(), ELoc);
  return Actions.ActOnApproxDTClause(CK, Locs);
}

ApproxClause *Parser::ParseApproxNNClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation ELoc = ConsumeAnyToken();
  ApproxVarListLocTy Locs(Loc, SourceLocation(), ELoc);
  return Actions.ActOnApproxNNClause(CK, Locs);
}

ApproxClause *Parser::ParseApproxUserClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation ELoc = ConsumeAnyToken();
  ApproxVarListLocTy Locs(Loc, SourceLocation(), ELoc);
  return Actions.ActOnApproxUserClause(CK, Locs);
}

ApproxClause *Parser::ParseApproxIfClause(ClauseKind CK) {
  //Start Location
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LParenLoc = ConsumeAnyToken();
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_approx_end);
  if (T.expectAndConsume(diag::err_expected_lparen_after, ApproxClause::Name[CK].c_str()))
    return nullptr;

  SourceLocation ExprLoc = Tok.getLocation();
  ExprResult LHS(ParseCastExpression(AnyCastExpr, false, NotTypeCast));
  ExprResult Val = ParseRHSOfBinaryExpression(LHS, prec::Conditional);
  Val = Actions.ActOnFinishFullExpr(Val.get(), ExprLoc, false );

  SourceLocation ELoc = Tok.getLocation();
  if (!T.consumeClose())
    ELoc = T.getCloseLocation();

  if ( Val.isInvalid() )
    return nullptr;

  ApproxVarListLocTy Locs(Loc, LParenLoc, ELoc);
  return Actions.ActOnApproxIfClause(CK, Locs, Val.get());
}

ApproxClause *Parser::ParseApproxInClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  SourceLocation RLoc;
  SmallVector<Expr *, 8> Vars;
  if (!ParseApproxVarList(Vars, RLoc)) {
    return nullptr;
  }
  ApproxVarListLocTy Locs(Loc, LOpen, RLoc);
  return Actions.ActOnApproxVarList(CK, Vars, Locs);
}

ApproxClause *Parser::ParseApproxOutClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  SourceLocation RLoc;
  SmallVector<Expr *, 8> Vars;
  if (!ParseApproxVarList(Vars, RLoc)) {
    return nullptr;
  }
  ApproxVarListLocTy Locs(Loc, LOpen, RLoc);
  return Actions.ActOnApproxVarList(CK, Vars, Locs);
}

ApproxClause *Parser::ParseApproxInOutClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  SourceLocation RLoc;
  SmallVector<Expr *, 8> Vars;
  if (!ParseApproxVarList(Vars, RLoc)) {
    return nullptr;
  }
  ApproxVarListLocTy Locs(Loc, LOpen, RLoc);
  return Actions.ActOnApproxVarList(CK, Vars, Locs);
}

ApproxClause *Parser::ParseApproxLabelClause(ClauseKind CK) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LParenLoc = ConsumeAnyToken();
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_approx_end);
  if (T.expectAndConsume(diag::err_expected_lparen_after, ApproxClause::Name[CK].c_str()))
    return nullptr;

  SourceLocation ExprLoc = Tok.getLocation();
  ExprResult Val(ParseExpression());
  Val = Actions.ActOnFinishFullExpr(Val.get(), ExprLoc, false);

  SourceLocation ELoc = Tok.getLocation();
  if (!T.consumeClose())
    ELoc = T.getCloseLocation();

  ApproxVarListLocTy Locs(Loc, LParenLoc, ELoc);

  return Actions.ActOnApproxLabelClause(CK, Locs, Val.get());
}

bool isApproxClause(Token &Tok, ClauseKind &Kind) {
  for (unsigned i = CK_START; i < CK_END; i++) {
    enum ClauseKind CK = (enum ClauseKind)i;
    if (Tok.getIdentifierInfo()->getName().equals(ApproxClause::Name[CK])) {
      Kind = CK;
      return true;
    }
  }
  return false;
}

StmtResult Parser::ParseApproxDirective(ParsedStmtContext StmtCtx) {
  assert(Tok.is(tok::annot_pragma_approx_start));
  /// This should be a function call;
  inApproxScope = true;

#define PARSER_CALL(method) ((*this).*(method))

  StmtResult Directive = StmtError();
  SourceLocation DirectiveStart = Tok.getLocation();
  SmallVector<ApproxClause*, CK_END> Clauses;

  /// I am consuming the pragma identifier atm.
  ConsumeAnyToken();

  SourceLocation ClauseStartLocation = Tok.getLocation();

  /// we do not support just
  /// #pragma approx
  /// we need extra information. So just
  /// return with an error
  if (Tok.is(tok::eod) || Tok.is(tok::eof)) {
    PP.Diag(Tok, diag::err_pragma_approx_expected_directive);
    ConsumeAnyToken();
    inApproxScope = false;
    return Directive;
  }

  ClauseKind CK;
  while (Tok.isNot(tok::annot_pragma_approx_end)) {
    if (isApproxClause(Tok, CK)) {
      ApproxClause *Clause = PARSER_CALL(ParseApproxClause[CK])(CK);
      if (!Clause) {
        SkipUntil(tok::annot_pragma_approx_end);
        inApproxScope = false;
        return Directive;
      }
      Clauses.push_back(Clause);
    } else {
      PP.Diag(Tok, diag::err_pragma_approx_unrecognized_directive);
      SkipUntil(tok::annot_pragma_approx_end);
      inApproxScope = false;
      return Directive;
    }
  }

  /// Update the end location of the directive.
  SourceLocation DirectiveEnd = Tok.getLocation();
  ConsumeAnnotationToken();
  ApproxVarListLocTy Locs(DirectiveStart, ClauseStartLocation, DirectiveEnd);

  // Start captured region sema, will end withing ActOnApproxDirective.
  Actions.ActOnCapturedRegionStart(Tok.getEndLoc(), getCurScope(), CR_Default, /* NumParams = */1);
  StmtResult AssociatedStmt = (Sema::CompoundScopeRAII(Actions), ParseStatement());
  Directive = Actions.ActOnApproxDirective(AssociatedStmt.get(), Clauses, Locs);
  inApproxScope = false;
  return Directive;
}
