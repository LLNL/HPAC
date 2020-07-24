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

#include "clang/Basic/Approx.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"

#include "llvm/Support/Debug.h"

#include <iostream>

using namespace clang;
using namespace llvm;
using namespace approx;

const std::string DirectiveInfo::Name[DK_END] = {"perfo", "memo", "dt", "nn"};

bool isApproxDirective(Token &Tok, DirectiveApproxKind &Kind) {
  for (unsigned i = DK_START; i < DK_END; i++) {
    enum DirectiveApproxKind DK = (enum DirectiveApproxKind)i;
    if (Tok.getIdentifierInfo()->getName().equals(DirectiveInfo::Name[DK])) {
      Kind = DK;
      return true;
    }
  }
  return false;
}

StmtResult Parser::ParseApproxDirective(ParsedStmtContext StmtCtx) {
  assert(Tok.is(tok::annot_pragma_approx_start));

  StmtResult Directive = StmtError();

  DirectiveApproxKind DK;

  /// I am consuming the pragma identifier atm.
  ConsumeAnyToken();

  /// A.T.M we do not support just
  /// #pragma approx
  /// we need extra information. So just
  /// return with an error
  if (Tok.is(tok::eod) || Tok.is(tok::eof)) {
    PP.Diag(Tok, diag::err_pragma_approx_expected_directive);
    ConsumeAnyToken();
    return Directive;
  }

  while (Tok.isNot(tok::annot_pragma_approx_end)) {
    if (!Tok.isNot(tok::identifier)) {
      if (isApproxDirective(Tok, DK)) {
        dbgs() << "Identified directive: " << DirectiveInfo::Name[DK] << "\n";
      } else {
        PP.Diag(Tok, diag::err_pragma_approx_unrecognized_directive);
        SkipUntil(tok::annot_pragma_approx_end);
        return Directive;
      }
      ConsumeAnyToken();
    }
  }

  Directive = Actions.ActOnApproxDirective();
  /// We need to consume also annot_pragma_approx_end
  ConsumeAnyToken();

  return Directive;
}
