
#include "clang/Basic/Approx.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"

#include "llvm/Support/Debug.h"

#include <iostream>

using namespace clang;
using namespace llvm;
using namespace approx;


const std::string DirectiveInfo::Name[DK_END] =  {"perfo", "memo", "dt", "nn"};

bool isApproxDirective ( Token &Tok, DirectiveApproxKind &Kind){
    for (unsigned i  = DK_START; i < DK_END; i++){
        enum DirectiveApproxKind DK  = (enum DirectiveApproxKind ) i;
        if ( Tok.getIdentifierInfo()->getName().equals(DirectiveInfo::Name[DK])){
            Kind = DK;
            return true;
        }
    }
    return false;
}


void Parser::HandleApproximate(){
    dbgs()<< "Parsing Tokens Of Pragma\n";
    assert(Tok.is(tok::annot_pragma_approx_start));

    DirectiveApproxKind DK;

    /// I am consuming the pragma identifier atm.
    ConsumeAnyToken();

    /// A.T.M we do not support just
    /// #pragma approx
    /// we need extra information. So just
    /// return with an error
    if ( Tok.is (tok::eod) || Tok.is(tok::eof)){
        PP.Diag(Tok,diag::err_pragma_approx_expected_directive);
        ConsumeAnyToken();
        return;
    }

    while(Tok.isNot(tok::annot_pragma_approx_end)){
        if (!Tok.isNot(tok::identifier)){
            if ( isApproxDirective(Tok, DK) ){
                dbgs() << "Identfied directive: " << DirectiveInfo::Name[DK] << "\n";
            }
            else{
                PP.Diag(Tok,diag::err_pragma_approx_unrecognized_directive);
                SkipUntil(tok::annot_pragma_approx_end);
                return;
            }
            ConsumeAnyToken();
        }
    }
    /// We need to consume also annot_pragma_approx_end
    ConsumeAnyToken();

    return;
}
