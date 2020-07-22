#ifndef LLVM_CLANG_BASIC_APPROX_H
#define LLVM_CLANG_BASIC_APPROX_H

#include "clang/Basic/SourceLocation.h"

namespace clang{
namespace approx{

    enum DirectiveApproxKind {
        DK_PERFO = 0,
        DK_MEMO,
        DK_DT,
        DK_NN
    };

    const unsigned DK_START = DK_PERFO;
    const unsigned DK_END = DK_NN+1;

    enum ClauseKind{
        /// Lets have such a clause to
        //let user identify regions,
        /// and for dbg perposes
        CK_LABEL = 0,
        CK_IN,
        CK_OUT,
        CK_USERFN,
        CK_HINT,
        CK_KNOB,
        CK_THRESHOLD
    };

    const unsigned CK_START = CK_LABEL;
    const unsigned CK_END = CK_THRESHOLD +1;

    class CommonInfo {
        private :
        SourceLocation StartLoc;
        SourceLocation EndLoc;

        protected:
        const SourceLocation &getLocStart() const { return StartLoc;}
        const SourceLocation &getLocEnd() const { return EndLoc;}

        public:
        CommonInfo(SourceLocation start, SourceLocation end): StartLoc(start), EndLoc(end) {}
        CommonInfo() {}
    };

    class DirectiveInfo : public CommonInfo {
        private:

        const DirectiveApproxKind DK;

        public:

        static const std::string Name[DK_END];
        DirectiveApproxKind getKind() const { return DK; }

        DirectiveInfo ( DirectiveApproxKind dk, SourceLocation Loc):
            CommonInfo(Loc, Loc), DK(dk) {};
        static const unsigned ValidDirective[DK_END];
        std::string getAsString() const { return Name[DK]; }
    };

}// namespace approx
}// namespace clang

#endif // LLVM_CLANG_BASIC_APPROX_H
