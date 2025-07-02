/**
 * @file
 * Frontend to the rv32 assembly to i960 assembly translator application
 * @copyright
 * maya-app
 * Copyright (c) 2012-2025, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include "platform/os.h"
extern "C" {
    #include "clips/clips.h"
}
#include "electron/Environment.h"
#include "fs/path.h"
#include <boost/program_options.hpp>
#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <tao/pegtl.hpp>
namespace tp = tao::pegtl; 

// taken from json grammar.hpp
template<char letter>
struct GenericLineComment : tp::seq< tp::one <letter>, tp::until< tp::eolf>> { };

using CStyleLineComment = GenericLineComment<'/'>;
using PoundLineComment = GenericLineComment<'#'>;
struct EndBlockComment : tp::until<tp::string<'*', '/'>> { };
struct BlockComment : tp::if_must<tp::one<'*'>, EndBlockComment> { };

struct CStyleComment : tp::sor< CStyleLineComment, BlockComment> { };
struct WhiteSpace : 
    tp::sor <
        tp::one < ' ', '\t', '\n', '\t' >,
        PoundLineComment,
        tp::if_must<tp::one<'/'>, CStyleLineComment>
    > { };

#define X(title, text) struct Keyword ## title : TAO_PEGTL_STRING( text ) { };
#define Directive(title, text) X( Directive ## title , "." text)
#define DefModifier(title, text) X( Modifier ## title , "%" text )
#define DefRegister(title) X( Register_ ## title , #title )
Directive(Altmacro, "altmacro");
Directive(Align, "align");
Directive(Ascii, "ascii");
Directive(Asciz, "asciz");
Directive(AttachToGroup, "attach_to_group");
Directive(BAlign, "balign");
Directive(Base64, "base64");
Directive(Bss, "bss");
Directive(Byte, "byte");
Directive(Comm, "comm");
Directive(Data, "data");
Directive(Def, "def");
Directive(Desc, "desc");
Directive(Dim, "dim");
Directive(Double, "double");
Directive(Eject, "eject");
Directive(Else, "else");
Directive(ElseIf, "elseif");
Directive(End, "end");
Directive(Endef, "endef");
Directive(EndFunc, "endfunc");
Directive(EndIf, "endif");
Directive(Equ, "equ");
Directive(Equiv, "equiv");
Directive(Eqv, "eqv");
Directive(Err, "err");
Directive(Error, "error");
Directive(Exitm, "exitm");
Directive(Extern, "extern");
Directive(Fail, "fail");
Directive(File, "file");
Directive(Fill, "fill");
Directive(Float, "float");
Directive(Func, "func");
Directive(Global, "global");
Directive(GnuAttribute, "gnu_attribute");
Directive(Hidden, "hidden");
Directive(HWord, "hword");
Directive(Ident, "ident");
Directive(If, "if");
Directive(IncBin, "incbin");
Directive(Include, "include");
Directive(Int, "int");
Directive(Internal, "internal");
Directive(Irp, "irp");
Directive(Irpc, "irpc");
Directive(LComm, "lcomm");
Directive(LFlags, "lflags");
Directive(Line, "line");
Directive(LinkOnce, "linkonce");
Directive(List, "list");
Directive(Ln, "ln");
Directive(Loc, "loc");
Directive(LocMarkLabels, "loc_mark_labels");
Directive(Local, "local");
Directive(Long, "long");
Directive(Macro, "macro");
Directive(Mri, "mri");
Directive(NoAltMacro, "noaltmacro");
Directive(NoList, "nolist");
Directive(Nop, "nop");
Directive(Nops, "nops");
Directive(Octa, "octa");
Directive(Offset, "offset");
Directive(Org, "org");
Directive(P2Align, "p2align");
Directive(PopSection, "popsection");
Directive(Previous, "previous");
Directive(Print, "print");
Directive(Protected, "protected");
Directive(PSize, "psize");
Directive(Purgem, "purgem");
Directive(PushSection, "pushsection");
Directive(Quad, "quad");
Directive(Reloc, "reloc");
Directive(Rept, "rept");
Directive(Sbttl, "sbttl");
Directive(Scl, "scl");
Directive(Section, "section");
Directive(Set, "set");
Directive(Short, "short");
Directive(Single, "single");
Directive(Size, "size");
Directive(Skip, "skip");
Directive(SLEB128 ,"sleb128");
Directive(Space, "space");
Directive(Stabd, "stabd");
Directive(Stabn, "stabn");
Directive(Stabs, "stabs");
Directive(String, "string");
Directive(Struct, "struct");
Directive(Subsection, "subsection");
Directive(Symver, "symver");
Directive(Tag, "tag");
Directive(Text, "text");
Directive(Title, "title");
Directive(TLS, "tls");
Directive(Type, "type");
Directive(ULEB128 ,"uleb128");
Directive(Val, "val");
Directive(Version, "version");
Directive(VTable, "vtable");
Directive(VTableInherit, "vtable_inherit");
Directive(Warning, "warning");
Directive(Weak, "weak");
Directive(Weakref, "weakref");
Directive(Word, "word");
Directive(Zero, "zero");
Directive(TwoByte, "2byte");
Directive(FourByte, "4byte");
Directive(EightByte, "8byte");

Directive(Half, "half");
Directive(DWord, "dword");
Directive(DTPrelWord ,"dtprelword");
Directive(DTPrelDWord ,"dtpreldword");
Directive(Option ,"option");
Directive(Insn ,"insn");
Directive(Attribute ,"attribute") ;

#define CFIDirective(text) Directive(CFI_ ## text, ".cfi_" #text );
CFIDirective(sections);
CFIDirective(startproc);
CFIDirective(endproc);
CFIDirective(personality);
CFIDirective(personality_id);
CFIDirective(fde_data);
CFIDirective(lsda);
CFIDirective(inline_lsda);
CFIDirective(def_cfa);
CFIDirective(def_cfa_register);
CFIDirective(def_cfa_offset);
CFIDirective(adjust);
CFIDirective(offset);
CFIDirective(val_offset);
CFIDirective(rel_offset);
CFIDirective(register);
CFIDirective(restore);
CFIDirective(undefined);
CFIDirective(same_value);
CFIDirective(remember_state);
CFIDirective(restore_state);
CFIDirective(return_column);
CFIDirective(signal_frame);
CFIDirective(window);
CFIDirective(escape);
CFIDirective(val_encoded_addr);
#undef CFIDirective
// assembler modifiers
DefModifier(Lo, "lo");
DefModifier(Hi, "hi");
DefModifier(PCRELLo, "pcrel_lo");
DefModifier(PCRELHi, "pcrel_hi");
DefModifier(GotPCRELHi, "got_pcrel_hi");
DefModifier(TPRelAdd, "tprel_add");
DefModifier(TPRelLo, "tprel_lo");
DefModifier(TPRelHi, "tprel_hi");
DefModifier(TLSIEPCRELHi, "tls_ie_pcrel_hi");
DefModifier(TLSGDPCRELHi, "tls_gd_pcrel_hi");
// registers
#define RegisterDecl(idx, alias) \
    DefRegister( x ## idx ); \
    DefRegister( alias ) 
#define FloatRegisterDecl(idx, alias) \
    DefRegister( f ## idx ); \
    DefRegister( alias )
RegisterDecl(0, zero);
RegisterDecl(1, ra);
RegisterDecl(2, sp);
RegisterDecl(3, gp);
RegisterDecl(4, tp);
RegisterDecl(5, t0);
RegisterDecl(6, t1);
RegisterDecl(7, t2);
RegisterDecl(8, s0);
RegisterDecl(9, s1);
RegisterDecl(10, a0);
RegisterDecl(11, a1);
RegisterDecl(12, a2);
RegisterDecl(13, a3);
RegisterDecl(14, a4);
RegisterDecl(15, a5);
RegisterDecl(16, a6);
RegisterDecl(17, a7);
RegisterDecl(18, s2);
RegisterDecl(19, s3);
RegisterDecl(20, s4);
RegisterDecl(21, s5);
RegisterDecl(22, s6);
RegisterDecl(23, s7);
RegisterDecl(24, s8);
RegisterDecl(25, s9);
RegisterDecl(26, s10);
RegisterDecl(27, s11);
RegisterDecl(28, t3);
RegisterDecl(29, t4);
RegisterDecl(30, t5);
RegisterDecl(31, t6);

FloatRegisterDecl(0, ft0);
FloatRegisterDecl(1, ft1);
FloatRegisterDecl(2, ft2);
FloatRegisterDecl(3, ft3);
FloatRegisterDecl(4, ft4);
FloatRegisterDecl(5, ft5);
FloatRegisterDecl(6, ft6);
FloatRegisterDecl(7, ft7);
FloatRegisterDecl(8, fs0);
FloatRegisterDecl(9, fs1);
FloatRegisterDecl(10, fa0);
FloatRegisterDecl(11, fa1);
FloatRegisterDecl(12, fa2);
FloatRegisterDecl(13, fa3);
FloatRegisterDecl(14, fa4);
FloatRegisterDecl(15, fa5);
FloatRegisterDecl(16, fa6);
FloatRegisterDecl(17, fa7);
FloatRegisterDecl(18, fs2);
FloatRegisterDecl(19, fs3);
FloatRegisterDecl(20, fs4);
FloatRegisterDecl(21, fs5);
FloatRegisterDecl(22, fs6);
FloatRegisterDecl(23, fs7);
FloatRegisterDecl(24, fs8);
FloatRegisterDecl(25, fs9);
FloatRegisterDecl(26, fs10);
FloatRegisterDecl(27, fs11);
FloatRegisterDecl(28, ft8);
FloatRegisterDecl(29, ft9);
FloatRegisterDecl(30, ft10);
FloatRegisterDecl(31, ft11);
#undef FloatRegisterDecl
#undef RegisterDecl
#undef DefRegister
#undef DefModifier
#undef Directive
#undef X


#if   UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
#include <signal.h>
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
   static void                    CatchCtrlC(int);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

Electron::Environment mainEnv;
/****************************************/
/* main: Starts execution of the expert */
/*   system development environment.    */
/****************************************/
int main(int argc, char *argv[]) {
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT, CatchCtrlC);
#endif
    using PathVector = std::vector<Neutron::Path>;
    try {
        boost::program_options::options_description desc{"Options"};
        //clang-format off
        desc.add_options()
            ("help,h", "Help screen")
            ("include,I", boost::program_options::value<PathVector>(), "add the given path to the back of include path")
            ("working-dir,w", boost::program_options::value<Neutron::Path>()->default_value("."),
             "Set the root of this application")
            ("repl,r", boost::program_options::bool_switch()->default_value(false),
             "Enter into the repl instead of invoking the standard design loop")
            ("batch,f", boost::program_options::value<PathVector>(), "files to batch")
            ("batch-star", boost::program_options::value<PathVector>(), "files to batch*")
            ("f2", boost::program_options::value<PathVector>(), "files to batch*")
            ("load,l", boost::program_options::value<PathVector>(), "files to load");
        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);
        //clang-format on
        if (vm.count("help")) {
            std::cerr << desc << std::endl;
            return 1;
        }
        if (vm.count("include")) {
            for (const auto &path: vm["include"].as<PathVector>()) {
                mainEnv.addToIncludePathBack(path);
            }
        }
        auto value = vm["working-dir"].as<Neutron::Path>();
        mainEnv.addToIncludePathFront(value);
        Neutron::Path initLocation{value / "init.clp"};
        if (!Neutron::exists(initLocation)) {
            std::cerr << "ERROR: " << initLocation << " does not exist!" << std::endl;
            return 1;
        }
        if (!mainEnv.batchFile(initLocation)) {
            std::cerr << "ERROR: Failed to batch " << initLocation << std::endl;
            return 1;
        }
        // okay so we have loaded the init.clp

        if (vm.count("batch")) {
            for (const auto &path: vm["batch"].as<PathVector>()) {
                if (!mainEnv.batchFile(path, false)) {
                    std::cerr << "couldn't batch "  << path << std::endl;
                    return 1;
                }
            }
        }
        if (vm.count("batch-star")) {
            for (const auto &path: vm["batch-star"].as<PathVector>()) {
                if (!mainEnv.batchFile(path)) {
                    std::cerr << "couldn't batch* " << path <<  std::endl;
                    return 1;
                }
            }
        }
        if (vm.count("f2")) {
            for (const auto &path: vm["f2"].as<PathVector>()) {
                if (!mainEnv.batchFile(path)) {
                    std::cerr << "couldn't batch* " << path <<  std::endl;
                    return 1;
                }
            }
        }
        if (vm.count("load")) {
            for (const auto& path : vm["load"].as<PathVector>()) {
                mainEnv.loadFile(path);
            }
        }
        bool enableRepl = vm["repl"].as<bool>();
        if (enableRepl) {
            std::cout << "REPL MODE" << std::endl;
            std::cout << "NOTE: begin and reset must be invoked manually" << std::endl;
            CommandLoop(mainEnv);
            return -1;
        } else {
            mainEnv.call("begin");
            mainEnv.reset();
            mainEnv.run(-1);
        }
        // unlike normal CLIPS, the environment will automatically clean itself up
        return 0;
    } catch (const Neutron::Exception& ex) {
        std::cerr << ex.what() << std::endl;
        return 1;
    } catch (const boost::program_options::error& ex) {
        std::cerr << ex.what() << std::endl;
        return 1;
    }
}

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
static void CatchCtrlC(
        int sgnl)
{
    SetHaltExecution(mainEnv,true);
    CloseAllBatchSources(mainEnv);
    signal(SIGINT,CatchCtrlC);
}
#endif
