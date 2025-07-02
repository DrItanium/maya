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
#define DefRegister(title, text) X( Register ## title , text )
Directive(Abort, "abort");
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
DefRegister(Zero, "zero");
DefRegister(RA, "ra");
DefRegister(SP, "sp");
DefRegister(GP, "gp");
DefRegister(TP, "tp");
DefRegister(T0, "t0");
DefRegister(T1, "t1");
DefRegister(T2, "t2");
DefRegister(S0, "s0");
DefRegister(S1, "s1");
DefRegister(A0, "a0");
DefRegister(A1, "a1");
DefRegister(A2, "a2");
DefRegister(A3, "a3");
DefRegister(A4, "a4");
DefRegister(A5, "a5");
DefRegister(A6, "a6");
DefRegister(A7, "a7");
DefRegister(S2, "s2");
DefRegister(S3, "s3");
DefRegister(S4, "s4");
DefRegister(S5, "s5");
DefRegister(S6, "s6");
DefRegister(S7, "s7");
DefRegister(S8, "s8");
DefRegister(S9, "s9");
DefRegister(S10, "s10");
DefRegister(S11, "s11");
DefRegister(T3, "t3");
DefRegister(T4, "t4");
DefRegister(T5, "t5");
DefRegister(T6, "t6");
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
