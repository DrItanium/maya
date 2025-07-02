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
struct SingleWhiteSpace : 
    tp::sor <
        tp::ascii::space,
        PoundLineComment,
        tp::if_must<tp::one<'/'>, CStyleLineComment>
    > { };
struct WhiteSpace : tp::star<SingleWhiteSpace> { };
//struct LabelStatement : tp::if_must<tp::one<':'>, WhiteSpace, 
#define X(title, text) struct Keyword ## title : TAO_PEGTL_STRING( text ) { };
#define Directive(title) X( Directive_ ## title , "." #title )
#define DefModifier(title) X( Modifier_ ## title , "%" #title )
#define DefRegister(title) X( Register_ ## title , #title  )
Directive(altmacro);
Directive(align);
Directive(ascii);
Directive(asciz);
Directive(attach_to_group);
Directive(balign);
Directive(base64);
Directive(bss);
Directive(byte);
Directive(comm);
Directive(data);
Directive(def);
Directive(desc);
Directive(dim);
Directive(double);
Directive(eject);
Directive(else);
Directive(elseif);
Directive(end);
Directive(endef);
Directive(endfunc);
Directive(endif);
Directive(equ);
Directive(equiv);
Directive(eqv);
Directive(err);
Directive(error);
Directive(exitm);
Directive(extern);
Directive(fail);
Directive(file);
Directive(fill);
Directive(float);
Directive(func);
Directive(global);
Directive(gnu_attribute);
Directive(hidden);
Directive(hword);
Directive(ident);
Directive(if);
Directive(incbin);
Directive(include);
Directive(int);
Directive(internal);
Directive(irp);
Directive(irpc);
Directive(lcomm);
Directive(lflags);
Directive(line);
Directive(linkonce);
Directive(list);
Directive(ln);
Directive(loc);
Directive(loc_mark_labels);
Directive(local);
Directive(long);
Directive(macro);
Directive(mri);
Directive(noaltmacro);
Directive(nolist);
Directive(nop);
Directive(nops);
Directive(octa);
Directive(offset);
Directive(org);
Directive(p2align);
Directive(popsection);
Directive(previous);
Directive(print);
Directive(protected);
Directive(psize);
Directive(purgem);
Directive(pushsection);
Directive(quad);
Directive(reloc);
Directive(rept);
Directive(sbttl);
Directive(scl);
Directive(section);
Directive(set);
Directive(short);
Directive(single);
Directive(size);
Directive(skip);
Directive(sleb128);
Directive(space);
Directive(stabd);
Directive(stabn);
Directive(stabs);
Directive(string);
Directive(struct);
Directive(subsection);
Directive(symver);
Directive(tag);
Directive(text);
Directive(title);
Directive(tls);
Directive(type);
Directive(uleb128);
Directive(val);
Directive(version);
Directive(vtable);
Directive(vtable_inherit);
Directive(warning);
Directive(weak);
Directive(weakref);
Directive(word);
Directive(zero);
Directive(2byte);
Directive(4byte);
Directive(8byte);


Directive(half);
Directive(dword);
Directive(dtprelword);
Directive(dtpreldword);
Directive(option);
Directive(insn);
Directive(attribute) ;

#define CFIDirective(text) Directive(cfi_ ## text);
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
DefModifier(lo);
DefModifier(hi);
DefModifier(pcrel_lo);
DefModifier(pcrel_hi);
DefModifier(got_pcrel_hi);
DefModifier(tprel_add);
DefModifier(tprel_lo);
DefModifier(tprel_hi);
DefModifier(tls_ie_pcrel_hi);
DefModifier(tls_gd_pcrel_hi);
// registers
#define RegisterDecl(idx, alias) \
    DefRegister( x ## idx ); \
    DefRegister( alias );
#define FirstRegisterDecl(i, a) RegisterDecl(i, a)
#define LastRegisterDecl(i, a) RegisterDecl(i, a)
#include "RegisterDecl.def"
#undef FirstRegisterDecl
#undef LastRegisterDecl
#undef RegisterDecl
#define FloatRegisterDecl(idx, alias) \
    DefRegister( f ## idx ); \
    DefRegister( alias );
#define FirstFloatRegisterDecl(a, b) FloatRegisterDecl(a, b)
#define LastFloatRegisterDecl(a, b) FloatRegisterDecl(a, b)
#include "FloatRegisterDecl.def"
#undef FirstFloatRegisterDecl
#undef LastFloatRegisterDecl
#undef FloatRegisterDecl
#undef DefRegister
#undef DefModifier
#undef Directive
#undef X

struct GPRs : tp::sor<
#define FirstRegisterDecl(idx, alias) KeywordRegister_x ## idx , KeywordRegister_ ## alias 
#define RegisterDecl(idx, alias) , FirstRegisterDecl(idx, alias)
#define LastRegisterDecl(idx, alias) RegisterDecl(idx, alias)
#include "RegisterDecl.def"
#undef FirstRegisterDecl
#undef LastRegisterDecl
#undef RegisterDecl
              > {} ;
struct FPRs : tp::sor<
#define FirstFloatRegisterDecl(idx, alias) KeywordRegister_f ## idx , KeywordRegister_ ## alias 
#define FloatRegisterDecl(idx, alias) , FirstFloatRegisterDecl(idx, alias)
#define LastFloatRegisterDecl(idx, alias) FloatRegisterDecl(idx, alias)
#include "FloatRegisterDecl.def"
#undef FirstFloatRegisterDecl
#undef LastFloatRegisterDecl
#undef FloatRegisterDecl
              > { };


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
