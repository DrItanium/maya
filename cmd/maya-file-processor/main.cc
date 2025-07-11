/**
 * @file
 * Frontend to my maya-app library
 * @copyright
 * maya-app
 * Copyright (c) 2012-2023, Joshua Scoggins
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
#include <boost/program_options.hpp>
#include "fs/path.h"
#include <iostream>
#include <string>
#include <vector>
#include <list>

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
int main(
  int argc,
  char *argv[])
  {
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT, CatchCtrlC);
#endif
    try {
        boost::program_options::options_description desc{"Options"};
        //clang-format off
        desc.add_options()
                ("help,h", "Help screen")
                ("input-file,p", boost::program_options::value<Neutron::Path>(), "input file")
                ("output-file,o", boost::program_options::value<Neutron::Path>(), "output file to save output to")
                ("include,I", boost::program_options::value<std::vector<Neutron::Path>>(), "add the given path to the back of include path")
                ("working-dir,w", boost::program_options::value<Neutron::Path>()->default_value("."), "Set the root of this application")
                ("repl,r", boost::program_options::bool_switch()->default_value(false), "Enter into the repl instead of invoking the standard design loop")
                ;
        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
        boost::program_options::notify(vm);
        // clang-format on
        if (vm.count("help")) {
            std::cerr << desc << std::endl;
            return 1;
        }
        if (vm.count("include")) {
            for (const auto &path: vm["include"].as<std::vector<Neutron::Path>>()) {
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
        auto parseTarget = vm["input-file"];
        if (parseTarget.empty()) {
            std::cerr << "ERROR: no file to parse provided!" << std::endl;
            return 1;
        } 
        auto outputParseTarget = vm["output-file"];
        Electron::Value returnValue;
        // okay so we have loaded the init.clp
        // now pass the input and output paths to the expert system
        if (outputParseTarget.empty()) {
            mainEnv.call("begin", &returnValue, parseTarget.as<Neutron::Path>(), false);
        } else {
            mainEnv.call("begin", &returnValue, parseTarget.as<Neutron::Path>(), outputParseTarget.as<Neutron::Path>());
        }
        bool enableRepl = vm["repl"].as<bool>();
        if (enableRepl) {
            std::cout << "REPL MODE" << std::endl;
            std::cout << "begin has been called but reset has not" << std::endl;
            CommandLoop(mainEnv);
            return -1;
        } else {
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
