/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  05/03/19             */
/*                                                     */
/*            SYSTEM DEPENDENT HEADER FILE             */
/*******************************************************/

/*************************************************************/
/* Purpose: Isolation of system dependent routines.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modified GenOpen to check the file length      */
/*            against the system constant FILENAME_MAX.      */
/*                                                           */
/*      6.24: Support for run-time programs directly passing */
/*            the hash tables for initialization.            */
/*                                                           */
/*            Made gensystem functional for Xcode.           */
/*                                                           */
/*            Added BeforeOpenFunction and AfterOpenFunction */
/*            hooks.                                         */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Updated UNIX_V gentime functionality.          */
/*                                                           */
/*            Removed GenOpen check against FILENAME_MAX.    */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, IBM_ICB, IBM_TBC, IBM_ZTC, and        */
/*            IBM_SC).                                       */
/*                                                           */
/*            Renamed IBM_MSC and WIN_MVC compiler flags     */
/*            and IBM_GCC to WIN_GCC.                        */
/*                                                           */
/*            Added LINUX and DARWIN compiler flags.         */
/*                                                           */
/*            Removed HELP_FUNCTIONS compilation flag and    */
/*            associated functionality.                      */
/*                                                           */
/*            Removed EMACS_EDITOR compilation flag and      */
/*            associated functionality.                      */
/*                                                           */
/*            Combined BASIC_IO and EXT_IO compilation       */
/*            flags into the single IO_FUNCTIONS flag.       */
/*                                                           */
/*            Changed the EX_MATH compilation flag to        */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS_TYPE.       */
/*                                                           */
/*            GenOpen function checks for UTF-8 Byte Order   */
/*            Marker.                                        */
/*                                                           */
/*            Added gengetchar, genungetchar, genprintfile,  */
/*            genstrcpy, genstrncpy, genstrcat, genstrncat,  */
/*            and gensprintf functions.                      */
/*                                                           */
/*            Added SetJmpBuffer function.                   */
/*                                                           */
/*            Added environment argument to genexit.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Added genchdir function for changing the       */
/*            current directory.                             */
/*                                                           */
/*            Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Removed ContinueEnvFunction, PauseEnvFunction, */
/*            and RedrawScreenFunction callbacks.            */
/*                                                           */
/*            Completion code now returned by gensystem.     */
/*                                                           */
/*            Added flush, rewind, tell, and seek functions. */
/*                                                           */
/*************************************************************/

#ifndef _H_sysdep

#pragma once

#define _H_sysdep

#include <cstdio>
#include <setjmp.h>

double gentime();
int gensystem(const Environment::Ptr&, const char *);
bool GenOpenReadBinary(const Environment::Ptr&, const char *, const char *);
void GetSeekCurBinary(const Environment::Ptr&, long);
void GetSeekSetBinary(const Environment::Ptr&, long);
void GenTellBinary(const Environment::Ptr&, long *);
void GenCloseBinary(const Environment::Ptr&);
void GenReadBinary(const Environment::Ptr&, void *, size_t);
FILE *GenOpen(const Environment::Ptr&, const char *, const char *);
int GenClose(const Environment::Ptr&, FILE *);
int GenFlush(const Environment::Ptr&, FILE *);
void GenRewind(const Environment::Ptr&, FILE *);
long long GenTell(const Environment::Ptr&, FILE *);
int GenSeek(const Environment::Ptr&, FILE *, long, int);
void genexit(const Environment::Ptr&, int);
int genrand();
void genseed(unsigned int);
bool genremove(const Environment::Ptr&, const char *);
bool genrename(const Environment::Ptr&, const char *, const char *);
char *gengetcwd(char *, int);
void GenWrite(void *, size_t, FILE *);
int (*SetBeforeOpenFunction(const Environment::Ptr&, int (*)(const Environment::Ptr&)))(const Environment::Ptr&);
int (*SetAfterOpenFunction(const Environment::Ptr&, int (*)(const Environment::Ptr&)))(const Environment::Ptr&);
int gensprintf(char *, const char *, ...);
char *genstrcpy(char *, const char *);
char *genstrncpy(char *, const char *, size_t);
char *genstrcat(char *, const char *);
char *genstrncat(char *, const char *, size_t);
int genchdir(const Environment::Ptr&, const char *);
void genprintfile(const Environment::Ptr&, FILE *, const char *);
int gengetchar(const Environment::Ptr&);
int genungetchar(const Environment::Ptr&, int);
void InitializeSystemDependentData(const Environment::Ptr&);
void InitializeNonportableFeatures(const Environment::Ptr&);

#endif /* _H_sysdep */





