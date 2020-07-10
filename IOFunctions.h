/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/05/18            */
/*                                                     */
/*               I/O FUNCTIONS HEADER FILE             */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added the get-char, set-locale, and            */
/*            read-number functions.                         */
/*                                                           */
/*            Modified printing of floats in the format      */
/*            function to use the locale from the set-locale */
/*            function.                                      */
/*                                                           */
/*            Moved IllegalLogicalNameMessage function to    */
/*            argacces.c.                                    */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Removed the undocumented use of t in the       */
/*            printout command to perform the same function  */
/*            as crlf.                                       */
/*                                                           */
/*            Replaced EXT_IO and BASIC_IO compiler flags    */
/*            with IO_FUNCTIONS compiler flag.               */
/*                                                           */
/*            Added a+, w+, rb, ab, r+b, w+b, and a+b modes  */
/*            for the open function.                         */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Added put-char function.                       */
/*                                                           */
/*            Added SetFullCRLF which allows option to       */
/*            specify crlf as \n or \r\n.                    */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added print and println functions.             */
/*                                                           */
/*            Added unget-char function.                     */
/*                                                           */
/*            Added flush, rewind, tell, seek, and chdir     */
/*            functions.                                     */
/*                                                           */
/*************************************************************/

#ifndef _H_iofun

#pragma once

#define _H_iofun

void IOFunctionDefinitions(const Environment&);
#if IO_FUNCTIONS
bool SetFullCRLF(const Environment&, bool);
void PrintoutFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void PrintFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void PrintlnFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ReadFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void OpenFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void CloseFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FlushFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void RewindFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void TellFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SeekFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetCharFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void UngetCharFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void PutCharFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ReadlineFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FormatFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void RemoveFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ChdirFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void RenameFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SetLocaleFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ReadNumberFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
#endif

#endif /* _H_iofun */






