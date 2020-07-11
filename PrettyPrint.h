/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/02/17            */
/*                                                     */
/*               PRETTY PRINT HEADER FILE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
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
/*************************************************************/

#ifndef _H_pprint

#pragma once

#define _H_pprint

constexpr auto PRETTY_PRINT_DATA = 52;

struct prettyPrintData : public EnvironmentModule {
    bool PPBufferStatus;
    bool PPBufferEnabled;
    size_t IndentationDepth;
    size_t PPBufferPos;
    size_t PPBufferMax;
    size_t PPBackupOnce;
    size_t PPBackupTwice;
    char *PrettyPrintBuffer;
};
RegisterEnvironmentModule(prettyPrintData, PRETTY_PRINT_DATA, PrettyPrint);

void InitializePrettyPrintData(const Environment&);
void FlushPPBuffer(const Environment&);
void DestroyPPBuffer(const Environment&);
void SavePPBuffer(const Environment&, const char *);
void PPBackup(const Environment&);
char *CopyPPBuffer(const Environment&);
char *GetPPBuffer(const Environment&);
void PPCRAndIndent(const Environment&);
void IncrementIndentDepth(const Environment&, size_t);
void DecrementIndentDepth(const Environment&, size_t);
void SetIndentDepth(const Environment&, size_t);
void SetPPBufferStatus(const Environment&, bool);
bool GetPPBufferStatus(const Environment&);
bool SetPPBufferEnabled(const Environment&, bool);
bool GetPPBufferEnabled(const Environment&);

#endif



