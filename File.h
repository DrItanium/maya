/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  04/04/19            */
/*                                                     */
/*              FILE COMMANDS HEADER FILE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added code for capturing errors/warnings.      */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.31: Fixed error in AppendDribble for older         */
/*            compilers not allowing variable definition     */
/*            within for statement.                          */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_filecom

#pragma once

#define _H_filecom

#include <cstdio>
#include <cstdlib>

#include "Entities.h"

typedef struct batchEntry BatchEntry;

/***************/
/* STRUCTURES  */
/***************/

struct batchEntry {
    int batchType;
    FILE *fileSource;
    const char *logicalSource;
    const char *theString;
    const char *fileName;
    long lineNumber;
    BatchEntry *next;
};

/***************/
/* DEFINITIONS */
/***************/

constexpr auto FILE_BATCH = 0;
constexpr auto STRING_BATCH = 1;
constexpr auto BUFFER_SIZE = 120;
constexpr auto FILECOM_DATA = 14;

struct fileCommandData {
#if DEBUGGING_FUNCTIONS
    FILE *DribbleFP;
    char *DribbleBuffer;
    size_t DribbleCurrentPosition;
    size_t DribbleMaximumPosition;
    int (*DribbleStatusFunction)(const Environment&, bool);
#endif
    int BatchType;
    FILE *BatchFileSource;
    const char *BatchLogicalSource;
    char *BatchBuffer;
    size_t BatchCurrentPosition;
    size_t BatchMaximumPosition;
    BatchEntry *TopOfBatchList;
    BatchEntry *BottomOfBatchList;
    char *batchPriorParsingFile;
};

#define FileCommandData(theEnv) ((fileCommandData *) GetEnvironmentData(theEnv,FILECOM_DATA))

void FileCommandDefinitions(const Environment&);
void BatchCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void BatchStarCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void LoadCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void LoadStarCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SaveCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DribbleOnCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DribbleOffCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);

constexpr auto FILE_ROUTER_DATA = 47;

struct fileRouter {
    const char *logicalName;
    FILE *stream;
    struct fileRouter *next;
};

struct fileRouterData {
    struct fileRouter *ListOfFileRouters;
};

#define FileRouterData(theEnv) ((fileRouterData *) GetEnvironmentData(theEnv,FILE_ROUTER_DATA))

void InitializeFileRouter(const Environment&);
FILE *FindFptr(const Environment&, const char *);
bool OpenAFile(const Environment&, const char *, const char *, const char *);
bool CloseAllFiles(const Environment&);
bool CloseFile(const Environment&, const char *);
bool FindFile(const Environment&, const char *, void *context = nullptr);
bool FlushAllFiles(const Environment&);
bool FlushFile(const Environment&, const char *);
bool RewindFile(const Environment&, const char *);
long long TellFile(const Environment&, const char *);
bool SeekFile(const Environment&, const char *, long, int);

bool DribbleOn(const Environment&, const char *);
bool DribbleActive(const Environment&);
bool DribbleOff(const Environment&);
void AppendDribble(const Environment&, const char *);
int LLGetcBatch(const Environment&, const char *, bool);
bool Batch(const Environment&, const char *);
bool OpenBatch(const Environment&, const char *, bool);
bool OpenStringBatch(const Environment&, const char *, const char *, bool);
bool RemoveBatch(const Environment&);
bool BatchActive(const Environment&);
void CloseAllBatchSources(const Environment&);
bool BatchStar(const Environment&, const char *);
#endif /* _H_filecom */






