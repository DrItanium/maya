/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/24/17            */
/*                                                     */
/*                 FILE COMMANDS MODULE                */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bebe Ly                                              */
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
/*            Added STDOUT and STDIN logical name            */
/*            definitions.                                   */
/*                                                           */
/*      6.31: Unprocessed batch files did not deallocate     */
/*            all memory on exit.                            */
/*                                                           */
/*      6.40: Split inputSource to fileSource and            */
/*            logicalSource.                                 */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstring>

#include "Setup.h"

#include "ArgumentAccess.h"
#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "BinarySave.h"
#endif
#include "CommandLine.h"
#include "Construct.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "File.h"
#include "MemoryAllocation.h"
#include "Router.h"
#include "SystemDependency.h"
#include "Utility.h"

#include "File.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void DeallocateFileCommandData(Environment *);

/***************************************/
/* FileCommandDefinitions: Initializes */
/*   file commands.                    */
/***************************************/
void FileCommandDefinitions(
        Environment *theEnv) {
    AllocateEnvironmentData(theEnv, FILECOM_DATA, sizeof(fileCommandData), DeallocateFileCommandData);

#if DEBUGGING_FUNCTIONS
    AddUDF(theEnv, "batch", "b", 1, 1, "sy", BatchCommand);
    AddUDF(theEnv, "batch*", "b", 1, 1, "sy", BatchStarCommand);
    AddUDF(theEnv, "dribble-on", "b", 1, 1, "sy", DribbleOnCommand);
    AddUDF(theEnv, "dribble-off", "b", 0, 0, nullptr, DribbleOffCommand);
    AddUDF(theEnv, "save", "b", 1, 1, "sy", SaveCommand);
#endif
    AddUDF(theEnv, "load", "b", 1, 1, "sy", LoadCommand);
    AddUDF(theEnv, "load*", "b", 1, 1, "sy", LoadStarCommand);
#if BLOAD_AND_BSAVE
    AddUDF(theEnv, "bsave", "b", 1, 1, "sy", BsaveCommand);
#endif
#if BLOAD_AND_BSAVE
    InitializeBsaveData(theEnv);
    InitializeBloadData(theEnv);
    AddUDF(theEnv, "bload", "b", 1, 1, "sy", BloadCommand);
#endif
}

/******************************************************/
/* DeallocateFileCommandData: Deallocates environment */
/*    data for file commands.                         */
/******************************************************/
static void DeallocateFileCommandData(
        Environment *theEnv) {
    struct batchEntry *theEntry, *nextEntry;

    theEntry = FileCommandData(theEnv)->TopOfBatchList;
    while (theEntry != nullptr) {
        nextEntry = theEntry->next;

        if (theEntry->batchType == FILE_BATCH) { GenClose(theEnv, FileCommandData(theEnv)->TopOfBatchList->fileSource); }
        else { rm(theEnv, (void *) theEntry->theString, strlen(theEntry->theString) + 1); }

        DeleteString(theEnv, (char *) theEntry->fileName);
        DeleteString(theEnv, (char *) theEntry->logicalSource);
        rtn_struct(theEnv, batchEntry, theEntry);

        theEntry = nextEntry;
    }

    if (FileCommandData(theEnv)->BatchBuffer != nullptr) {
        rm(theEnv, FileCommandData(theEnv)->BatchBuffer, FileCommandData(theEnv)->BatchMaximumPosition);
    }

    DeleteString(theEnv, FileCommandData(theEnv)->batchPriorParsingFile);
    FileCommandData(theEnv)->batchPriorParsingFile = nullptr;

#if DEBUGGING_FUNCTIONS
    if (FileCommandData(theEnv)->DribbleBuffer != nullptr) {
        rm(theEnv, FileCommandData(theEnv)->DribbleBuffer, FileCommandData(theEnv)->DribbleMaximumPosition);
    }

    if (FileCommandData(theEnv)->DribbleFP != nullptr) { GenClose(theEnv, FileCommandData(theEnv)->DribbleFP); }
#endif
}

#if DEBUGGING_FUNCTIONS

/******************************************/
/* DribbleOnCommand: H/L access routine   */
/*   for the dribble-on command.          */
/******************************************/
void DribbleOnCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *fileName;

    if ((fileName = GetFileName(context)) == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    returnValue->lexemeValue = CreateBoolean(theEnv, DribbleOn(theEnv, fileName));
}

/*******************************************/
/* DribbleOffCommand: H/L access  routine  */
/*   for the dribble-off command.          */
/*******************************************/
void DribbleOffCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    returnValue->lexemeValue = CreateBoolean(theEnv, DribbleOff(theEnv));
}

#endif /* DEBUGGING_FUNCTIONS */

/**************************************/
/* BatchCommand: H/L access routine   */
/*   for the batch command.           */
/**************************************/
void BatchCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *fileName;

    if ((fileName = GetFileName(context)) == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    returnValue->lexemeValue = CreateBoolean(theEnv, OpenBatch(theEnv, fileName, false));
}

/******************************************/
/* BatchStarCommand: H/L access routine   */
/*   for the batch* command.              */
/******************************************/
void BatchStarCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *fileName;

    if ((fileName = GetFileName(context)) == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    returnValue->lexemeValue = CreateBoolean(theEnv, BatchStar(theEnv, fileName));
}

/***********************************************************/
/* LoadCommand: H/L access routine for the load command.   */
/***********************************************************/
void LoadCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *theFileName;
    LoadError rv;

    if ((theFileName = GetFileName(context)) == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    if (CommandLineData(theEnv)->EvaluatingTopLevelCommand) { SetPrintWhileLoading(theEnv, true); }

    if ((rv = Load(theEnv, theFileName)) == LE_OPEN_FILE_ERROR) {
        SetPrintWhileLoading(theEnv, false);
        OpenErrorMessage(theEnv, "load", theFileName);
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    if (CommandLineData(theEnv)->EvaluatingTopLevelCommand) { SetPrintWhileLoading(theEnv, false); }

    if (rv == LE_PARSING_ERROR) returnValue->lexemeValue = FalseSymbol(theEnv);
    else returnValue->lexemeValue = TrueSymbol(theEnv);
}

/****************************************************************/
/* LoadStarCommand: H/L access routine for the load* command.   */
/****************************************************************/
void LoadStarCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *theFileName;
    LoadError rv;

    if ((theFileName = GetFileName(context)) == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    if ((rv = Load(theEnv, theFileName)) == LE_OPEN_FILE_ERROR) {
        OpenErrorMessage(theEnv, "load*", theFileName);
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    if (rv == LE_PARSING_ERROR) returnValue->lexemeValue = FalseSymbol(theEnv);
    else returnValue->lexemeValue = TrueSymbol(theEnv);
}

#if DEBUGGING_FUNCTIONS
/*********************************************************/
/* SaveCommand: H/L access routine for the save command. */
/*********************************************************/
void SaveCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *theFileName;

    if ((theFileName = GetFileName(context)) == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    if (!Save(theEnv, theFileName)) {
        OpenErrorMessage(theEnv, "save", theFileName);
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    returnValue->lexemeValue = TrueSymbol(theEnv);
}
#endif
