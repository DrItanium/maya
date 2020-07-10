/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/02/17             */
/*                                                     */
/*                 PRETTY PRINT MODULE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Corrected code generating compilation          */
/*            warnings.                                      */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Fix for pretty print buffer overflow.          */
/*                                                           */
/*      6.40: Added nullptr pointer check in CopyPPBuffer.      */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstring>
#include <ctype.h>

#include "Setup.h"

#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "SystemDependency.h"
#include "Utility.h"

#include "PrettyPrint.h"

#define PP_CR_FIXED_BUFFER_SIZE 120

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void DeallocatePrettyPrintData(const Environment&);

/****************************************************/
/* InitializePrettyPrintData: Allocates environment */
/*    data for pretty print routines.               */
/****************************************************/
void InitializePrettyPrintData(
        const Environment&theEnv) {
    AllocateEnvironmentData(theEnv, PRETTY_PRINT_DATA, sizeof(prettyPrintData), DeallocatePrettyPrintData);

    PrettyPrintData(theEnv)->PPBufferEnabled = true;
}

/******************************************************/
/* DeallocatePrettyPrintData: Deallocates environment */
/*    data for the pretty print routines.             */
/******************************************************/
static void DeallocatePrettyPrintData(
        const Environment&theEnv) {
    if (PrettyPrintData(theEnv)->PrettyPrintBuffer != nullptr) {
        rm(theEnv, PrettyPrintData(theEnv)->PrettyPrintBuffer, PrettyPrintData(theEnv)->PPBufferMax);
    }
}

/*******************************************************/
/* FlushPPBuffer: Resets the pretty print save buffer. */
/*******************************************************/
void FlushPPBuffer(
        const Environment&theEnv) {
    if (PrettyPrintData(theEnv)->PrettyPrintBuffer == nullptr) return;
    PrettyPrintData(theEnv)->PPBackupOnce = 0;
    PrettyPrintData(theEnv)->PPBackupTwice = 0;
    PrettyPrintData(theEnv)->PPBufferPos = 0;
    PrettyPrintData(theEnv)->PrettyPrintBuffer[0] = EOS;
    return;
}

/*********************************************************************/
/* DestroyPPBuffer: Resets and removes the pretty print save buffer. */
/*********************************************************************/
void DestroyPPBuffer(
        const Environment&theEnv) {
    PrettyPrintData(theEnv)->PPBackupOnce = 0;
    PrettyPrintData(theEnv)->PPBackupTwice = 0;
    PrettyPrintData(theEnv)->PPBufferPos = 0;
    if (PrettyPrintData(theEnv)->PrettyPrintBuffer != nullptr)
        rm(theEnv, PrettyPrintData(theEnv)->PrettyPrintBuffer, PrettyPrintData(theEnv)->PPBufferMax);
    PrettyPrintData(theEnv)->PrettyPrintBuffer = nullptr;
    PrettyPrintData(theEnv)->PPBufferMax = 0;
}

/*********************************************/
/* SavePPBuffer: Appends a string to the end */
/*   of the pretty print save buffer.        */
/*********************************************/
void SavePPBuffer(
        const Environment&theEnv,
        const char *str) {
    size_t increment;

    /*==========================================*/
    /* If the pretty print buffer isn't needed, */
    /* then don't bother writing to it.         */
    /*==========================================*/

    if (!PrettyPrintData(theEnv)->PPBufferStatus || (!PrettyPrintData(theEnv)->PPBufferEnabled)) { return; }

    /*===============================*/
    /* Determine the increment size. */
    /*===============================*/

    increment = 512;
    if (PrettyPrintData(theEnv)->PPBufferPos > increment) { increment = PrettyPrintData(theEnv)->PPBufferPos * 3; }

    /*================================================*/
    /* If the pretty print buffer isn't big enough to */
    /* contain the string, then increase its size.    */
    /*================================================*/

    if (strlen(str) + PrettyPrintData(theEnv)->PPBufferPos + 1 >= PrettyPrintData(theEnv)->PPBufferMax) {
        PrettyPrintData(theEnv)->PrettyPrintBuffer =
                (char *) genrealloc(theEnv, PrettyPrintData(theEnv)->PrettyPrintBuffer,
                                    PrettyPrintData(theEnv)->PPBufferMax,
                                    PrettyPrintData(theEnv)->PPBufferMax + increment);
        PrettyPrintData(theEnv)->PPBufferMax += increment;
    }

    /*==================================================*/
    /* Remember the previous tokens saved to the pretty */
    /* print buffer in case it is necessary to back up. */
    /*==================================================*/

    PrettyPrintData(theEnv)->PPBackupTwice = PrettyPrintData(theEnv)->PPBackupOnce;
    PrettyPrintData(theEnv)->PPBackupOnce = PrettyPrintData(theEnv)->PPBufferPos;

    /*=============================================*/
    /* Save the string to the pretty print buffer. */
    /*=============================================*/

    PrettyPrintData(theEnv)->PrettyPrintBuffer = AppendToString(theEnv, str, PrettyPrintData(theEnv)->PrettyPrintBuffer,
                                                                &PrettyPrintData(theEnv)->PPBufferPos,
                                                                &PrettyPrintData(theEnv)->PPBufferMax);
}

/***************************************************/
/* PPBackup:  Removes the last string added to the */
/*   pretty print save buffer. Only capable of     */
/*   backing up for the two most recent additions. */
/***************************************************/
void PPBackup(
        const Environment&theEnv) {
    if (!PrettyPrintData(theEnv)->PPBufferStatus ||
        (PrettyPrintData(theEnv)->PrettyPrintBuffer == nullptr) ||
        (!PrettyPrintData(theEnv)->PPBufferEnabled)) { return; }

    PrettyPrintData(theEnv)->PPBufferPos = PrettyPrintData(theEnv)->PPBackupOnce;
    PrettyPrintData(theEnv)->PPBackupOnce = PrettyPrintData(theEnv)->PPBackupTwice;
    PrettyPrintData(theEnv)->PrettyPrintBuffer[PrettyPrintData(theEnv)->PPBufferPos] = EOS;
}

/**************************************************/
/* CopyPPBuffer: Makes a copy of the pretty print */
/*   save buffer.                                 */
/**************************************************/
char *CopyPPBuffer(
        const Environment&theEnv) {
    size_t length;
    char *newString;
    char *theBuffer = PrettyPrintData(theEnv)->PrettyPrintBuffer;

    if (theBuffer == nullptr) return nullptr;

    length = (1 + strlen(theBuffer)) * sizeof(char);
    newString = (char *) gm2(theEnv, length);

    genstrcpy(newString, theBuffer);
    return (newString);
}

/************************************************************/
/* GetPPBuffer: Returns a pointer to the PrettyPrintBuffer. */
/************************************************************/
char *GetPPBuffer(
        const Environment&theEnv) {
    return (PrettyPrintData(theEnv)->PrettyPrintBuffer);
}

/*******************************************/
/* PPCRAndIndent: Prints white spaces into */
/*   the pretty print buffer.              */
/*******************************************/
void PPCRAndIndent(
        const Environment&theEnv) {
    size_t i;
    char *buffer;
    char fixedBuffer[PP_CR_FIXED_BUFFER_SIZE];

    if (!PrettyPrintData(theEnv)->PPBufferStatus ||
        (!PrettyPrintData(theEnv)->PPBufferEnabled)) { return; }

    if ((PrettyPrintData(theEnv)->IndentationDepth + 2) > PP_CR_FIXED_BUFFER_SIZE) {
        buffer = (char *) genalloc(theEnv, PrettyPrintData(theEnv)->IndentationDepth + 2);
    } else { buffer = fixedBuffer; }

    buffer[0] = '\n';

    for (i = 1; i <= PrettyPrintData(theEnv)->IndentationDepth; i++) { buffer[i] = ' '; }
    buffer[i] = EOS;

    SavePPBuffer(theEnv, buffer);

    if ((PrettyPrintData(theEnv)->IndentationDepth + 2) > PP_CR_FIXED_BUFFER_SIZE) {
        genfree(theEnv, buffer, PrettyPrintData(theEnv)->IndentationDepth + 2);
    }
}

/************************************************/
/* IncrementIndentDepth: Increments indentation */
/*   depth for pretty printing.                 */
/************************************************/
void IncrementIndentDepth(
        const Environment&theEnv,
        size_t value) {
    PrettyPrintData(theEnv)->IndentationDepth += value;
}

/************************************************/
/* DecrementIndentDepth: Decrements indentation */
/*   depth for pretty printing.                 */
/************************************************/
void DecrementIndentDepth(
        const Environment&theEnv,
        size_t value) {
    PrettyPrintData(theEnv)->IndentationDepth -= value;
}

/************************************/
/* SetIndentDepth: Sets indentation */
/*   depth for pretty printing.     */
/************************************/
void SetIndentDepth(
        const Environment&theEnv,
        size_t value) {
    PrettyPrintData(theEnv)->IndentationDepth = value;
}

/******************************************/
/* SetPPBufferStatus: Sets PPBufferStatus */
/*   flag to boolean value of ON or OFF.  */
/******************************************/
void SetPPBufferStatus(
        const Environment&theEnv,
        bool value) {
    PrettyPrintData(theEnv)->PPBufferStatus = value;
}

/************************************/
/* GetPPBufferStatus: Returns value */
/*   of the PPBufferStatus flag.    */
/************************************/
bool GetPPBufferStatus(
        const Environment&theEnv) {
    return (PrettyPrintData(theEnv)->PPBufferStatus);
}

/***********************/
/* SetPPBufferEnabled: */
/***********************/
bool SetPPBufferEnabled(
        const Environment&theEnv,
        bool value) {
    bool oldValue;

    oldValue = PrettyPrintData(theEnv)->PPBufferEnabled;
    PrettyPrintData(theEnv)->PPBufferEnabled = value;
    return oldValue;
}

/***********************/
/* GetPPBufferEnabled: */
/***********************/
bool GetPPBufferEnabled(
        const Environment&theEnv) {
    return PrettyPrintData(theEnv)->PPBufferEnabled;
}

