/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/13/17             */
/*                                                     */
/*               PARSING FUNCTIONS MODULE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several parsing related    */
/*   functions including...                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed function declaration issue when          */
/*            BLOAD_ONLY compiler flag is set to 1.          */
/*                                                           */
/*      6.40: Changed check-syntax router name because of    */
/*            a conflict with another error-capture router.  */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <cstring>

#include "ArgumentAccess.h"
#include "Construct.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "PrettyPrint.h"
#include "ProceduralFunctionsParser.h"
#include "PrintUtility.h"
#include "Router.h"
#include "StringRouter.h"
#include "Utility.h"

#include "ParsingFunctions.h"

constexpr auto PARSEFUN_DATA = 11;

struct parseFunctionData {
    char *ErrorString;
    size_t ErrorCurrentPosition;
    size_t ErrorMaximumPosition;
    char *WarningString;
    size_t WarningCurrentPosition;
    size_t WarningMaximumPosition;
};

#define ParseFunctionData(theEnv) ((parseFunctionData *) GetEnvironmentData(theEnv,PARSEFUN_DATA))

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static bool QueryErrorCaptureCallback(Environment *, const char *, void *);
static void WriteErrorCaptureCallback(Environment *, const char *, const char *, void *);
static void DeactivateErrorCapture(Environment *);
static void SetErrorCaptureValues(Environment *, UDFValue *);

/*****************************************/
/* ParseFunctionDefinitions: Initializes */
/*   the parsing related functions.      */
/*****************************************/
void ParseFunctionDefinitions(
        Environment *theEnv) {
    AllocateEnvironmentData(theEnv, PARSEFUN_DATA, sizeof(parseFunctionData));

    AddUDF(theEnv, "check-syntax", "ym", 1, 1, "s", CheckSyntaxFunction);
}

/*******************************************/
/* CheckSyntaxFunction: H/L access routine */
/*   for the check-syntax function.        */
/*******************************************/
void CheckSyntaxFunction(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theArg;

    /*========================================*/
    /* The argument should be of type STRING_TYPE. */
    /*========================================*/

    if (!UDFFirstArgument(context, STRING_BIT, &theArg)) { return; }

    /*===================*/
    /* Check the syntax. */
    /*===================*/

    CheckSyntax(theEnv, theArg.lexemeValue->contents, returnValue);
}

/*********************************/
/* CheckSyntax: C access routine */
/*   for the build function.     */
/*********************************/
bool CheckSyntax(
        Environment *theEnv,
        const char *theString,
        UDFValue *returnValue) {
    struct token theToken;
    struct expr *top;

    /*==============================*/
    /* Set the default return value */
    /* (TRUE for problems found).   */
    /*==============================*/

    returnValue->lexemeValue = TrueSymbol(theEnv);

    /*===========================================*/
    /* Create a string source router so that the */
    /* string can be used as an input source.    */
    /*===========================================*/

    if (OpenStringSource(theEnv, "check-syntax", theString, 0) == 0) { return true; }

    /*=================================*/
    /* Only expressions and constructs */
    /* can have their syntax checked.  */
    /*=================================*/

    GetToken(theEnv, "check-syntax", &theToken);

    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        CloseStringSource(theEnv, "check-syntax");
        returnValue->lexemeValue = CreateSymbol(theEnv, "MISSING-LEFT-PARENTHESIS");
        return true;
    }

    /*========================================*/
    /* The next token should be the construct */
    /* type or function name.                 */
    /*========================================*/

    GetToken(theEnv, "check-syntax", &theToken);
    if (theToken.tknType != SYMBOL_TOKEN) {
        CloseStringSource(theEnv, "check-syntax");
        returnValue->lexemeValue = CreateSymbol(theEnv, "EXPECTED-SYMBOL_TYPE-AFTER-LEFT-PARENTHESIS");
        return true;
    }

    auto name = theToken.lexemeValue->contents;

    /*==============================================*/
    /* Set up a router to capture the error output. */
    /*==============================================*/

    AddRouter(theEnv, "cs-error-capture", 40,
              QueryErrorCaptureCallback, WriteErrorCaptureCallback,
              nullptr, nullptr, nullptr, nullptr);

    /*================================*/
    /* Determine if it's a construct. */
    /*================================*/

    if (FindConstruct(theEnv, name)) {
        ConstructData(theEnv)->CheckSyntaxMode = true;

        auto rv = ParseConstruct(theEnv, name, "check-syntax") == BE_PARSING_ERROR;

        GetToken(theEnv, "check-syntax", &theToken);
        ConstructData(theEnv)->CheckSyntaxMode = false;

        if (rv) {
            WriteString(theEnv, STDERR, "\nERROR:\n");
            WriteString(theEnv, STDERR, GetPPBuffer(theEnv));
            WriteString(theEnv, STDERR, "\n");
        }

        DestroyPPBuffer(theEnv);

        CloseStringSource(theEnv, "check-syntax");

        if (rv || (ParseFunctionData(theEnv)->WarningString != nullptr)) {
            SetErrorCaptureValues(theEnv, returnValue);
            DeactivateErrorCapture(theEnv);
            return true;
        }

        if (theToken.tknType != STOP_TOKEN) {
            returnValue->value = CreateSymbol(theEnv, "EXTRANEOUS-INPUT-AFTER-LAST-PARENTHESIS");
            DeactivateErrorCapture(theEnv);
            return true;
        }

        returnValue->lexemeValue = FalseSymbol(theEnv);
        DeactivateErrorCapture(theEnv);
        return false;
    }

    /*=======================*/
    /* Parse the expression. */
    /*=======================*/

    top = Function2Parse(theEnv, "check-syntax", name);
    GetToken(theEnv, "check-syntax", &theToken);
    ClearParsedBindNames(theEnv);
    CloseStringSource(theEnv, "check-syntax");

    if (top == nullptr) {
        SetErrorCaptureValues(theEnv, returnValue);
        DeactivateErrorCapture(theEnv);
        return true;
    }

    if (theToken.tknType != STOP_TOKEN) {
        returnValue->lexemeValue = CreateSymbol(theEnv, "EXTRANEOUS-INPUT-AFTER-LAST-PARENTHESIS");
        DeactivateErrorCapture(theEnv);
        ReturnExpression(theEnv, top);
        return true;
    }

    DeactivateErrorCapture(theEnv);

    ReturnExpression(theEnv, top);
    returnValue->lexemeValue = FalseSymbol(theEnv);
    return false;
}

/**************************************************/
/* DeactivateErrorCapture: Deactivates the error  */
/*   capture router and the strings used to store */
/*   the captured information.                    */
/**************************************************/
static void DeactivateErrorCapture(
        Environment *theEnv) {
    if (ParseFunctionData(theEnv)->ErrorString != nullptr) {
        rm(theEnv, ParseFunctionData(theEnv)->ErrorString, ParseFunctionData(theEnv)->ErrorMaximumPosition);
        ParseFunctionData(theEnv)->ErrorString = nullptr;
    }

    if (ParseFunctionData(theEnv)->WarningString != nullptr) {
        rm(theEnv, ParseFunctionData(theEnv)->WarningString, ParseFunctionData(theEnv)->WarningMaximumPosition);
        ParseFunctionData(theEnv)->WarningString = nullptr;
    }

    ParseFunctionData(theEnv)->ErrorCurrentPosition = 0;
    ParseFunctionData(theEnv)->ErrorMaximumPosition = 0;
    ParseFunctionData(theEnv)->WarningCurrentPosition = 0;
    ParseFunctionData(theEnv)->WarningMaximumPosition = 0;

    DeleteRouter(theEnv, "cs-error-capture");
}

/*******************************************************************/
/* SetErrorCaptureValues: Stores the error/warnings captured when  */
/*   parsing an expression or construct into a multifield value.   */
/*   The first field contains the output sent to the STDERR        */
/*   logical name and the second field contains the output sent    */
/*   to the STDWRN logical name. The symbol FALSE is stored in   */
/*   either position if no output was sent to those logical names. */
/*******************************************************************/
static void SetErrorCaptureValues(
        Environment *theEnv,
        UDFValue *returnValue) {
    Multifield *theMultifield;

    theMultifield = CreateMultifield(theEnv, 2L);

    if (ParseFunctionData(theEnv)->ErrorString != nullptr) {
        theMultifield->contents[0].lexemeValue = CreateString(theEnv, ParseFunctionData(
                theEnv)->ErrorString);
    } else { theMultifield->contents[0].lexemeValue = FalseSymbol(theEnv); }

    if (ParseFunctionData(theEnv)->WarningString != nullptr) {
        theMultifield->contents[1].lexemeValue = CreateString(theEnv, ParseFunctionData(
                theEnv)->WarningString);
    } else { theMultifield->contents[1].lexemeValue = FalseSymbol(theEnv); }

    returnValue->begin = 0;
    returnValue->range = 2;
    returnValue->value = theMultifield;
}

/*********************************************/
/* QueryErrorCaptureCallback: Query callback */
/*   for the check-syntax router.            */
/*********************************************/
static bool QueryErrorCaptureCallback(
        Environment *theEnv,
        const char *logicalName,
        void *context) {
#if MAC_XCD
#pragma unused(theEnv,context)
#endif

    return (strcmp(logicalName, STDERR) == 0) ||
           (strcmp(logicalName, STDWRN) == 0);

}

/*********************************************/
/* WriteErrorCaptureCallback: Write callback */
/*   for the check-syntax router.            */
/*********************************************/
static void WriteErrorCaptureCallback(
        Environment *theEnv,
        const char *logicalName,
        const char *str,
        void *context) {
    if (strcmp(logicalName, STDERR) == 0) {
        ParseFunctionData(theEnv)->ErrorString = AppendToString(theEnv, str, ParseFunctionData(theEnv)->ErrorString,
                                                                &ParseFunctionData(theEnv)->ErrorCurrentPosition,
                                                                &ParseFunctionData(theEnv)->ErrorMaximumPosition);
    } else if (strcmp(logicalName, STDWRN) == 0) {
        ParseFunctionData(theEnv)->WarningString = AppendToString(theEnv, str, ParseFunctionData(theEnv)->WarningString,
                                                                  &ParseFunctionData(theEnv)->WarningCurrentPosition,
                                                                  &ParseFunctionData(theEnv)->WarningMaximumPosition);
    }
}



