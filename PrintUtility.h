/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  01/29/18            */
/*                                                     */
/*              PRINT UTILITY HEADER FILE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for printing various items      */
/*   and messages.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Link error occurs for the SlotExistError       */
/*            function when OBJECT_SYSTEM is set to 0 in     */
/*            setup.h. DR0865                                */
/*                                                           */
/*            Added DataObjectToString function.             */
/*                                                           */
/*            Added SlotExistError function.                 */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Support for DATA_OBJECT_ARRAY primitive.       */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS_TYPE.       */
/*                                                           */
/*            Used gensprintf and genstrcat instead of       */
/*            sprintf and strcat.                            */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added code for capturing errors/warnings.      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.31: Added additional error messages for retracted  */
/*            facts, deleted instances, and invalid slots.   */
/*                                                           */
/*            Added under/overflow error message.            */
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
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*************************************************************/

#ifndef _H_prntutil

#pragma once

#define _H_prntutil

#include <cstdio>

#include "Entities.h"

constexpr auto PRINT_UTILITY_DATA = 53;

struct printUtilityData : public EnvironmentModule {
    bool PreserveEscapedCharacters;
    bool AddressesToStrings;
    bool InstanceAddressesToNames;
};
RegisterEnvironmentModule(printUtilityData, PRINT_UTILITY_DATA, PrintUtility);

void InitializePrintUtilityData(const Environment&);
void WriteFloat(const Environment&, const char *, double);
void WriteInteger(const Environment&, const char *, long long);
void PrintUnsignedInteger(const Environment&, const char *, unsigned long long);
void PrintAtom(const Environment&, const char *, unsigned short, void *);
void PrintTally(const Environment&, const char *, unsigned long long, const char *, const char *);
std::string FloatToString(const Environment& theEnv, double value);
std::string LongIntegerToString(const Environment& theEnv, long long value);
std::string DataObjectToString(const Environment& theEnv, UDFValue * value);
void SyntaxErrorMessage(const Environment&, const char *);
void SystemError(const Environment&, const char *, int);
void PrintErrorID(const Environment&, const char *, int, bool);
void PrintWarningID(const Environment&, const char *, int, bool);
void CantFindItemErrorMessage(const Environment&, const char *, const char *, bool);
void CantDeleteItemErrorMessage(const Environment&, const char *, const char *);
void AlreadyParsedErrorMessage(const Environment&, const char *, const char *);
void LocalVariableErrorMessage(const Environment&, const char *);
void DivideByZeroErrorMessage(const Environment&, const char *);
void SalienceInformationError(const Environment&, const char *, const char *);
void SalienceRangeError(const Environment&, int, int);
void SalienceNonIntegerError(const Environment&);
void CantFindItemInFunctionErrorMessage(const Environment&, const char *, const char *, const char *, bool);
void SlotExistError(const Environment&, const char *, const char *);
void FactRetractedErrorMessage(const Environment&, Fact *);
void FactVarSlotErrorMessage1(const Environment&, Fact *, const char *);
void FactVarSlotErrorMessage2(const Environment&, Fact *, const char *);
void InvalidVarSlotErrorMessage(const Environment&, const char *);
void InstanceVarSlotErrorMessage1(const Environment&, Instance *, const char *);
void InstanceVarSlotErrorMessage2(const Environment&, Instance *, const char *);
void ArgumentOverUnderflowErrorMessage(const Environment&, const char *, bool);

#endif /* _H_prntutil */






