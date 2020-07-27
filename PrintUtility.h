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

#include "PatternEntity.hxx"

constexpr auto PRINT_UTILITY_DATA = 53;

struct printUtilityData : public EnvironmentModule {
    bool PreserveEscapedCharacters;
    bool AddressesToStrings;
    bool InstanceAddressesToNames;
};
RegisterEnvironmentModule(printUtilityData, PRINT_UTILITY_DATA, PrintUtility);

void InitializePrintUtilityData(const Environment::Ptr&);
void WriteFloat(const Environment::Ptr&, const char *, double);
void WriteInteger(const Environment::Ptr&, const char *, long long);
void PrintUnsignedInteger(const Environment::Ptr&, const char *, unsigned long long);
void PrintAtom(const Environment::Ptr&, const char *, unsigned short, void *);
void PrintTally(const Environment::Ptr&, const char *, unsigned long long, const char *, const char *);
std::string FloatToString(const Environment::Ptr& theEnv, double value);
std::string LongIntegerToString(const Environment::Ptr& theEnv, long long value);
std::string DataObjectToString(const Environment::Ptr& theEnv, UDFValue * value);
void SyntaxErrorMessage(const Environment::Ptr&, const char *);
void SystemError(const Environment::Ptr&, const char *, int);
void PrintErrorID(const Environment::Ptr&, const char *, int, bool);
void PrintWarningID(const Environment::Ptr&, const char *, int, bool);
void CantFindItemErrorMessage(const Environment::Ptr&, const char *, const char *, bool);
void CantDeleteItemErrorMessage(const Environment::Ptr&, const char *, const char *);
void AlreadyParsedErrorMessage(const Environment::Ptr&, const char *, const char *);
void LocalVariableErrorMessage(const Environment::Ptr&, const char *);
void DivideByZeroErrorMessage(const Environment::Ptr&, const char *);
void SalienceInformationError(const Environment::Ptr&, const char *, const char *);
void SalienceRangeError(const Environment::Ptr&, int, int);
void SalienceNonIntegerError(const Environment::Ptr&);
void CantFindItemInFunctionErrorMessage(const Environment::Ptr&, const char *, const char *, const char *, bool);
void SlotExistError(const Environment::Ptr&, const char *, const char *);
void FactRetractedErrorMessage(const Environment::Ptr&, Fact *);
void FactVarSlotErrorMessage1(const Environment::Ptr&, Fact *, const char *);
void FactVarSlotErrorMessage2(const Environment::Ptr&, Fact *, const char *);
void InvalidVarSlotErrorMessage(const Environment::Ptr&, const char *);
void InstanceVarSlotErrorMessage1(const Environment::Ptr&, Instance *, const char *);
void InstanceVarSlotErrorMessage2(const Environment::Ptr&, Instance *, const char *);
void ArgumentOverUnderflowErrorMessage(const Environment::Ptr&, const char *, bool);

#endif /* _H_prntutil */






