/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  12/02/19            */
/*                                                     */
/*                CONSTRAINT HEADER FILE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for creating and removing     */
/*   constraint records, adding them to the contraint hash   */
/*   table, and enabling and disabling static and dynamic    */
/*   constraint checking.                                    */
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
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*            Static constraint checking is always enabled.  */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#ifndef _H_constrnt

#pragma once

#define _H_constrnt

struct constraintRecord;
typedef struct constraintRecord CONSTRAINT_RECORD;

#include "Evaluation.h"

struct constraintRecord {
private:
    bool anyAllowed: 1;
public:
    bool symbolsAllowed: 1;
    bool stringsAllowed: 1;
    bool floatsAllowed: 1;
    bool integersAllowed: 1;
    bool instanceNamesAllowed: 1;
    bool instanceAddressesAllowed: 1;
    bool externalAddressesAllowed: 1;
    bool factAddressesAllowed: 1;
    bool voidAllowed: 1;
    bool anyRestriction: 1;
    bool symbolRestriction: 1;
    bool stringRestriction: 1;
    bool floatRestriction: 1;
    bool integerRestriction: 1;
    bool classRestriction: 1;
    bool instanceNameRestriction: 1;
    bool multifieldsAllowed: 1;
    bool singlefieldsAllowed: 1;
    bool installed: 1;
#define X(field,form) \
    constexpr auto get ## form () const noexcept { return field ;} \
    void set ## form (bool value) noexcept { field = value ; }
    X(anyAllowed, AnyAllowed);
#undef X
    unsigned long bsaveID;
    expr *classList;
    expr *restrictionList;
    expr *minValue;
    expr *maxValue;
    expr *minFields;
    expr *maxFields;
private:
    constraintRecord *multifield;
    constraintRecord *next;
    unsigned int bucket;
    unsigned int count;
public:
    auto getMultifield() const noexcept { return multifield; }
    void setMultifield(constraintRecord* value) noexcept { multifield = value; }
    auto getNext() const noexcept { return next; }
    void setNext(constraintRecord* value) noexcept { next = value; }
    constexpr auto getBucket() const noexcept { return bucket; }
    void setBucket(unsigned int value) noexcept { bucket = value; }
    constexpr auto getCount() const noexcept { return count; }
    void setCount(unsigned int value) noexcept { count = value; }
    void decrementCount() noexcept { --count; }
    void incrementCount() noexcept { ++count; }
};

constexpr auto SIZE_CONSTRAINT_HASH  = 167;
constexpr auto CONSTRAINT_DATA = 43;

struct constraintData {
    struct constraintRecord **ConstraintHashtable;
    bool DynamicConstraintChecking;
#if (BLOAD_AND_BSAVE)
    struct constraintRecord *ConstraintArray;
    unsigned long NumberOfConstraints;
#endif
};

#define ConstraintData(theEnv) ((constraintData *) GetEnvironmentData(theEnv,CONSTRAINT_DATA))

void InitializeConstraints(Environment *);
void GDCCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void SDCCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
bool SetDynamicConstraintChecking(Environment *, bool);
bool GetDynamicConstraintChecking(Environment *);
unsigned long HashConstraint(constraintRecord *);
struct constraintRecord *AddConstraint(Environment *, struct constraintRecord *);
void RemoveConstraint(Environment *, struct constraintRecord *);

#endif




