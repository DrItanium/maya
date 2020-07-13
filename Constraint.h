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
#include "Constraint.h"

struct constraintRecord {
private:
    bool anyAllowed: 1;
    bool symbolsAllowed: 1;
    bool stringsAllowed: 1;
public:
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
private:
    unsigned long bsaveID;
    Expression *classList;
    Expression *restrictionList;
    Expression *minValue;
    Expression *maxValue;
    Expression *minFields;
    Expression *maxFields;
    constraintRecord *multifield;
    constraintRecord *next;
    unsigned int bucket;
    unsigned int count;
public:
#define X(field,form) \
    constexpr auto get ## form () const noexcept { return field ;} \
    void set ## form (bool value) noexcept { field = value ; }
#define Y(field, form) X(field ## Allowed, form ## Allowed)
#define Z(field, form) X(field ## Restriction, form ## Restriction)
    Y(any, Any);
    Y(symbols, Symbols);
    Y(strings, Strings);
    Y(floats, Floats);
    Y(integers, Integers);
    Y(instanceNames, InstanceNames);
    Y(instanceAddresses, InstanceAddresses);
    Y(externalAddresses, ExternalAddresses);
    Y(factAddresses, FactAddresses);
    Y(void, Void);
    Z(any, Any);
    Z(symbol, Symbol);
    Z(string, String);
    Z(float, Float);
    Z(integer, Integer);
    Z(class, Class);
    Z(instanceName, InstanceName);
    Y(multifields, Multifields);
    Y(singlefields, Singlefields);
#undef Z
#undef Y
#undef X
    constexpr auto getBSaveID() const noexcept { return bsaveID; }
    void setBSaveID(unsigned long value) noexcept { bsaveID = value; }
#define X(field, form) \
    auto get ## form () const noexcept { return field ; } \
    void set ## form (Expression* value) noexcept { field = value ; }
    X(maxFields, MaxFields);
    X(minFields, MinFields);
    X(maxValue, MaxValue);
    X(minValue, MinValue);
    X(restrictionList, RestrictionList);
    X(classList, ClassList);
#undef X
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

struct constraintData : public EnvironmentModule {
    struct constraintRecord **ConstraintHashtable;
    bool DynamicConstraintChecking;
#if (BLOAD_AND_BSAVE)
    struct constraintRecord *ConstraintArray;
    unsigned long NumberOfConstraints;
#endif
};

RegisterEnvironmentModule(constraintData, CONSTRAINT_DATA, Constraint);

void InitializeConstraints(const Environment&);
void GDCCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SDCCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SetDynamicConstraintChecking(const Environment&, bool);
bool GetDynamicConstraintChecking(const Environment&);
unsigned long HashConstraint(constraintRecord *);
struct constraintRecord *AddConstraint(const Environment&, struct constraintRecord *);
void RemoveConstraint(const Environment&, struct constraintRecord *);


#define ConstraintIndex(theConstraint) (((! GetDynamicConstraintChecking(theEnv)) || (theConstraint == nullptr)) ? ULONG_MAX : (theConstraint->getBSaveID()))
#define ConstraintPointer(i) (((i) == ULONG_MAX) ? nullptr : (CONSTRAINT_RECORD *) &ConstraintData(theEnv)->ConstraintArray[i])

#if BLOAD_AND_BSAVE
void WriteNeededConstraints(const Environment&, FILE *);
#endif
void ReadNeededConstraints(const Environment&);
void ClearBloadedConstraints(const Environment&);

#include "Constraint.h"
#include "Evaluation.h"

enum ConstraintViolationType {
    NO_VIOLATION,
    TYPE_VIOLATION,
    RANGE_VIOLATION,
    ALLOWED_VALUES_VIOLATION,
    FUNCTION_RETURN_TYPE_VIOLATION,
    CARDINALITY_VIOLATION,
    ALLOWED_CLASSES_VIOLATION
};

bool CheckCardinalityConstraint(const Environment&, size_t, CONSTRAINT_RECORD *);
bool CheckAllowedValuesConstraint(int, void *, CONSTRAINT_RECORD *);
bool CheckAllowedClassesConstraint(const Environment&, int, void *, CONSTRAINT_RECORD *);
ConstraintViolationType ConstraintCheckExpressionChain(const Environment&, Expression *,
                                                       CONSTRAINT_RECORD *);
void ConstraintViolationErrorMessage(const Environment&, const char *, const char *, bool,
                                     unsigned short, CLIPSLexeme *, unsigned short,
                                     ConstraintViolationType, CONSTRAINT_RECORD *, bool);
ConstraintViolationType ConstraintCheckValue(const Environment&, int, void *, CONSTRAINT_RECORD *);
ConstraintViolationType ConstraintCheckDataObject(const Environment&, UDFValue *, CONSTRAINT_RECORD *);
ConstraintViolationType ConstraintCheckExpression(const Environment&, Expression *,
                                                  CONSTRAINT_RECORD *);
bool UnmatchableConstraint(constraintRecord *);
#include "Evaluation.h"
#include "Constraint.h"

struct constraintRecord *IntersectConstraints(const Environment&, struct constraintRecord *, struct constraintRecord *);
struct constraintRecord *UnionConstraints(const Environment&, struct constraintRecord *, struct constraintRecord *);
void RemoveConstantFromConstraint(const Environment&, int, void *, CONSTRAINT_RECORD *);

#include "Constraint.h"

struct constraintParseRecord {
    bool type: 1;
    bool range: 1;
    bool allowedSymbols: 1;
    bool allowedStrings: 1;
    bool allowedLexemes: 1;
    bool allowedFloats: 1;
    bool allowedIntegers: 1;
    bool allowedNumbers: 1;
    bool allowedValues: 1;
    bool allowedClasses: 1;
    bool allowedInstanceNames: 1;
    bool cardinality: 1;
};

typedef struct constraintParseRecord CONSTRAINT_PARSE_RECORD;

bool CheckConstraintParseConflicts(const Environment&, CONSTRAINT_RECORD *);
void AttributeConflictErrorMessage(const Environment&, const char *, const char *);
void InitializeConstraintParseRecord(CONSTRAINT_PARSE_RECORD *);
bool StandardConstraint(const char *);
bool ParseStandardConstraint(const Environment&, const char *, const char *,
                             CONSTRAINT_RECORD *,
                             CONSTRAINT_PARSE_RECORD *,
                             bool);
void OverlayConstraint(const Environment&, CONSTRAINT_PARSE_RECORD *,
                       CONSTRAINT_RECORD *, CONSTRAINT_RECORD *);
void OverlayConstraintParseRecord(CONSTRAINT_PARSE_RECORD *,
                                  CONSTRAINT_PARSE_RECORD *);


#include "Constraint.h"

struct constraintRecord *GetConstraintRecord(const Environment&);
int CompareNumbers(const Environment&, int, void *, int, void *);
struct constraintRecord *CopyConstraintRecord(const Environment&, CONSTRAINT_RECORD *);
bool SetConstraintType(int, CONSTRAINT_RECORD *);
void SetAnyAllowedFlags(CONSTRAINT_RECORD *, bool);
void SetAnyRestrictionFlags(CONSTRAINT_RECORD *, bool);
CONSTRAINT_RECORD *FunctionCallToConstraintRecord(const Environment&, void *);
CONSTRAINT_RECORD *ExpressionToConstraintRecord(const Environment&, Expression *);
CONSTRAINT_RECORD *ArgumentTypeToConstraintRecord(const Environment&, unsigned);

#endif




