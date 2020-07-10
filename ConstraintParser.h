/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*             CONSTRAINT PARSER HEADER FILE           */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing constraint        */
/*   declarations.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Used gensprintf instead of sprintf.            */
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

#ifndef _H_cstrnpsr

#pragma once

#define _H_cstrnpsr

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

#endif /* _H_cstrnpsr */



