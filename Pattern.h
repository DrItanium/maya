/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*                PATTERN HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides the mechanism for recognizing and       */
/*   parsing the various types of patterns that can be used  */
/*   in the LHS of a rule. In version 6.0, the only pattern  */
/*   types provided are for deftemplate and instance         */
/*   patterns.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
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
/*            Removed initial-fact and initial-object        */
/*            support.                                       */
/*                                                           */
/*************************************************************/

#ifndef _H_pattern

#pragma once

#define _H_pattern

#include <cstdio>

#include "Evaluation.h"
#include "Entities.h"

#include "Symbol.h"
#include "Scanner.h"
#include "Expression.h"
#include "Match.h"
#include "Reorder.h"
#include "Constraint.h"

constexpr auto MAXIMUM_NUMBER_OF_PATTERNS = 128;

struct patternParser {
    const char *name;
    patternEntityRecord *entityType;
    unsigned short positionInArray;
    bool (*recognizeFunction)(CLIPSLexeme *);
    lhsParseNode *(*parseFunction)(Environment *, const char *, token *);
    bool (*postAnalysisFunction)(Environment *, lhsParseNode *);
    patternNodeHeader *(*addPatternFunction)(Environment *, lhsParseNode *);
    void (*removePatternFunction)(Environment *, patternNodeHeader *);
    expr *(*genJNConstantFunction)(void *, lhsParseNode *, int);
    void (*replaceGetJNValueFunction)(Environment *, expr *, lhsParseNode *, int);
    expr *(*genGetJNValueFunction)(Environment *, lhsParseNode *, int);
    expr *(*genCompareJNValuesFunction)(Environment *, lhsParseNode *, lhsParseNode *, bool);
    expr *(*genPNConstantFunction)(Environment *, lhsParseNode *);
    void (*replaceGetPNValueFunction)(Environment *, expr *, lhsParseNode *);
    expr *(*genGetPNValueFunction)(Environment *, lhsParseNode *);
    expr *(*genComparePNValuesFunction)(Environment *, lhsParseNode *, lhsParseNode *);
    void (*returnUserDataFunction)(Environment *, void *);
    void *(*copyUserDataFunction)(Environment *, void *);
    void (*markIRPatternFunction)(Environment *, patternNodeHeader *, bool);
    void (*incrementalResetFunction)(Environment *);
    void (*codeReferenceFunction)(Environment *, void *, FILE *, unsigned int, unsigned int);
    int priority;
    patternParser *next;
};

struct reservedSymbol {
    const char *theSymbol;
    const char *reservedBy;
    reservedSymbol *next;
};

constexpr auto MAX_POSITIONS = 8;
constexpr auto PATTERN_DATA = 19;

struct patternData {
    patternParser *ListOfPatternParsers;
    patternParser *PatternParserArray[MAX_POSITIONS];
    unsigned short NextPosition;
    reservedSymbol *ListOfReservedPatternSymbols;
    bool WithinNotCE;
    int GlobalSalience;
    bool GlobalAutoFocus;
    expr *SalienceExpression;
    patternNodeHashEntry **PatternHashTable;
    unsigned long PatternHashTableSize;
};

#define PatternData(theEnv) ((patternData *) GetEnvironmentData(theEnv,PATTERN_DATA))

void InitializePatterns(Environment *);
bool AddPatternParser(Environment *, struct patternParser *);
struct patternParser *FindPatternParser(Environment *, const char *);
void DetachPattern(Environment *, unsigned short, struct patternNodeHeader *);
void GetNextPatternEntity(Environment *,
                          struct patternParser **,
                          struct patternEntity **);
struct patternParser *GetPatternParser(Environment *, unsigned short);
struct lhsParseNode *RestrictionParse(Environment *, const char *, struct token *, bool,
                                      CLIPSLexeme *, unsigned short,
                                      struct constraintRecord *, unsigned short);
bool PostPatternAnalysis(Environment *, struct lhsParseNode *);
void PatternNodeHeaderToCode(Environment *, FILE *, struct patternNodeHeader *,
                             unsigned int, unsigned int);
void AddReservedPatternSymbol(Environment *, const char *, const char *);
bool ReservedPatternSymbol(Environment *, const char *, const char *);
void ReservedPatternSymbolErrorMsg(Environment *, const char *, const char *);
void AddHashedPatternNode(Environment *, void *, void *, unsigned short, void *);
bool RemoveHashedPatternNode(Environment *, void *, void *, unsigned short, void *);
void *FindHashedPatternNode(Environment *, void *, unsigned short, void *);

#endif /* _H_pattern */









