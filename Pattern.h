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
    PatternEntityRecord *entityType;
    unsigned short positionInArray;
    bool (*recognizeFunction)(CLIPSLexeme *);
    lhsParseNode *(*parseFunction)(const Environment&, const char *, token *);
    bool (*postAnalysisFunction)(const Environment&, lhsParseNode *);
    PatternNodeHeader *(*addPatternFunction)(const Environment&, lhsParseNode *);
    void (*removePatternFunction)(const Environment&, PatternNodeHeader *);
    Expression *(*genJNConstantFunction)(void *, lhsParseNode *, int);
    void (*replaceGetJNValueFunction)(const Environment&, Expression *, lhsParseNode *, int);
    Expression *(*genGetJNValueFunction)(const Environment&, lhsParseNode *, int);
    Expression *(*genCompareJNValuesFunction)(const Environment&, lhsParseNode *, lhsParseNode *, bool);
    Expression *(*genPNConstantFunction)(const Environment&, lhsParseNode *);
    void (*replaceGetPNValueFunction)(const Environment&, Expression *, lhsParseNode *);
    Expression *(*genGetPNValueFunction)(const Environment&, lhsParseNode *);
    Expression *(*genComparePNValuesFunction)(const Environment&, lhsParseNode *, lhsParseNode *);
    void (*returnUserDataFunction)(const Environment&, void *);
    void *(*copyUserDataFunction)(const Environment&, void *);
    void (*markIRPatternFunction)(const Environment&, PatternNodeHeader *, bool);
    void (*incrementalResetFunction)(const Environment&);
    void (*codeReferenceFunction)(const Environment&, void *, FILE *, unsigned int, unsigned int);
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

struct patternData : public EnvironmentModule {
    patternParser *ListOfPatternParsers;
    patternParser *PatternParserArray[MAX_POSITIONS];
    unsigned short NextPosition;
    reservedSymbol *ListOfReservedPatternSymbols;
    bool WithinNotCE;
    int GlobalSalience;
    bool GlobalAutoFocus;
    Expression *SalienceExpression;
    patternNodeHashEntry **PatternHashTable;
    unsigned long PatternHashTableSize;
};
RegisterEnvironmentModule(patternData, PATTERN_DATA, Pattern);

void InitializePatterns(const Environment&);
bool AddPatternParser(const Environment&, struct patternParser *);
struct patternParser *FindPatternParser(const Environment&, const char *);
void DetachPattern(const Environment&, unsigned short, PatternNodeHeader *);
void GetNextPatternEntity(const Environment&,
                          struct patternParser **,
                          PatternEntity **);
struct patternParser *GetPatternParser(const Environment&, unsigned short);
struct lhsParseNode *RestrictionParse(const Environment&, const char *, struct token *, bool,
                                      CLIPSLexeme *, unsigned short,
                                      struct constraintRecord *, unsigned short);
bool PostPatternAnalysis(const Environment&, struct lhsParseNode *);
void PatternNodeHeaderToCode(const Environment&, FILE *, PatternNodeHeader *,
                             unsigned int, unsigned int);
void AddReservedPatternSymbol(const Environment&, const char *, const char *);
bool ReservedPatternSymbol(const Environment&, const char *, const char *);
void ReservedPatternSymbolErrorMsg(const Environment&, const char *, const char *);
void AddHashedPatternNode(const Environment&, void *, void *, unsigned short, void *);
bool RemoveHashedPatternNode(const Environment&, void *, void *, unsigned short, void *);
void *FindHashedPatternNode(const Environment&, void *, unsigned short, void *);

#endif /* _H_pattern */









