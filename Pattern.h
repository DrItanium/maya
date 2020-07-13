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
    lhsParseNode *(*parseFunction)(const Environment::Ptr&, const char *, token *);
    bool (*postAnalysisFunction)(const Environment::Ptr&, lhsParseNode *);
    PatternNodeHeader *(*addPatternFunction)(const Environment::Ptr&, lhsParseNode *);
    void (*removePatternFunction)(const Environment::Ptr&, PatternNodeHeader *);
    Expression *(*genJNConstantFunction)(void *, lhsParseNode *, int);
    void (*replaceGetJNValueFunction)(const Environment::Ptr&, Expression *, lhsParseNode *, int);
    Expression *(*genGetJNValueFunction)(const Environment::Ptr&, lhsParseNode *, int);
    Expression *(*genCompareJNValuesFunction)(const Environment::Ptr&, lhsParseNode *, lhsParseNode *, bool);
    Expression *(*genPNConstantFunction)(const Environment::Ptr&, lhsParseNode *);
    void (*replaceGetPNValueFunction)(const Environment::Ptr&, Expression *, lhsParseNode *);
    Expression *(*genGetPNValueFunction)(const Environment::Ptr&, lhsParseNode *);
    Expression *(*genComparePNValuesFunction)(const Environment::Ptr&, lhsParseNode *, lhsParseNode *);
    void (*returnUserDataFunction)(const Environment::Ptr&, void *);
    void *(*copyUserDataFunction)(const Environment::Ptr&, void *);
    void (*markIRPatternFunction)(const Environment::Ptr&, PatternNodeHeader *, bool);
    void (*incrementalResetFunction)(const Environment::Ptr&);
    void (*codeReferenceFunction)(const Environment::Ptr&, void *, FILE *, unsigned int, unsigned int);
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

void InitializePatterns(const Environment::Ptr&);
bool AddPatternParser(const Environment::Ptr&, struct patternParser *);
struct patternParser *FindPatternParser(const Environment::Ptr&, const char *);
void DetachPattern(const Environment::Ptr&, unsigned short, PatternNodeHeader *);
void GetNextPatternEntity(const Environment::Ptr&,
                          struct patternParser **,
                          PatternEntity **);
struct patternParser *GetPatternParser(const Environment::Ptr&, unsigned short);
struct lhsParseNode *RestrictionParse(const Environment::Ptr&, const char *, struct token *, bool,
                                      CLIPSLexeme *, unsigned short,
                                      struct constraintRecord *, unsigned short);
bool PostPatternAnalysis(const Environment::Ptr&, struct lhsParseNode *);
void PatternNodeHeaderToCode(const Environment::Ptr&, FILE *, PatternNodeHeader *,
                             unsigned int, unsigned int);
void AddReservedPatternSymbol(const Environment::Ptr&, const char *, const char *);
bool ReservedPatternSymbol(const Environment::Ptr&, const char *, const char *);
void ReservedPatternSymbolErrorMsg(const Environment::Ptr&, const char *, const char *);
void AddHashedPatternNode(const Environment::Ptr&, void *, void *, unsigned short, void *);
bool RemoveHashedPatternNode(const Environment::Ptr&, void *, void *, unsigned short, void *);
void *FindHashedPatternNode(const Environment::Ptr&, void *, unsigned short, void *);

#endif /* _H_pattern */









