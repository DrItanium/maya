/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  06/22/18            */
/*                                                     */
/*                 REORDER HEADER FILE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines necessary for converting the   */
/*   the LHS of a rule into an appropriate form suitable for */
/*   the KB Rete topology. This includes transforming the    */
/*   LHS so there is at most one "or" CE (and this is the    */
/*   first CE of the LHS if it is used), adding initial      */
/*   patterns to the LHS (if no LHS is specified or a "test" */
/*   or "not" CE is the first pattern within an "and" CE),   */
/*   removing redundant CEs, and determining appropriate     */
/*   information on nesting for implementing joins from the  */
/*   right.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Support for join network changes.              */
/*                                                           */
/*            Changes to the algorithm for processing        */
/*            not/and CE groups.                             */
/*                                                           */
/*            Additional optimizations for combining         */
/*            conditional elements.                          */
/*                                                           */
/*            Added support for hashed alpha memories.       */
/*                                                           */
/*      6.31: Removed the marked flag used for not/and       */
/*            unification.                                   */
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

#ifndef _H_reorder

#pragma once

#define _H_reorder

struct lhsParseNode;

#include "Expression.h"
#include "Pattern.h"
#include "Defrule.h"

enum ParseNodeType {
    PATTERN_CE_NODE = 2049,
    AND_CE_NODE,
    OR_CE_NODE,
    NOT_CE_NODE,
    TEST_CE_NODE,
    NAND_CE_NODE,
    EXISTS_CE_NODE,
    FORALL_CE_NODE,
    SF_WILDCARD_NODE,
    MF_WILDCARD_NODE,
    SF_VARIABLE_NODE,
    MF_VARIABLE_NODE,
    GBL_VARIABLE_NODE,
    PREDICATE_CONSTRAINT_NODE,
    RETURN_VALUE_CONSTRAINT_NODE,
    FCALL_NODE,
    GCALL_NODE,
    PCALL_NODE,
    INTEGER_NODE,
    FLOAT_NODE,
    SYMBOL_NODE,
    STRING_NODE,
    INSTANCE_NAME_NODE,
    FACT_STORE_MULTIFIELD_NODE,
    DEFTEMPLATE_PTR_NODE,
    DEFCLASS_PTR_NODE,
    UNKNOWN_NODE
};

constexpr auto UNSPECIFIED_SLOT = USHRT_MAX;
constexpr auto NO_INDEX = USHRT_MAX;

/***********************************************************************/
/* lhsParseNode structure: Stores information about the intermediate   */
/*   parsed representation of the lhs of a rule.                       */
/***********************************************************************/
struct lhsParseNode {
    ParseNodeType pnType;
    union {
        void *value;
        CLIPSLexeme *lexemeValue;
        struct functionDefinition *functionValue;
    };
    bool negated: 1;
    bool exists: 1;
    bool existsNand: 1;
    bool logical: 1;
    bool multifieldSlot: 1;
    bool bindingVariable: 1;
    bool derivedConstraints: 1;
    bool userCE: 1;
    unsigned int whichCE: 7;
    //bool marked: 1;
    bool withinMultifieldSlot: 1;
    unsigned short multiFieldsBefore;
    unsigned short multiFieldsAfter;
    unsigned short singleFieldsBefore;
    unsigned short singleFieldsAfter;
    struct constraintRecord *constraints;
    struct lhsParseNode *referringNode;
    struct patternParser *patternType;
    short pattern;
    unsigned short index; // TBD is this 1 or 0 based?
    CLIPSLexeme *slot;
    unsigned short slotNumber; // TBD 1 or 0 based?
    int beginNandDepth;
    int endNandDepth;
    unsigned short joinDepth;
    struct expr *networkTest;
    struct expr *externalNetworkTest;
    struct expr *secondaryNetworkTest;
    struct expr *externalLeftHash;
    struct expr *externalRightHash;
    struct expr *constantSelector;
    struct expr *constantValue;
    struct expr *leftHash;
    struct expr *rightHash;
    struct expr *betaHash;
    struct lhsParseNode *expression;
    struct lhsParseNode *secondaryExpression;
    void *userData;
    struct lhsParseNode *right;
    struct lhsParseNode *bottom;
};

struct lhsParseNode *ReorderPatterns(const Environment&, struct lhsParseNode *, bool *);
struct lhsParseNode *CopyLHSParseNodes(const Environment&, struct lhsParseNode *);
void CopyLHSParseNode(const Environment&, struct lhsParseNode *, struct lhsParseNode *, bool);
struct lhsParseNode *GetLHSParseNode(const Environment&);
void ReturnLHSParseNodes(const Environment&, struct lhsParseNode *);
struct lhsParseNode *ExpressionToLHSParseNodes(const Environment&, struct expr *);
struct expr *LHSParseNodesToExpression(const Environment&, struct lhsParseNode *);
void AddInitialPatterns(const Environment&, struct lhsParseNode *);
bool IsExistsSubjoin(lhsParseNode *, int);
struct lhsParseNode *CombineLHSParseNodes(const Environment&, struct lhsParseNode *, struct lhsParseNode *);
bool ConstantNode(lhsParseNode *);
unsigned short NodeTypeToType(lhsParseNode *);
ParseNodeType TypeToNodeType(unsigned short);

#endif /* _H_reorder */





