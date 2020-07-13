/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/25/16             */
/*                                                     */
/*             DEFRULE LHS PARSING MODULE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of the LHS conditional       */
/*   elements of a rule.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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
/*************************************************************/

#include "Setup.h"


#include <cstdio>
#include <cstring>

#include "Agenda.h"
#include "ArgumentAccess.h"
#include "Constants.h"
#include "Construct.h"
#include "Constraint.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "Pattern.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Reorder.h"
#include "Router.h"
#include "Defrule.h"
#include "Scanner.h"
#include "Symbol.h"

#include "RuleLHSParsing.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static struct lhsParseNode *RuleBodyParse(const Environment&, const char *, struct token *, const char *, bool *);
static void DeclarationParse(const Environment&, const char *, const char *, bool *);
static struct lhsParseNode *LHSPattern(const Environment&, const char *, TokenType, const char *, bool *, bool,
                                       struct token *, const char *);
static struct lhsParseNode *ConnectedPatternParse(const Environment&, const char *, struct token *, bool *);
static struct lhsParseNode *GroupPatterns(const Environment&, const char *, TokenType, const char *, bool *);
static struct lhsParseNode *TestPattern(const Environment&, const char *, bool *);
static struct lhsParseNode *AssignmentParse(const Environment&, const char *, CLIPSLexeme *, bool *);
static void TagLHSLogicalNodes(lhsParseNode *);
static struct lhsParseNode *SimplePatternParse(const Environment&, const char *, struct token *, bool *);
static void ParseSalience(const Environment&, const char *, const char *, bool *);
static void ParseAutoFocus(const Environment&, const char *, bool *);

/*******************************************************************/
/* ParseRuleLHS: Coordinates all the actions necessary for parsing */
/*   the LHS of a rule including the reordering of pattern         */
/*   conditional elements to conform with the KB Rete topology.    */
/*******************************************************************/
struct lhsParseNode *ParseRuleLHS(
        const Environment&theEnv,
        const char *readSource,
        struct token *theToken,
        const char *ruleName,
        bool *error) {
    struct lhsParseNode *theLHS;
    bool result;

    *error = false;

    /*========================================*/
    /* Initialize salience parsing variables. */
    /*========================================*/

    PatternData(theEnv)->GlobalSalience = 0;
    PatternData(theEnv)->GlobalAutoFocus = false;
    PatternData(theEnv)->SalienceExpression = nullptr;

    /*============================*/
    /* Set the indentation depth. */
    /*============================*/

    SetIndentDepth(theEnv, 3);

    /*=====================================================*/
    /* Get the raw representation for the LHS of the rule. */
    /*=====================================================*/

    theLHS = RuleBodyParse(theEnv, readSource, theToken, ruleName, error);

    if (*error) return nullptr;

    /*====================================================*/
    /* Reorder the raw representation so that it consists */
    /* of at most a single top level OR CE containing one */
    /* or more AND CEs.                                   */
    /*====================================================*/

    theLHS = ReorderPatterns(theEnv, theLHS, &result);

    /*================================*/
    /* Return the LHS representation. */
    /*================================*/

    return (theLHS);
}

/*********************************************************/
/* RuleBodyParse: Parses the LHS of a rule, but does not */
/*   reorder any of the LHS patterns to conform with the */
/*   KB Rete Topology.                                   */
/*                                                       */
/* <rule-body> ::= [<declaration>]                       */
/*                 <conditional-element>*                */
/*                 =>                                    */
/*********************************************************/
static struct lhsParseNode *RuleBodyParse(
        const Environment&theEnv,
        const char *readSource,
        struct token *theToken,
        const char *ruleName,
        bool *error) {
    struct lhsParseNode *theNode, *otherNodes;

    /*=============================*/
    /* Set the error return value. */
    /*=============================*/

    *error = false;

    /*==================================================*/
    /* If we're already at the separator, "=>", between */
    /* the LHS and RHS, then the LHS is empty.          */
    /*==================================================*/

    if ((theToken->tknType == SYMBOL_TOKEN) ?
        (strcmp(theToken->lexemeValue->contents, "=>") == 0) : false) { return nullptr; }

    /*===========================================*/
    /* Parse the first pattern as a special case */
    /* (the declare statement is allowed).       */
    /*===========================================*/

    theNode = LHSPattern(theEnv, readSource, SYMBOL_TOKEN, "=>", error, true, theToken, ruleName);

    if (*error) {
        ReturnLHSParseNodes(theEnv, theNode);
        return nullptr;
    }

    PPCRAndIndent(theEnv);

    /*======================================*/
    /* Parse the other patterns in the LHS. */
    /*======================================*/

    otherNodes = GroupPatterns(theEnv, readSource, SYMBOL_TOKEN, "=>", error);

    if (*error) {
        ReturnLHSParseNodes(theEnv, theNode);
        return nullptr;
    }

    /*================================================*/
    /* Construct the final LHS by combining the first */
    /* pattern with the remaining patterns.           */
    /*================================================*/

    if (theNode == nullptr) { theNode = otherNodes; }
    else { theNode->bottom = otherNodes; }

    /*=======================*/
    /* Return the final LHS. */
    /*=======================*/

    return (theNode);
}

/********************************************************/
/* DeclarationParse: Parses a defrule declaration.      */
/*                                                      */
/* <declaration> ::= (declare <rule-property>+)         */
/*                                                      */
/* <rule-property> ::= (salience <integer-expression>)  */
/* <rule-property> ::= (auto-focus TRUE | FALSE)        */
/********************************************************/
static void DeclarationParse(
        const Environment&theEnv,
        const char *readSource,
        const char *ruleName,
        bool *error) {
    struct token theToken;
    Expression *packPtr;
    bool notDone = true;
    bool salienceParsed = false, autoFocusParsed = false;

    /*===========================*/
    /* Next token must be a '('. */
    /*===========================*/

    SavePPBuffer(theEnv, " ");

    GetToken(theEnv, readSource, &theToken);
    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "declare statement");
        *error = true;
        return;
    }

    /*==========================================*/
    /* Continue parsing until there are no more */
    /* valid rule property declarations.        */
    /*==========================================*/

    while (notDone) {
        /*=============================================*/
        /* The name of a rule property must be symbol. */
        /*=============================================*/

        GetToken(theEnv, readSource, &theToken);
        if (theToken.tknType != SYMBOL_TOKEN) {
            SyntaxErrorMessage(theEnv, "declare statement");
            *error = true;
        }

            /*==============================================*/
            /* Parse a salience declaration if encountered. */
            /*==============================================*/

        else if (strcmp(theToken.lexemeValue->contents, "salience") == 0) {
            if (salienceParsed) {
                AlreadyParsedErrorMessage(theEnv, "salience declaration", nullptr);
                *error = true;
            } else {
                ParseSalience(theEnv, readSource, ruleName, error);
                salienceParsed = true;
            }
        }

            /*=================================================*/
            /* Parse an auto-focus declaration if encountered. */
            /* A global flag is used to indicate if the        */
            /* auto-focus feature for a rule was parsed.       */
            /*=================================================*/

        else if (strcmp(theToken.lexemeValue->contents, "auto-focus") == 0) {
            if (autoFocusParsed) {
                AlreadyParsedErrorMessage(theEnv, "auto-focus declaration", nullptr);
                *error = true;
            } else {
                ParseAutoFocus(theEnv, readSource, error);
                autoFocusParsed = true;
            }
        }

            /*==========================================*/
            /* Otherwise the symbol does not correspond */
            /* to a valid rule property.                */
            /*==========================================*/

        else {
            SyntaxErrorMessage(theEnv, "declare statement");
            *error = true;
        }

        /*=====================================*/
        /* Return if an error was encountered. */
        /*=====================================*/

        if (*error) {
            ReturnExpression(theEnv, PatternData(theEnv)->SalienceExpression);
            PatternData(theEnv)->SalienceExpression = nullptr;
            return;
        }

        /*=======================================*/
        /* Both the salience and auto-focus rule */
        /* properties are closed with a ')'.     */
        /*=======================================*/

        GetToken(theEnv, readSource, &theToken);
        if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
            PPBackup(theEnv);
            SavePPBuffer(theEnv, " ");
            SavePPBuffer(theEnv, theToken.printForm);
            ReturnExpression(theEnv, PatternData(theEnv)->SalienceExpression);
            PatternData(theEnv)->SalienceExpression = nullptr;
            SyntaxErrorMessage(theEnv, "declare statement");
            *error = true;
            return;
        }

        /*=============================================*/
        /* The declare statement is closed with a ')'. */
        /*=============================================*/

        GetToken(theEnv, readSource, &theToken);
        if (theToken.tknType == RIGHT_PARENTHESIS_TOKEN) notDone = false;
        else if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
            ReturnExpression(theEnv, PatternData(theEnv)->SalienceExpression);
            PatternData(theEnv)->SalienceExpression = nullptr;
            SyntaxErrorMessage(theEnv, "declare statement");
            *error = true;
            return;
        } else {
            PPBackup(theEnv);
            SavePPBuffer(theEnv, " (");
        }
    }

    /*==========================================*/
    /* Return the value of the salience through */
    /* the global variable SalienceExpression.  */
    /*==========================================*/

    packPtr = PackExpression(theEnv, PatternData(theEnv)->SalienceExpression);
    ReturnExpression(theEnv, PatternData(theEnv)->SalienceExpression);
    PatternData(theEnv)->SalienceExpression = packPtr;
    return;
}

/************************************************************/
/* ParseSalience: Parses the rest of a defrule salience     */
/*   declaration once the salience keyword has been parsed. */
/************************************************************/
static void ParseSalience(
        const Environment&theEnv,
        const char *readSource,
        const char *ruleName,
        bool *error) {
    int salience;
    UDFValue salienceValue;

    /*==============================*/
    /* Get the salience expression. */
    /*==============================*/

    SavePPBuffer(theEnv, " ");

    PatternData(theEnv)->SalienceExpression = ParseAtomOrExpression(theEnv, readSource, nullptr);
    if (PatternData(theEnv)->SalienceExpression == nullptr) {
        *error = true;
        return;
    }

    /*============================================================*/
    /* Evaluate the expression and determine if it is an integer. */
    /*============================================================*/

    SetEvaluationError(theEnv, false);
    if (EvaluateExpression(theEnv, PatternData(theEnv)->SalienceExpression, &salienceValue)) {
        SalienceInformationError(theEnv, "defrule", ruleName);
        *error = true;
        return;
    }

    if (salienceValue.header->type != INTEGER_TYPE) {
        SalienceNonIntegerError(theEnv);
        *error = true;
        return;
    }

    /*=======================================================*/
    /* Salience number must be in the range -10000 to 10000. */
    /*=======================================================*/

    salience = (int) salienceValue.integerValue->contents;

    if ((salience > MAX_DEFRULE_SALIENCE) || (salience < MIN_DEFRULE_SALIENCE)) {
        SalienceRangeError(theEnv, MIN_DEFRULE_SALIENCE, MAX_DEFRULE_SALIENCE);
        *error = true;
        return;
    }

    /*==========================================*/
    /* If the expression is a constant integer, */
    /* don't bother storing the expression.     */
    /*==========================================*/

    if (PatternData(theEnv)->SalienceExpression->type == INTEGER_TYPE) {
        ReturnExpression(theEnv, PatternData(theEnv)->SalienceExpression);
        PatternData(theEnv)->SalienceExpression = nullptr;
    }

    PatternData(theEnv)->GlobalSalience = salience;
}

/**************************************************************/
/* ParseAutoFocus: Parses the rest of a defrule auto-focus    */
/*   declaration once the auto-focus keyword has been parsed. */
/**************************************************************/
static void ParseAutoFocus(
        const Environment&theEnv,
        const char *readSource,
        bool *error) {
    struct token theToken;

    /*========================================*/
    /* The auto-focus value must be a symbol. */
    /*========================================*/

    SavePPBuffer(theEnv, " ");

    GetToken(theEnv, readSource, &theToken);
    if (theToken.tknType != SYMBOL_TOKEN) {
        SyntaxErrorMessage(theEnv, "auto-focus statement");
        *error = true;
        return;
    }

    /*====================================================*/
    /* The auto-focus value must be either TRUE or FALSE. */
    /* If a valid value is parsed, then set the value of  */
    /* the global variable GlobalAutoFocus.               */
    /*====================================================*/

    if (strcmp(theToken.lexemeValue->contents, "TRUE") == 0) { PatternData(theEnv)->GlobalAutoFocus = true; }
    else if (strcmp(theToken.lexemeValue->contents, "FALSE") == 0) { PatternData(theEnv)->GlobalAutoFocus = false; }
    else {
        SyntaxErrorMessage(theEnv, "auto-focus statement");
        *error = true;
    }
}

/*****************************************************************/
/* LHSPattern: Parses a single conditional element found on the  */
/*   LHS of a rule. Conditonal element types include pattern CEs */
/*   (which may be assigned to a variable), test CEs, not CEs,   */
/*   logical CEs, and CEs, and or CEs.                           */
/*                                                               */
/* <conditional-element> ::= <pattern-CE> |                      */
/*                           <assigned-pattern-CE> |             */
/*                           <not-CE> | <and-CE> | <or-CE> |     */
/*                           <logical-CE> | <test-CE> |          */
/*                           <forall-CE> | <exists-CE>           */
/*****************************************************************/
static struct lhsParseNode *LHSPattern(
        const Environment&theEnv,
        const char *readSource,
        TokenType terminator,
        const char *terminatorString,
        bool *error,
        bool allowDeclaration,
        struct token *firstToken,
        const char *ruleName) {
    struct token theToken;
    struct lhsParseNode *theNode;

    /*=========================================================*/
    /* Check to see if the first token has already been read.  */
    /* This should only occur for the first pattern in a rule. */
    /*=========================================================*/

    if (firstToken == nullptr) GetToken(theEnv, readSource, &theToken);
    else CopyToken(&theToken, firstToken);

    /*=====================================================*/
    /* A left parenthesis begins all CEs and declarations. */
    /*=====================================================*/

    if (theToken.tknType == LEFT_PARENTHESIS_TOKEN) {
        /*================================================*/
        /* The first field of a pattern must be a symbol. */
        /*================================================*/

        GetToken(theEnv, readSource, &theToken);
        if (theToken.tknType != SYMBOL_TOKEN) {
            SyntaxErrorMessage(theEnv, "the first field of a pattern");
            *error = true;
            return nullptr;
        }

        /*====================================*/
        /* If this is the first CE of a rule, */
        /* then a declare statement is valid. */
        /*====================================*/

        if (allowDeclaration &&
            (strcmp(theToken.lexemeValue->contents, "declare") == 0)) {
            if (ruleName == nullptr) SystemError(theEnv, "RULELHS", 1);
            DeclarationParse(theEnv, readSource, ruleName, error);
            theNode = nullptr;
        }

            /*==================================*/
            /* Otherwise check for a *test* CE. */
            /*==================================*/

        else if (strcmp(theToken.lexemeValue->contents, "test") == 0) { theNode = TestPattern(theEnv, readSource, error); }

            /*============================================*/
            /* Otherwise check for an *and*, *or*, *not*, */
            /* *logical*, *exists*, or *forall* CE.       */
            /*============================================*/

        else if ((strcmp(theToken.lexemeValue->contents, "and") == 0) ||
                 (strcmp(theToken.lexemeValue->contents, "logical") == 0) ||
                 (strcmp(theToken.lexemeValue->contents, "not") == 0) ||
                 (strcmp(theToken.lexemeValue->contents, "exists") == 0) ||
                 (strcmp(theToken.lexemeValue->contents, "forall") == 0) ||
                 (strcmp(theToken.lexemeValue->contents, "or") == 0)) {
            theNode = ConnectedPatternParse(theEnv, readSource, &theToken, error);
        }

            /*=================================*/
            /* Otherwise parse a *pattern* CE. */
            /*=================================*/

        else { theNode = SimplePatternParse(theEnv, readSource, &theToken, error); }
    }

        /*=======================================*/
        /* Check for a pattern address variable. */
        /*=======================================*/

    else if (theToken.tknType == SF_VARIABLE_TOKEN) { theNode = AssignmentParse(theEnv, readSource, theToken.lexemeValue, error); }

        /*=================================================*/
        /* Check for the group terminator (either a "=>"   */
        /* separating the LHS from the RHS or a ")" ending */
        /* a CE containing other CEs such as an *and* CE). */
        /*=================================================*/

    else if ((theToken.tknType == terminator) ?
             (strcmp(theToken.printForm, terminatorString) == 0) : false) { return nullptr; }

        /*====================================*/
        /* Otherwise invalid syntax was used. */
        /*====================================*/

    else {
        SyntaxErrorMessage(theEnv, "defrule");
        *error = true;
        return nullptr;
    }

    /*================================*/
    /* If an error occurred, free any */
    /* allocated data structures.     */
    /*================================*/

    if (*error) {
        ReturnLHSParseNodes(theEnv, theNode);
        return nullptr;
    }

    /*=========================*/
    /* Return the LHS pattern. */
    /*=========================*/

    return (theNode);
}

/*********************************************************************/
/* ConnectedPatternParse:  Handles parsing of the connected          */
/*   conditional elements (i.e. those conditional elements that may  */
/*   contain one or more other conditional elements).  The connected */
/*   conditional elements include the *and*, *or*, *not*, *logical*, */
/*   *exists*, and *forall* CEs. This routine is entered with the    */
/*   parsing pointing to the name of the connected CE. It is exited  */
/*   with the parser pointing to the closing right parenthesis of    */
/*   the connected CE.                                               */
/*                                                                   */
/* <and-CE>      ::= (and <conditional-element>+)                    */
/*                                                                   */
/* <or-CE>       ::= (or <conditional-element>+)                     */
/*                                                                   */
/* <logical-CE>  ::= (logical <conditional-element>+)                */
/*                                                                   */
/* <not-CE>      ::= (not <conditional-element>)                     */
/*                                                                   */
/* <exists-CE>   ::= (exists <conditional-element>+)                 */
/*                                                                   */
/* <forall-CE>   ::= (forall <conditional-element>                   */
/*                           <conditional-element>+)                 */
/*********************************************************************/
static struct lhsParseNode *ConnectedPatternParse(
        const Environment&theEnv,
        const char *readSource,
        struct token *theToken,
        bool *error) {
    ParseNodeType connectorValue = AND_CE_NODE;
    struct lhsParseNode *theNode, *tempNode, *theGroup;
    const char *errorCE = nullptr;
    bool logical = false;
    bool tempValue;

    /*==========================================================*/
    /* Use appropriate spacing for pretty printing of the rule. */
    /*==========================================================*/

    IncrementIndentDepth(theEnv, 5);
    if (strcmp(theToken->lexemeValue->contents, "or") == 0) {
        connectorValue = OR_CE_NODE;
        errorCE = "the or conditional element";
        SavePPBuffer(theEnv, "  ");
    } else if (strcmp(theToken->lexemeValue->contents, "and") == 0) {
        connectorValue = AND_CE_NODE;
        errorCE = "the and conditional element";
        SavePPBuffer(theEnv, " ");
    } else if (strcmp(theToken->lexemeValue->contents, "not") == 0) {
        connectorValue = NOT_CE_NODE;
        errorCE = "the not conditional element";
        SavePPBuffer(theEnv, " ");
    } else if (strcmp(theToken->lexemeValue->contents, "exists") == 0) {
        connectorValue = EXISTS_CE_NODE;
        errorCE = "the exists conditional element";
        PPCRAndIndent(theEnv);
    } else if (strcmp(theToken->lexemeValue->contents, "forall") == 0) {
        connectorValue = FORALL_CE_NODE;
        errorCE = "the forall conditional element";
        PPCRAndIndent(theEnv);
    } else if (strcmp(theToken->lexemeValue->contents, "logical") == 0) {
        connectorValue = AND_CE_NODE;
        errorCE = "the logical conditional element";
        logical = true;
        PPCRAndIndent(theEnv);
    }

    /*=====================================================*/
    /* The logical CE cannot be contained within a not CE. */
    /*=====================================================*/

    if (PatternData(theEnv)->WithinNotCE && logical) {
        PrintErrorID(theEnv, "RULELHS", 1, true);
        WriteString(theEnv, STDERR, "The logical CE cannot be used within a not/exists/forall CE.\n");
        *error = true;
        return nullptr;
    }

    /*=====================================================*/
    /* Remember if we're currently within a *not* CE and   */
    /* then check to see if we're entering a new *not* CE. */
    /*=====================================================*/

    tempValue = PatternData(theEnv)->WithinNotCE;
    if ((connectorValue == NOT_CE_NODE) ||
        (connectorValue == EXISTS_CE_NODE) ||
        (connectorValue == FORALL_CE_NODE)) { PatternData(theEnv)->WithinNotCE = true; }

    /*===========================================*/
    /* Parse all of the CEs contained with the   */
    /* CE. A ) will terminate the end of the CE. */
    /*===========================================*/

    theGroup = GroupPatterns(theEnv, readSource, RIGHT_PARENTHESIS_TOKEN, ")", error);

    /*====================================*/
    /* Restore the "with a *not* CE" flag */
    /* and reset the indentation depth.   */
    /*====================================*/

    PatternData(theEnv)->WithinNotCE = tempValue;
    DecrementIndentDepth(theEnv, 5);

    /*============================================*/
    /* If an error occured while parsing, return. */
    /*============================================*/

    if (*error) {
        ReturnLHSParseNodes(theEnv, theGroup);
        return nullptr;
    }

    /*=========================================================*/
    /* If we parsed a *logical* CE, then mark the logical flag */
    /* for all of the CEs contained within the logical CE.     */
    /*=========================================================*/

    if (logical) TagLHSLogicalNodes(theGroup);

    /*=====================================================*/
    /* All the connected CEs must contain at least one CE. */
    /*=====================================================*/

    if (theGroup == nullptr) {
        SyntaxErrorMessage(theEnv, errorCE);
        *error = true;
        return nullptr;
    }

    /*============================================*/
    /* A not CE may not contain more than one CE. */
    /*============================================*/

    if ((connectorValue == NOT_CE_NODE) && (theGroup->bottom != nullptr)) {
        SyntaxErrorMessage(theEnv, errorCE);
        ReturnLHSParseNodes(theEnv, theGroup);
        *error = true;
        return nullptr;
    }

    /*============================================*/
    /* A forall CE must contain at least two CEs. */
    /*============================================*/

    if ((connectorValue == FORALL_CE_NODE) && (theGroup->bottom == nullptr)) {
        SyntaxErrorMessage(theEnv, errorCE);
        ReturnLHSParseNodes(theEnv, theGroup);
        *error = true;
        return nullptr;
    }

    /*========================================================*/
    /* Remove an "and" and "or" CE that only contains one CE. */
    /*========================================================*/

    if (((connectorValue == AND_CE_NODE) || (connectorValue == OR_CE_NODE)) &&
        (theGroup->bottom == nullptr)) {
        theGroup->logical = logical;
        return (theGroup);
    }

    /*===========================================================*/
    /* Create the top most node which connects the CEs together. */
    /*===========================================================*/

    theNode = GetLHSParseNode(theEnv);
    theNode->logical = logical;

    /*======================================================*/
    /* Attach and/or/not CEs directly to the top most node. */
    /*======================================================*/

    if ((connectorValue == AND_CE_NODE) ||
        (connectorValue == OR_CE_NODE) ||
        (connectorValue == NOT_CE_NODE)) {
        theNode->pnType = connectorValue;
        theNode->right = theGroup;
    }

        /*=================================================================*/
        /* Wrap two not CEs around the patterns contained in an exists CE. */
        /*=================================================================*/

    else if (connectorValue == EXISTS_CE_NODE) {
        theNode->pnType = NOT_CE_NODE;

        theNode->right = GetLHSParseNode(theEnv);
        theNode->right->pnType = NOT_CE_NODE;
        theNode->right->logical = logical;

        if (theGroup->bottom != nullptr) {
            theNode->right->right = GetLHSParseNode(theEnv);
            theNode->right->right->pnType = AND_CE_NODE;
            theNode->right->right->logical = logical;
            theNode->right->right->right = theGroup;
        } else { theNode->right->right = theGroup; }
    }

        /*==================================================*/
        /* For a forall CE, wrap a not CE around all of the */
        /* CEs and a not CE around the 2nd through nth CEs. */
        /*==================================================*/

    else if (connectorValue == FORALL_CE_NODE) {
        theNode->pnType = NOT_CE_NODE;

        tempNode = theGroup->bottom;
        theGroup->bottom = nullptr;

        theNode->right = GetLHSParseNode(theEnv);
        theNode->right->pnType = AND_CE_NODE;
        theNode->right->logical = logical;
        theNode->right->right = theGroup;

        theGroup = tempNode;

        theNode->right->right->bottom = GetLHSParseNode(theEnv);
        theNode->right->right->bottom->pnType = NOT_CE_NODE;
        theNode->right->right->bottom->logical = logical;

        tempNode = theNode->right->right->bottom;

        if (theGroup->bottom == nullptr) { tempNode->right = theGroup; }
        else {
            tempNode->right = GetLHSParseNode(theEnv);
            tempNode->right->pnType = AND_CE_NODE;
            tempNode->right->logical = logical;
            tempNode->right->right = theGroup;
        }
    }

    /*================*/
    /* Return the CE. */
    /*================*/

    return (theNode);
}

/***********************************************/
/* GroupPatterns: Groups a series of connected */
/*   conditional elements together.            */
/***********************************************/
static struct lhsParseNode *GroupPatterns(
        const Environment&theEnv,
        const char *readSource,
        TokenType terminator,
        const char *terminatorString,
        bool *error) {
    struct lhsParseNode *lastNode, *newNode, *theNode;

    lastNode = theNode = nullptr;

    while (true) {
        /*==================*/
        /* Get the next CE. */
        /*==================*/

        newNode = LHSPattern(theEnv, readSource, terminator, terminatorString,
                             error, false, nullptr, nullptr);

        /*=======================================================*/
        /* If an error occurred, release any LHS data structures */
        /* previously allocated by this routine.                 */
        /*=======================================================*/

        if (*error) {
            ReturnLHSParseNodes(theEnv, theNode);
            return nullptr;
        }

        /*===============================================*/
        /* A nullptr value for the CE just parsed indicates */
        /* that the terminator for the group of patterns */
        /* was encountered (either a "=>" or a ")".      */
        /*===============================================*/

        if (newNode == nullptr) {
            PPBackup(theEnv);
            PPBackup(theEnv);
            if (terminator == RIGHT_PARENTHESIS_TOKEN) { SavePPBuffer(theEnv, terminatorString); }
            else {
                PPCRAndIndent(theEnv);
                SavePPBuffer(theEnv, terminatorString);
            }

            return (theNode);
        }

        /*============================*/
        /* Add the new CE to the list */
        /* of CEs being grouped.      */
        /*============================*/

        if (lastNode == nullptr) { theNode = newNode; }
        else { lastNode->bottom = newNode; }

        lastNode = newNode;

        /*======================================*/
        /* Fix the pretty print representation. */
        /*======================================*/

        PPCRAndIndent(theEnv);
    }
}

/**************************************************************/
/* TestPattern: Handles parsing of test conditional elements. */
/*                                                            */
/* <test-CE> ::= (test <function-call>)                       */
/**************************************************************/
static struct lhsParseNode *TestPattern(
        const Environment&theEnv,
        const char *readSource,
        bool *error) {
    struct lhsParseNode *theNode;
    struct token theToken;
    Expression *theExpression;

    /*================================================*/
    /* Create the data specification for the test CE. */
    /*================================================*/

    SavePPBuffer(theEnv, " ");
    theNode = GetLHSParseNode(theEnv);
    theNode->pnType = TEST_CE_NODE;
    theExpression = Function0Parse(theEnv, readSource);
    theNode->expression = ExpressionToLHSParseNodes(theEnv, theExpression);
    ReturnExpression(theEnv, theExpression);

    if (theNode->expression == nullptr) {
        *error = true;
        ReturnLHSParseNodes(theEnv, theNode);
        return nullptr;
    }

    /*=========================================================*/
    /* Check for the closing right parenthesis of the test CE. */
    /*=========================================================*/

    GetToken(theEnv, readSource, &theToken);
    if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "test conditional element");
        *error = true;
        ReturnLHSParseNodes(theEnv, theNode);
        return nullptr;
    }

    /*=====================*/
    /* Return the test CE. */
    /*=====================*/

    return (theNode);
}

/****************************************************************/
/* AssignmentParse: Finishes the parsing of pattern conditional */
/*   elements that have been bound to a  variable.              */
/*                                                              */
/* <assigned-pattern-CE> ::= ?<variable-symbol> <- <pattern-CE> */
/****************************************************************/
static struct lhsParseNode *AssignmentParse(
        const Environment&theEnv,
        const char *readSource,
        CLIPSLexeme *factAddress,
        bool *error) {
    struct lhsParseNode *theNode;
    struct token theToken;

    /*=====================================================*/
    /* Patterns cannot be bound if they are with a not CE. */
    /*=====================================================*/

    if (PatternData(theEnv)->WithinNotCE) {
        PrintErrorID(theEnv, "RULELHS", 2, true);
        WriteString(theEnv, STDERR, "A pattern CE cannot be bound to a pattern-address within a not CE\n");
        *error = true;
        return nullptr;
    }

    /*===============================================*/
    /* Check for binder token, "<-", after variable. */
    /*===============================================*/

    SavePPBuffer(theEnv, " ");

    GetToken(theEnv, readSource, &theToken);

    if ((theToken.tknType == SYMBOL_TOKEN) ? (strcmp(theToken.lexemeValue->contents, "<-") != 0) :
        true) {
        SyntaxErrorMessage(theEnv, "binding patterns");
        *error = true;
        return nullptr;
    }

    SavePPBuffer(theEnv, " ");

    /*================================================*/
    /* Check for opening left parenthesis of pattern. */
    /*================================================*/

    GetToken(theEnv, readSource, &theToken);
    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "binding patterns");
        *error = true;
        return nullptr;
    }

    /*======================================================*/
    /* Parse the pattern and return the data specification. */
    /*======================================================*/

    GetToken(theEnv, readSource, &theToken);

    theNode = SimplePatternParse(theEnv, readSource, &theToken, error);

    if (*error) {
        ReturnLHSParseNodes(theEnv, theNode);
        return nullptr;
    }

    /*=============================================*/
    /* Store the name of the variable to which the */
    /* pattern is bound and return the pattern.    */
    /*=============================================*/

    theNode->value = factAddress;
    return (theNode);
}

/************************************************************/
/* TagLHSLogicalNodes: Marks all *and*, *or*, and *not* CEs */
/*   contained within a logical CE as having the properties */
/*   associated with a logical CE.                          */
/************************************************************/
static void TagLHSLogicalNodes(
        struct lhsParseNode *nodePtr) {
    while (nodePtr != nullptr) {
        nodePtr->logical = true;
        if ((nodePtr->pnType == AND_CE_NODE) ||
            (nodePtr->pnType == OR_CE_NODE) ||
            (nodePtr->pnType == NOT_CE_NODE)) { TagLHSLogicalNodes(nodePtr->right); }
        nodePtr = nodePtr->bottom;
    }
}

/***********************************************************/
/* SimplePatternParse: Parses a simple pattern (an opening */
/*   parenthesis followed by one or more fields followed   */
/*   by a closing parenthesis).                            */
/*                                                         */
/* <pattern-CE> ::= <ordered-pattern-CE> |                 */
/*                  <template-pattern-CE>                  */
/***********************************************************/
static struct lhsParseNode *SimplePatternParse(
        const Environment&theEnv,
        const char *readSource,
        struct token *theToken,
        bool *error) {
    struct lhsParseNode *theNode;
    struct patternParser *tempParser;

    /*=================================================*/
    /* The first field of a pattern must be a symbol.  */
    /* In addition, the symbols ":" and "=" can not    */
    /* be used because they have special significance. */
    /*=================================================*/

    if (theToken->tknType != SYMBOL_TOKEN) {
        SyntaxErrorMessage(theEnv, "the first field of a pattern");
        *error = true;
        return nullptr;
    } else if ((strcmp(theToken->lexemeValue->contents, "=") == 0) ||
               (strcmp(theToken->lexemeValue->contents, ":") == 0)) {
        SyntaxErrorMessage(theEnv, "the field field of a pattern");
        *error = true;
        return nullptr;
    }

    /*===============================================*/
    /* Construct the topmost node of the pattern CE. */
    /*===============================================*/

    theNode = GetLHSParseNode(theEnv);
    theNode->pnType = PATTERN_CE_NODE;
    theNode->negated = false;
    theNode->exists = false;

    /*======================================================*/
    /* Search for a pattern parser that claims the pattern. */
    /*======================================================*/

    for (tempParser = PatternData(theEnv)->ListOfPatternParsers;
         tempParser != nullptr;
         tempParser = tempParser->next) {
        if ((*tempParser->recognizeFunction)(theToken->lexemeValue)) {
            theNode->patternType = tempParser;
            theNode->right = (*tempParser->parseFunction)(theEnv, readSource, theToken);
            if (theNode->right == nullptr) {
                *error = true;
                ReturnLHSParseNodes(theEnv, theNode);
                return nullptr;
            }

            PropagatePatternType(theNode, tempParser);
            return (theNode);
        }
    }

    /*=================================*/
    /* If a pattern parser couldn't be */
    /* found, then signal an error.    */
    /*=================================*/

    *error = true;
    SyntaxErrorMessage(theEnv, "the field field of a pattern");
    ReturnLHSParseNodes(theEnv, theNode);
    return nullptr;
}

/**************************************************************/
/* PropagatePatternType: Sets the selfPattern field for all   */
/*   lhsParseNodes in a linked list of those data structures. */
/**************************************************************/
void PropagatePatternType(
        struct lhsParseNode *theLHS,
        struct patternParser *theParser) {
    while (theLHS != nullptr) {
        theLHS->patternType = theParser;
        if (theLHS->right != nullptr) PropagatePatternType(theLHS->right, theParser);
        if (theLHS->expression != nullptr) PropagatePatternType(theLHS->expression, theParser);
        theLHS = theLHS->bottom;
    }
}



