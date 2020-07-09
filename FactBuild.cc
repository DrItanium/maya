/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  12/07/17             */
/*                                                     */
/*                   FACT BUILD MODULE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Given a new fact pattern, adds the pattern to    */
/*   the pattern network of the associated deftemplate. Also */
/*   contains routines for deleting a pattern from the fact  */
/*   pattern network.                                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed INCREMENTAL_RESET compilation flag.    */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Added support for hashed comparisons to        */
/*            constants.                                     */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Removed initial-fact support.                  */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <cstdio>

#include "ArgumentAccess.h"
#include "Environment.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "DefmoduleUtility.h"
#include "Reorder.h"
#include "ReteUtility.h"
#include "Router.h"
#include "Deftemplate.h"


/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static struct factPatternNode *FindPatternNode(factPatternNode *, struct lhsParseNode *,
                                               struct factPatternNode **, bool, bool);
static struct factPatternNode *CreateNewPatternNode(Environment *, struct lhsParseNode *, struct factPatternNode *,
                                                    struct factPatternNode *, bool, bool);
static void ClearPatternMatches(Environment *, struct factPatternNode *);
static void DetachFactPattern(Environment *, struct patternNodeHeader *);
static struct patternNodeHeader *PlaceFactPattern(Environment *, struct lhsParseNode *);
static struct lhsParseNode *RemoveUnneededSlots(Environment *, struct lhsParseNode *);
static void FindAndSetDeftemplatePatternNetwork(Environment *, struct factPatternNode *, struct factPatternNode *);

/*********************************************************/
/* InitializeFactPatterns: Adds fact patterns to the set */
/*   of patterns recognized by the rule pattern parsing  */
/*   and pattern/join network integration routines.      */
/*********************************************************/
void InitializeFactPatterns(
        Environment *theEnv) {
    struct patternParser *newPtr;

    InitializeFactReteFunctions(theEnv);

    newPtr = get_struct(theEnv, patternParser);

    newPtr->name = "facts";
    newPtr->priority = 0;
    newPtr->entityType = &FactData(theEnv)->FactInfo;

    newPtr->recognizeFunction = FactPatternParserFind;
    newPtr->parseFunction = FactPatternParse;
    newPtr->postAnalysisFunction = nullptr;
    newPtr->addPatternFunction = PlaceFactPattern;
    newPtr->removePatternFunction = DetachFactPattern;
    newPtr->genJNConstantFunction = nullptr;
    newPtr->replaceGetJNValueFunction = FactReplaceGetvar;
    newPtr->genGetJNValueFunction = FactGenGetvar;
    newPtr->genCompareJNValuesFunction = FactJNVariableComparison;
    newPtr->genPNConstantFunction = FactGenPNConstant;
    newPtr->replaceGetPNValueFunction = FactReplaceGetfield;
    newPtr->genGetPNValueFunction = FactGenGetfield;
    newPtr->genComparePNValuesFunction = FactPNVariableComparison;
    newPtr->returnUserDataFunction = nullptr;
    newPtr->copyUserDataFunction = nullptr;

    newPtr->markIRPatternFunction = MarkFactPatternForIncrementalReset;
    newPtr->incrementalResetFunction = FactsIncrementalReset;

    newPtr->codeReferenceFunction = nullptr;

    AddPatternParser(theEnv, newPtr);
}


/******************************************************************************/
/* PlaceFactPattern: Integrates a fact pattern into the fact pattern network. */
/******************************************************************************/
static struct patternNodeHeader *PlaceFactPattern(
        Environment *theEnv,
        struct lhsParseNode *thePattern) {
    struct lhsParseNode *tempPattern;
    struct factPatternNode *currentLevel, *lastLevel;
    struct factPatternNode *nodeBeforeMatch, *newNode = nullptr;
    bool endSlot;
    unsigned int count;
    const char *deftemplateName;

    /*======================================================================*/
    /* Get the name of the deftemplate associated with the pattern being    */
    /* added (recall that the first field of any pattern must be a symbol). */
    /*======================================================================*/

    deftemplateName = thePattern->right->bottom->lexemeValue->contents;

    /*=====================================================*/
    /* Remove any slot tests that test only for existance. */
    /*=====================================================*/

    thePattern->right = RemoveUnneededSlots(theEnv, thePattern->right);

    /*========================================================*/
    /* If the constant test for the relation name is the only */
    /* pattern network test and there are no other network    */
    /* tests, then remove the test, but keep the node since   */
    /* there must be a link from the fact pattern network to  */
    /* the join network. Otherwise, remove the test for the   */
    /* relation name since this test has already been done    */
    /* before entering the pattern network (since each        */
    /* deftemplate has its own pattern network).              */
    /*========================================================*/

    if (thePattern->right->right == nullptr) {
        ReturnExpression(theEnv, thePattern->right->networkTest);
        ReturnExpression(theEnv, thePattern->right->constantSelector);
        ReturnExpression(theEnv, thePattern->right->constantValue);
        thePattern->right->networkTest = nullptr;
        thePattern->right->constantSelector = nullptr;
        thePattern->right->constantValue = nullptr;
    } else {
        tempPattern = thePattern->right;
        thePattern->right = thePattern->right->right;
        tempPattern->right = nullptr;
        ReturnLHSParseNodes(theEnv, tempPattern);
    }

    /*====================================================*/
    /* Get the expression for hashing in the alpha memory */
    /* and attach it to the last node of the pattern.     */
    /*====================================================*/

    tempPattern = thePattern->right;
    while (tempPattern->right != nullptr) { tempPattern = tempPattern->right; }

    if ((tempPattern->multifieldSlot) && (tempPattern->bottom != nullptr)) {
        tempPattern = tempPattern->bottom;

        while (tempPattern->right != nullptr) { tempPattern = tempPattern->right; }
    }

    tempPattern->rightHash = thePattern->rightHash;
    thePattern->rightHash = nullptr;

    tempPattern = nullptr;

    /*============================================================*/
    /* Get a pointer to the deftemplate data structure associated */
    /* with the pattern (use the deftemplate name extracted from  */
    /* the first field of the pattern).                           */
    /*============================================================*/

    FactData(theEnv)->CurrentDeftemplate = (Deftemplate *)
            FindImportedConstruct(theEnv, "deftemplate", nullptr,
                                  deftemplateName, &count,
                                  true, nullptr);

    /*================================================*/
    /* Initialize some pointers to indicate where the */
    /* pattern is being added to the pattern network. */
    /*================================================*/

    currentLevel = FactData(theEnv)->CurrentDeftemplate->patternNetwork;
    lastLevel = nullptr;
    thePattern = thePattern->right;

    /*===========================================*/
    /* Loop until all fields in the pattern have */
    /* been added to the pattern network.        */
    /*===========================================*/

    while (thePattern != nullptr) {
        /*===========================================================*/
        /* If a multifield slot is being processed, then process the */
        /* pattern nodes attached to the multifield pattern node.    */
        /*===========================================================*/

        if (thePattern->multifieldSlot) {
            tempPattern = thePattern;
            thePattern = thePattern->bottom;
        }

        /*============================================*/
        /* Determine if the last pattern field within */
        /* a multifield slot is being processed.      */
        /*============================================*/

        endSlot = (thePattern->right == nullptr) && (tempPattern != nullptr);

        /*========================================*/
        /* Is there a node in the pattern network */
        /* that can be reused (shared)?           */
        /*========================================*/

        newNode = FindPatternNode(currentLevel, thePattern, &nodeBeforeMatch, endSlot, false);

        /*================================================*/
        /* If the pattern node cannot be shared, then add */
        /* a new pattern node to the pattern network.     */
        /*================================================*/

        if (newNode == nullptr) { newNode = CreateNewPatternNode(theEnv, thePattern, nodeBeforeMatch, lastLevel, endSlot, false); }

        if (thePattern->constantSelector != nullptr) {
            currentLevel = newNode->nextLevel;
            lastLevel = newNode;
            newNode = FindPatternNode(currentLevel, thePattern, &nodeBeforeMatch, endSlot, true);

            if (newNode == nullptr) { newNode = CreateNewPatternNode(theEnv, thePattern, nodeBeforeMatch, lastLevel, endSlot, true); }
        }

        /*===========================================================*/
        /* Move on to the next field in the new pattern to be added. */
        /*===========================================================*/

        if ((thePattern->right == nullptr) && (tempPattern != nullptr)) {
            thePattern = tempPattern;
            tempPattern = nullptr;
        }

        thePattern = thePattern->right;

        /*==========================================================*/
        /* If there are no more pattern nodes to be added to the    */
        /* pattern network, then mark the last pattern node added   */
        /* as a stop node (i.e. if you get to this node and the     */
        /* network test succeeds, then a pattern has been matched). */
        /*==========================================================*/

        if (thePattern == nullptr) newNode->header.stopNode = true;

        /*================================================*/
        /* Update the pointers which indicate where we're */
        /* trying to add the new pattern to the currently */
        /* existing pattern network.                      */
        /*================================================*/

        lastLevel = newNode;
        currentLevel = newNode->nextLevel;
    }

    /*==================================================*/
    /* Return the leaf node of the newly added pattern. */
    /*==================================================*/

    return ((patternNodeHeader *) newNode);
}

/*************************************************************/
/* FindPatternNode: Looks for a pattern node at a specified  */
/*  level in the pattern network that can be reused (shared) */
/*  with a pattern field being added to the pattern network. */
/*************************************************************/
static struct factPatternNode *FindPatternNode(
        struct factPatternNode *listOfNodes,
        struct lhsParseNode *thePattern,
        struct factPatternNode **nodeBeforeMatch,
        bool endSlot,
        bool constantSelector) {
    struct expr *compareTest;
    *nodeBeforeMatch = nullptr;

    if (constantSelector) { compareTest = thePattern->constantValue; }
    else if (thePattern->constantSelector != nullptr) { compareTest = thePattern->constantSelector; }
    else { compareTest = thePattern->networkTest; }

    /*==========================================================*/
    /* Loop through the nodes at the given level in the pattern */
    /* network looking for a node that can be reused (shared)?  */
    /*==========================================================*/

    while (listOfNodes != nullptr) {
        /*==========================================================*/
        /* If the type of the pattern node and the expression being */
        /* tested by the pattern node are the same as the type and  */
        /* expression for the pattern field being added, then       */
        /* return the pattern node because it can be shared with    */
        /* the pattern field being added.                           */
        /*==========================================================*/

        if ((thePattern->pnType == SF_WILDCARD_NODE) || (thePattern->pnType == SF_VARIABLE_NODE)) {
            if ((listOfNodes->header.singlefieldNode) &&
                (listOfNodes->header.endSlot == endSlot) &&
                (listOfNodes->whichField == thePattern->index) &&
                (listOfNodes->whichSlot == (thePattern->slotNumber - 1)) &&
                IdenticalExpression(listOfNodes->networkTest, compareTest) &&
                IdenticalExpression(listOfNodes->header.rightHash, thePattern->rightHash)) { return (listOfNodes); }
        } else if ((thePattern->pnType == MF_WILDCARD_NODE) || (thePattern->pnType == MF_VARIABLE_NODE)) {
            if ((listOfNodes->header.multifieldNode) &&
                (listOfNodes->header.endSlot == endSlot) &&
                (listOfNodes->leaveFields == thePattern->singleFieldsAfter) &&
                (listOfNodes->whichField == thePattern->index) &&
                (listOfNodes->whichSlot == (thePattern->slotNumber - 1)) &&
                IdenticalExpression(listOfNodes->networkTest, compareTest) &&
                IdenticalExpression(listOfNodes->header.rightHash, thePattern->rightHash)) { return (listOfNodes); }
        }

        /*==================================*/
        /* Move on to the next node at this */
        /* level in the pattern network.    */
        /*==================================*/

        *nodeBeforeMatch = listOfNodes;
        listOfNodes = listOfNodes->rightNode;
    }

    /*==============================================*/
    /* A shareable pattern node could not be found. */
    /*==============================================*/

    return nullptr;
}

/*************************************************************/
/* RemoveUnneededSlots: Removes fact pattern nodes that have */
/*   no effect on pattern matching. For example, given the   */
/*   following deftemplate and a pattern using it,           */
/*                                                           */
/*   (deftemplate foo (slot x) (slot y))                     */
/*                                                           */
/*   (foo (x ?x) (y ?y))                                     */
/*                                                           */
/*   The x and y slot pattern nodes can be discarded since   */
/*   all foo facts will have these two slots in the fact     */
/*   data structure used to store them.                      */
/*************************************************************/
static struct lhsParseNode *RemoveUnneededSlots(
        Environment *theEnv,
        struct lhsParseNode *thePattern) {
    struct lhsParseNode *tempPattern = thePattern;
    struct lhsParseNode *lastPattern = nullptr, *head = thePattern;
    struct expr *theTest;

    while (tempPattern != nullptr) {
        /*=============================================================*/
        /* A single field slot that has no pattern network expression  */
        /* associated with it can be removed (i.e. any value contained */
        /* in this slot will satisfy the pattern being matched).       */
        /*=============================================================*/

        if (((tempPattern->pnType == SF_WILDCARD_NODE) || (tempPattern->pnType == SF_VARIABLE_NODE)) &&
            (tempPattern->networkTest == nullptr)) {
            if (lastPattern != nullptr) lastPattern->right = tempPattern->right;
            else head = tempPattern->right;

            tempPattern->right = nullptr;
            ReturnLHSParseNodes(theEnv, tempPattern);

            if (lastPattern != nullptr) tempPattern = lastPattern->right;
            else tempPattern = head;
        }

            /*=======================================================*/
            /* A multifield variable or wildcard within a multifield */
            /* slot can be removed if there are no other multifield  */
            /* variables or wildcards contained in the same slot     */
            /* (and the multifield has no expressions which must be  */
            /* evaluated in the fact pattern network).               */
            /*=======================================================*/

        else if (((tempPattern->pnType == MF_WILDCARD_NODE) || (tempPattern->pnType == MF_VARIABLE_NODE)) &&
                 (tempPattern->multifieldSlot == false) &&
                 (tempPattern->networkTest == nullptr) &&
                 (tempPattern->multiFieldsBefore == 0) &&
                 (tempPattern->multiFieldsAfter == 0)) {
            if (lastPattern != nullptr) lastPattern->right = tempPattern->right;
            else head = tempPattern->right;

            tempPattern->right = nullptr;
            ReturnLHSParseNodes(theEnv, tempPattern);

            if (lastPattern != nullptr) tempPattern = lastPattern->right;
            else tempPattern = head;
        }

            /*==================================================================*/
            /* A multifield wildcard or variable contained in a multifield slot */
            /* that contains no other multifield wildcards or variables, but    */
            /* does have an expression that must be evaluated, can be changed   */
            /* to a single field pattern node with the same expression.         */
            /*==================================================================*/

        else if (((tempPattern->pnType == MF_WILDCARD_NODE) || (tempPattern->pnType == MF_VARIABLE_NODE)) &&
                 (tempPattern->multifieldSlot == false) &&
                 (tempPattern->networkTest != nullptr) &&
                 (tempPattern->multiFieldsBefore == 0) &&
                 (tempPattern->multiFieldsAfter == 0)) {
            tempPattern->pnType = SF_WILDCARD_NODE;
            lastPattern = tempPattern;
            tempPattern = tempPattern->right;
        }

            /*=========================================================*/
            /* If we're dealing with a multifield slot with no slot    */
            /* restrictions, then treat the multfield slot as a single */
            /* field slot, but attach a test which verifies that the   */
            /* slot contains a zero length multifield value.           */
            /*=========================================================*/

        else if ((tempPattern->pnType == MF_WILDCARD_NODE) &&
                 (tempPattern->multifieldSlot == true) &&
                 (tempPattern->bottom == nullptr)) {
            tempPattern->pnType = SF_WILDCARD_NODE;
            tempPattern->networkTest = FactGenCheckZeroLength(theEnv, tempPattern->slotNumber);
            tempPattern->multifieldSlot = false;
            lastPattern = tempPattern;
            tempPattern = tempPattern->right;
        }

            /*===================================================*/
            /* Recursively call RemoveUnneededSlots for the slot */
            /* restrictions contained within a multifield slot.  */
            /*===================================================*/

        else if ((tempPattern->pnType == MF_WILDCARD_NODE) &&
                 (tempPattern->multifieldSlot == true)) {
            /*=======================================================*/
            /* Add an expression to the first pattern restriction in */
            /* the multifield slot that determines whether or not    */
            /* the fact's slot value contains the minimum number of  */
            /* required fields to satisfy the pattern restrictions   */
            /* for this slot. The length check is place before any   */
            /* other tests, so that preceeding checks do not have to */
            /* determine if there are enough fields in the slot to   */
            /* safely retrieve a value.                              */
            /*=======================================================*/

            theTest = FactGenCheckLength(theEnv, tempPattern->bottom);
            if (tempPattern->bottom->constantSelector != nullptr) {
                tempPattern->bottom->constantSelector->nextArg = CopyExpression(theEnv, theTest);
            }
            theTest = CombineExpressions(theEnv, theTest, tempPattern->bottom->networkTest);
            tempPattern->bottom->networkTest = theTest;

            /*=========================================================*/
            /* Remove any unneeded pattern restrictions from the slot. */
            /*=========================================================*/

            tempPattern->bottom = RemoveUnneededSlots(theEnv, tempPattern->bottom);

            /*===========================================================*/
            /* If the slot no longer contains any restrictions, then the */
            /* multifield slot can be completely removed. In any case,   */
            /* move on to the next slot to be examined for removal.      */
            /*===========================================================*/

            if (tempPattern->bottom == nullptr) {
                if (lastPattern != nullptr) lastPattern->right = tempPattern->right;
                else head = tempPattern->right;

                tempPattern->right = nullptr;
                ReturnLHSParseNodes(theEnv, tempPattern);

                if (lastPattern != nullptr) tempPattern = lastPattern->right;
                else tempPattern = head;
            } else {
                lastPattern = tempPattern;
                tempPattern = tempPattern->right;
            }
        }

            /*=======================================================*/
            /* If none of the other tests for removing slots or slot */
            /* restrictions apply, then move on to the next slot or  */
            /* slot restriction to be tested.                        */
            /*=======================================================*/

        else {
            lastPattern = tempPattern;
            tempPattern = tempPattern->right;
        }
    }

    /*======================================*/
    /* Return the pattern with unused slots */
    /* and slot restrictions removed.       */
    /*======================================*/

    return (head);
}

/****************************************************/
/* CreateNewPatternNode: Creates a new pattern node */
/*   and initializes all of its values.             */
/****************************************************/
static struct factPatternNode *CreateNewPatternNode(
        Environment *theEnv,
        struct lhsParseNode *thePattern,
        struct factPatternNode *nodeBeforeMatch,
        struct factPatternNode *upperLevel,
        bool endSlot,
        bool constantSelector) {
    struct factPatternNode *newNode;

    /*========================================*/
    /* Create the pattern node and initialize */
    /* its slots to the default values.       */
    /*========================================*/

    newNode = get_struct(theEnv, factPatternNode);
    newNode->nextLevel = nullptr;
    newNode->rightNode = nullptr;
    newNode->leftNode = nullptr;
    newNode->leaveFields = thePattern->singleFieldsAfter;
    InitializePatternHeader(theEnv, (patternNodeHeader *) &newNode->header);

    if (thePattern->index > 0) { newNode->whichField = thePattern->index; }
    else newNode->whichField = 0;

    if (thePattern->slotNumber != UNSPECIFIED_SLOT) { newNode->whichSlot = thePattern->slotNumber - 1; }
    else { newNode->whichSlot = newNode->whichField; }

    if ((thePattern->constantSelector != nullptr) && (!constantSelector)) { newNode->header.selector = true; }

    /*=============================================================*/
    /* Set the slot values which indicate whether the pattern node */
    /* is a single-field, multifield, or end-of-pattern node.      */
    /*=============================================================*/

    if ((thePattern->pnType == SF_WILDCARD_NODE) || (thePattern->pnType == SF_VARIABLE_NODE)) { newNode->header.singlefieldNode = true; }
    else if ((thePattern->pnType == MF_WILDCARD_NODE) ||
             (thePattern->pnType == MF_VARIABLE_NODE)) { newNode->header.multifieldNode = true; }
    newNode->header.endSlot = endSlot;

    /*===========================================================*/
    /* Install the expression associated with this pattern node. */
    /*===========================================================*/

    if (constantSelector) { newNode->networkTest = AddHashedExpression(theEnv, thePattern->constantValue); }
    else if (thePattern->constantSelector != nullptr) { newNode->networkTest = AddHashedExpression(theEnv, thePattern->constantSelector); }
    else { newNode->networkTest = AddHashedExpression(theEnv, thePattern->networkTest); }

    /*==========================================*/
    /* Add the expression used for adding alpha */
    /* matches to the alpha memory.             */
    /*==========================================*/

    newNode->header.rightHash = AddHashedExpression(theEnv, thePattern->rightHash);

    /*===============================================*/
    /* Set the upper level pointer for the new node. */
    /*===============================================*/

    newNode->lastLevel = upperLevel;

    if ((upperLevel != nullptr) && (upperLevel->header.selector)) {
        AddHashedPatternNode(theEnv, upperLevel, newNode, newNode->networkTest->type, newNode->networkTest->value);
    }

    /*======================================================*/
    /* If there are no nodes on this level, then attach the */
    /* new node to the child pointer of the upper level.    */
    /*======================================================*/

    if (nodeBeforeMatch == nullptr) {
        if (upperLevel == nullptr) FactData(theEnv)->CurrentDeftemplate->patternNetwork = newNode;
        else upperLevel->nextLevel = newNode;
        return (newNode);
    }

    /*=====================================================*/
    /* If there is an upper level above the new node, then */
    /* place the new node as the first child in the upper  */
    /* level's nextLevel (child) link.                     */
    /*=====================================================*/

    if (upperLevel != nullptr) {
        newNode->rightNode = upperLevel->nextLevel;
        if (upperLevel->nextLevel != nullptr) { upperLevel->nextLevel->leftNode = newNode; }
        upperLevel->nextLevel = newNode;
        return (newNode);
    }

    /*=====================================================*/
    /* Since there is no upper level above the new node,   */
    /* (i.e. the new node is being added to the highest    */
    /* level in the pattern network), the new node becomes */
    /* the first node visited in the pattern network.      */
    /*=====================================================*/

    newNode->rightNode = FactData(theEnv)->CurrentDeftemplate->patternNetwork;
    if (FactData(theEnv)->CurrentDeftemplate->patternNetwork != nullptr) {
        FactData(theEnv)->CurrentDeftemplate->patternNetwork->leftNode = newNode;
    }

    FactData(theEnv)->CurrentDeftemplate->patternNetwork = newNode;
    return (newNode);
}

/*************************************************************/
/* DetachFactPattern: Removes a pattern node and all of its  */
/*   parent nodes from the pattern network. Nodes are only   */
/*   removed if they are no longer shared (i.e. a pattern    */
/*   node that has more than one child node is shared). A    */
/*   pattern from a rule is typically removed by removing    */
/*   the bottom most pattern node used by the pattern and    */
/*   then removing any parent nodes which are not shared by  */
/*   other patterns.                                         */
/*                                                           */
/*   Example:                                                */
/*     Patterns (a b c d) and (a b e f) would be represented */
/*     by the pattern net shown on the left.  If (a b c d)   */
/*     was detached, the resultant pattern net would be the  */
/*     one shown on the right.                               */
/*                                                           */
/*           a                  a                            */
/*           |                  |                            */
/*           b                  b                            */
/*           |                  |                            */
/*           c--e               e                            */
/*           |  |               |                            */
/*           d  f               f                            */
/*                                                           */
/*************************************************************/
static void DetachFactPattern(
        Environment *theEnv,
        struct patternNodeHeader *thePattern) {
    struct factPatternNode *patternPtr;
    struct factPatternNode *upperLevel;

    /*=====================================================*/
    /* Get rid of any matches stored in the alpha memory.  */
    /*=====================================================*/

    patternPtr = (factPatternNode *) thePattern;
    ClearPatternMatches(theEnv, patternPtr);

    /*=======================================================*/
    /* If there are no joins entered from this pattern, then */
    /* the pattern node is no longer a stop node. Also if    */
    /* the pattern has a next level pointer, then it can     */
    /* not be removed since other patterns make use of it.   */
    /*=======================================================*/

    if (patternPtr->header.entryJoin == nullptr) patternPtr->header.stopNode = false;
    if (patternPtr->nextLevel != nullptr) return;

    /*==============================================================*/
    /* Loop until all appropriate pattern nodes have been detached. */
    /*==============================================================*/

    upperLevel = patternPtr;
    while (upperLevel != nullptr) {
        if ((upperLevel->leftNode == nullptr) &&
            (upperLevel->rightNode == nullptr)) {
            /*===============================================*/
            /* Pattern node is the only node on this level.  */
            /* Remove it and continue detaching other nodes  */
            /* above this one, because no other patterns are */
            /* dependent upon this node.                     */
            /*===============================================*/

            patternPtr = upperLevel;
            upperLevel = patternPtr->lastLevel;

            if (upperLevel == nullptr) { FindAndSetDeftemplatePatternNetwork(theEnv, patternPtr, nullptr); }
            else {
                if (upperLevel->header.selector) {
                    RemoveHashedPatternNode(theEnv, upperLevel, patternPtr, patternPtr->networkTest->type, patternPtr->networkTest->value);
                }

                upperLevel->nextLevel = nullptr;
                if (upperLevel->header.stopNode) upperLevel = nullptr;
            }

            RemoveHashedExpression(theEnv, patternPtr->networkTest);
            RemoveHashedExpression(theEnv, patternPtr->header.rightHash);
            rtn_struct(theEnv, factPatternNode, patternPtr);
        } else if (upperLevel->leftNode != nullptr) {
            /*====================================================*/
            /* Pattern node has another pattern node which must   */
            /* be checked preceding it.  Remove the pattern node, */
            /* but do not detach any nodes above this one.        */
            /*====================================================*/

            patternPtr = upperLevel;

            if ((patternPtr->lastLevel != nullptr) &&
                (patternPtr->lastLevel->header.selector)) {
                RemoveHashedPatternNode(theEnv, patternPtr->lastLevel, patternPtr, patternPtr->networkTest->type,
                                        patternPtr->networkTest->value);
            }

            upperLevel->leftNode->rightNode = upperLevel->rightNode;
            if (upperLevel->rightNode != nullptr) { upperLevel->rightNode->leftNode = upperLevel->leftNode; }

            RemoveHashedExpression(theEnv, patternPtr->networkTest);
            RemoveHashedExpression(theEnv, patternPtr->header.rightHash);
            rtn_struct(theEnv, factPatternNode, patternPtr);
            upperLevel = nullptr;
        } else {
            /*====================================================*/
            /* Pattern node has no pattern node preceding it, but */
            /* does have one succeeding it. Remove the pattern    */
            /* node, but do not detach any nodes above this one.  */
            /*====================================================*/

            patternPtr = upperLevel;
            upperLevel = upperLevel->lastLevel;
            if (upperLevel == nullptr) { FindAndSetDeftemplatePatternNetwork(theEnv, patternPtr, patternPtr->rightNode); }
            else {
                if (upperLevel->header.selector) {
                    RemoveHashedPatternNode(theEnv, upperLevel, patternPtr, patternPtr->networkTest->type, patternPtr->networkTest->value);
                }

                upperLevel->nextLevel = patternPtr->rightNode;
            }
            patternPtr->rightNode->leftNode = nullptr;

            RemoveHashedExpression(theEnv, patternPtr->networkTest);
            RemoveHashedExpression(theEnv, patternPtr->header.rightHash);
            rtn_struct(theEnv, factPatternNode, patternPtr);
            upperLevel = nullptr;
        }
    }
}


/**************************************************************/
/* DestroyFactPatternNetwork: Deallocates the data structures */
/*   associated with a fact pattern network.                  */
/**************************************************************/
void DestroyFactPatternNetwork(
        Environment *theEnv,
        struct factPatternNode *thePattern) {
    struct factPatternNode *patternPtr;

    if (thePattern == nullptr) return;

    while (thePattern != nullptr) {
        patternPtr = thePattern->rightNode;

        DestroyFactPatternNetwork(theEnv, thePattern->nextLevel);

        DestroyAlphaMemory(theEnv, &thePattern->header, false);

        if ((thePattern->lastLevel != nullptr) &&
            (thePattern->lastLevel->header.selector)) {
            RemoveHashedPatternNode(theEnv, thePattern->lastLevel, thePattern, thePattern->networkTest->type,
                                    thePattern->networkTest->value);
        }

        rtn_struct(theEnv, factPatternNode, thePattern);

        thePattern = patternPtr;
    }
}


/***********************************************************/
/* FindAndSetDeftemplatePatternNetwork: When a deftemplate */
/*   pattern is detached from the fact pattern network, it */
/*   is not possible to directly detach the link from the  */
/*   deftemplate to the pattern network (it is a one way   */
/*   link). Therefore if the top most pointer to a         */
/*   deftemplates pattern network must be changed, it is   */
/*   necessary to search the list of deftemplates to find  */
/*   the appropriate one to modify.                        */
/***********************************************************/
static void FindAndSetDeftemplatePatternNetwork(
        Environment *theEnv,
        struct factPatternNode *rootNode,
        struct factPatternNode *newRootNode) {
    Deftemplate *theDeftemplate;
    Defmodule *theModule;

    /*=======================================================*/
    /* Save the current module since we will be changing it. */
    /*=======================================================*/

    SaveCurrentModule(theEnv);

    /*=======================================================*/
    /* Loop through every module looking for the deftemplate */
    /* associated with the specified root node.              */
    /*=======================================================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*======================================================*/
        /* Set the current module to the module being examined. */
        /*======================================================*/

        SetCurrentModule(theEnv, theModule);

        /*======================================================*/
        /* Loop through every deftemplate in the current module */
        /* searching for the deftemplate associated with the    */
        /* specified root node.                                 */
        /*======================================================*/

        for (theDeftemplate = GetNextDeftemplate(theEnv, nullptr);
             theDeftemplate != nullptr;
             theDeftemplate = GetNextDeftemplate(theEnv, theDeftemplate)) {
            /*===========================================================*/
            /* When the associated deftemplate is found, change its root */
            /* node from the current value to the new value. Restore the */
            /* current module before leaving this routine.               */
            /*===========================================================*/

            if (theDeftemplate->patternNetwork == rootNode) {
                RestoreCurrentModule(theEnv);
                theDeftemplate->patternNetwork = newRootNode;
                return;
            }
        }
    }

    /*========================================================*/
    /* If the deftemplate wasn't found, then we're presumably */
    /* we're in the the middle of a clear and the deftemplate */
    /* has already been deleted so there's no need to update  */
    /* the links to the fact pattern network.                 */
    /*========================================================*/

    RestoreCurrentModule(theEnv);
}

/***************************************************************/
/* ClearPatternMatches: Clears the fact list of all pointers   */
/*   which point to a specific pattern.  The pointers are used */
/*   to remember which patterns were matched by a fact to      */
/*   make retraction easier.  When a rule is excised, the      */
/*   pointers need to be removed.                              */
/***************************************************************/
static void ClearPatternMatches(
        Environment *theEnv,
        struct factPatternNode *patternPtr) {
    Fact *theFact;
    struct patternMatch *lastMatch, *theMatch;

    /*===========================================*/
    /* Loop through every fact in the fact list. */
    /*===========================================*/

    for (theFact = GetNextFact(theEnv, nullptr);
         theFact != nullptr;
         theFact = GetNextFact(theEnv, theFact)) {
        /*========================================*/
        /* Loop through every match for the fact. */
        /*========================================*/

        lastMatch = nullptr;
        theMatch = (patternMatch *) theFact->list;

        while (theMatch != nullptr) {
            /*================================================*/
            /* If the match is for the pattern being deleted, */
            /* then remove the match.                         */
            /*================================================*/

            if (theMatch->matchingPattern == (patternNodeHeader *) patternPtr) {
                if (lastMatch == nullptr) {
                    /*=====================================*/
                    /* Remove the first match of the fact. */
                    /*=====================================*/

                    theFact->list = theMatch->next;
                    rtn_struct(theEnv, patternMatch, theMatch);
                    theMatch = (patternMatch *) theFact->list;
                } else {
                    /*===================================*/
                    /* Remove a match for the fact which */
                    /* follows the first match.          */
                    /*===================================*/

                    lastMatch->next = theMatch->next;
                    rtn_struct(theEnv, patternMatch, theMatch);
                    theMatch = lastMatch->next;
                }
            }

                /*====================================================*/
                /* If the match is not for the pattern being deleted, */
                /* then move on to the next match for the fact.       */
                /*====================================================*/

            else {
                lastMatch = theMatch;
                theMatch = theMatch->next;
            }
        }
    }
}

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */



