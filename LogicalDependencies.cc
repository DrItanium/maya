/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/25/16             */
/*                                                     */
/*             LOGICAL DEPENDENCIES MODULE             */
/*******************************************************/

/*************************************************************/
/* Purpose: Provide support routines for managing truth      */
/*   maintenance using the logical conditional element.      */
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
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
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

#include <cstdio>

#include "Setup.h"


#include "ArgumentAccess.h"
#include "Engine.h"
#include "Environment.h"
#include "Evaluation.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "Pattern.h"
#include "ReteUtility.h"
#include "Router.h"

#include "LogicalDependencies.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static struct dependency *DetachAssociatedDependencies(const Environment&, struct dependency *, void *);

/***********************************************************************/
/* AddLogicalDependencies: Adds the logical dependency links between a */
/*   data entity (such as a fact or instance) and the partial match    */
/*   which logically supports that data entity. If a data entity is    */
/*   unconditionally asserted (i.e. the global variable TheLogicalJoin */
/*   is nullptr), then existing logical support for the data entity is no */
/*   longer needed and it is removed. If a data entity is already      */
/*   unconditionally supported and that data entity is conditionally   */
/*   asserted (i.e. the global variable TheLogicalJoin is not nullptr),   */
/*   then the logical support is ignored. Otherwise, the partial match */
/*   is linked to the data entity and the data entity is linked to the */
/*   partial match. Note that the word assert is used to refer to      */
/*   creating a fact with the assert command and creating an instance  */
/*   with the make-instance command.                                   */
/***********************************************************************/
bool AddLogicalDependencies(
        const Environment&theEnv,
        struct patternEntity *theEntity,
        bool existingEntity) {
    struct partialMatch *theBinds;
    struct dependency *newDependency;

    /*==============================================*/
    /* If the rule has no logical patterns, then no */
    /* dependencies have to be established.         */
    /*==============================================*/

    if (EngineData(theEnv)->TheLogicalJoin == nullptr) {
        if (existingEntity) RemoveEntityDependencies(theEnv, theEntity);
        return true;
    } else if (existingEntity && (theEntity->dependents == nullptr)) { return true; }

    /*===========================================================*/
    /* Retrieve the partial match in the logical join associated */
    /* with activation partial match (retrieved when the run     */
    /* command was initiated). If the partial match's parent     */
    /* links have been removed, then the partial match must have */
    /* been deleted by a previous RHS action and the dependency  */
    /* link should not be added.                                 */
    /*===========================================================*/

    theBinds = EngineData(theEnv)->TheLogicalBind;
    if (theBinds == nullptr) return false;
    if ((theBinds->leftParent == nullptr) && (theBinds->rightParent == nullptr)) { return false; }

    /*==============================================================*/
    /* Add a dependency link between the partial match and the data */
    /* entity. The dependency links are stored in the partial match */
    /* behind the data entities stored in the partial match and the */
    /* activation link, if any.                                     */
    /*==============================================================*/

    newDependency = get_struct(theEnv, dependency);
    newDependency->dPtr = theEntity;
    newDependency->next = (dependency *) theBinds->dependents;
    theBinds->dependents = newDependency;

    /*================================================================*/
    /* Add a dependency link between the entity and the partialMatch. */
    /*================================================================*/

    newDependency = get_struct(theEnv, dependency);
    newDependency->dPtr = theBinds;
    newDependency->next = (dependency *) theEntity->dependents;
    theEntity->dependents = newDependency;

    /*==================================================================*/
    /* Return true to indicate that the data entity should be asserted. */
    /*==================================================================*/

    return true;
}

/************************************************************************/
/* FindLogicalBind: Finds the partial match associated with the logical */
/*   CE which will provide logical support for a data entity asserted   */
/*   from the currently executing rule. The function is called when     */
/*   creating logical support links between the data entity and         */
/*   supporting partial matches.                                        */
/************************************************************************/
struct partialMatch *FindLogicalBind(
        struct joinNode *theJoin,
        struct partialMatch *theBinds) {
    struct partialMatch *compPtr;

    /*========================================================*/
    /* Follow the parent link of the activation back through  */
    /* the join network until the join containing the logical */
    /* partial match is found. The partial match at this      */
    /* join will have the dependency link assigned to it.     */
    /*========================================================*/

    for (compPtr = theBinds;
         compPtr != nullptr;
         compPtr = compPtr->leftParent) {
        if (compPtr->owner == theJoin) { return (compPtr); }
    }

    return nullptr;
}

/*********************************************************************/
/* RemoveEntityDependencies: Removes all logical support links from  */
/*   a pattern entity that point to partial matches or other pattern */
/*   entities. Also removes the associated links from the partial    */
/*   matches or pattern entities which point back to the pattern     */
/*   entities.                                                       */
/*********************************************************************/
void RemoveEntityDependencies(
        const Environment&theEnv,
        struct patternEntity *theEntity) {
    struct dependency *fdPtr, *nextPtr, *theList;
    struct partialMatch *theBinds;

    /*===============================*/
    /* Get the list of dependencies. */
    /*===============================*/

    fdPtr = (dependency *) theEntity->dependents;

    /*========================================*/
    /* Loop through each of the dependencies. */
    /*========================================*/

    while (fdPtr != nullptr) {
        /*===============================*/
        /* Remember the next dependency. */
        /*===============================*/

        nextPtr = fdPtr->next;

        /*================================================================*/
        /* Remove the link between the data entity and the partial match. */
        /*================================================================*/

        theBinds = (partialMatch *) fdPtr->dPtr;
        theList = (dependency *) theBinds->dependents;
        theList = DetachAssociatedDependencies(theEnv, theList, theEntity);
        theBinds->dependents = theList;

        /*========================*/
        /* Return the dependency. */
        /*========================*/

        rtn_struct(theEnv, dependency, fdPtr);

        /*=================================*/
        /* Move on to the next dependency. */
        /*=================================*/

        fdPtr = nextPtr;
    }

    /*=====================================================*/
    /* Set the dependency list of the data entity to nullptr. */
    /*=====================================================*/

    theEntity->dependents = nullptr;
}

/********************************************************************/
/* ReturnEntityDependencies: Removes all logical support links from */
/*   a pattern entity. This is unidirectional. The links from the   */
/*   the partial match to the entity are not removed.               */
/********************************************************************/
void ReturnEntityDependencies(
        const Environment&theEnv,
        struct patternEntity *theEntity) {
    struct dependency *fdPtr, *nextPtr;

    fdPtr = (dependency *) theEntity->dependents;

    while (fdPtr != nullptr) {
        nextPtr = fdPtr->next;
        rtn_struct(theEnv, dependency, fdPtr);
        fdPtr = nextPtr;
    }

    theEntity->dependents = nullptr;
}

/*******************************************************************/
/* DetachAssociatedDependencies: Removes all logical support links */
/*   which pointer to a pattern entity from a list of dependencies */
/*   (which may be associated with either a partial match or       */
/*   another pattern entity). Does not remove links which point in */
/*   the other direction.                                          */
/*******************************************************************/
static struct dependency *DetachAssociatedDependencies(
        const Environment&theEnv,
        struct dependency *theList,
        void *theEntity) {
    struct dependency *fdPtr, *nextPtr, *lastPtr = nullptr;

    fdPtr = theList;

    while (fdPtr != nullptr) {
        if (fdPtr->dPtr == theEntity) {
            nextPtr = fdPtr->next;
            if (lastPtr == nullptr) theList = nextPtr;
            else lastPtr->next = nextPtr;
            rtn_struct(theEnv, dependency, fdPtr);
            fdPtr = nextPtr;
        } else {
            lastPtr = fdPtr;
            fdPtr = fdPtr->next;
        }
    }

    return (theList);
}

/**************************************************************************/
/* RemovePMDependencies: Removes all logical support links from a partial */
/*   match that point to any data entities. Also removes the associated   */
/*   links from the data entities which point back to the partial match.  */
/**************************************************************************/
void RemovePMDependencies(
        const Environment&theEnv,
        struct partialMatch *theBinds) {
    struct dependency *fdPtr, *nextPtr, *theList;
    struct patternEntity *theEntity;

    fdPtr = (dependency *) theBinds->dependents;

    while (fdPtr != nullptr) {
        nextPtr = fdPtr->next;

        theEntity = (patternEntity *) fdPtr->dPtr;

        theList = (dependency *) theEntity->dependents;
        theList = DetachAssociatedDependencies(theEnv, theList, theBinds);
        theEntity->dependents = theList;

        rtn_struct(theEnv, dependency, fdPtr);
        fdPtr = nextPtr;
    }

    theBinds->dependents = nullptr;
}

/************************************************************/
/* DestroyPMDependencies: Removes all logical support links */
/*   from a partial match that point to any data entities.  */
/************************************************************/
void DestroyPMDependencies(
        const Environment&theEnv,
        struct partialMatch *theBinds) {
    struct dependency *fdPtr, *nextPtr;

    fdPtr = (dependency *) theBinds->dependents;

    while (fdPtr != nullptr) {
        nextPtr = fdPtr->next;

        rtn_struct(theEnv, dependency, fdPtr);
        fdPtr = nextPtr;
    }

    theBinds->dependents = nullptr;
}

/************************************************************************/
/* RemoveLogicalSupport: Removes the dependency links between a partial */
/*   match and the data entities it logically supports. Also removes    */
/*   the associated links from the data entities which point back to    */
/*   the partial match by calling DetachAssociatedEntityDependencies.   */
/*   If an entity has all of its logical support removed as a result of */
/*   this procedure, the dependency link from the partial match is      */
/*   added to the list of unsupported data entities so that the entity  */
/*   will be deleted as a result of losing its logical support.         */
/************************************************************************/
void RemoveLogicalSupport(
        const Environment&theEnv,
        struct partialMatch *theBinds) {
    struct dependency *dlPtr, *tempPtr, *theList;
    struct patternEntity *theEntity;

    /*========================================*/
    /* If the partial match has no associated */
    /* dependencies, then return.             */
    /*========================================*/

    if (theBinds->dependents == nullptr) return;

    /*=======================================*/
    /* Loop through each of the dependencies */
    /* attached to the partial match.        */
    /*=======================================*/

    dlPtr = (dependency *) theBinds->dependents;

    while (dlPtr != nullptr) {
        /*===============================*/
        /* Remember the next dependency. */
        /*===============================*/

        tempPtr = dlPtr->next;

        /*==========================================================*/
        /* Determine the data entity associated with the dependency */
        /* structure and delete its dependency references to this   */
        /* partial match.                                           */
        /*==========================================================*/

        theEntity = (patternEntity *) dlPtr->dPtr;

        theList = (dependency *) theEntity->dependents;
        theList = DetachAssociatedDependencies(theEnv, theList, theBinds);
        theEntity->dependents = theList;

        /*==============================================================*/
        /* If the data entity has lost all of its logical support, then */
        /* add the dependency structure from the partial match to the   */
        /* list of unsupported data entities to be deleted. Otherwise,  */
        /* just delete the dependency structure.                        */
        /*==============================================================*/

        if (theEntity->dependents == nullptr) {
            (*theEntity->theInfo->base.incrementBusyCount)(theEnv, theEntity);
            dlPtr->next = EngineData(theEnv)->UnsupportedDataEntities;
            EngineData(theEnv)->UnsupportedDataEntities = dlPtr;
        } else { rtn_struct(theEnv, dependency, dlPtr); }

        /*==================================*/
        /* Move on to the next dependency.  */
        /*==================================*/

        dlPtr = tempPtr;
    }

    /*=====================================*/
    /* The partial match no longer has any */
    /* dependencies associated with it.    */
    /*=====================================*/

    theBinds->dependents = nullptr;
}

/********************************************************************/
/* ForceLogicalRetractions: Deletes the data entities found on the  */
/*   list of items that have lost their logical support. The delete */
/*   function associated with each data entity is called to delete  */
/*   that data entity. Calling the delete function may in turn      */
/*   add more data entities to the list of data entities which have */
/*   lost their logical support.                                    */
/********************************************************************/
void ForceLogicalRetractions(
        const Environment&theEnv) {
    struct dependency *tempPtr;
    struct patternEntity *theEntity;

    /*===================================================*/
    /* Don't reenter this function once it's called. Any */
    /* new additions to the list of items to be deleted  */
    /* as a result of losing their logical support will  */
    /* be handled properly.                              */
    /*===================================================*/

    if (EngineData(theEnv)->alreadyEntered) return;
    EngineData(theEnv)->alreadyEntered = true;

    /*=======================================================*/
    /* Continue to delete the first item on the list as long */
    /* as one exists. This is done because new items may be  */
    /* placed at the beginning of the list as other data     */
    /* entities are deleted.                                 */
    /*=======================================================*/

    while (EngineData(theEnv)->UnsupportedDataEntities != nullptr) {
        /*==========================================*/
        /* Determine the data entity to be deleted. */
        /*==========================================*/

        theEntity = (patternEntity *) EngineData(theEnv)->UnsupportedDataEntities->dPtr;

        /*================================================*/
        /* Remove the dependency structure from the list. */
        /*================================================*/

        tempPtr = EngineData(theEnv)->UnsupportedDataEntities;
        EngineData(theEnv)->UnsupportedDataEntities = EngineData(theEnv)->UnsupportedDataEntities->next;
        rtn_struct(theEnv, dependency, tempPtr);

        /*=========================*/
        /* Delete the data entity. */
        /*=========================*/

        (*theEntity->theInfo->base.decrementBusyCount)(theEnv, theEntity);
        (*theEntity->theInfo->base.deleteFunction)(theEntity, theEnv);
    }

    /*============================================*/
    /* Deletion of items on the list is complete. */
    /*============================================*/

    EngineData(theEnv)->alreadyEntered = false;
}

/****************************************************************/
/* Dependencies: C access routine for the dependencies command. */
/****************************************************************/
void Dependencies(
        const Environment&theEnv,
        struct patternEntity *theEntity) {
    struct dependency *fdPtr;

    /*=========================================*/
    /* If the data entity has no dependencies, */
    /* then print "None" and return.           */
    /*=========================================*/

    if (theEntity->dependents == nullptr) {
        WriteString(theEnv, STDOUT, "None\n");
        return;
    }

    /*============================================*/
    /* Loop through the list of the data entities */
    /* dependencies and print them.               */
    /*============================================*/

    for (fdPtr = (dependency *) theEntity->dependents;
         fdPtr != nullptr;
         fdPtr = fdPtr->next) {
        if (GetHaltExecution(theEnv)) return;
        PrintPartialMatch(theEnv, STDOUT, (partialMatch *) fdPtr->dPtr);
        WriteString(theEnv, STDOUT, "\n");
    }
}

/************************************************************/
/* Dependents: C access routine for the dependents command. */
/************************************************************/
void Dependents(
        const Environment&theEnv,
        struct patternEntity *theEntity) {
    struct patternEntity *entityPtr = nullptr;
    struct patternParser *theParser = nullptr;
    struct dependency *fdPtr;
    struct partialMatch *theBinds;
    bool found = false;

    /*=================================*/
    /* Loop through every data entity. */
    /*=================================*/

    for (GetNextPatternEntity(theEnv, &theParser, &entityPtr);
         entityPtr != nullptr;
         GetNextPatternEntity(theEnv, &theParser, &entityPtr)) {
        if (GetHaltExecution(theEnv)) return;

        /*====================================*/
        /* Loop through every dependency link */
        /* associated with the data entity.   */
        /*====================================*/

        for (fdPtr = (dependency *) entityPtr->dependents;
             fdPtr != nullptr;
             fdPtr = fdPtr->next) {
            if (GetHaltExecution(theEnv)) return;

            /*=====================================================*/
            /* If the data entity which was the argument passed to */
            /* the dependents command is contained in one of the   */
            /* partial matches of the data entity currently being  */
            /* examined, then the data entity being examined is a  */
            /* dependent. Print the data entity and then move on   */
            /* to the next data entity.                            */
            /*=====================================================*/

            theBinds = (partialMatch *) fdPtr->dPtr;
            if (FindEntityInPartialMatch(theEntity, theBinds)) {
                if (found) WriteString(theEnv, STDOUT, ",");
                (*entityPtr->theInfo->base.shortPrintFunction)(theEnv, STDOUT, entityPtr);
                found = true;
                break;
            }
        }
    }

    /*=================================================*/
    /* If no dependents were found, then print "None." */
    /* Otherwise print a carriage return after the     */
    /* list of dependents.                             */
    /*=================================================*/

    if (!found) WriteString(theEnv, STDOUT, "None\n");
    else WriteString(theEnv, STDOUT, "\n");
}

#if DEBUGGING_FUNCTIONS

/*********************************************/
/* DependenciesCommand: H/L access routine   */
/*   for the dependencies command.           */
/*********************************************/
void DependenciesCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue item;
    void *ptr;

    ptr = GetFactOrInstanceArgument(context, 1, &item);

    if (ptr == nullptr) return;

    Dependencies(theEnv, (patternEntity *) ptr);
}

/*******************************************/
/* DependentsCommand: H/L access routine   */
/*   for the dependents command.           */
/*******************************************/
void DependentsCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue item;
    void *ptr;

    ptr = GetFactOrInstanceArgument(context, 1, &item);

    if (ptr == nullptr) return;

    Dependents(theEnv, (patternEntity *) ptr);
}

#endif /* DEBUGGING_FUNCTIONS */


