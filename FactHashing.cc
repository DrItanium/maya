/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  12/30/16             */
/*                                                     */
/*                 FACT HASHING MODULE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for maintaining a fact hash    */
/*   table so that duplication of facts can quickly be       */
/*   determined.                                             */
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
/*      6.30: Fact hash table is resizable.                  */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added FactWillBeAsserted.                      */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Modify command preserves fact id and address.  */
/*                                                           */
/*            Assert returns duplicate fact. FALSE is now    */
/*            returned only if an error occurs.              */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstdlib>

#include "Setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include "Constants.h"
#include "Environment.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "Router.h"
#include "SystemDependency.h"
#include "Utility.h"

#include "LogicalDependencies.h"


/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static Fact *FactExists(const Environment::Ptr&, Fact *, size_t);
static struct factHashEntry **CreateFactHashTable(const Environment::Ptr&, size_t);
static void ResizeFactHashTable(const Environment::Ptr&);
static void ResetFactHashTable(const Environment::Ptr&);

/************************************************/
/* HashFact: Returns the hash value for a fact. */
/************************************************/
size_t HashFact(
        Fact *theFact) {
    size_t count = 0;

    /*============================================*/
    /* Get a hash value for the deftemplate name. */
    /*============================================*/

    count += theFact->whichDeftemplate->header.name->bucket * 73981;

    /*=================================================*/
    /* Add in the hash value for the rest of the fact. */
    /*=================================================*/

    count += HashMultifield(&theFact->theProposition, 0);

    /*================================*/
    /* Make sure the hash value falls */
    /* in the appropriate range.      */
    /*================================*/

    theFact->hashValue = (unsigned long) count;

    /*========================*/
    /* Return the hash value. */
    /*========================*/

    return count;
}

/**********************************************/
/* FactExists: Determines if a specified fact */
/*   already exists in the fact hash table.   */
/**********************************************/
static Fact *FactExists(
        const Environment::Ptr&theEnv,
        Fact *theFact,
        size_t hashValue) {
    struct factHashEntry *theFactHash;

    hashValue = (hashValue % FactData(theEnv)->FactHashTableSize);

    for (theFactHash = FactData(theEnv)->FactHashTable[hashValue];
         theFactHash != nullptr;
         theFactHash = theFactHash->next) {
        if (theFact->hashValue != theFactHash->theFact->hashValue) { continue; }

        if ((theFact->whichDeftemplate == theFactHash->theFact->whichDeftemplate) ?
            MultifieldsEqual(&theFact->theProposition,
                             &theFactHash->theFact->theProposition) : false) { return (theFactHash->theFact); }
    }

    return nullptr;
}

/************************************************************/
/* AddHashedFact: Adds a fact entry to the fact hash table. */
/************************************************************/
void AddHashedFact(
        const Environment::Ptr&theEnv,
        Fact *theFact,
        size_t hashValue) {
    struct factHashEntry *newhash, *temp;

    if (FactData(theEnv)->NumberOfFacts > FactData(theEnv)->FactHashTableSize) { ResizeFactHashTable(theEnv); }

    newhash = get_struct(theEnv, factHashEntry);
    newhash->theFact = theFact;

    hashValue = (hashValue % FactData(theEnv)->FactHashTableSize);

    temp = FactData(theEnv)->FactHashTable[hashValue];
    FactData(theEnv)->FactHashTable[hashValue] = newhash;
    newhash->next = temp;
}

/******************************************/
/* RemoveHashedFact: Removes a fact entry */
/*   from the fact hash table.            */
/******************************************/
bool RemoveHashedFact(
        const Environment::Ptr&theEnv,
        Fact *theFact) {
    size_t hashValue;
    struct factHashEntry *hptr, *prev;

    hashValue = HashFact(theFact);
    hashValue = (hashValue % FactData(theEnv)->FactHashTableSize);

    for (hptr = FactData(theEnv)->FactHashTable[hashValue], prev = nullptr;
         hptr != nullptr;
         hptr = hptr->next) {
        if (hptr->theFact == theFact) {
            if (prev == nullptr) {
                FactData(theEnv)->FactHashTable[hashValue] = hptr->next;
                rtn_struct(theEnv, factHashEntry, hptr);
                if (FactData(theEnv)->NumberOfFacts == 1) { ResetFactHashTable(theEnv); }
                return true;
            } else {
                prev->next = hptr->next;
                rtn_struct(theEnv, factHashEntry, hptr);
                if (FactData(theEnv)->NumberOfFacts == 1) { ResetFactHashTable(theEnv); }
                return true;
            }
        }
        prev = hptr;
    }

    return false;
}

/****************************************************/
/* FactWillBeAsserted: Determines if a fact will be */
/*   asserted based on the duplication settings.    */
/****************************************************/
bool FactWillBeAsserted(
        const Environment::Ptr&theEnv,
        Fact *theFact) {
    Fact *tempPtr;
    size_t hashValue;

    if (FactData(theEnv)->FactDuplication) return true;

    hashValue = HashFact(theFact);

    tempPtr = FactExists(theEnv, theFact, hashValue);
    return tempPtr == nullptr;

}

/*****************************************************/
/* HandleFactDuplication: Determines if a fact to be */
/*   added to the fact-list is a duplicate entry and */
/*   takes appropriate action based on the current   */
/*   setting of the fact-duplication flag.           */
/*****************************************************/
size_t HandleFactDuplication(
        const Environment::Ptr&theEnv,
        Fact *theFact,
        Fact **duplicate,
        long long reuseIndex) {
    size_t hashValue;
    *duplicate = nullptr;

    hashValue = HashFact(theFact);

    if (FactData(theEnv)->FactDuplication) { return hashValue; }

    *duplicate = FactExists(theEnv, theFact, hashValue);
    if (*duplicate == nullptr) return hashValue;

    if (reuseIndex == 0) { ReturnFact(theEnv, theFact); }
    else {
        theFact->nextFact = FactData(theEnv)->GarbageFacts;
        FactData(theEnv)->GarbageFacts = theFact;
        UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
        theFact->garbage = true;
    }

    AddLogicalDependencies(theEnv, (PatternEntity *) *duplicate, true);

    return 0;
}

/*******************************************/
/* GetFactDuplication: C access routine    */
/*   for the get-fact-duplication command. */
/*******************************************/
bool GetFactDuplication(
        const Environment::Ptr&theEnv) {
    return FactData(theEnv)->FactDuplication;
}

/*******************************************/
/* SetFactDuplication: C access routine    */
/*   for the set-fact-duplication command. */
/*******************************************/
bool SetFactDuplication(
        const Environment::Ptr&theEnv,
        bool value) {
    bool ov;

    ov = FactData(theEnv)->FactDuplication;
    FactData(theEnv)->FactDuplication = value;
    return ov;
}

/**************************************************/
/* InitializeFactHashTable: Initializes the table */
/*   entries in the fact hash table to nullptr.      */
/**************************************************/
void InitializeFactHashTable(
        const Environment::Ptr&theEnv) {
    FactData(theEnv)->FactHashTable = CreateFactHashTable(theEnv, SIZE_FACT_HASH);
    FactData(theEnv)->FactHashTableSize = SIZE_FACT_HASH;
}

/*******************************************************************/
/* CreateFactHashTable: Creates and initializes a fact hash table. */
/*******************************************************************/
static struct factHashEntry **CreateFactHashTable(
        const Environment::Ptr&theEnv,
        size_t tableSize) {
    unsigned long i;
    struct factHashEntry **theTable;

    theTable = (factHashEntry **)
            gm2(theEnv, sizeof(factHashEntry *) * tableSize);

    if (theTable == nullptr) ExitRouter(theEnv, EXIT_FAILURE);

    for (i = 0; i < tableSize; i++) theTable[i] = nullptr;

    return theTable;
}

/************************/
/* ResizeFactHashTable: */
/************************/
static void ResizeFactHashTable(
        const Environment::Ptr&theEnv) {
    unsigned long i, newSize, newLocation;
    struct factHashEntry **theTable, **newTable;
    struct factHashEntry *theEntry, *nextEntry;

    theTable = FactData(theEnv)->FactHashTable;

    newSize = (FactData(theEnv)->FactHashTableSize * 2) + 1;
    newTable = CreateFactHashTable(theEnv, newSize);

    /*========================================*/
    /* Copy the old entries to the new table. */
    /*========================================*/

    for (i = 0; i < FactData(theEnv)->FactHashTableSize; i++) {
        theEntry = theTable[i];
        while (theEntry != nullptr) {
            nextEntry = theEntry->next;

            newLocation = theEntry->theFact->hashValue % newSize;
            theEntry->next = newTable[newLocation];
            newTable[newLocation] = theEntry;

            theEntry = nextEntry;
        }
    }

    /*=====================================================*/
    /* Replace the old hash table with the new hash table. */
    /*=====================================================*/

    rm(theEnv, theTable, sizeof(factHashEntry *) * FactData(theEnv)->FactHashTableSize);
    FactData(theEnv)->FactHashTableSize = newSize;
    FactData(theEnv)->FactHashTable = newTable;
}

/***********************/
/* ResetFactHashTable: */
/***********************/
static void ResetFactHashTable(
        const Environment::Ptr&theEnv) {
    struct factHashEntry **newTable;

    /*=============================================*/
    /* Don't reset the table unless the hash table */
    /* has been expanded from its original size.   */
    /*=============================================*/

    if (FactData(theEnv)->FactHashTableSize == SIZE_FACT_HASH) { return; }

    /*=======================*/
    /* Create the new table. */
    /*=======================*/

    newTable = CreateFactHashTable(theEnv, SIZE_FACT_HASH);

    /*=====================================================*/
    /* Replace the old hash table with the new hash table. */
    /*=====================================================*/

    rm(theEnv, FactData(theEnv)->FactHashTable, sizeof(factHashEntry *) * FactData(theEnv)->FactHashTableSize);
    FactData(theEnv)->FactHashTableSize = SIZE_FACT_HASH;
    FactData(theEnv)->FactHashTable = newTable;
}

#if DEVELOPER

/****************************************************/
/* ShowFactHashTableCommand: Displays the number of */
/*   entries in each slot of the fact hash table.   */
/****************************************************/
void ShowFactHashTableCommand(
  const Environment::Ptr&theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   unsigned long i, count;
   struct factHashEntry *theEntry;
   char buffer[20];

   for (i = 0; i < FactData(theEnv)->FactHashTableSize; i++)
     {
      for (theEntry =  FactData(theEnv)->FactHashTable[i], count = 0;
           theEntry != nullptr;
           theEntry = theEntry->next)
        { count++; }

      if (count != 0)
        {
         gensprintf(buffer,"%4lu: %4d\n",i,count);
         WriteString(theEnv,STDOUT,buffer);
        }
     }
  }

#endif /* DEVELOPER */

#endif /* DEFTEMPLATE_CONSTRUCT */

