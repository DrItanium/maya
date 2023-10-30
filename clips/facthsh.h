   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.50  08/31/23            */
   /*                                                     */
   /*                 FACT HASHING MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
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
/*            UDF redesign.                                  */
/*                                                           */
/*            Modify command preserves fact id and address.  */
/*                                                           */
/*            Assert returns duplicate fact. FALSE is now    */
/*            returned only if an error occurs.              */
/*                                                           */
/*      6.50: Support for data driven backward chaining.     */
/*                                                           */
/*************************************************************/

#ifndef _H_facthsh

#pragma once

#define _H_facthsh

#include "entities.h"

typedef struct factHashEntry FactHashEntry;

struct factHashEntry
  {
   Fact *theFact;
   FactHashEntry *next;
  };

#define SIZE_FACT_HASH 16231

   void                           AddHashedFact(Environment *,Fact *,size_t);
   bool                           RemoveHashedFact(Environment *,Fact *);
   size_t                         HandleFactDuplication(Environment *,Fact *,Fact **,long long);
   bool                           GetFactDuplication(Environment *);
   bool                           SetFactDuplication(Environment *,bool);
   void                           InitializeFactHashTable(Environment *);
   void                           ShowFactHashTableCommand(Environment *,UDFContext *,UDFValue *);
   size_t                         HashFact(Fact *);
   bool                           FactWillBeAsserted(Environment *,Fact *);
   size_t                         HandleGoalDuplication(Environment *,Fact *,Fact **);
   Fact                          *FactExists(Environment *,Fact *,size_t);

#endif /* _H_facthsh */


