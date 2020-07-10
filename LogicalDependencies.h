/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*          LOGICAL DEPENDENCIES HEADER FILE           */
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
/*      6.40: Removed LOCALE definition.                     */
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

#ifndef _H_lgcldpnd

#pragma once

#define _H_lgcldpnd

struct dependency {
    void *dPtr;
    struct dependency *next;
};

#include "Entities.h"
#include "Match.h"

bool AddLogicalDependencies(const Environment&, PatternEntity *, bool);
void RemoveEntityDependencies(const Environment&, PatternEntity *);
void RemovePMDependencies(const Environment&, PartialMatch *);
void DestroyPMDependencies(const Environment&, PartialMatch *);
void RemoveLogicalSupport(const Environment&, PartialMatch *);
void ForceLogicalRetractions(const Environment&);
void Dependencies(const Environment&, PatternEntity *);
void Dependents(const Environment&, PatternEntity *);
void DependenciesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DependentsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ReturnEntityDependencies(const Environment&, PatternEntity *);
PartialMatch *FindLogicalBind(joinNode *, PartialMatch *);

#endif /* _H_lgcldpnd */





