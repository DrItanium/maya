/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/03/19            */
/*                                                     */
/*              RETE UTILITY HEADER FILE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules.                                          */
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
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for join network changes.              */
/*                                                           */
/*            Support for using an asterick (*) to indicate  */
/*            that existential patterns are matched.         */
/*                                                           */
/*            Support for partial match changes.             */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
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
/*************************************************************/

#ifndef _H_reteutil

#pragma once

#define _H_reteutil

#include "Evaluation.h"
#include "Match.h"
#include "Network.h"
#include "RuleCommands.h"

constexpr auto NETWORK_ASSERT  = 0;
constexpr auto NETWORK_RETRACT = 1;

void PrintPartialMatch(const Environment::Ptr&, const char *, PartialMatch::Ptr);
PartialMatch *CopyPartialMatch(const Environment::Ptr&, PartialMatch *);
PartialMatch *MergePartialMatches(const Environment::Ptr&, PartialMatch *, PartialMatch *);
long IncrementPseudoFactIndex();
PartialMatch *GetAlphaMemory(const Environment::Ptr&, PatternNodeHeader *, unsigned long);
PartialMatch *GetLeftBetaMemory(joinNode *, unsigned long);
PartialMatch *GetRightBetaMemory(joinNode *, unsigned long);
void ReturnLeftMemory(const Environment::Ptr&, struct joinNode *);
void ReturnRightMemory(const Environment::Ptr&, struct joinNode *);
void DestroyBetaMemory(const Environment::Ptr&, struct joinNode *, int);
void FlushBetaMemory(const Environment::Ptr&, struct joinNode *, int);
bool BetaMemoryNotEmpty(joinNode *);
void RemoveAlphaMemoryMatches(const Environment::Ptr&, PatternNodeHeader *, PartialMatch *,
                              struct alphaMatch *);
void DestroyAlphaMemory(const Environment::Ptr&, PatternNodeHeader *, bool);
void FlushAlphaMemory(const Environment::Ptr&, PatternNodeHeader *);
void FlushAlphaBetaMemory(const Environment::Ptr&, PartialMatch *);
void DestroyAlphaBetaMemory(const Environment::Ptr&, PartialMatch *);
int GetPatternNumberFromJoin(joinNode *);
struct multifieldMarker *CopyMultifieldMarkers(const Environment::Ptr&, struct multifieldMarker *);
PartialMatch *CreateAlphaMatch(const Environment::Ptr&, void *, struct multifieldMarker *,
                                      PatternNodeHeader *, unsigned long);
void TraceErrorToRule(const Environment::Ptr&, struct joinNode *, const char *);
void InitializePatternHeader(const Environment::Ptr&, PatternNodeHeader *);
void MarkRuleNetwork(const Environment::Ptr&, bool);
void TagRuleNetwork(const Environment::Ptr&, unsigned long *, unsigned long *, unsigned long *, unsigned long *);
bool FindEntityInPartialMatch(PatternEntity *, PartialMatch *);
unsigned long ComputeRightHashValue(const Environment::Ptr&, PatternNodeHeader *);
void UpdateBetaPMLinks(const Environment::Ptr&, PartialMatch *, PartialMatch *, PartialMatch *,
                       struct joinNode *, unsigned long, int);
void UnlinkBetaPMFromNodeAndLineage(const Environment::Ptr&, struct joinNode *, PartialMatch *, int);
void UnlinkNonLeftLineage(const Environment::Ptr&, struct joinNode *, PartialMatch *, int);
PartialMatch *CreateEmptyPartialMatch(const Environment::Ptr&);
void MarkRuleJoins(joinNode *, bool);
void AddBlockedLink(PartialMatch *, PartialMatch *);
void RemoveBlockedLink(PartialMatch *);
unsigned long PrintBetaMemory(const Environment::Ptr&, const char *, struct betaMemory *, bool, const char *, Verbosity);

#endif /* _H_reteutil */



