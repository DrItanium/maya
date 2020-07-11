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

void PrintPartialMatch(const Environment&, const char *, PartialMatch *);
PartialMatch *CopyPartialMatch(const Environment&, PartialMatch *);
PartialMatch *MergePartialMatches(const Environment&, PartialMatch *, PartialMatch *);
long IncrementPseudoFactIndex();
PartialMatch *GetAlphaMemory(const Environment&, struct patternNodeHeader *, unsigned long);
PartialMatch *GetLeftBetaMemory(joinNode *, unsigned long);
PartialMatch *GetRightBetaMemory(joinNode *, unsigned long);
void ReturnLeftMemory(const Environment&, struct joinNode *);
void ReturnRightMemory(const Environment&, struct joinNode *);
void DestroyBetaMemory(const Environment&, struct joinNode *, int);
void FlushBetaMemory(const Environment&, struct joinNode *, int);
bool BetaMemoryNotEmpty(joinNode *);
void RemoveAlphaMemoryMatches(const Environment&, struct patternNodeHeader *, PartialMatch *,
                              struct alphaMatch *);
void DestroyAlphaMemory(const Environment&, struct patternNodeHeader *, bool);
void FlushAlphaMemory(const Environment&, struct patternNodeHeader *);
void FlushAlphaBetaMemory(const Environment&, PartialMatch *);
void DestroyAlphaBetaMemory(const Environment&, PartialMatch *);
int GetPatternNumberFromJoin(joinNode *);
struct multifieldMarker *CopyMultifieldMarkers(const Environment&, struct multifieldMarker *);
PartialMatch *CreateAlphaMatch(const Environment&, void *, struct multifieldMarker *,
                                      struct patternNodeHeader *, unsigned long);
void TraceErrorToRule(const Environment&, struct joinNode *, const char *);
void InitializePatternHeader(const Environment&, struct patternNodeHeader *);
void MarkRuleNetwork(const Environment&, bool);
void TagRuleNetwork(const Environment&, unsigned long *, unsigned long *, unsigned long *, unsigned long *);
bool FindEntityInPartialMatch(PatternEntity *, PartialMatch *);
unsigned long ComputeRightHashValue(const Environment&, struct patternNodeHeader *);
void UpdateBetaPMLinks(const Environment&, PartialMatch *, PartialMatch *, PartialMatch *,
                       struct joinNode *, unsigned long, int);
void UnlinkBetaPMFromNodeAndLineage(const Environment&, struct joinNode *, PartialMatch *, int);
void UnlinkNonLeftLineage(const Environment&, struct joinNode *, PartialMatch *, int);
PartialMatch *CreateEmptyPartialMatch(const Environment&);
void MarkRuleJoins(joinNode *, bool);
void AddBlockedLink(partialMatch *, PartialMatch *);
void RemoveBlockedLink(partialMatch *);
unsigned long PrintBetaMemory(const Environment&, const char *, struct betaMemory *, bool, const char *, Verbosity);

#endif /* _H_reteutil */



