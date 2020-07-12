/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  02/19/20            */
/*                                                     */
/*                 DEFRULE HEADER FILE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defrule primitive functions such   */
/*   as allocating and deallocating, traversing, and finding */
/*   defrule data structures.                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE and                   */
/*            LOGICAL_DEPENDENCIES compilation flags.        */
/*                                                           */
/*            Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            compilation flag.                              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*            Added EnvGetDisjunctCount and                  */
/*            EnvGetNthDisjunct functions.                   */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
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
/*************************************************************/

#ifndef _H_ruledef

#pragma once

#define _H_ruledef

#define GetDisjunctIndex(r) (r->header.bsaveID)

#include "Construct.h"
#include "Expression.h"
#include "Network.h"
#include "Defrule.h"
#include "Symbol.h"
#include "Agenda.h"
#include "Constraint.h"
#include "Construct.h"
#include "Evaluation.h"
#include "Defmodule.h"
struct joinNode;
struct Defrule {
public:
    using Ptr = std::shared_ptr<Defrule>;
public:
    ConstructHeader header;
    int salience;
    unsigned short localVarCnt;
    unsigned int complexity: 11;
    bool afterBreakpoint: 1;
    bool watchActivation: 1;
    bool watchFiring: 1;
    bool autoFocus: 1;
    bool executing: 1;
    std::shared_ptr<Expression> dynamicSalience;
    std::shared_ptr<Expression> actions;
    std::shared_ptr<joinNode> logicalJoin;
    std::shared_ptr<joinNode> lastJoin;
    std::shared_ptr<Defrule> disjunct;
};



struct defruleModule {
    struct defmoduleItemHeader header;
    SalienceGroup::Ptr groupings;
    Activation::Ptr agenda;
};

#ifndef ALPHA_MEMORY_HASH_SIZE
#define ALPHA_MEMORY_HASH_SIZE       63559L
#endif

constexpr auto DEFRULE_DATA = 16;

struct defruleData : public EnvironmentModule {
    std::shared_ptr<Construct> DefruleConstruct;
    unsigned DefruleModuleIndex;
    unsigned long long CurrentEntityTimeTag;
    std::shared_ptr<alphaMemoryHash>* AlphaMemoryTable;
    bool BetaMemoryResizingFlag;
    std::shared_ptr<joinLink> RightPrimeJoins;
    std::shared_ptr<joinLink> LeftPrimeJoins;

#if DEBUGGING_FUNCTIONS
    bool WatchRules;
    int DeletedRuleDebugFlags;
#endif
#if DEVELOPER
    bool WatchRuleAnalysis;
#endif
};
RegisterEnvironmentModule(defruleData, DEFRULE_DATA, Defrule);

#define GetPreviousJoin(theJoin) \
   (((theJoin)->joinFromTheRight) ? \
    ((joinNode *) (theJoin)->rightSideEntryStructure) : \
    ((theJoin)->lastLevel))
#define GetPatternForJoin(theJoin) \
   (((theJoin)->joinFromTheRight) ? \
    nullptr : \
    ((theJoin)->rightSideEntryStructure))

void InitializeDefrules(const Environment&);
Defrule::Ptr FindDefrule(const Environment&, const char *);
Defrule::Ptr FindDefruleInModule(const Environment&, const char *);
Defrule::Ptr GetNextDefrule(const Environment&, Defrule::Ptr );
struct defruleModule *GetDefruleModuleItem(const Environment&, Defmodule *);
bool DefruleIsDeletable(Defrule::Ptr );
#if BLOAD_AND_BSAVE
void AddBetaMemoriesToJoin(const Environment&, struct joinNode *);
#endif
long GetDisjunctCount(const Environment&, Defrule::Ptr );
Defrule::Ptr GetNthDisjunct(const Environment&, Defrule::Ptr , long);
const char *DefruleModule(Defrule::Ptr );
const char *DefruleName(Defrule::Ptr );
const char *DefrulePPForm(Defrule::Ptr );

#endif /* _H_ruledef */


