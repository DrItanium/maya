/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*                 AGENDA HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*   Provides functionality for examining, manipulating,     */
/*   adding, and removing activations from the agenda.       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES and     */
/*            DYNAMIC_SALIENCE compilation flags.            */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EnvGetActivationBasisPPForm function.    */
/*                                                           */
/*      6.30: Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*            Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*************************************************************/

#ifndef _H_agenda

#pragma once

#define _H_agenda

#include "Environment.h"
#include "Defrule.h"
#include "Symbol.h"
#include "Match.h"

enum SalienceEvaluationType {
    WHEN_DEFINED,
    WHEN_ACTIVATED,
    EVERY_CYCLE
};

constexpr auto MAX_DEFRULE_SALIENCE =  10000;
constexpr auto MIN_DEFRULE_SALIENCE = -10000;

/*******************/
/* DATA STRUCTURES */
/*******************/
struct PartialMatch;
struct Defrule;
struct Activation {
public:
    using Ptr = std::shared_ptr<Activation>;
private:
    std::shared_ptr<Defrule> theRule;
    std::shared_ptr<PartialMatch> basis;
    int salience;
    unsigned long long timetag;
    int randomID;
    Ptr prev;
    Ptr next;
public:
    void printActivation(const Environment::Ptr& theEnv, const std::string& logicalName);
    auto getRule() const noexcept { return theRule; }
    void setRule(std::shared_ptr<Defrule> value) noexcept { theRule = value; }
    auto getBasis() const noexcept { return basis; }
    void setBasis(std::shared_ptr<PartialMatch>value) noexcept { basis = value; }
    constexpr auto getSalience() const noexcept { return salience; }
    void setSalience(int value) noexcept { salience = value; }
    constexpr auto getTimetag() const noexcept { return timetag; }
    void setTimetag(unsigned long long value) noexcept { timetag = value; }
    constexpr auto getRandomID() const noexcept { return randomID; }
    void setRandomID(int value) noexcept { randomID = value; }
    void setPrevious(Ptr value) noexcept { prev = value; }
    auto getPrevious() const noexcept { return prev; }
    void setNext(Ptr value) noexcept { next = value; }
    auto getNext() const noexcept { return next; }
    void getPPForm(StringBuilder*) noexcept;
};

struct SalienceGroup {
public:
    using Ptr = std::shared_ptr<SalienceGroup>;
private:
    int salience;
    Activation::Ptr first;
    Activation::Ptr last;
    Ptr next;
    Ptr prev;
public:
    constexpr auto getSalience() const noexcept { return salience; }
    void setSalience(int value) noexcept { salience = value; }
    void setFirst(Activation::Ptr value) noexcept { first = value; }
    auto getFirst() const noexcept { return first; }
    void setLast(Activation::Ptr value) noexcept { last = value; }
    auto getLast() const noexcept { return last; }
    void setPrevious(Ptr value) noexcept { prev = value; }
    auto getPrevious() const noexcept { return prev; }
    void setNext(Ptr value) noexcept { next = value; }
    auto getNext() const noexcept { return next; }
};

#include "ConflictResolutionStrategy.h"

constexpr size_t AGENDA_DATA = 17;

struct AgendaModule : public EnvironmentModule{
#if DEBUGGING_FUNCTIONS
    bool WatchActivations = false;
    constexpr auto shouldWatchActivations() const noexcept { return WatchActivations; }
#endif
private:
    unsigned long NumberOfActivations;
    unsigned long long CurrentTimetag;
    bool AgendaChanged;
    SalienceEvaluationType SalienceEvaluation = SalienceEvaluationType::WHEN_DEFINED;
    StrategyType Strategy = StrategyType::DEPTH_STRATEGY;
public:
    ~AgendaModule() override = default;
    constexpr auto getNumberOfActivations() const noexcept { return NumberOfActivations; }
    void setNumberOfActivations(unsigned long value) noexcept { NumberOfActivations = value; }
    void incrementActivationCount() noexcept { ++NumberOfActivations; }
    void decrementActivationCount() noexcept { --NumberOfActivations; }
    constexpr auto getCurrentTimetag() const noexcept { return CurrentTimetag; }
    void setCurrentTimetag(unsigned long long value) noexcept { CurrentTimetag = value; }
    auto newTimetag() noexcept {
        auto output = CurrentTimetag;
        ++CurrentTimetag;
        return output;
    }
    constexpr auto agendaHasChanged() const noexcept { return AgendaChanged; }
    void markAgendaHasChanged(bool value = true) noexcept { AgendaChanged = value; }
    constexpr auto getSalienceEvaluation() const noexcept { return SalienceEvaluation; }
    void setSalienceEvaluation(SalienceEvaluationType value) noexcept { SalienceEvaluation = value; }
    constexpr auto getStrategy() const noexcept { return Strategy; }
    void setStrategy(StrategyType value) noexcept { Strategy = value; }
};
RegisterEnvironmentModule(AgendaModule, AGENDA_DATA, Agenda);

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/
using DefrulePtr = std::shared_ptr<Defrule>;
using PartialMatchPtr = std::shared_ptr<PartialMatch>;
void AddActivation(const Environment::Ptr& theEnv, DefrulePtr , PartialMatchPtr);
void ClearRuleFromAgenda(const Environment::Ptr&, DefrulePtr );
Activation::Ptr GetNextActivation(const Environment::Ptr&, Activation::Ptr );
const char *ActivationRuleName(Activation::Ptr );
void GetActivationBasisPPForm(const Environment::Ptr&, char *, size_t, Activation::Ptr );
bool MoveActivationToTop(const Environment::Ptr&, Activation::Ptr );
void DeleteActivation(Activation::Ptr );
bool DetachActivation(const Environment::Ptr&, Activation::Ptr );
void DeleteAllActivations(Defmodule *);
void Agenda(const Environment::Ptr&, const char *, Defmodule *);
void RemoveActivation(const Environment::Ptr&, Activation::Ptr , bool, bool);
void RemoveAllActivations(const Environment::Ptr&);
SalienceEvaluationType GetSalienceEvaluation(const Environment::Ptr&);
SalienceEvaluationType SetSalienceEvaluation(const Environment::Ptr&, SalienceEvaluationType);
void RefreshAgenda(Defmodule *);
void RefreshAllAgendas(const Environment::Ptr&);
void ReorderAgenda(Defmodule *);
void ReorderAllAgendas(const Environment::Ptr&);
void InitializeAgenda(const Environment::Ptr&);
void SetSalienceEvaluationCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetSalienceEvaluationCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RefreshAgendaCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RefreshCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void Refresh(DefrulePtr );
#if DEBUGGING_FUNCTIONS
void AgendaCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
#endif

#endif






