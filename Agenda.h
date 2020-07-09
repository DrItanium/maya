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

typedef struct activation Activation;

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

struct activation {
private:
    Defrule *theRule;
    struct partialMatch *basis;
    int salience;
    unsigned long long timetag;
    int randomID;
    activation *prev;
    activation *next;
public:
    auto getRule() const noexcept { return theRule; }
    void setRule(Defrule* value) noexcept { theRule = value; }
    auto getBasis() const noexcept { return basis; }
    void setBasis(partialMatch* value) noexcept { basis = value; }
    constexpr auto getSalience() const noexcept { return salience; }
    void setSalience(int value) noexcept { salience = value; }
    constexpr auto getTimetag() const noexcept { return timetag; }
    void setTimetag(unsigned long long value) noexcept { timetag = value; }
    constexpr auto getRandomID() const noexcept { return randomID; }
    void setRandomID(int value) noexcept { randomID = value; }
    void setPrevious(activation* value) noexcept { prev = value; }
    auto getPrevious() const noexcept { return prev; }
    void setNext(activation* value) noexcept { next = value; }
    auto getNext() const noexcept { return next; }
};

struct SalienceGroup {
private:
    int salience;
    activation *first;
    activation *last;
    SalienceGroup *next;
    SalienceGroup *prev;
public:
    constexpr auto getSalience() const noexcept { return salience; }
    void setSalience(int value) noexcept { salience = value; }
    void setFirst(activation* value) noexcept { first = value; }
    auto getFirst() const noexcept { return first; }
    void setLast(activation* value) noexcept { last = value; }
    auto getLast() const noexcept { return last; }
    void setPrevious(SalienceGroup* value) noexcept { prev = value; }
    auto getPrevious() const noexcept { return prev; }
    void setNext(SalienceGroup* value) noexcept { next = value; }
    auto getNext() const noexcept { return next; }
};

#include "ConflictResolutionStrategy.h"

constexpr auto AGENDA_DATA = 17;

struct AgendaData {
#if DEBUGGING_FUNCTIONS
    bool WatchActivations;
    constexpr auto shouldWatchActivations() const noexcept { return WatchActivations; }
#endif
private:
    unsigned long NumberOfActivations;
    unsigned long long CurrentTimetag;
    bool AgendaChanged;
    SalienceEvaluationType SalienceEvaluation;
    StrategyType Strategy;
public:
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

#define AgendaData(theEnv) ((AgendaData *) GetEnvironmentData(theEnv,AGENDA_DATA))

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

void AddActivation(Environment *, Defrule *, PartialMatch *);
void ClearRuleFromAgenda(Environment *, Defrule *);
Activation *GetNextActivation(Environment *, Activation *);
const char *ActivationRuleName(Activation *);
void ActivationPPForm(Activation *, StringBuilder *);
void GetActivationBasisPPForm(Environment *, char *, size_t, Activation *);
bool MoveActivationToTop(Environment *, Activation *);
void DeleteActivation(Activation *);
bool DetachActivation(Environment *, Activation *);
void DeleteAllActivations(Defmodule *);
void Agenda(Environment *, const char *, Defmodule *);
void RemoveActivation(Environment *, Activation *, bool, bool);
void RemoveAllActivations(Environment *);
bool GetAgendaChanged(Environment *);
void SetAgendaChanged(Environment *, bool);
unsigned long GetNumberOfActivations(Environment *);
SalienceEvaluationType GetSalienceEvaluation(Environment *);
SalienceEvaluationType SetSalienceEvaluation(Environment *, SalienceEvaluationType);
void RefreshAgenda(Defmodule *);
void RefreshAllAgendas(Environment *);
void ReorderAgenda(Defmodule *);
void ReorderAllAgendas(Environment *);
void InitializeAgenda(Environment *);
void SetSalienceEvaluationCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void GetSalienceEvaluationCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void RefreshAgendaCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void RefreshCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void Refresh(Defrule *);
#if DEBUGGING_FUNCTIONS
void AgendaCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
#endif

#endif






