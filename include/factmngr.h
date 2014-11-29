   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  07/25/14            */
   /*                                                     */
   /*              FACTS MANAGER HEADER FILE              */
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
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Removed unused global variables.               */
/*                                                           */
/*************************************************************/

#ifndef _H_factmngr

#define _H_factmngr

struct fact;

#ifndef _H_facthsh
#include "facthsh.h"
#endif
#ifndef _H_conscomp
#include "conscomp.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif
#include "multifld.h"
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

struct fact
  {
   struct patternEntity factHeader;
   struct deftemplate *whichDeftemplate;
   void *list;
   long long factIndex;
   unsigned long hashValue;
   unsigned int garbage : 1;
   struct fact *previousFact;
   struct fact *nextFact;
   struct fact *previousTemplateFact;
   struct fact *nextTemplateFact;
   struct multifield theProposition;
  };
  
#define FACTS_DATA 3

struct factsData
  {
   int ChangeToFactList;
#if DEBUGGING_FUNCTIONS
   unsigned WatchFacts;
#endif
   struct fact DummyFact;
   struct fact *GarbageFacts;
   struct fact *LastFact;
   struct fact *FactList;
   long long NextFactIndex;
   struct callFunctionItemWithArg* ListOfAssertFunctions;
   struct callFunctionItemWithArg* ListOfRetractFunctions;
   struct callFunctionItemWithArg* ListOfModifyFunctions;
   unsigned long NumberOfFacts;
   struct patternEntityRecord  FactInfo;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct deftemplate *CurrentDeftemplate;
#endif
#if DEFRULE_CONSTRUCT && (! RUN_TIME) && DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER
   struct CodeGeneratorItem *FactCodeItem;
#endif
   struct factHashEntry **FactHashTable;
   unsigned long FactHashTableSize;
   intBool FactDuplication;
#if DEFRULE_CONSTRUCT
   struct fact             *CurrentPatternFact;
   struct multifieldMarker *CurrentPatternMarks;
#endif
   long LastModuleIndex;
  };
  
#define FactData(theEnv) ((struct factsData *) GetEnvironmentData(theEnv,FACTS_DATA))

#ifdef LOCALE
#undef LOCALE
#endif
#ifdef _FACTMNGR_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                          *EnvAssert(void *,void *);
   LOCALE void                          *EnvAssertString(void *,const char *);
   LOCALE struct fact                   *EnvCreateFact(void *,void *);
   LOCALE void                           EnvDecrementFactCount(void *,void *);
   LOCALE long long                      EnvFactIndex(void *,void *);
   LOCALE intBool                        EnvGetFactSlot(void *,void *,const char *,DATA_OBJECT *);
   LOCALE void                           PrintFactWithIdentifier(void *,const char *,struct fact *);
   LOCALE void                           PrintFact(void *,const char *,struct fact *,int,int);
   LOCALE void                           PrintFactIdentifierInLongForm(void *,const char *,void *);
   LOCALE intBool                        EnvRetract(void *,void *);
   LOCALE void                           RemoveAllFacts(void *);
   LOCALE struct fact                   *CreateFactBySize(void *,unsigned);
   LOCALE void                           FactInstall(void *,struct fact *);
   LOCALE void                           FactDeinstall(void *,struct fact *);
   LOCALE void                          *EnvGetNextFact(void *,void *);
   LOCALE void                          *GetNextFactInScope(void *theEnv,void *);
   LOCALE void                           EnvGetFactPPForm(void *,char *,unsigned,void *);
   LOCALE int                            EnvGetFactListChanged(void *);
   LOCALE void                           EnvSetFactListChanged(void *,int);
   LOCALE unsigned long                  GetNumberOfFacts(void *);
   LOCALE void                           InitializeFacts(void *);
   LOCALE struct fact                   *FindIndexedFact(void *,long long);
   LOCALE void                           EnvIncrementFactCount(void *,void *);
   LOCALE void                           PrintFactIdentifier(void *,const char *,void *);
   LOCALE void                           DecrementFactBasisCount(void *,void *);
   LOCALE void                           IncrementFactBasisCount(void *,void *);
   LOCALE void                           ReturnFact(void *,struct fact *);
   LOCALE void                           MatchFactFunction(void *,void *);
   LOCALE intBool                        EnvPutFactSlot(void *,void *,const char *,DATA_OBJECT *);
   LOCALE intBool                        EnvAssignFactSlotDefaults(void *,void *);
   LOCALE intBool                        CopyFactSlotValues(void *,void *,void *);
   LOCALE intBool                        DeftemplateSlotDefault(void *,struct deftemplate *,
                                                                struct templateSlot *,DATA_OBJECT *,int);
   LOCALE intBool                        EnvAddAssertFunction(void *,const char *,
                                                                    void (*)(void *,void *),int);
   LOCALE intBool                        EnvAddAssertFunctionWithContext(void *,const char *,
                                                                               void (*)(void *,void *),int,void *);
   LOCALE intBool                        EnvRemoveAssertFunction(void *,const char *);

   LOCALE intBool                        EnvAddRetractFunction(void *,const char *,
                                                                    void (*)(void *,void *),int);
   LOCALE intBool                        EnvAddRetractFunctionWithContext(void *,const char *,
                                                                               void (*)(void *,void *),int,void *);
   LOCALE intBool                        EnvRemoveRetractFunction(void *,const char *);


   LOCALE intBool                        EnvAddModifyFunction(void *,const char *,
                                                                    void (*)(void *,void *,void *),int);
   LOCALE intBool                        EnvAddModifyFunctionWithContext(void *,const char *,
                                                                               void (*)(void *,void *,void *),int,void *);
   LOCALE intBool                        EnvRemoveModifyFunction(void *,const char *);




#if ALLOW_ENVIRONMENT_GLOBALS
 
   LOCALE intBool                        AddAssertFunction(const char *,void (*)(void *,void *),int);
   LOCALE intBool                        AddModifyFunction(const char *,void (*)(void *,void *,void *),int);
   LOCALE intBool                        AddRetractFunction(const char *,void (*)(void *,void *),int);
   LOCALE void                          *Assert(void *);
   LOCALE void                          *AssertString(const char *);
   LOCALE intBool                        AssignFactSlotDefaults(void *,void *);
   LOCALE struct fact                   *CreateFact(void *);
   LOCALE void                           DecrementFactCount(void *);
   LOCALE long long                      FactIndex(void *);
   LOCALE int                            GetFactListChanged(void *);
   LOCALE void                           GetFactPPForm(char *,unsigned,void *);
   LOCALE intBool                        GetFactSlot(void *,const char *,DATA_OBJECT *);
   LOCALE void                          *GetNextFact(void *);
   LOCALE void                           IncrementFactCount(void *);
   LOCALE intBool                        PutFactSlot(void *,const char *,DATA_OBJECT *);
   LOCALE intBool                        RemoveAssertFunction(const char *);
   LOCALE intBool                        RemoveModifyFunction(const char *);
   LOCALE intBool                        RemoveRetractFunction(const char *);
   LOCALE intBool                        Retract(void *);
   LOCALE void                           SetFactListChanged(int);
 
#endif /* ALLOW_ENVIRONMENT_GLOBALS */
 
#endif /* _H_factmngr */
 
 
 






