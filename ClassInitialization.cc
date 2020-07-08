/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  04/03/19             */
/*                                                     */
/*               CLASS INITIALIZATION MODULE           */
/*******************************************************/

/**************************************************************/
/* Purpose: Defclass Initialization Routines                  */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Dantes                                       */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.23: Corrected compilation errors for files          */
/*            generated by constructs-to-c. DR0861            */
/*                                                            */
/*      6.24: Added allowed-classes slot facet.               */
/*                                                            */
/*            Converted INSTANCE_PATTERN_MATCHING to          */
/*            DEFRULE_CONSTRUCT.                              */
/*                                                            */
/*            Corrected code to remove run-time program       */
/*            compiler warning.                               */
/*                                                            */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior  */
/*            (MAC_MCW, IBM_MCW) are no longer supported.     */
/*                                                            */
/*            Changed integer type/precision.                 */
/*                                                            */
/*            Support for hashed alpha memories.              */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
/*            Changed find construct functionality so that    */
/*            imported modules are search when locating a     */
/*            named construct.                                */
/*                                                            */
/*      6.31: Optimization of slot ID creation previously    */
/*            provided by NewSlotNameID function.            */
/*                                                           */
/*            Changed allocation of multifield slot default  */
/*            from ephemeral to explicit deallocation.       */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.        */
/*                                                            */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed initial-object support.                 */
/*                                                            */
/*            Pretty print functions accept optional logical */
/*            name argument.                                 */
/*                                                           */
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "Setup.h"


#include <stdio.h>

#include "ClassCommands.h"
#include "ClassExamination.h"
#include "ClassFunctions.h"
#include "ClassInfo.h"
#include "ClassParser.h"
#include "Construct.h"
#include "Construct.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "InstanceCommand.h"
#include "MemoryAllocation.h"
#include "DefmoduleParser.h"
#include "DefmoduleUtility.h"
#include "DefmessageHandlerCommands.h"
#include "Watch.h"

#if DEFINSTANCES_CONSTRUCT
#include "Definstances.h"
#endif

#if INSTANCE_SET_QUERIES
#include "InstanceQuery.h"
#endif

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "ObjectBinaryLoadSave.h"
#endif

#include "ObjectPatternMatcher.h"
#include "InferenceEngineObjectAccessRotuines.h"
#include "ObjectReteMatch.h"

#include "ClassInitialization.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define SUPERCLASS_RLN       "is-a"
#define NAME_RLN             "name"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static void SetupDefclasses(Environment *);
static void DeallocateDefclassData(Environment *);

static void DestroyDefclassAction(Environment *, ConstructHeader *, void *);
static Defclass *AddSystemClass(Environment *, const char *, Defclass *);
static void *AllocateModule(Environment *);
static void ReturnModule(Environment *, void *);

#if DEFMODULE_CONSTRUCT
static void UpdateDefclassesScope(Environment *, void *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/**********************************************************
  NAME         : SetupObjectSystem
  DESCRIPTION  : Initializes all COOL constructs, functions,
                   and data structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : COOL initialized
  NOTES        : Order of setup calls is important
 **********************************************************/
void SetupObjectSystem(
        Environment *theEnv) {
    EntityRecord defclassEntityRecord = {"DEFCLASS_PTR", DEFCLASS_PTR, 1, 0, 0,
                                         nullptr, nullptr, nullptr, nullptr, nullptr,
                                         (EntityBusyCountFunction *) DecrementDefclassBusyCount,
                                         (EntityBusyCountFunction *) IncrementDefclassBusyCount,
                                         nullptr, nullptr, nullptr, nullptr, nullptr};

    AllocateEnvironmentData(theEnv, DEFCLASS_DATA, sizeof(struct defclassData), nullptr);
    AddEnvironmentCleanupFunction(theEnv, "defclasses", DeallocateDefclassData, -500);

    memcpy(&DefclassData(theEnv)->DefclassEntityRecord, &defclassEntityRecord, sizeof(struct entityRecord));

    DefclassData(theEnv)->newSlotID = 2; // IS_A and NAME assigned 0 and 1

    DefclassData(theEnv)->ClassDefaultsModeValue = CONVENIENCE_MODE;
    DefclassData(theEnv)->ISA_SYMBOL = CreateSymbol(theEnv, SUPERCLASS_RLN);
    IncrementLexemeCount(DefclassData(theEnv)->ISA_SYMBOL);
    DefclassData(theEnv)->NAME_SYMBOL = CreateSymbol(theEnv, NAME_RLN);
    IncrementLexemeCount(DefclassData(theEnv)->NAME_SYMBOL);

    SetupDefclasses(theEnv);
    SetupInstances(theEnv);
    SetupMessageHandlers(theEnv);

#if DEFINSTANCES_CONSTRUCT
    SetupDefinstances(theEnv);
#endif

#if INSTANCE_SET_QUERIES
    SetupQuery(theEnv);
#endif

#if BLOAD_AND_BSAVE
    SetupObjectsBload(theEnv);
#endif

    SetupObjectPatternStuff(theEnv);
}

/***************************************************/
/* DeallocateDefclassData: Deallocates environment */
/*    data for the defclass construct.             */
/***************************************************/
static void DeallocateDefclassData(
        Environment *theEnv) {
    SLOT_NAME *tmpSNPPtr, *nextSNPPtr;
    int i;
    struct defclassModule *theModuleItem;
    Defmodule *theModule;
    bool bloaded = false;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) bloaded = true;
#endif

    /*=============================*/
    /* Destroy all the defclasses. */
    /*=============================*/

    if (!bloaded) {
        DoForAllConstructs(theEnv, DestroyDefclassAction, DefclassData(theEnv)->DefclassModuleIndex, false, nullptr);

        for (theModule = GetNextDefmodule(theEnv, nullptr);
             theModule != nullptr;
             theModule = GetNextDefmodule(theEnv, theModule)) {
            theModuleItem = (struct defclassModule *)
                    GetModuleItem(theEnv, theModule,
                                  DefclassData(theEnv)->DefclassModuleIndex);
            rtn_struct(theEnv, defclassModule, theModuleItem);
        }
    }

    /*==========================*/
    /* Remove the class tables. */
    /*==========================*/

    if (!bloaded) {
        if (DefclassData(theEnv)->ClassIDMap != nullptr) {
            genfree(theEnv, DefclassData(theEnv)->ClassIDMap, DefclassData(theEnv)->AvailClassID * sizeof(Defclass *));
        }
    }

    if (DefclassData(theEnv)->ClassTable != nullptr) {
        genfree(theEnv, DefclassData(theEnv)->ClassTable, sizeof(Defclass *) * CLASS_TABLE_HASH_SIZE);
    }

    /*==============================*/
    /* Free up the slot name table. */
    /*==============================*/

    if (!bloaded) {
        for (i = 0; i < SLOT_NAME_TABLE_HASH_SIZE; i++) {
            tmpSNPPtr = DefclassData(theEnv)->SlotNameTable[i];

            while (tmpSNPPtr != nullptr) {
                nextSNPPtr = tmpSNPPtr->nxt;
                rtn_struct(theEnv, slotName, tmpSNPPtr);
                tmpSNPPtr = nextSNPPtr;
            }
        }
    }

    if (DefclassData(theEnv)->SlotNameTable != nullptr) {
        genfree(theEnv, DefclassData(theEnv)->SlotNameTable, sizeof(SLOT_NAME *) * SLOT_NAME_TABLE_HASH_SIZE);
    }
}

/*********************************************************/
/* DestroyDefclassAction: Action used to remove defclass */
/*   as a result of DestroyEnvironment.                  */
/*********************************************************/
static void DestroyDefclassAction(
        Environment *theEnv,
        ConstructHeader *theConstruct,
        void *buffer) {
#if MAC_XCD
#pragma unused(buffer)
#endif
    Defclass *theDefclass = (Defclass *) theConstruct;

    if (theDefclass == nullptr) return;

    DestroyDefclass(theEnv, theDefclass);
}

/***************************************************************
  NAME         : CreateSystemClasses
  DESCRIPTION  : Creates the built-in system classes
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : System classes inserted in the
                   class hash table
  NOTES        : The binary/load save indices for the primitive
                   types (integer, float, symbol and string,
                   multifield, external-address and fact-address)
                   are very important.  Need to be able to refer
                   to types with the same index regardless of
                   whether the object system is installed or
                   not.  Thus, the bsave/blaod indices of these
                   classes match their integer codes.
                WARNING!!: Assumes no classes exist yet!
 ***************************************************************/
void CreateSystemClasses(
        Environment *theEnv,
        void *context) {
    Defclass *user, *any, *primitive, *number, *lexeme, *address, *instance;

    /* ===================================
       Add canonical slot name entries for
       the is-a and name fields - used for
       object patterns
       =================================== */
    AddSlotName(theEnv, DefclassData(theEnv)->ISA_SYMBOL, ISA_ID, true);
    AddSlotName(theEnv, DefclassData(theEnv)->NAME_SYMBOL, NAME_ID, true);

    DefclassData(theEnv)->newSlotID = 2; // IS_A and NAME assigned 0 and 1

    /* =========================================================
       Bsave Indices for non-primitive classes start at 9
                Object is 9, Primitive is 10, Number is 11,
                Lexeme is 12, Address is 13, and Instance is 14.
       because: float = 0, integer = 1, symbol = 2, string = 3,
                multifield = 4, and external-address = 5 and
                fact-address = 6, instance-adress = 7 and
                instance-name = 8.
       ========================================================= */
    any = AddSystemClass(theEnv, OBJECT_TYPE_NAME, nullptr);
    primitive = AddSystemClass(theEnv, PRIMITIVE_TYPE_NAME, any);
    user = AddSystemClass(theEnv, USER_TYPE_NAME, any);

    number = AddSystemClass(theEnv, NUMBER_TYPE_NAME, primitive);
    DefclassData(theEnv)->PrimitiveClassMap[INTEGER_TYPE] = AddSystemClass(theEnv, INTEGER_TYPE_NAME, number);
    DefclassData(theEnv)->PrimitiveClassMap[FLOAT_TYPE] = AddSystemClass(theEnv, FLOAT_TYPE_NAME, number);
    lexeme = AddSystemClass(theEnv, LEXEME_TYPE_NAME, primitive);
    DefclassData(theEnv)->PrimitiveClassMap[SYMBOL_TYPE] = AddSystemClass(theEnv, SYMBOL_TYPE_NAME, lexeme);
    DefclassData(theEnv)->PrimitiveClassMap[STRING_TYPE] = AddSystemClass(theEnv, STRING_TYPE_NAME, lexeme);
    DefclassData(theEnv)->PrimitiveClassMap[MULTIFIELD_TYPE] = AddSystemClass(theEnv, MULTIFIELD_TYPE_NAME, primitive);
    address = AddSystemClass(theEnv, ADDRESS_TYPE_NAME, primitive);
    DefclassData(theEnv)->PrimitiveClassMap[EXTERNAL_ADDRESS_TYPE] = AddSystemClass(theEnv, EXTERNAL_ADDRESS_TYPE_NAME, address);
    DefclassData(theEnv)->PrimitiveClassMap[FACT_ADDRESS_TYPE] = AddSystemClass(theEnv, FACT_ADDRESS_TYPE_NAME, address);
    instance = AddSystemClass(theEnv, INSTANCE_TYPE_NAME, primitive);
    DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE] = AddSystemClass(theEnv, INSTANCE_ADDRESS_TYPE_NAME, instance);
    DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_NAME_TYPE] = AddSystemClass(theEnv, INSTANCE_NAME_TYPE_NAME, instance);

    /* ================================================================================
        INSTANCE-ADDRESS is-a INSTANCE and ADDRESS.  The links between INSTANCE-ADDRESS
        and ADDRESS still need to be made.
        =============================================================================== */
    AddClassLink(theEnv, &DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE]->directSuperclasses, address, true, 0);
    AddClassLink(theEnv, &DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE]->allSuperclasses, address, false, 2);
    AddClassLink(theEnv, &address->directSubclasses, DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE], true, 0);

    /* =======================================================================
       The order of the class in the list MUST correspond to their type codes!
       See CONSTANT.H
       ======================================================================= */
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[FLOAT_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[INTEGER_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[SYMBOL_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[STRING_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[MULTIFIELD_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[EXTERNAL_ADDRESS_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[FACT_ADDRESS_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE]->header);
    AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_NAME_TYPE]->header);
    AddConstructToModule(&any->header);
    AddConstructToModule(&primitive->header);
    AddConstructToModule(&number->header);
    AddConstructToModule(&lexeme->header);
    AddConstructToModule(&address->header);
    AddConstructToModule(&instance->header);
    AddConstructToModule(&user->header);

    for (any = GetNextDefclass(theEnv, nullptr);
         any != nullptr;
         any = GetNextDefclass(theEnv, any))
        AssignClassID(theEnv, any);
}

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*********************************************************
  NAME         : SetupDefclasses
  DESCRIPTION  : Initializes Class Hash Table,
                   Function Parsers, and Data Structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS :
  NOTES        : None
 *********************************************************/
static void SetupDefclasses(
        Environment *theEnv) {
    InstallPrimitive(theEnv, &DefclassData(theEnv)->DefclassEntityRecord, DEFCLASS_PTR);

    DefclassData(theEnv)->DefclassModuleIndex =
            RegisterModuleItem(theEnv, "defclass",
                               AllocateModule,
                               ReturnModule,
#if BLOAD_AND_BSAVE
                               BloadDefclassModuleReference,
#else
                    nullptr,
#endif
                               (FindConstructFunction *) FindDefclassInModule);

    DefclassData(theEnv)->DefclassConstruct = AddConstruct(theEnv, "defclass", "defclasses",
                                                           ParseDefclass,
                                                           (FindConstructFunction *) FindDefclass,
                                                           GetConstructNamePointer, GetConstructPPForm,
                                                           GetConstructModuleItem,
                                                           (GetNextConstructFunction *) GetNextDefclass,
                                                           SetNextConstruct,
                                                           (IsConstructDeletableFunction *) DefclassIsDeletable,
                                                           (DeleteConstructFunction *) Undefclass,
                                                           (FreeConstructFunction *) RemoveDefclass
    );

    AddClearReadyFunction(theEnv, "defclass", InstancesPurge, 0, nullptr);

    AddClearFunction(theEnv, "defclass", CreateSystemClasses, 0, nullptr);
    InitializeClasses(theEnv);

#if DEFMODULE_CONSTRUCT
    AddPortConstructItem(theEnv, "defclass", SYMBOL_TOKEN);
    AddAfterModuleDefinedFunction(theEnv, "defclass", UpdateDefclassesScope, 0, nullptr);
#endif
    AddUDF(theEnv, "undefclass", "v", 1, 1, "y", UndefclassCommand, nullptr);

    AddSaveFunction(theEnv, "defclass", SaveDefclasses, 10, nullptr);

#if DEBUGGING_FUNCTIONS
    AddUDF(theEnv, "list-defclasses", "v", 0, 1, "y", ListDefclassesCommand, nullptr);
    AddUDF(theEnv, "ppdefclass", "vs", 1, 2, ";y;ldsyn", PPDefclassCommand, nullptr);
    AddUDF(theEnv, "describe-class", "v", 1, 1, "y", DescribeClassCommand, nullptr);
    AddUDF(theEnv, "browse-classes", "v", 0, 1, "y", BrowseClassesCommand, nullptr);
#endif

    AddUDF(theEnv, "get-defclass-list", "m", 0, 1, "y", GetDefclassListFunction, nullptr);
    AddUDF(theEnv, "superclassp", "b", 2, 2, "y", SuperclassPCommand, nullptr);
    AddUDF(theEnv, "subclassp", "b", 2, 2, "y", SubclassPCommand, nullptr);
    AddUDF(theEnv, "class-existp", "b", 1, 1, "y", ClassExistPCommand, nullptr);
    AddUDF(theEnv, "message-handler-existp", "b", 2, 3, "y", MessageHandlerExistPCommand, nullptr);
    AddUDF(theEnv, "class-abstractp", "b", 1, 1, "y", ClassAbstractPCommand, nullptr);
    AddUDF(theEnv, "class-reactivep", "b", 1, 1, "y", ClassReactivePCommand, nullptr);
    AddUDF(theEnv, "class-slots", "m", 1, 2, "y", ClassSlotsCommand, nullptr);
    AddUDF(theEnv, "class-superclasses", "m", 1, 2, "y", ClassSuperclassesCommand, nullptr);
    AddUDF(theEnv, "class-subclasses", "m", 1, 2, "y", ClassSubclassesCommand, nullptr);
    AddUDF(theEnv, "get-defmessage-handler-list", "m", 0, 2, "y", GetDefmessageHandlersListCmd, nullptr);
    AddUDF(theEnv, "slot-existp", "b", 2, 3, "y", SlotExistPCommand, nullptr);
    AddUDF(theEnv, "slot-facets", "m", 2, 2, "y", SlotFacetsCommand, nullptr);
    AddUDF(theEnv, "slot-sources", "m", 2, 2, "y", SlotSourcesCommand, nullptr);
    AddUDF(theEnv, "slot-types", "m", 2, 2, "y", SlotTypesCommand, nullptr);
    AddUDF(theEnv, "slot-allowed-values", "m", 2, 2, "y", SlotAllowedValuesCommand, nullptr);
    AddUDF(theEnv, "slot-allowed-classes", "m", 2, 2, "y", SlotAllowedClassesCommand, nullptr);
    AddUDF(theEnv, "slot-range", "m", 2, 2, "y", SlotRangeCommand, nullptr);
    AddUDF(theEnv, "slot-cardinality", "m", 2, 2, "y", SlotCardinalityCommand, nullptr);
    AddUDF(theEnv, "slot-writablep", "b", 2, 2, "y", SlotWritablePCommand, nullptr);
    AddUDF(theEnv, "slot-initablep", "b", 2, 2, "y", SlotInitablePCommand, nullptr);
    AddUDF(theEnv, "slot-publicp", "b", 2, 2, "y", SlotPublicPCommand, nullptr);
    AddUDF(theEnv, "slot-direct-accessp", "b", 2, 2, "y", SlotDirectAccessPCommand, nullptr);
    AddUDF(theEnv, "slot-default-value", "*", 2, 2, "y", SlotDefaultValueCommand, nullptr);
    AddUDF(theEnv, "defclass-module", "y", 1, 1, "y", GetDefclassModuleCommand, nullptr);
    AddUDF(theEnv, "get-class-defaults-mode", "y", 0, 0, nullptr, GetClassDefaultsModeCommand, nullptr);
    AddUDF(theEnv, "set-class-defaults-mode", "y", 1, 1, "y", SetClassDefaultsModeCommand, nullptr);

#if DEBUGGING_FUNCTIONS
    AddWatchItem(theEnv, "instances", 0, &DefclassData(theEnv)->WatchInstances, 75, DefclassWatchAccess, DefclassWatchPrint);
    AddWatchItem(theEnv, "slots", 1, &DefclassData(theEnv)->WatchSlots, 74, DefclassWatchAccess, DefclassWatchPrint);
#endif
}

/*********************************************************
  NAME         : AddSystemClass
  DESCRIPTION  : Performs all necessary allocations
                   for adding a system class
  INPUTS       : 1) The name-string of the system class
                 2) The address of the parent class
                    (nullptr if none)
  RETURNS      : The address of the new system class
  SIDE EFFECTS : Allocations performed
  NOTES        : Assumes system-class name is unique
                 Also assumes SINGLE INHERITANCE for
                   system classes to simplify precedence
                   list determination
                 Adds classes to has table but NOT to
                  class list (this is responsibility
                  of caller)
 *********************************************************/
static Defclass *AddSystemClass(
        Environment *theEnv,
        const char *name,
        Defclass *parent) {
    Defclass *sys;
    unsigned long i;
    char defaultScopeMap[1];

    sys = NewClass(theEnv, CreateSymbol(theEnv, name));
    sys->abstract = 1;
    sys->reactive = 0;
    IncrementLexemeCount(sys->header.name);
    sys->installed = 1;
    sys->system = 1;
    sys->hashTableIndex = HashClass(sys->header.name);

    AddClassLink(theEnv, &sys->allSuperclasses, sys, true, 0);
    if (parent != nullptr) {
        AddClassLink(theEnv, &sys->directSuperclasses, parent, true, 0);
        AddClassLink(theEnv, &parent->directSubclasses, sys, true, 0);
        AddClassLink(theEnv, &sys->allSuperclasses, parent, true, 0);
        for (i = 1; i < parent->allSuperclasses.classCount; i++)
            AddClassLink(theEnv, &sys->allSuperclasses, parent->allSuperclasses.classArray[i], true, 0);
    }
    sys->nxtHash = DefclassData(theEnv)->ClassTable[sys->hashTableIndex];
    DefclassData(theEnv)->ClassTable[sys->hashTableIndex] = sys;

    /* =========================================
       Add default scope maps for a system class
       There is only one module (MAIN) so far -
       which has an id of 0
       ========================================= */
    ClearBitString(defaultScopeMap, sizeof(char));
    SetBitMap(defaultScopeMap, 0);
#if DEFMODULE_CONSTRUCT
    sys->scopeMap = (CLIPSBitMap *) AddBitMap(theEnv, defaultScopeMap, sizeof(char));
    IncrementBitMapCount(sys->scopeMap);
#endif
    return (sys);
}

/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of defclasses for a new module
  INPUTS       : None
  RETURNS      : The new defclass module
  SIDE EFFECTS : Defclass module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule(
        Environment *theEnv) {
    return (void *) get_struct(theEnv, defclassModule);
}

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a defclass module and
                 all associated defclasses
  INPUTS       : The defclass module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and defclasses deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
        Environment *theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (struct defmoduleItemHeader *) theItem, DefclassData(theEnv)->DefclassConstruct);
    DeleteSlotName(theEnv, FindIDSlotNameHash(theEnv, ISA_ID));
    DeleteSlotName(theEnv, FindIDSlotNameHash(theEnv, NAME_ID));
    rtn_struct(theEnv, defclassModule, theItem);
}

#if DEFMODULE_CONSTRUCT

/***************************************************
  NAME         : UpdateDefclassesScope
  DESCRIPTION  : This function updates the scope
                 bitmaps for existing classes when
                 a new module is defined
  INPUTS       : None
  RETURNS      : Nothing
  SIDE EFFECTS : Class scope bitmaps are updated
  NOTES        : None
 ***************************************************/
static void UpdateDefclassesScope(
        Environment *theEnv,
        void *context) {
    unsigned i;
    Defclass *theDefclass;
    unsigned long newModuleID;
    unsigned int count;
    char *newScopeMap;
    unsigned short newScopeMapSize;
    const char *className;
    Defmodule *matchModule;

    newModuleID = GetCurrentModule(theEnv)->header.bsaveID;
    newScopeMapSize = (sizeof(char) * ((GetNumberOfDefmodules(theEnv) / BITS_PER_BYTE) + 1));
    newScopeMap = (char *) gm2(theEnv, newScopeMapSize);
    for (i = 0; i < CLASS_TABLE_HASH_SIZE; i++)
        for (theDefclass = DefclassData(theEnv)->ClassTable[i];
             theDefclass != nullptr;
             theDefclass = theDefclass->nxtHash) {
            matchModule = theDefclass->header.whichModule->theModule;
            className = theDefclass->header.name->contents;
            ClearBitString(newScopeMap, newScopeMapSize);
            GenCopyMemory(char, theDefclass->scopeMap->size,
                          newScopeMap, theDefclass->scopeMap->contents);
            DecrementBitMapReferenceCount(theEnv, theDefclass->scopeMap);
            if (theDefclass->system)
                SetBitMap(newScopeMap, newModuleID);
            else if (FindImportedConstruct(theEnv, "defclass", matchModule,
                                           className, &count, true, nullptr) != nullptr)
                SetBitMap(newScopeMap, newModuleID);
            theDefclass->scopeMap = (CLIPSBitMap *) AddBitMap(theEnv, newScopeMap, newScopeMapSize);
            IncrementBitMapCount(theDefclass->scopeMap);
        }
    rm(theEnv, newScopeMap, newScopeMapSize);
}

#endif

