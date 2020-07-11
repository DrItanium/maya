/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/02/18             */
/*                                                     */
/*                  DEFINSTANCES MODULE                */
/*******************************************************/

/*************************************************************/
/* Purpose: Kernel definstances interface commands           */
/*              and routines                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            GetConstructNameAndComment API change.         */
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
/*            Removed initial-object support.                */
/*                                                           */
/*            Pretty print functions accept optional logical */
/*            name argument.                                 */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "Setup.h"

#if DEFINSTANCES_CONSTRUCT

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "DefinstancesBinaryLoadSave.h"
#endif

#include "ArgumentAccess.h"
#include "ClassCommands.h"
#include "ClassFunctions.h"
#include "Construct.h"
#include "Construct.h"
#include "Constants.h"
#include "Construct.h"
#include "Environment.h"
#include "Evaluation.h"
#include "ExternalFunctions.h"
#include "InstanceFunctions.h"
#include "InstanceParser.h"
#include "MemoryAllocation.h"
#include "DefmoduleParser.h"
#include "DefmoduleUtility.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Scanner.h"
#include "Symbol.h"
#include "Utility.h"

#include "Definstances.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define ACTIVE_RLN "active"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

static bool ParseDefinstances(const Environment&, const char *);
static CLIPSLexeme *ParseDefinstancesName(const Environment&, const char *, bool *);
static void RemoveDefinstances(const Environment&, Definstances *);
static void SaveDefinstances(const Environment&, Defmodule *, const char *, void *);

static void *AllocateModule(const Environment&);
static void ReturnModule(const Environment&, void *);
static bool ClearDefinstancesReady(const Environment&, void *);
static void CheckDefinstancesBusy(const Environment&, ConstructHeader *, void *);
static void DestroyDefinstancesAction(const Environment&, ConstructHeader *, void *);

static void ResetDefinstances(const Environment&, void *);
static void ResetDefinstancesAction(const Environment&, ConstructHeader *, void *);
static void DeallocateDefinstancesData(const Environment&);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupDefinstances
  DESCRIPTION  : Adds the definstance support routines
                   to the Kernel
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Appropriate function lists modified
  NOTES        : None
 ***************************************************/
void SetupDefinstances(
        const Environment&theEnv) {
    AllocateEnvironmentData(theEnv, DEFINSTANCES_DATA, sizeof(definstancesData), DeallocateDefinstancesData);

    DefinstancesData(theEnv)->DefinstancesModuleIndex =
            RegisterModuleItem(theEnv, "definstances",
                               AllocateModule,
                               ReturnModule,
#if BLOAD_AND_BSAVE
                               BloadDefinstancesModuleRef,
#else
                    nullptr,
#endif
                               (FindConstructFunction *) FindDefinstancesInModule);

    DefinstancesData(theEnv)->DefinstancesConstruct =
            AddConstruct(theEnv, "definstances", "definstances",
                         ParseDefinstances,
                         (FindConstructFunction *) FindDefinstances,
                         GetConstructNamePointer, GetConstructPPForm,
                         GetConstructModuleItem,
                         (GetNextConstructFunction *) GetNextDefinstances,
                         SetNextConstruct,
                         (IsConstructDeletableFunction *) DefinstancesIsDeletable,
                         (DeleteConstructFunction *) Undefinstances,
                         (FreeConstructFunction *) RemoveDefinstances
            );

    AddClearReadyFunction(theEnv, "definstances", ClearDefinstancesReady, 0, nullptr);

    AddUDF(theEnv, "undefinstances", "v", 1, 1, "y", UndefinstancesCommand, nullptr);
    AddSaveFunction(theEnv, "definstances", SaveDefinstances, 0, nullptr);

#if DEBUGGING_FUNCTIONS
    AddUDF(theEnv, "ppdefinstances", "vs", 1, 2, ";y;ldsyn", PPDefinstancesCommand, nullptr);
    AddUDF(theEnv, "list-definstances", "v", 0, 1, "y", ListDefinstancesCommand, nullptr);
#endif

    AddUDF(theEnv, "get-definstances-list", "m", 0, 1, "y", GetDefinstancesListFunction, nullptr);
    AddUDF(theEnv, "definstances-module", "y", 1, 1, "y", GetDefinstancesModuleCommand, nullptr);

    AddResetFunction(theEnv, "definstances", ResetDefinstances, 0, nullptr);

#if BLOAD_AND_BSAVE
    SetupDefinstancesBload(theEnv);
#endif

}

/*******************************************************/
/* DeallocateDefinstancesData: Deallocates environment */
/*    data for the definstances construct.             */
/*******************************************************/
static void DeallocateDefinstancesData(
        const Environment&theEnv) {
    struct definstancesModule *theModuleItem;
    Defmodule *theModule;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) return;
#endif

    DoForAllConstructs(theEnv, DestroyDefinstancesAction, DefinstancesData(theEnv)->DefinstancesModuleIndex, false, nullptr);

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        theModuleItem = (definstancesModule *)
                GetModuleItem(theEnv, theModule,
                              DefinstancesData(theEnv)->DefinstancesModuleIndex);
        rtn_struct(theEnv, definstancesModule, theModuleItem);
    }
}

/*****************************************************/
/* DestroyDefinstancesAction: Action used to remove  */
/*   definstances as a result of DestroyEnvironment. */
/*****************************************************/
static void DestroyDefinstancesAction(
        const Environment&theEnv,
        ConstructHeader *theConstruct,
        void *buffer) {
#if MAC_XCD
#pragma unused(buffer)
#endif
    struct definstances *theDefinstances = (definstances *) theConstruct;

    if (theDefinstances == nullptr) return;

    ReturnPackedExpression(theEnv, theDefinstances->mkinstance);

    DestroyConstructHeader(theEnv, &theDefinstances->header);

    rtn_struct(theEnv, definstances, theDefinstances);
}

/***********************************************************
  NAME         : GetNextDefinstances
  DESCRIPTION  : Finds first or next definstances
  INPUTS       : The address of the current definstances
  RETURNS      : The address of the next definstances
                   (nullptr if none)
  SIDE EFFECTS : None
  NOTES        : If ptr == nullptr, the first definstances
                    is returned.
 ***********************************************************/
Definstances *GetNextDefinstances(
        const Environment&theEnv,
        Definstances *theDefinstances) {
    return (Definstances *) GetNextConstructItem(theEnv, &theDefinstances->header,
                                                 DefinstancesData(theEnv)->DefinstancesModuleIndex);
}

/***************************************************
  NAME         : FindDefinstances
  DESCRIPTION  : Looks up a definstance construct
                   by name-string
  INPUTS       : The symbolic name
  RETURNS      : The definstance address, or nullptr
                    if not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
Definstances *FindDefinstances(
        const Environment&theEnv,
        const char *name) {
    return (Definstances *) FindNamedConstructInModuleOrImports(theEnv, name, DefinstancesData(theEnv)->DefinstancesConstruct);
}

/***************************************************
  NAME         : FindDefinstancesInModule
  DESCRIPTION  : Looks up a definstance construct
                   by name-string
  INPUTS       : The symbolic name
  RETURNS      : The definstance address, or nullptr
                    if not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
Definstances *FindDefinstancesInModule(
        const Environment&theEnv,
        const char *name) {
    return (Definstances *) FindNamedConstructInModule(theEnv, name, DefinstancesData(theEnv)->DefinstancesConstruct);
}

/***************************************************
  NAME         : DefinstancesIsDeletable
  DESCRIPTION  : Determines if a definstances
                   can be deleted
  INPUTS       : Address of the definstances
  RETURNS      : True if deletable, false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
bool DefinstancesIsDeletable(
        Definstances *theDefinstances) {
    const Environment&theEnv = theDefinstances->header.env;

    if (!ConstructsDeletable(theEnv)) { return false; }

    return (theDefinstances->busy == 0) ? true : false;
}

/***********************************************************
  NAME         : UndefinstancesCommand
  DESCRIPTION  : Removes a definstance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstance deallocated
  NOTES        : H/L Syntax : (undefinstances <name> | *)
 ***********************************************************/
void UndefinstancesCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UndefconstructCommand(context, "undefinstances", DefinstancesData(theEnv)->DefinstancesConstruct);
}

/*****************************************************************
  NAME         : GetDefinstancesModuleCommand
  DESCRIPTION  : Determines to which module a definstances belongs
  INPUTS       : None
  RETURNS      : The symbolic name of the module
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (definstances-module <defins-name>)
 *****************************************************************/
void GetDefinstancesModuleCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    returnValue->value = GetConstructModuleCommand(context, "definstances-module", DefinstancesData(theEnv)->DefinstancesConstruct);
}

/***********************************************************
  NAME         : Undefinstances
  DESCRIPTION  : Removes a definstance
  INPUTS       : Address of definstances to remove
  RETURNS      : True if successful,
                 false otherwise
  SIDE EFFECTS : Definstance deallocated
  NOTES        : None
 ***********************************************************/
bool Undefinstances(
        Definstances *theDefinstances,
        const Environment&allEnv) {
    Environment theEnv;

    if (theDefinstances == nullptr) {
        theEnv = allEnv;
        return Undefconstruct(theEnv, nullptr, DefinstancesData(theEnv)->DefinstancesConstruct);
    } else {
        theEnv = theDefinstances->header.env;
        return Undefconstruct(theEnv, &theDefinstances->header, DefinstancesData(theEnv)->DefinstancesConstruct);
    }
}

#if DEBUGGING_FUNCTIONS

/***************************************************************
  NAME         : PPDefinstancesCommand
  DESCRIPTION  : Prints out the pretty-print form of a definstance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (ppdefinstances <name>)
 ***************************************************************/
void PPDefinstancesCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    PPConstructCommand(context, "ppdefinstances", DefinstancesData(theEnv)->DefinstancesConstruct, returnValue);
}

/***************************************************
  NAME         : ListDefinstancesCommand
  DESCRIPTION  : Displays all definstances names
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances name sprinted
  NOTES        : H/L Interface
 ***************************************************/
void ListDefinstancesCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    ListConstructCommand(context, DefinstancesData(theEnv)->DefinstancesConstruct);
}

/***************************************************
  NAME         : ListDefinstances
  DESCRIPTION  : Displays all definstances names
  INPUTS       : 1) The logical name of the output
                 2) The module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances names printed
  NOTES        : C Interface
 ***************************************************/
void ListDefinstances(
        const Environment&theEnv,
        const char *logicalName,
        Defmodule *theModule) {
    ListConstruct(theEnv, DefinstancesData(theEnv)->DefinstancesConstruct, logicalName, theModule);
}

#endif

/****************************************************************
  NAME         : GetDefinstancesListFunction
  DESCRIPTION  : Groups all definstances names into
                 a multifield list
  INPUTS       : A data object buffer to hold
                 the multifield result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : H/L Syntax: (get-definstances-list [<module>])
 ****************************************************************/
void GetDefinstancesListFunction(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    GetConstructListFunction(context, returnValue, DefinstancesData(theEnv)->DefinstancesConstruct);
}

/***************************************************************
  NAME         : GetDefinstancesList
  DESCRIPTION  : Groups all definstances names into
                 a multifield list
  INPUTS       : 1) A data object buffer to hold
                    the multifield result
                 2) The module from which to obtain definstances
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : External C access
 ***************************************************************/
void GetDefinstancesList(
        const Environment&theEnv,
        CLIPSValue *returnValue,
        Defmodule *theModule) {
    UDFValue result;

    GetConstructList(theEnv, &result, DefinstancesData(theEnv)->DefinstancesConstruct, theModule);
    NormalizeMultifield(theEnv, &result);
    returnValue->value = result.value;
}

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*********************************************************************
  NAME         : ParseDefinstances
  DESCRIPTION  : Parses and allocates a definstances construct
  INPUTS       : The logical name of the input source
  RETURNS      : False if no errors, true otherwise
  SIDE EFFECTS : Definstances parsed and created
  NOTES        : H/L Syntax :

                 (definstances  <name> [active] [<comment>]
                    <instance-definition>+)

                 <instance-definition> ::=
                    (<instance-name> of <class-name> <slot-override>*)

                 <slot-override> ::= (<slot-name> <value-expression>*)
 *********************************************************************/
static bool ParseDefinstances(
        const Environment&theEnv,
        const char *readSource) {
    CLIPSLexeme *dname;
    FunctionDefinition *mkinsfcall;
    Expression *mkinstance, *mkbot = nullptr;
    Definstances *dobj;
    bool active;

    SetPPBufferStatus(theEnv, true);
    FlushPPBuffer(theEnv);
    SetIndentDepth(theEnv, 3);
    SavePPBuffer(theEnv, "(definstances ");

#if BLOAD_AND_BSAVE
    if ((Bloaded(theEnv)) && (!ConstructData(theEnv)->CheckSyntaxMode)) {
        CannotLoadWithBloadMessage(theEnv, "definstances");
        return true;
    }
#endif
    dname = ParseDefinstancesName(theEnv, readSource, &active);
    if (dname == nullptr)
        return true;

    dobj = get_struct(theEnv, definstances);
    InitializeConstructHeader(theEnv, "definstances", DEFINSTANCES, &dobj->header, dname);
    dobj->busy = 0;
    dobj->mkinstance = nullptr;
    if (active)
        mkinsfcall = FindFunction(theEnv, "active-make-instance");
    else
        mkinsfcall = FindFunction(theEnv, "make-instance");
    while (DefclassData(theEnv)->ObjectParseToken.tknType == LEFT_PARENTHESIS_TOKEN) {
        mkinstance = GenConstant(theEnv, UNKNOWN_VALUE, mkinsfcall);
        mkinstance = ParseInitializeInstance(theEnv, mkinstance, readSource);
        if (mkinstance == nullptr) {
            ReturnExpression(theEnv, dobj->mkinstance);
            rtn_struct(theEnv, definstances, dobj);
            return true;
        }
        if (ExpressionContainsVariables(mkinstance, false)) {
            LocalVariableErrorMessage(theEnv, "definstances");
            ReturnExpression(theEnv, mkinstance);
            ReturnExpression(theEnv, dobj->mkinstance);
            rtn_struct(theEnv, definstances, dobj);
            return true;
        }
        if (mkbot == nullptr)
            dobj->mkinstance = mkinstance;
        else
            GetNextArgument(mkbot) = mkinstance;
        mkbot = mkinstance;
        GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
        PPBackup(theEnv);
        PPCRAndIndent(theEnv);
        SavePPBuffer(theEnv, DefclassData(theEnv)->ObjectParseToken.printForm);
    }

    if (DefclassData(theEnv)->ObjectParseToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        ReturnExpression(theEnv, dobj->mkinstance);
        rtn_struct(theEnv, definstances, dobj);
        SyntaxErrorMessage(theEnv, "definstances");
        return true;
    } else {
        if (ConstructData(theEnv)->CheckSyntaxMode) {
            ReturnExpression(theEnv, dobj->mkinstance);
            rtn_struct(theEnv, definstances, dobj);
            return false;
        }
#if DEBUGGING_FUNCTIONS
        if (!GetConserveMemory(theEnv)) {
            if (dobj->mkinstance != nullptr)
                PPBackup(theEnv);
            PPBackup(theEnv);
            SavePPBuffer(theEnv, ")\n");
            SetDefinstancesPPForm(theEnv, dobj, CopyPPBuffer(theEnv));
        }
#endif
        mkinstance = dobj->mkinstance;
        dobj->mkinstance = PackExpression(theEnv, mkinstance);
        ReturnExpression(theEnv, mkinstance);
        IncrementLexemeCount(GetDefinstancesNamePointer(theEnv, dobj));
        ExpressionInstall(theEnv, dobj->mkinstance);
    }

    AddConstructToModule(&dobj->header);
    return false;
}

/*************************************************************
  NAME         : ParseDefinstancesName
  DESCRIPTION  : Parses definstance name and optional comment
                 and optional "active" keyword
  INPUTS       : 1) The logical name of the input source
                 2) Buffer to hold flag indicating if
                    definstances should cause pattern-matching
                    to occur during slot-overrides
  RETURNS      : Address of name symbol, or
                   nullptr if there was an error
  SIDE EFFECTS : Token after name or comment is scanned
  NOTES        : Assumes "(definstances" has already
                   been scanned.
 *************************************************************/
static CLIPSLexeme *ParseDefinstancesName(
        const Environment&theEnv,
        const char *readSource,
        bool *active) {
    CLIPSLexeme *dname;

    *active = false;
    dname = GetConstructNameAndComment(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken, "definstances",
                                       (FindConstructFunction *) FindDefinstancesInModule,
                                       (DeleteConstructFunction *) Undefinstances, "@",
                                       true, false, true, false);
    if (dname == nullptr)
        return nullptr;

    if ((DefclassData(theEnv)->ObjectParseToken.tknType != SYMBOL_TOKEN) ? false :
        (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, ACTIVE_RLN) == 0)) {
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, " ");
        SavePPBuffer(theEnv, DefclassData(theEnv)->ObjectParseToken.printForm);
        PPCRAndIndent(theEnv);
        GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
        *active = true;
    }
    if (DefclassData(theEnv)->ObjectParseToken.tknType == STRING_TOKEN) {
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, " ");
        SavePPBuffer(theEnv, DefclassData(theEnv)->ObjectParseToken.printForm);
        PPCRAndIndent(theEnv);
        GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
    }
    return (dname);
}

/**************************************************************
  NAME         : RemoveDefinstances
  DESCRIPTION  : Deallocates and removes a definstance construct
  INPUTS       : The definstance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Existing definstance construct deleted
  NOTES        : Assumes busy count of definstance is 0
 **************************************************************/
static void RemoveDefinstances(
        const Environment&theEnv,
        Definstances *theDefinstances) {
    ReleaseLexeme(theEnv, theDefinstances->header.name);
    ExpressionDeinstall(theEnv, theDefinstances->mkinstance);
    ReturnPackedExpression(theEnv, theDefinstances->mkinstance);
    SetDefinstancesPPForm(theEnv, theDefinstances, nullptr);
    ClearUserDataList(theEnv, theDefinstances->header.usrData);
    rtn_struct(theEnv, definstances, theDefinstances);
}

/***************************************************
  NAME         : SaveDefinstances
  DESCRIPTION  : Prints pretty print form of
                   definstances to specified output
  INPUTS       : The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void SaveDefinstances(
        const Environment&theEnv,
        Defmodule *theModule,
        const char *logName,
        void *context) {
    SaveConstruct(theEnv, theModule, logName, DefinstancesData(theEnv)->DefinstancesConstruct);
}

/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of definstances for a new module
  INPUTS       : None
  RETURNS      : The new definstances module
  SIDE EFFECTS : Definstances module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule(
        const Environment&theEnv) {
    return (void *) get_struct(theEnv, definstancesModule);
}

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a definstances module and
                 all associated definstances
  INPUTS       : The definstances module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and definstances deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
        const Environment&theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (defmoduleItemHeader *) theItem, DefinstancesData(theEnv)->DefinstancesConstruct);
    rtn_struct(theEnv, definstancesModule, theItem);
}

/***************************************************
  NAME         : ClearDefinstancesReady
  DESCRIPTION  : Determines if it is safe to
                 remove all definstances
                 Assumes *all* constructs will be
                 deleted
  INPUTS       : None
  RETURNS      : True if all definstances can
                 be deleted, false otherwise
  SIDE EFFECTS : None
  NOTES        : Used by (clear) and (bload)
 ***************************************************/
static bool ClearDefinstancesReady(
        const Environment&theEnv,
        void *context) {
    bool flagBuffer = true;

    DoForAllConstructs(theEnv, CheckDefinstancesBusy, DefinstancesData(theEnv)->DefinstancesModuleIndex,
                       false, &flagBuffer);
    return (flagBuffer);
}

/***************************************************
  NAME         : CheckDefinstancesBusy
  DESCRIPTION  : Determines if a definstances is
                 in use or not
  INPUTS       : 1) The definstances
                 2) A buffer to set to 0 if the
                    the definstances is busy
  RETURNS      : Nothing useful
  SIDE EFFECTS : Buffer set to 0 if definstances
                 busy
  NOTES        : The flag buffer is not modified
                 if definstances is not busy
                 (assumed to be initialized to 1)
 ***************************************************/
static void CheckDefinstancesBusy(
        const Environment&theEnv,
        ConstructHeader *theDefinstances,
        void *userBuffer) {
#if MAC_XCD
#pragma unused(theEnv)
#endif

    if (((Definstances *) theDefinstances)->busy > 0) { *((bool *) userBuffer) = false; }
}

/***************************************************
  NAME         : ResetDefinstances
  DESCRIPTION  : Calls EvaluateExpression for each of
                   the make-instance calls in all
                   of the definstances constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : All instances in the definstances
                   are evaluated (and created if
                   there are no errors)
                 Any previously existing instances
                 are deleted first.
  NOTES        : None
 ***************************************************/
static void ResetDefinstances(
        const Environment&theEnv,
        void *context) {
    DoForAllConstructs(theEnv, ResetDefinstancesAction, DefinstancesData(theEnv)->DefinstancesModuleIndex, true, nullptr);
}

/***************************************************
  NAME         : ResetDefinstancesAction
  DESCRIPTION  : Performs all the make-instance
                 calls in a definstances
  INPUTS       : 1) The definstances
                 2) User data buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instances created
  NOTES        : None
 ***************************************************/
static void ResetDefinstancesAction(
        const Environment&theEnv,
        ConstructHeader *vDefinstances,
        void *userBuffer) {
#if MAC_XCD
#pragma unused(userBuffer)
#endif
    Definstances *theDefinstances = (Definstances *) vDefinstances;
    Expression *theExp;
    UDFValue temp;

    SaveCurrentModule(theEnv);
    SetCurrentModule(theEnv, vDefinstances->whichModule->theModule);
    theDefinstances->busy++;
    for (theExp = theDefinstances->mkinstance;
         theExp != nullptr;
         theExp = GetNextArgument(theExp)) {
        EvaluateExpression(theEnv, theExp, &temp);
        if (EvaluationData(theEnv)->HaltExecution ||
            (temp.value == FalseSymbol(theEnv))) {
            RestoreCurrentModule(theEnv);
            theDefinstances->busy--;
            return;
        }
    }
    theDefinstances->busy--;
    RestoreCurrentModule(theEnv);
}

/*##################################*/
/* Additional Environment Functions */
/*##################################*/

const char *DefinstancesName(
        Definstances *theDefinstances) {
    return GetConstructNameString(&theDefinstances->header);
}

const char *DefinstancesPPForm(
        Definstances *theDefinstances) {
    return GetConstructPPForm(&theDefinstances->header);
}

void SetDefinstancesPPForm(
        const Environment&theEnv,
        Definstances *theDefinstances,
        const char *thePPForm) {
    SetConstructPPForm(theEnv, &theDefinstances->header, thePPForm);
}

const char *DefinstancesModule(
        Definstances *theDefinstances) {
    return GetConstructModuleName(&theDefinstances->header);
}

CLIPSLexeme *GetDefinstancesNamePointer(
        const Environment&theEnv,
        Definstances *theDefinstances) {
    return GetConstructNamePointer(&theDefinstances->header);
}

const char *DefinstancesModuleName(
        const Environment&theEnv,
        Definstances *theDefinstances) {
    return GetConstructModuleName(&theDefinstances->header);
}

#endif


