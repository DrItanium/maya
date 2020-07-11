/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*                                                     */
/*******************************************************/

/**************************************************************/
/* Purpose: Procedural Code Support Routines for              */
/*          Deffunctions, Generic Function Methods,           */
/*          Message-Handlersand Rules                         */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Dantes                                       */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859   */
/*                                                            */
/*            Changed name of variable log to logName         */
/*            because of Unix compiler warnings of shadowed   */
/*            definitions.                                    */
/*                                                            */
/*      6.24: Renamed BOOLEAN macro type to intBool.          */
/*                                                            */
/*            Added pragmas to remove compilation warnings.   */
/*                                                            */
/*      6.30: Updated ENTITY_RECORD definitions to include    */
/*            additional nullptr initializers.                   */
/*                                                            */
/*            Added ReleaseProcParameters call.               */
/*                                                            */
/*            Added tracked memory calls.                     */
/*                                                            */
/*            Removed conditional code for unsupported        */
/*            compilers/operating systems (IBM_MCW,           */
/*            MAC_MCW, and IBM_TBC).                          */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
/*      6.40: Added Env prefix to GetEvaluationError and      */
/*            SetEvaluationError functions.                   */
/*                                                            */
/*            Pragma once and other inclusion changes.        */
/*                                                            */
/*            Added support for booleans with <stdbool.h>.    */
/*                                                            */
/*            Removed use of void pointers for specific       */
/*            data structures.                                */
/*                                                            */
/*            UDF redesign.                                   */
/*                                                            */
/*            Generic error message no longer printed when    */
/*            an alternate variable handling function         */
/*            generates an error.                             */
/*                                                            */
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "Setup.h"

#include <cstdio>

#include <cstdlib>
#include <ctype.h>

#include "MemoryAllocation.h"
#include "Constants.h"
#include "Environment.h"
#if DEFGLOBAL_CONSTRUCT
#include "DefglobalParser.h"
#endif
#include "Expression.h"
#include "Multifield.h"
#include "Object.h"
#include "PrettyPrint.h"
#include "ProceduralFunctionsParser.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Utility.h"

#include "ProceduralCodeSupportRoutines.h"

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */
typedef struct {
    unsigned firstFlag: 1;
    unsigned first: 15;
    unsigned secondFlag: 1;
    unsigned second: 15;
} PACKED_PROC_VAR;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void EvaluateProcParameters(const Environment&, Expression *, unsigned int, const char *, const char *);
static bool RtnProcParam(const Environment&, void *, UDFValue *);
static bool GetProcBind(const Environment&, void *, UDFValue *);
static bool PutProcBind(const Environment&, void *, UDFValue *);
static bool RtnProcWild(const Environment&, void *, UDFValue *);
static void DeallocateProceduralPrimitiveData(const Environment&);
static void ReleaseProcParameters(const Environment&);

static unsigned int FindProcParameter(CLIPSLexeme *, Expression *, CLIPSLexeme *);
static bool ReplaceProcBinds(const Environment&, Expression *,
                             int (*)(const Environment&, Expression *, void *), void *);
static Expression *CompactActions(const Environment&, Expression *);

#if (!DEFFUNCTION_CONSTRUCT) || (!DEFGENERIC_CONSTRUCT)
static bool                    EvaluateBadCall(const Environment&,void *,UDFValue *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/****************************************************
  NAME         : InstallProcedurePrimitives
  DESCRIPTION  : Installs primitive function handlers
                 for accessing parameters and local
                 variables within the bodies of
                 message-handlers, methods, rules and
                 deffunctions.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Primitive entities installed
  NOTES        : None
 ****************************************************/
void InstallProcedurePrimitives(
        const Environment&theEnv) {
    EntityRecord procParameterInfo = {"PROC_PARAM", PROC_PARAM, 0, 1, 0, nullptr, nullptr, nullptr,
                                      (EntityEvaluationFunction *) RtnProcParam,
                                      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr},
            procWildInfo = {"PROC_WILD_PARAM", PROC_WILD_PARAM, 0, 1, 0, nullptr, nullptr, nullptr,
                            (EntityEvaluationFunction *) RtnProcWild,
                            nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr},
            procGetInfo = {"PROC_GET_BIND", PROC_GET_BIND, 0, 1, 0, nullptr, nullptr, nullptr,
                           (EntityEvaluationFunction *) GetProcBind,
                           nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr},
            procBindInfo = {"PROC_BIND", PROC_BIND, 0, 1, 0, nullptr, nullptr, nullptr,
                            (EntityEvaluationFunction *) PutProcBind,
                            nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr};

#if !DEFFUNCTION_CONSTRUCT
    EntityRecord deffunctionEntityRecord =
                      { "PCALL", PCALL,0,0,1,
                        nullptr,nullptr,nullptr,
                        EvaluateBadCall,
                        nullptr,nullptr,nullptr,nullptr,nullptr,nullptr,nullptr,nullptr };
#endif
#if !DEFGENERIC_CONSTRUCT
    EntityRecord genericEntityRecord =
                      { "GCALL", GCALL,0,0,1,
                        nullptr,nullptr,nullptr,
                        EvaluateBadCall,
                        nullptr,nullptr,nullptr,nullptr,nullptr,nullptr,nullptr,nullptr };
#endif

    //AllocateEnvironmentData(theEnv, PROCEDURAL_PRIMITIVE_DATA, sizeof(proceduralPrimitiveData), DeallocateProceduralPrimitiveData);
    theEnv->allocateEnvironmentModule<proceduralPrimitiveData>();
    ProceduralPrimitiveData(theEnv)->ProcParameterInfo= procParameterInfo;
    ProceduralPrimitiveData(theEnv)->ProcWildInfo = procWildInfo;
    ProceduralPrimitiveData(theEnv)->ProcGetInfo = procGetInfo;
    ProceduralPrimitiveData(theEnv)->ProcBindInfo = procBindInfo;

    InstallPrimitive(theEnv, &ProceduralPrimitiveData(theEnv)->ProcParameterInfo, PROC_PARAM);
    InstallPrimitive(theEnv, &ProceduralPrimitiveData(theEnv)->ProcWildInfo, PROC_WILD_PARAM);
    InstallPrimitive(theEnv, &ProceduralPrimitiveData(theEnv)->ProcGetInfo, PROC_GET_BIND);
    InstallPrimitive(theEnv, &ProceduralPrimitiveData(theEnv)->ProcBindInfo, PROC_BIND);

    ProceduralPrimitiveData(theEnv)->Oldindex = UINT_MAX;

    /* ===============================================
       Make sure a default evaluation function is
       in place for deffunctions and generic functions
       in the event that a binary image containing
       these items is loaded into a configuration
       that does not support them.
       =============================================== */

#if !DEFFUNCTION_CONSTRUCT
    ProceduralPrimitiveData(theEnv)->DeffunctionEntityRecord = deffunctionEntityRecord;
    InstallPrimitive(theEnv,&ProceduralPrimitiveData(theEnv)->DeffunctionEntityRecord,PCALL);
#endif

#if !DEFGENERIC_CONSTRUCT
    ProceduralPrimitiveData(theEnv)->GenericEntityRecord=genericEntityRecord;
    InstallPrimitive(theEnv,&ProceduralPrimitiveData(theEnv)->GenericEntityRecord,GCALL);
#endif

    /* =============================================
       Install the special empty multifield to
       let callers distinguish between no parameters
       and zero-length multifield parameters
       ============================================= */
    ProceduralPrimitiveData(theEnv)->NoParamValue = CreateUnmanagedMultifield(theEnv, 0L);
    RetainMultifield(theEnv, ProceduralPrimitiveData(theEnv)->NoParamValue);
}

/**************************************************************/
/* DeallocateProceduralPrimitiveData: Deallocates environment */
/*    data for the procedural primitives functionality.       */
/**************************************************************/
static void DeallocateProceduralPrimitiveData(
        const Environment&theEnv) {
    ReturnMultifield(theEnv, ProceduralPrimitiveData(theEnv)->NoParamValue);
    ReleaseProcParameters(theEnv);
}


/************************************************************
  NAME         : ParseProcParameters
  DESCRIPTION  : Parses a parameter list for a
                  procedural routine, such as a
                  deffunction or message-handler
  INPUTS       : 1) The logical name of the input
                 2) A buffer for scanned tokens
                 3) The partial list of parameters so far
                    (can be nullptr)
                 3) A buffer for a wildcard symbol (if any)
                 4) A buffer for a minimum of parameters
                 5) A buffer for a maximum of parameters
                    (will be set to -1 if there is a wilcard)
                 6) A buffer for an error flag
                 7) The address of a function to do specialized
                    checking on a parameter (can be nullptr)
                    The function should accept a string and
                    return false if the parameter is OK, true
                    otherwise.
  RETURNS      : A list of expressions containing the
                   parameter names
  SIDE EFFECTS : Parameters parsed and expressions formed
  NOTES        : None
 ************************************************************/
Expression *ParseProcParameters(
        const Environment&theEnv,
        const char *readSource,
        struct token *tkn,
        Expression *parameterList,
        CLIPSLexeme **wildcard,
        unsigned short *min,
        unsigned short *max,
        bool *error,
        bool (*checkfunc)(const Environment&, const char *)) {
    Expression *nextOne, *lastOne, *check;
    int paramprintp = 0;

    *wildcard = nullptr;
    *min = 0;
    *error = true;
    lastOne = nextOne = parameterList;
    while (nextOne != nullptr) {
        (*min)++;
        lastOne = nextOne;
        nextOne = nextOne->nextArg;
    }
    if (tkn->tknType != LEFT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "parameter list");
        ReturnExpression(theEnv, parameterList);
        return nullptr;
    }
    GetToken(theEnv, readSource, tkn);
    while ((tkn->tknType == SF_VARIABLE_TOKEN) || (tkn->tknType == MF_VARIABLE_TOKEN)) {
        for (check = parameterList; check != nullptr; check = check->nextArg)
            if (check->value == tkn->value) {
                PrintErrorID(theEnv, "PRCCODE", 7, false);
                WriteString(theEnv, STDERR, "Duplicate parameter names not allowed.\n");
                ReturnExpression(theEnv, parameterList);
                return nullptr;
            }
        if (*wildcard != nullptr) {
            PrintErrorID(theEnv, "PRCCODE", 8, false);
            WriteString(theEnv, STDERR, "No parameters allowed after wildcard parameter.\n");
            ReturnExpression(theEnv, parameterList);
            return nullptr;
        }
        if ((checkfunc != nullptr) ? (*checkfunc)(theEnv, tkn->lexemeValue->contents) : false) {
            ReturnExpression(theEnv, parameterList);
            return nullptr;
        }
        nextOne = GenConstant(theEnv, TokenTypeToType(tkn->tknType), tkn->value);
        if (tkn->tknType == MF_VARIABLE_TOKEN)
            *wildcard = tkn->lexemeValue;
        else
            (*min)++;
        if (lastOne == nullptr) { parameterList = nextOne; }
        else { lastOne->nextArg = nextOne; }
        lastOne = nextOne;
        SavePPBuffer(theEnv, " ");
        paramprintp = 1;
        GetToken(theEnv, readSource, tkn);
    }
    if (tkn->tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "parameter list");
        ReturnExpression(theEnv, parameterList);
        return nullptr;
    }
    if (paramprintp) {
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, ")");
    }
    *error = false;
    *max = (*wildcard != nullptr) ? PARAMETERS_UNBOUNDED : *min;
    return (parameterList);
}

/*************************************************************************
  NAME         : ParseProcActions
  DESCRIPTION  : Parses the bodies of deffunctions, generic function
                 methods and message-handlers.  Replaces parameter
                 and local variable references with appropriate
                 runtime access functions
  INPUTS       : 1) The environment
                 2) The type of procedure body being parsed
                 3) The logical name of the input
                 4) A buffer for scanned tokens
                 5) A list of expressions containing the names
                    of the parameters
                 6) The wilcard parameter symbol (nullptr if none)
                 7) A pointer to a function to parse variables not
                    recognized by the standard parser
                    The function should accept the variable
                    expression and a generic pointer for special
                    data (can be nullptr) as arguments.  If the variable
                    is recognized, the function should modify the
                    expression to access this variable.  Return 1
                    if recognized, 0 if not, -1 on errors
                    This argument can be nullptr.
                 8) A pointer to a function to handle binds in a
                    special way. The function should accept the
                    bind function call expression as an argument.
                    If the variable is recognized and treated specially,
                    the function should modify the expression
                    appropriately (including attaching/removing
                    any necessary argument expressions).  Return 1
                    if recognized, 0 if not, -1 on errors.
                    This argument can be nullptr.
                 9) A buffer for holding the number of local vars
                    used by this procedure body.
                10) Special user data buffer to pass to variable
                    reference and bind replacement functions
RETURNS      : A packed expression containing the body, nullptr on
                   errors.
SIDE EFFECTS : Variable references replaced with runtime calls
                  to access the paramter and local variable array
NOTES        : None
*************************************************************************/
Expression *ParseProcActions(
        const Environment&theEnv,
        const char *bodytype,
        const char *readSource,
        struct token *tkn,
        Expression *params,
        CLIPSLexeme *wildcard,
        int (*altvarfunc)(const Environment&, Expression *, void *),
        int (*altbindfunc)(const Environment&, Expression *, void *),
        unsigned short *lvarcnt,
        void *userBuffer) {
    Expression *actions, *pactions;

    /* ====================================================================
       Clear parsed bind list - so that only local vars from this body will
       be on it.  The position of vars on thsi list are used to generate
       indices into the LocalVarArray at runtime.  The parsing of the
       "bind" function adds vars to this list.
       ==================================================================== */
    ClearParsedBindNames(theEnv);
    actions = GroupActions(theEnv, readSource, tkn, true, nullptr, false);
    if (actions == nullptr)
        return nullptr;

    /* ====================================================================
       Replace any bind functions with special functions before replacing
       any variable references.  This allows those bind names to be removed
       before they can be seen by variable replacement and thus generate
       incorrect indices.
       ==================================================================== */
    if (altbindfunc != nullptr) {
        if (ReplaceProcBinds(theEnv, actions, altbindfunc, userBuffer)) {
            ClearParsedBindNames(theEnv);
            ReturnExpression(theEnv, actions);
            return nullptr;
        }
    }

    /* ======================================================================
       The number of names left on the bind list is the number of local
       vars for this procedure body.  Replace all variable reference with
       runtime access functions for ProcParamArray, LocalVarArray or
       other special items, such as direct slot references, global variables,
       or fact field references.
       ====================================================================== */
    *lvarcnt = CountParsedBindNames(theEnv);
    if (ReplaceProcVars(theEnv, bodytype, actions, params, wildcard, altvarfunc, userBuffer)) {
        ClearParsedBindNames(theEnv);
        ReturnExpression(theEnv, actions);
        return nullptr;
    }

    /* =======================================================================
       Normally, actions are grouped in a progn.  If there is only one action,
       the progn is unnecessary and can be removed.  Also, the actions are
       packed into a contiguous array to save on memory overhead.  The
       intermediate parsed bind names are freed to avoid tying up memory.
       ======================================================================= */
    actions = CompactActions(theEnv, actions);
    pactions = PackExpression(theEnv, actions);
    ReturnExpression(theEnv, actions);
    ClearParsedBindNames(theEnv);
    return (pactions);
}

/*************************************************************************
  NAME         : ReplaceProcVars
  DESCRIPTION  : Examines an expression for variables
                   and replaces any that correspond to
                   procedure parameters or globals
                   with function calls that get these
                   variables' values at run-time.
                   For example, procedure arguments
                   are stored an array at run-time, so at
                   parse-time, parameter-references are replaced
                   with function calls referencing this array at
                   the appropriate position.
  INPUTS       : 1) The type of procedure being parsed
                 2) The expression-actions to be examined
                 3) The parameter list
                 4) The wildcard parameter symbol (nullptr if none)
                 5) A pointer to a function to parse variables not
                    recognized by the standard parser
                    The function should accept the variable
                    expression and a generic pointer for special
                    data (can be nullptr) as arguments.  If the variable
                    is recognized, the function should modify the
                    expression to access this variable.  Return 1
                    if recognized, 0 if not, -1 on errors
                    This argument can be nullptr.
                 6) Data buffer to be passed to alternate parsing
                    function
  RETURNS      : False if OK, true on errors
  SIDE EFFECTS : Variable references replaced with function calls
  NOTES        : This function works from the ParsedBindNames list in
                    SPCLFORM.C to access local binds.  Make sure that
                    the list accurately reflects the binds by calling
                    ClearParsedBindNames(theEnv) before the parse of the body
                    in which variables are being replaced.
 *************************************************************************/
int ReplaceProcVars(
        const Environment&theEnv,
        const char *bodytype,
        Expression *actions,
        Expression *parameterList,
        CLIPSLexeme *wildcard,
        int (*altvarfunc)(const Environment&, Expression *, void *),
        void *specdata) {
    int altcode;
    unsigned position, boundPosn;
    Expression *arg_lvl, *altvarexp;
    CLIPSLexeme *bindName;
    PACKED_PROC_VAR pvar;
    int errorCode;

    while (actions != nullptr) {
        if (actions->type == SF_VARIABLE) {
            /*===============================================*/
            /* See if the variable is in the parameter list. */
            /*===============================================*/

            bindName = actions->lexemeValue;
            position = FindProcParameter(bindName, parameterList, wildcard);

            /*=============================================================*/
            /* Check to see if the variable is bound within the procedure. */
            /*=============================================================*/

            boundPosn = SearchParsedBindNames(theEnv, bindName);

            /*=============================================*/
            /* If variable is not defined in the parameter */
            /* list or as part of a bind action then...    */
            /*=============================================*/

            if ((position == 0) && (boundPosn == 0)) {
                /*================================================================*/
                /* Check to see if the variable has a special access function,    */
                /* such as direct slot reference or a rule RHS pattern reference. */
                /*================================================================*/

                if (altvarfunc == nullptr) { errorCode = 0; }
                else { errorCode = (*altvarfunc)(theEnv, actions, specdata); }

                if (errorCode != 1) {
                    if (errorCode == 0) {
                        PrintErrorID(theEnv, "PRCCODE", 3, true);
                        WriteString(theEnv, STDERR, "Undefined variable ?");
                        WriteString(theEnv, STDERR, bindName->contents);
                        WriteString(theEnv, STDERR, " referenced in ");
                        WriteString(theEnv, STDERR, bodytype);
                        WriteString(theEnv, STDERR, ".\n");
                    }
                    return 1;
                }
            }

                /*===================================================*/
                /* Else if variable is defined in the parameter list */
                /* and not rebound within the procedure then...      */
                /*===================================================*/

            else if ((position > 0) && (boundPosn == 0)) {
                actions->type = ((bindName != wildcard) ? PROC_PARAM : PROC_WILD_PARAM);
                actions->value = AddBitMap(theEnv, &position, sizeof(int));
            }

                /*=========================================================*/
                /* Else the variable is rebound within the procedure so... */
                /*=========================================================*/

            else {
                if (altvarfunc != nullptr) {
                    altvarexp = GenConstant(theEnv, actions->type, actions->value);
                    altcode = (*altvarfunc)(theEnv, altvarexp, specdata);
                    if (altcode == 0) {
                        rtn_struct(theEnv, Expression, altvarexp);
                        altvarexp = nullptr;
                    } else if (altcode == -1) {
                        rtn_struct(theEnv, Expression, altvarexp);
                        return true;
                    }
                } else
                    altvarexp = nullptr;
                actions->type = PROC_GET_BIND;
                ClearBitString(&pvar, sizeof(PACKED_PROC_VAR));
                pvar.first = boundPosn;
                pvar.second = position;
                pvar.secondFlag = (bindName != wildcard) ? 0 : 1;
                actions->value = AddBitMap(theEnv, &pvar, sizeof(PACKED_PROC_VAR));
                actions->argList = GenConstant(theEnv, SYMBOL_TYPE, bindName);
                actions->argList->nextArg = altvarexp;
            }
        }
#if DEFGLOBAL_CONSTRUCT
        else if (actions->type == GBL_VARIABLE) {
            if (!ReplaceGlobalVariable(theEnv, actions))
                return (-1);
        }
#endif
        if ((altvarfunc != nullptr) ? ((*altvarfunc)(theEnv, actions, specdata) == -1) : false)
            return 1;
        if (actions->argList != nullptr) {
            if (ReplaceProcVars(theEnv, bodytype, actions->argList, parameterList,
                                wildcard, altvarfunc, specdata))
                return 1;

            /* ====================================================================
               Check to see if this is a call to the bind function.  If so (and the
               second argument is a symbol) then it is a locally bound variable
               (as opposed to a global).

               Replace the call to "bind" with a call to PROC_BIND - the
               special internal function for procedure local variables.
               ==================================================================== */
            if ((actions->value == (void *) FindFunction(theEnv, "bind")) &&
                (actions->argList->type == SYMBOL_TYPE)) {
                actions->type = PROC_BIND;
                boundPosn = SearchParsedBindNames(theEnv, actions->argList->lexemeValue);
                actions->value = AddBitMap(theEnv, &boundPosn, sizeof(int));
                arg_lvl = actions->argList->nextArg;
                rtn_struct(theEnv, Expression, actions->argList);
                actions->argList = arg_lvl;
            }
        }
        actions = actions->nextArg;
    }
    return 0;
}

#if DEFGENERIC_CONSTRUCT

/*****************************************************
  NAME         : GenProcWildcardReference
  DESCRIPTION  : Returns an expression to access the
                    wildcard parameter for a method
  INPUTS       : The starting index of the wildcard
  RETURNS      : An expression containing the wildcard
                 reference
  SIDE EFFECTS : Expression allocated
  NOTES        : None
 *****************************************************/
Expression *GenProcWildcardReference(
        const Environment&theEnv,
        int theIndex) {
    return (GenConstant(theEnv, PROC_WILD_PARAM, AddBitMap(theEnv, &theIndex, sizeof(int))));
}

#endif

/*******************************************************************
  NAME         : PushProcParameters
  DESCRIPTION  : Given a list of parameter expressions,
                   this function evaluates each expression
                   and stores the results in a contiguous
                   array of DATA_OBJECTS.  Used in creating a new
                   ProcParamArray for the execution of a
                   procedure
                 The current arrays are saved on a stack.
  INPUTS       : 1) The paramter expression list
                 2) The number of parameters in the list
                 3) The name of the procedure for which
                    these parameters are being evaluated
                 4) The type of procedure
                 5) A pointer to a function to print out a trace
                    message about the currently executing
                    procedure when unbound variables are detected
                    at runtime (The function should take no
                    arguments and have no return value.  The
                    function should print its synopsis to STDERR
                    and include the final carriage-return.)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of the evaluation of the
                   parameter expressions
                 UDFValue array allocated (deallocated on errors)
                 ProcParamArray set
  NOTES        : EvaluationError set on errors
 *******************************************************************/
void PushProcParameters(
        const Environment&theEnv,
        Expression *parameterList,
        unsigned int numberOfParameters,
        const char *pname,
        const char *bodytype,
        void (*UnboundErrFunc)(const Environment&, const char *)) {
    PROC_PARAM_STACK *ptmp;

    ptmp = get_struct(theEnv, ProcParamStack);
    ptmp->ParamArray = ProceduralPrimitiveData(theEnv)->ProcParamArray;
    ptmp->ParamArraySize = ProceduralPrimitiveData(theEnv)->ProcParamArraySize;
    ptmp->UnboundErrFunc = ProceduralPrimitiveData(theEnv)->ProcUnboundErrFunc;
    ptmp->nxt = ProceduralPrimitiveData(theEnv)->pstack;
    ProceduralPrimitiveData(theEnv)->pstack = ptmp;
    EvaluateProcParameters(theEnv, parameterList, numberOfParameters, pname, bodytype);
    if (EvaluationData(theEnv)->EvaluationError) {
        ptmp = ProceduralPrimitiveData(theEnv)->pstack;
        ProceduralPrimitiveData(theEnv)->pstack = ProceduralPrimitiveData(theEnv)->pstack->nxt;
        rtn_struct(theEnv, ProcParamStack, ptmp);
        return;
    }

    /* ================================================================
       Record ProcParamExpressions and WildcardValue for previous frame
       AFTER evaluating arguments for the new frame, because they could
       have gone from nullptr to non-nullptr (if they were already non-nullptr,
       they would remain unchanged.)
       ================================================================ */
#if DEFGENERIC_CONSTRUCT
    ptmp->ParamExpressions = ProceduralPrimitiveData(theEnv)->ProcParamExpressions;
    ProceduralPrimitiveData(theEnv)->ProcParamExpressions = nullptr;
#endif
    ptmp->WildcardValue = ProceduralPrimitiveData(theEnv)->WildcardValue;
    ProceduralPrimitiveData(theEnv)->WildcardValue = nullptr;
    ProceduralPrimitiveData(theEnv)->ProcUnboundErrFunc = UnboundErrFunc;
}

/******************************************************************
  NAME         : PopProcParameters
  DESCRIPTION  : Restores old procedure arrays
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Stack popped and globals restored
  NOTES        : Assumes pstack != nullptr
 ******************************************************************/
void PopProcParameters(
        const Environment&theEnv) {
    PROC_PARAM_STACK *ptmp;

    if (ProceduralPrimitiveData(theEnv)->ProcParamArray != nullptr)
        rm(theEnv, ProceduralPrimitiveData(theEnv)->ProcParamArray,
           (sizeof(UDFValue) * ProceduralPrimitiveData(theEnv)->ProcParamArraySize));

#if DEFGENERIC_CONSTRUCT
    if (ProceduralPrimitiveData(theEnv)->ProcParamExpressions != nullptr)
        rm(theEnv, ProceduralPrimitiveData(theEnv)->ProcParamExpressions,
           (sizeof(Expression) * ProceduralPrimitiveData(theEnv)->ProcParamArraySize));
#endif

    ptmp = ProceduralPrimitiveData(theEnv)->pstack;
    ProceduralPrimitiveData(theEnv)->pstack = ProceduralPrimitiveData(theEnv)->pstack->nxt;
    ProceduralPrimitiveData(theEnv)->ProcParamArray = ptmp->ParamArray;
    ProceduralPrimitiveData(theEnv)->ProcParamArraySize = ptmp->ParamArraySize;

#if DEFGENERIC_CONSTRUCT
    ProceduralPrimitiveData(theEnv)->ProcParamExpressions = ptmp->ParamExpressions;
#endif

    if (ProceduralPrimitiveData(theEnv)->WildcardValue != nullptr) {
        ReleaseMultifield(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        if (ProceduralPrimitiveData(theEnv)->WildcardValue->value != ProceduralPrimitiveData(theEnv)->NoParamValue)
            AddToMultifieldList(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        rtn_struct(theEnv, UDFValue, ProceduralPrimitiveData(theEnv)->WildcardValue);
    }
    ProceduralPrimitiveData(theEnv)->WildcardValue = ptmp->WildcardValue;
    ProceduralPrimitiveData(theEnv)->ProcUnboundErrFunc = ptmp->UnboundErrFunc;
    rtn_struct(theEnv, ProcParamStack, ptmp);
}

/******************************************************************
  NAME         : ReleaseProcParameters
  DESCRIPTION  : Restores old procedure arrays
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Stack popped and globals restored
  NOTES        : Assumes pstack != nullptr
 ******************************************************************/
static void ReleaseProcParameters(
        const Environment&theEnv) {
    PROC_PARAM_STACK *ptmp, *next;

    if (ProceduralPrimitiveData(theEnv)->ProcParamArray != nullptr)
        rm(theEnv, ProceduralPrimitiveData(theEnv)->ProcParamArray,
           (sizeof(UDFValue) * ProceduralPrimitiveData(theEnv)->ProcParamArraySize));

    if (ProceduralPrimitiveData(theEnv)->WildcardValue != nullptr) {
        if (ProceduralPrimitiveData(theEnv)->WildcardValue->value != ProceduralPrimitiveData(theEnv)->NoParamValue) {
            ReturnMultifield(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        }

        rtn_struct(theEnv, UDFValue, ProceduralPrimitiveData(theEnv)->WildcardValue);
    }

#if DEFGENERIC_CONSTRUCT
    if (ProceduralPrimitiveData(theEnv)->ProcParamExpressions != nullptr)
        rm(theEnv, ProceduralPrimitiveData(theEnv)->ProcParamExpressions,
           (sizeof(Expression) * ProceduralPrimitiveData(theEnv)->ProcParamArraySize));
#endif

    ptmp = ProceduralPrimitiveData(theEnv)->pstack;

    while (ptmp != nullptr) {
        next = ptmp->nxt;

        if (ptmp->ParamArray != nullptr) { rm(theEnv, ptmp->ParamArray, (sizeof(UDFValue) * ptmp->ParamArraySize)); }

#if DEFGENERIC_CONSTRUCT
        if (ptmp->ParamExpressions != nullptr) { rm(theEnv, ptmp->ParamExpressions, (sizeof(Expression) * ptmp->ParamArraySize)); }
#endif

        if (ptmp->WildcardValue != nullptr) {
            if (ptmp->WildcardValue->value != ProceduralPrimitiveData(theEnv)->NoParamValue) {
                ReturnMultifield(theEnv, ptmp->WildcardValue->multifieldValue);
            }

            rtn_struct(theEnv, UDFValue, ptmp->WildcardValue);
        }

        rtn_struct(theEnv, ProcParamStack, ptmp);
        ptmp = next;
    }
}

#if DEFGENERIC_CONSTRUCT

/***********************************************************
  NAME         : GetProcParamExpressions
  DESCRIPTION  : Forms an array of expressions equivalent to
                 the current procedure paramter array.  Used
                 to conveniently attach these parameters as
                 arguments to a H/L system function call
                 (used by the generic dispatch).
  INPUTS       : None
  RETURNS      : A pointer to an array of expressions
  SIDE EFFECTS : Expression array created
  NOTES        : None
 ***********************************************************/
Expression *GetProcParamExpressions(
        const Environment&theEnv) {
    unsigned int i;

    if ((ProceduralPrimitiveData(theEnv)->ProcParamArray == nullptr) || (ProceduralPrimitiveData(theEnv)->ProcParamExpressions != nullptr))
        return (ProceduralPrimitiveData(theEnv)->ProcParamExpressions);
    ProceduralPrimitiveData(theEnv)->ProcParamExpressions = (Expression *)
            gm2(theEnv, (sizeof(Expression) * ProceduralPrimitiveData(theEnv)->ProcParamArraySize));
    for (i = 0; i < ProceduralPrimitiveData(theEnv)->ProcParamArraySize; i++) {
        ProceduralPrimitiveData(theEnv)->ProcParamExpressions[i].type = ProceduralPrimitiveData(
                theEnv)->ProcParamArray[i].header->type; // TBD Remove
        if (ProceduralPrimitiveData(theEnv)->ProcParamArray[i].header->type != MULTIFIELD_TYPE)
            ProceduralPrimitiveData(theEnv)->ProcParamExpressions[i].value = ProceduralPrimitiveData(theEnv)->ProcParamArray[i].value;
        else
            ProceduralPrimitiveData(theEnv)->ProcParamExpressions[i].value = &ProceduralPrimitiveData(theEnv)->ProcParamArray[i];
        ProceduralPrimitiveData(theEnv)->ProcParamExpressions[i].argList = nullptr;
        ProceduralPrimitiveData(theEnv)->ProcParamExpressions[i].nextArg =
                ((i + 1) != ProceduralPrimitiveData(theEnv)->ProcParamArraySize) ? &ProceduralPrimitiveData(theEnv)->ProcParamExpressions[
                        i + 1] : nullptr;
    }
    return (ProceduralPrimitiveData(theEnv)->ProcParamExpressions);
}

#endif

/***********************************************************
  NAME         : EvaluateProcActions
  DESCRIPTION  : Evaluates the actions of a deffunction,
                 generic function method or message-handler.
  INPUTS       : 1) The module where the actions should be
                    executed
                 2) The actions (linked by nextArg fields)
                 3) The number of local variables to reserve
                    space for.
                 4) A buffer to hold the result of evaluating
                    the actions.
                 5) A function which prints out the name of
                    the currently executing body for error
                    messages (can be nullptr).
  RETURNS      : Nothing useful
  SIDE EFFECTS : Allocates and deallocates space for
                 local variable array.
  NOTES        : None
 ***********************************************************/
void EvaluateProcActions(
        const Environment&theEnv,
        Defmodule *theModule,
        Expression *actions,
        unsigned short lvarcnt,
        UDFValue *returnValue,
        void (*crtproc)(const Environment&, const char *)) {
    UDFValue *oldLocalVarArray;
    unsigned short i;
    Defmodule *oldModule;
    Expression *oldActions;
    struct trackedMemory *theTM;

    oldLocalVarArray = ProceduralPrimitiveData(theEnv)->LocalVarArray;
    ProceduralPrimitiveData(theEnv)->LocalVarArray = (lvarcnt == 0) ? nullptr :
                                                     (UDFValue *) gm2(theEnv, (sizeof(UDFValue) * lvarcnt));

    if (lvarcnt != 0) { theTM = AddTrackedMemory(theEnv, ProceduralPrimitiveData(theEnv)->LocalVarArray, sizeof(UDFValue) * lvarcnt); }
    else { theTM = nullptr; }

    for (i = 0; i < lvarcnt; i++)
        ProceduralPrimitiveData(theEnv)->LocalVarArray[i].supplementalInfo = FalseSymbol(theEnv);

    oldModule = GetCurrentModule(theEnv);
    if (oldModule != theModule)
        SetCurrentModule(theEnv, theModule);
    oldActions = ProceduralPrimitiveData(theEnv)->CurrentProcActions;
    ProceduralPrimitiveData(theEnv)->CurrentProcActions = actions;

    if (EvaluateExpression(theEnv, actions, returnValue)) {
        returnValue->value = FalseSymbol(theEnv);
    }

    ProceduralPrimitiveData(theEnv)->CurrentProcActions = oldActions;
    if (oldModule != GetCurrentModule(theEnv))
        SetCurrentModule(theEnv, oldModule);
    if ((crtproc != nullptr) ? EvaluationData(theEnv)->HaltExecution : false) {
        const char *logName;

        if (GetEvaluationError(theEnv)) {
            PrintErrorID(theEnv, "PRCCODE", 4, false);
            logName = STDERR;
        } else {
            PrintWarningID(theEnv, "PRCCODE", 4, false);
            logName = STDWRN;
        }
        WriteString(theEnv, logName, "Execution halted during the actions of ");
        (*crtproc)(theEnv, logName);
    }

    if ((ProceduralPrimitiveData(theEnv)->WildcardValue != nullptr) ? (returnValue->value ==
                                                                    ProceduralPrimitiveData(theEnv)->WildcardValue->value) : false) {
        ReleaseMultifield(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        if (ProceduralPrimitiveData(theEnv)->WildcardValue->value != ProceduralPrimitiveData(theEnv)->NoParamValue)
            AddToMultifieldList(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        rtn_struct(theEnv, UDFValue, ProceduralPrimitiveData(theEnv)->WildcardValue);
        ProceduralPrimitiveData(theEnv)->WildcardValue = nullptr;
    }

    if (lvarcnt != 0) {
        RemoveTrackedMemory(theEnv, theTM);
        for (i = 0; i < lvarcnt; i++)
            if (ProceduralPrimitiveData(theEnv)->LocalVarArray[i].supplementalInfo == TrueSymbol(theEnv))
                ReleaseUDFV(theEnv, &ProceduralPrimitiveData(theEnv)->LocalVarArray[i]);
        rm(theEnv, ProceduralPrimitiveData(theEnv)->LocalVarArray, (sizeof(UDFValue) * lvarcnt));
    }

    ProceduralPrimitiveData(theEnv)->LocalVarArray = oldLocalVarArray;
}

/****************************************************
  NAME         : PrintProcParamArray
  DESCRIPTION  : Displays the contents of the
                 current procedure parameter array
  INPUTS       : The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************/
void PrintProcParamArray(
        const Environment&theEnv,
        const char *logName) {
    unsigned int i;

    WriteString(theEnv, logName, " (");
    for (i = 0; i < ProceduralPrimitiveData(theEnv)->ProcParamArraySize; i++) {
        WriteUDFValue(theEnv, logName, &ProceduralPrimitiveData(theEnv)->ProcParamArray[i]);
        if (i != ProceduralPrimitiveData(theEnv)->ProcParamArraySize - 1)
            WriteString(theEnv, logName, " ");
    }
    WriteString(theEnv, logName, ")\n");
}

/****************************************************************
  NAME         : GrabProcWildargs
  DESCRIPTION  : Groups a portion of the ProcParamArray
                   into a multi-field variable
  INPUTS       : 1) Starting index in ProcParamArray
                      for grouping of arguments into
                      multi-field variable
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multi-field variable allocated and set
                   with corresponding values of ProcParamArray
  NOTES        : Multi-field is NOT on list of ephemeral segments
 ****************************************************************/
void GrabProcWildargs(
        const Environment&theEnv,
        UDFValue *returnValue,
        unsigned int theIndex) {
    unsigned int i, j;
    size_t k; /* 6.04 Bug Fix */
    size_t size;
    UDFValue *val;

    returnValue->begin = 0;
    if (ProceduralPrimitiveData(theEnv)->WildcardValue == nullptr) {
        ProceduralPrimitiveData(theEnv)->WildcardValue = get_struct(theEnv, UDFValue);
        ProceduralPrimitiveData(theEnv)->WildcardValue->begin = 0;
    } else if (theIndex == ProceduralPrimitiveData(theEnv)->Oldindex) {
        returnValue->range = ProceduralPrimitiveData(theEnv)->WildcardValue->range;
        returnValue->value = ProceduralPrimitiveData(theEnv)->WildcardValue->value;
        return;
    } else {
        ReleaseMultifield(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        if (ProceduralPrimitiveData(theEnv)->WildcardValue->value != ProceduralPrimitiveData(theEnv)->NoParamValue)
            AddToMultifieldList(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
    }
    ProceduralPrimitiveData(theEnv)->Oldindex = theIndex;
    size = ProceduralPrimitiveData(theEnv)->ProcParamArraySize + 1 - theIndex;

    if (size == 0) {
        returnValue->range = 0;
        ProceduralPrimitiveData(theEnv)->WildcardValue->range = 0;
        returnValue->value = ProceduralPrimitiveData(theEnv)->WildcardValue->value = ProceduralPrimitiveData(theEnv)->NoParamValue;
        RetainMultifield(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
        return;
    }
    for (i = theIndex - 1; i < ProceduralPrimitiveData(theEnv)->ProcParamArraySize; i++) {
        if (ProceduralPrimitiveData(theEnv)->ProcParamArray[i].header->type == MULTIFIELD_TYPE)
            size += ProceduralPrimitiveData(theEnv)->ProcParamArray[i].range - 1;
    }
    returnValue->range = size;
    ProceduralPrimitiveData(theEnv)->WildcardValue->range = size;
    returnValue->value = ProceduralPrimitiveData(theEnv)->WildcardValue->value = CreateUnmanagedMultifield(theEnv, size);
    for (i = theIndex - 1, j = 0; i < ProceduralPrimitiveData(theEnv)->ProcParamArraySize; i++) {
        if (ProceduralPrimitiveData(theEnv)->ProcParamArray[i].header->type != MULTIFIELD_TYPE) {
            returnValue->multifieldValue->contents[j].value = ProceduralPrimitiveData(theEnv)->ProcParamArray[i].value;
            j++;
        } else {
            val = &ProceduralPrimitiveData(theEnv)->ProcParamArray[i];
            for (k = val->begin; k < (val->begin + val->range); k++, j++) {
                returnValue->multifieldValue->contents[j].value = val->multifieldValue->contents[k].value;
            }
        }
    }
    RetainMultifield(theEnv, ProceduralPrimitiveData(theEnv)->WildcardValue->multifieldValue);
}

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*******************************************************************
  NAME         : EvaluateProcParameters
  DESCRIPTION  : Given a list of parameter expressions,
                   this function evaluates each expression
                   and stores the results in a contiguous
                   array of DATA_OBJECTS.  Used in creating a new
                   ProcParamArray for the execution of a
                   procedure
  INPUTS       : 1) The paramter expression list
                 2) The number of parameters in the list
                 3) The name of the procedure for which
                    these parameters are being evaluated
                 4) The type of procedure
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of the evaluation of the
                   parameter expressions
                 UDFValue array allocated (deallocated on errors)
                 ProcParamArray set
  NOTES        : EvaluationError set on errors
 *******************************************************************/
static void EvaluateProcParameters(
        const Environment&theEnv,
        Expression *parameterList,
        unsigned int numberOfParameters,
        const char *pname,
        const char *bodytype) {
    UDFValue *rva, temp;
    int i = 0;

    if (numberOfParameters == 0) {
        ProceduralPrimitiveData(theEnv)->ProcParamArray = nullptr;
        ProceduralPrimitiveData(theEnv)->ProcParamArraySize = 0;
        return;
    }

    rva = (UDFValue *) gm2(theEnv, (sizeof(UDFValue) * numberOfParameters));
    while (parameterList != nullptr) {
        if (EvaluateExpression(theEnv, parameterList, &temp) ? true :
            (temp.header->type == VOID_TYPE)) {
            if (temp.header->type == VOID_TYPE) {
                PrintErrorID(theEnv, "PRCCODE", 2, false);
                WriteString(theEnv, STDERR, "Functions without a return value are illegal as ");
                WriteString(theEnv, STDERR, bodytype);
                WriteString(theEnv, STDERR, " arguments.\n");
                SetEvaluationError(theEnv, true);
            }
            PrintErrorID(theEnv, "PRCCODE", 6, false);
            WriteString(theEnv, STDERR, "This error occurred while evaluating arguments ");
            WriteString(theEnv, STDERR, "for the ");
            WriteString(theEnv, STDERR, bodytype);
            WriteString(theEnv, STDERR, " '");
            WriteString(theEnv, STDERR, pname);
            WriteString(theEnv, STDERR, "'.\n");
            rm(theEnv, rva, (sizeof(UDFValue) * numberOfParameters));
            return;
        }
        rva[i].value = temp.value;
        rva[i].begin = temp.begin;
        rva[i].range = temp.range;
        parameterList = parameterList->nextArg;
        i++;
    }
    ProceduralPrimitiveData(theEnv)->ProcParamArraySize = numberOfParameters;
    ProceduralPrimitiveData(theEnv)->ProcParamArray = rva;
}

/***************************************************
  NAME         : RtnProcParam
  DESCRIPTION  : Internal function for getting the
                   value of an argument passed to
                   a procedure
  INPUTS       : 1) Expression to evaluate
                    (PROC_PARAM index)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Caller's buffer set to specified
                   node of ProcParamArray
  NOTES        : None
 ***************************************************/
static bool RtnProcParam(
        const Environment&theEnv,
        void *value,
        UDFValue *returnValue) {
    UDFValue *src;

    src = &ProceduralPrimitiveData(theEnv)->ProcParamArray[*((int *) ((CLIPSBitMap *) value)->contents) - 1];
    returnValue->value = src->value;
    returnValue->begin = src->begin;
    returnValue->range = src->range;
    return true;
}

/**************************************************************
  NAME         : GetProcBind
  DESCRIPTION  : Internal function for looking up the
                    values of parameters or bound variables
                    within procedures
  INPUTS       : 1) Expression to evaluate
                    (PROC_GET_BIND index)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Caller's buffer set to parameter value in
                   ProcParamArray or the value in LocalVarArray
  NOTES        : None
 **************************************************************/
static bool GetProcBind(
        const Environment&theEnv,
        void *value,
        UDFValue *returnValue) {
    UDFValue *src;
    PACKED_PROC_VAR *pvar;

    pvar = (PACKED_PROC_VAR *) ((CLIPSBitMap *) value)->contents;
    src = &ProceduralPrimitiveData(theEnv)->LocalVarArray[pvar->first - 1];
    if (src->supplementalInfo == TrueSymbol(theEnv)) {
        returnValue->value = src->value;
        returnValue->begin = src->begin;
        returnValue->range = src->range;
        return true;
    }
    if (GetFirstArgument()->nextArg != nullptr) {
        EvaluateExpression(theEnv, GetFirstArgument()->nextArg, returnValue);
        return true;
    }
    if (pvar->second == 0) {
        PrintErrorID(theEnv, "PRCCODE", 5, false);
        SetEvaluationError(theEnv, true);
        WriteString(theEnv, STDERR, "Variable ?");
        WriteString(theEnv, STDERR, GetFirstArgument()->lexemeValue->contents);
        if (ProceduralPrimitiveData(theEnv)->ProcUnboundErrFunc != nullptr) {
            WriteString(theEnv, STDERR, " unbound in ");
            (*ProceduralPrimitiveData(theEnv)->ProcUnboundErrFunc)(theEnv, STDERR);
        } else
            WriteString(theEnv, STDERR, " unbound.\n");
        returnValue->value = FalseSymbol(theEnv);
        return true;
    }
    if (pvar->secondFlag == 0) {
        src = &ProceduralPrimitiveData(theEnv)->ProcParamArray[pvar->second - 1];
        returnValue->value = src->value;
        returnValue->begin = src->begin;
        returnValue->range = src->range;
    } else
        GrabProcWildargs(theEnv, returnValue, pvar->second);
    return true;
}

/**************************************************************
  NAME         : PutProcBind
  DESCRIPTION  : Internal function for setting the values of
                 of locally bound variables within procedures
  INPUTS       : 1) Expression to evaluate
                    (PROC_PARAM index)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Bound variable in LocalVarArray set to
                   value in caller's buffer.
  NOTES        : None
 **************************************************************/
static bool PutProcBind(
        const Environment&theEnv,
        void *value,
        UDFValue *returnValue) {
    UDFValue *dst;

    dst = &ProceduralPrimitiveData(theEnv)->LocalVarArray[*((int *) ((CLIPSBitMap *) value)->contents) - 1];
    if (GetFirstArgument() == nullptr) {
        if (dst->supplementalInfo == TrueSymbol(theEnv))
            ReleaseUDFV(theEnv, dst);
        dst->supplementalInfo = FalseSymbol(theEnv);
        returnValue->value = FalseSymbol(theEnv);
    } else {
        if (GetFirstArgument()->nextArg != nullptr)
            StoreInMultifield(theEnv, returnValue, GetFirstArgument(), true);
        else
            EvaluateExpression(theEnv, GetFirstArgument(), returnValue);
        if (dst->supplementalInfo == TrueSymbol(theEnv))
            ReleaseUDFV(theEnv, dst);
        dst->supplementalInfo = TrueSymbol(theEnv);
        dst->value = returnValue->value;
        dst->begin = returnValue->begin;
        dst->range = returnValue->range;
        RetainUDFV(theEnv, dst);
    }
    return true;
}

/****************************************************************
  NAME         : RtnProcWild
  DESCRIPTION  : Groups a portion of the ProcParamArray
                   into a multi-field variable
  INPUTS       : 1) Starting index in ProcParamArray
                      for grouping of arguments into
                      multi-field variable (expression value)
                 2) Caller's result value buffer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multi-field variable allocated and set
                   with corresponding values of ProcParamArray
  NOTES        : Multi-field is NOT on list of ephemeral segments
 ****************************************************************/
static bool RtnProcWild(
        const Environment&theEnv,
        void *value,
        UDFValue *returnValue) {
    GrabProcWildargs(theEnv, returnValue, *(unsigned *) ((CLIPSBitMap *) value)->contents);
    return true;
}

/***************************************************
  NAME         : FindProcParameter
  DESCRIPTION  : Determines the relative position in
                   an n-element list of a certain
                   parameter.  The index is 1..n.
  INPUTS       : 1) Parameter name
                 2) Parameter list
                 3) Wildcard symbol (nullptr if none)
  RETURNS      : Index of parameter in list, 0 if
                   not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static unsigned int FindProcParameter(
        CLIPSLexeme *name,
        Expression *parameterList,
        CLIPSLexeme *wildcard) {
    unsigned int i = 1;

    while (parameterList != nullptr) {
        if (parameterList->value == (void *) name) { return i; }
        i++;
        parameterList = parameterList->nextArg;
    }

    /* ===================================================================
       Wildcard may not be stored in actual list but know is always at end
       =================================================================== */
    if (name == wildcard) { return i; }

    return 0;
}

/*************************************************************************
  NAME         : ReplaceProcBinds
  DESCRIPTION  : Examines an expression and replaces calls to the
                 "bind" function which are specially recognized

                 For example, in a message-handler,

                   (bind ?self <value>) would be illegal

                   and

                   (bind ?self:<slot-name> <value>) would be
                   replaced with
                   (put <slot-name> <value>)

  INPUTS       : 1) The actions in which to replace special binds
                 2) A pointer to a function to handle binds in a
                    special way. The function should accept the
                    bind function call expression and a specialized
                    data buffer (can be nullptr) as arguments.
                    If the variable is recognized and treated specially,
                    the function should modify the expression
                    appropriately (including attaching/removing
                    any necessary argument expressions).  Return 1
                    if recognized, 0 if not, -1 on errors.
                    This argument CANNOT be nullptr.
                 3) Specialized user data buffer
  RETURNS      : False if OK, true on errors
  SIDE EFFECTS : Some binds replaced with specialized calls
  NOTES        : Local variable binds are replaced in ReplaceProcVars
                 (after this routine has had a chance to replace all
                  special binds and remove the names from the parsed
                  bind list)
 *************************************************************************/
static bool ReplaceProcBinds(
        const Environment&theEnv,
        Expression *actions,
        int (*altbindfunc)(const Environment&, Expression *, void *),
        void *userBuffer) {
    int bcode;
    CLIPSLexeme *bname;

    while (actions != nullptr) {
        if (actions->argList != nullptr) {
            if (ReplaceProcBinds(theEnv, actions->argList, altbindfunc, userBuffer))
                return true;
            if ((actions->value == (void *) FindFunction(theEnv, "bind")) &&
                (actions->argList->type == SYMBOL_TYPE)) {
                bname = actions->argList->lexemeValue;
                bcode = (*altbindfunc)(theEnv, actions, userBuffer);
                if (bcode == -1)
                    return true;
                if (bcode == 1)
                    RemoveParsedBindName(theEnv, bname);
            }
        }
        actions = actions->nextArg;
    }
    return false;
}

/*****************************************************
  NAME         : CompactActions
  DESCRIPTION  : Examines a progn expression chain,
                 and if there is only one action,
                 the progn header is deallocated and
                 the action is returned.  If there are
                 no actions, the progn expression is
                 modified to be the FALSE symbol
                 and returned.  Otherwise, the progn
                 is simply returned.
  INPUTS       : The action expression
  RETURNS      : The compacted expression
  SIDE EFFECTS : Some expressions possibly deallocated
  NOTES        : Assumes actions is a progn expression
                 and actions->nextArg == nullptr
 *****************************************************/
static Expression *CompactActions(
        const Environment&theEnv,
        Expression *actions) {
    Expression *tmp;

    if (actions->argList == nullptr) {
        actions->type = SYMBOL_TYPE;
        actions->value = FalseSymbol(theEnv);
    } else if (actions->argList->nextArg == nullptr) {
        tmp = actions;
        actions = actions->argList;
        rtn_struct(theEnv, Expression, tmp);
    }
    return (actions);
}

#if (!DEFFUNCTION_CONSTRUCT) || (!DEFGENERIC_CONSTRUCT)

/******************************************************
  NAME         : EvaluateBadCall
  DESCRIPTION  : Default evaluation function for
                 deffunctions and gneric functions
                 in configurations where either
                 capability is not present.
  INPUTS       : 1) The function (ignored)
                 2) A data object buffer for the result
  RETURNS      : False
  SIDE EFFECTS : Data object buffer set to the
                 symbol FALSE and evaluation error set
  NOTES        : Used for binary images which
                 contain deffunctions and generic
                 functions which cannot be used
 ******************************************************/
static bool EvaluateBadCall(
  const Environment&theEnv,
  void *value,
  UDFValue *returnValue)
  {
#if MAC_XCD
#pragma unused(value)
#endif
   PrintErrorID(theEnv,"PRCCODE",1,false);
   WriteString(theEnv,STDERR,"Attempted to call a deffunction/generic function ");
   WriteString(theEnv,STDERR,"which does not exist.\n");
   SetEvaluationError(theEnv,true);
   returnValue->value = FalseSymbol(theEnv);
   return false;
  }

#endif

