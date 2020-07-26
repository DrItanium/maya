/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*                  EXPRESSION MODULE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for creating, deleting,        */
/*   compacting, installing, and hashing expressions.        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Corrected link errors with non-default         */
/*            setup.h configuration settings.                */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed expression hashing value.              */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include "Setup.h"

//#include "BinaryLoad.h"
#include "Environment.h"
//#include "Evaluation.h"
//#include "ExternalFunctions.h"
//#include "MemoryAllocation.h"
//#include "PrintUtility.h"
#include "Router.h"

#include "Expression.h"
#include "UDFContext.h"
namespace maya {

constexpr auto PRIME_ONE   = 257;
constexpr auto PRIME_TWO   = 263;
constexpr auto PRIME_THREE = 269;

/****************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS  */
/****************************************/

static unsigned long ListToPacked(Expression *, Expression *, unsigned long);
//static ExpressionHashNode *FindHashedExpression(const Environment::Ptr&, Expression *, unsigned *, ExpressionHashNode **);
static unsigned HashExpression(Expression *);

#if STUBBING_INACTIVE
/***************************************************/
/* ExpressionInstall: Increments the busy count of */
/*   atomic data values found in an expression.    */
/***************************************************/
void ExpressionInstall(
        const Environment::Ptr&theEnv,
        Expression *expression) {
    if (expression == nullptr) return;

    while (expression != nullptr) {
        AtomInstall(theEnv, expression->type, expression->value);
        ExpressionInstall(theEnv, expression->argList);
        expression = expression->nextArg;
    }
}
/*****************************************************/
/* ExpressionDeinstall: Decrements the busy count of */
/*   atomic data values found in an expression.      */
/*****************************************************/
void ExpressionDeinstall(
        const Environment::Ptr&theEnv,
        Expression *expression) {
    if (expression == nullptr) return;

    while (expression != nullptr) {
        AtomDeinstall(theEnv, expression->type, expression->value);
        ExpressionDeinstall(theEnv, expression->argList);
        expression = expression->nextArg;
    }
}


/***********************************************************************/
/* PackExpression: Copies an expression (created using multiple memory */
/*   requests) into an array (created using a single memory request)   */
/*   while maintaining all appropriate links in the expression. A      */
/*   packed expression requires less total memory because it reduces   */
/*   the overhead required for multiple memory allocations.            */
/***********************************************************************/
Expression *PackExpression(
        const Environment::Ptr&theEnv,
        Expression *original) {
    Expression *packPtr;

    if (original == nullptr) return nullptr;

    packPtr = (Expression*)
            gm2(theEnv, sizeof(Expression) * ExpressionSize(original));
    ListToPacked(original, packPtr, 0);

    return packPtr;
    return nullptr;
}
/***********************************************************/
/* ListToPacked: Copies a list of expressions to an array. */
/***********************************************************/
static unsigned long ListToPacked(
        Expression *original,
        Expression *destination,
        unsigned long count) {
    unsigned long i;

    if (original == nullptr) { return count; }

    while (original != nullptr) {
        i = count;
        count++;

        destination[i].type = original->type;
        destination[i].value = original->value;

        if (original->argList == nullptr) { destination[i].argList = nullptr; }
        else {
            destination[i].argList =
                    (Expression*) &destination[count];
            count = ListToPacked(original->argList, destination, count);
        }

        if (original->nextArg == nullptr) { destination[i].nextArg = nullptr; }
        else {
            destination[i].nextArg = &destination[count];
        }

        original = original->nextArg;
    }
    return count;
}

/***************************************************
  NAME         : FindHashedExpression
  DESCRIPTION  : Determines if a given expression
                 is in the expression hash table
  INPUTS       : 1) The expression
                 2) A buffer to hold the hash
                    value
                 3) A buffer to hold the previous
                    node in the hash chain
  RETURNS      : The expression hash table entry
                 (nullptr if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static ExpressionHashNode *FindHashedExpression(
        const Environment::Ptr&theEnv,
        Expression *theExp,
        unsigned *hashval,
        ExpressionHashNode **prv) {
    ExpressionHashNode *exphash;

    if (theExp == nullptr)
        return nullptr;
    *hashval = HashExpression(theExp);
    *prv = nullptr;
    exphash = ExpressionData(theEnv)->ExpressionHashTable[*hashval];
    while (exphash != nullptr) {
        if (IdenticalExpression(exphash->exp, theExp))
            return (exphash);
        *prv = exphash;
        exphash = exphash->next;
    }
    return nullptr;
}
/***************************************************
  NAME         : HashExpression
  DESCRIPTION  : Assigns a deterministic number to
                 an expression
  INPUTS       : The expression
  RETURNS      : The "value" of the expression
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static unsigned HashExpression(
        Expression *theExp) {
    unsigned long tally = PRIME_THREE;
#if STUBBING_INACTIVE
    union {
        void *vv;
        unsigned long uv;
    } fis;

    if (theExp->argList != nullptr)
        tally += HashExpression(theExp->argList) * PRIME_ONE;
    while (theExp != nullptr) {
        tally += theExp->type * PRIME_TWO;
        fis.uv = 0;
        fis.vv = theExp->value;
        tally += fis.uv;
        theExp = theExp->nextArg;
    }
#endif
    return (unsigned) (tally % EXPRESSION_HASH_SIZE);
}

/***************************************************
  NAME         : RemoveHashedExpression
  DESCRIPTION  : Removes a hashed expression from
                 the hash table
  INPUTS       : The expression
  RETURNS      : Nothing useful
  SIDE EFFECTS : Hash node removed (or use count
                 decremented).  If the hash node
                 is removed, the expression is
                 deinstalled and deleted
  NOTES        : If the expression is in use by
                 others, then the use count is
                 merely decremented
 ***************************************************/
void RemoveHashedExpression(
        const Environment::Ptr&theEnv,
        Expression *theExp) {
    ExpressionHashNode *exphash, *prv;
    unsigned hashval;

    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    if (exphash == nullptr)
        return;
    if (--exphash->count != 0)
        return;
    if (prv == nullptr)
        ExpressionData(theEnv)->ExpressionHashTable[hashval] = exphash->next;
    else
        prv->next = exphash->next;
    ExpressionDeinstall(theEnv, exphash->exp);
    ReturnPackedExpression(theEnv, exphash->exp);
    rtn_struct(theEnv, ExpressionHashNode, exphash);
}
/*****************************************************
  NAME         : AddHashedExpression
  DESCRIPTION  : Adds a new expression to the
                 expression hash table (or increments
                 the use count if it is already there)
  INPUTS       : The (new) expression
  RETURNS      : A pointer to the (new) hash node
  SIDE EFFECTS : Adds the new hash node or increments
                 the count of an existing one
  NOTES        : It is the caller's responsibility to
                 delete the passed expression.  This
                 routine copies, packs and installs
                 the given expression
 *****************************************************/
Expression *AddHashedExpression(
        const Environment::Ptr&theEnv,
        Expression *theExp) {
#if STUBBING_INACTIVE
    ExpressionHashNode *prv, *exphash;
    unsigned hashval;

    if (theExp == nullptr) return nullptr;
    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    if (exphash != nullptr) {
        exphash->count++;
        return (exphash->exp);
    }
    exphash = get_struct(theEnv, ExpressionHashNode);
    exphash->hashval = hashval;
    exphash->count = 1;
    exphash->exp = PackExpression(theEnv, theExp);
    ExpressionInstall(theEnv, exphash->exp);
    exphash->next = ExpressionData(theEnv)->ExpressionHashTable[exphash->hashval];
    ExpressionData(theEnv)->ExpressionHashTable[exphash->hashval] = exphash;
    exphash->bsaveID = 0L;
    return (exphash->exp);
#endif
    return nullptr;
}

#if (BLOAD_AND_BSAVE)

/***************************************************
  NAME         : HashedExpressionIndex
  DESCRIPTION  : Finds the expression bload array
                 index for a hashed expression
  INPUTS       : The expression
  RETURNS      : The bload index
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
unsigned long HashedExpressionIndex(
        const Environment::Ptr&theEnv,
        Expression *theExp) {
    ExpressionHashNode *exphash, *prv;
    unsigned hashval;

    if (theExp == nullptr)
        return ULONG_MAX;
    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    return ((exphash != nullptr) ? exphash->bsaveID : ULONG_MAX);
}

#endif /* (BLOAD_AND_BSAVE) */
#endif

// from Expression Parser
#if STUBBING_INACTIVE
/***************************************************/
/* Function0Parse: Parses a function. Assumes that */
/*   none of the function has been parsed yet.     */
/***************************************************/
Expression *Function0Parse(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    struct token theToken;
    Expression *top;

    /*=================================*/
    /* All functions begin with a '('. */
    /*=================================*/

    GetToken(theEnv, logicalName, &theToken);
    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "function calls");
        return nullptr;
    }

    /*=================================*/
    /* Parse the rest of the function. */
    /*=================================*/

    top = Function1Parse(theEnv, logicalName);
    return (top);
}


/*******************************************************/
/* Function1Parse: Parses a function. Assumes that the */
/*   opening left parenthesis has already been parsed. */
/*******************************************************/
Expression *Function1Parse(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    struct token theToken;
    Expression *top;

    /*========================*/
    /* Get the function name. */
    /*========================*/

    GetToken(theEnv, logicalName, &theToken);
    if (theToken.tknType != SYMBOL_TOKEN) {
        PrintErrorID(theEnv, "EXPRNPSR", 1, true);
        WriteString(theEnv, STDERR, "A function name must be a symbol.\n");
        return nullptr;
    }

    /*=================================*/
    /* Parse the rest of the function. */
    /*=================================*/

    top = Function2Parse(theEnv, logicalName, theToken.lexemeValue->contents);
    return (top);
}

/****************************************************/
/* Function2Parse: Parses a function. Assumes that  */
/*   the opening left parenthesis and function name */
/*   have already been parsed.                      */
/****************************************************/
Expression *Function2Parse(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        const char *name) {
    FunctionDefinition *theFunction;
    Expression *top;
    bool moduleSpecified = false;
    unsigned position;
    CLIPSLexeme *moduleName = nullptr, *constructName = nullptr;
#if DEFGENERIC_CONSTRUCT
    Defgeneric *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
    Deffunction *dptr;
#endif

    /*==============================*/
    /* Look for a module specifier. */
    /*==============================*/

    if ((position = FindModuleSeparator(name)) != 0) {
        moduleName = ExtractModuleName(theEnv, position, name);

        if (moduleName == nullptr) {
            PrintErrorID(theEnv, "EXPRNPSR", 7, true);
            WriteString(theEnv, STDERR, "Missing module name for '");
            WriteString(theEnv, STDERR, name);
            WriteString(theEnv, STDERR, "'.\n");
            return nullptr;
        }

        constructName = ExtractConstructName(theEnv, position, name, SYMBOL_TYPE);
        moduleSpecified = true;
    }

    /*================================*/
    /* Has the function been defined? */
    /*================================*/

    theFunction = FindFunction(theEnv, name);

#if DEFGENERIC_CONSTRUCT
    if (moduleSpecified) {
        if (ConstructExported(theEnv, "defgeneric", moduleName, constructName) ||
            GetCurrentModule(theEnv) == FindDefmodule(theEnv, moduleName->contents)) { gfunc = FindDefgenericInModule(theEnv, name); }
        else { gfunc = nullptr; }
    } else { gfunc = LookupDefgenericInScope(theEnv, name); }
#endif

#if DEFFUNCTION_CONSTRUCT
#if DEFGENERIC_CONSTRUCT
    if ((theFunction == nullptr)
        && (gfunc == nullptr))
#else
        if (theFunction == nullptr)
#endif
        if (moduleSpecified) {
            if (ConstructExported(theEnv, "deffunction", moduleName, constructName) ||
                GetCurrentModule(theEnv) == FindDefmodule(theEnv, moduleName->contents)) { dptr = FindDeffunctionInModule(theEnv, name); }
            else { dptr = nullptr; }
        } else { dptr = LookupDeffunctionInScope(theEnv, name); }
    else
        dptr = nullptr;
#endif

    /*=============================*/
    /* Define top level structure. */
    /*=============================*/

#if DEFFUNCTION_CONSTRUCT
    if (dptr != nullptr)
        top = GenConstant(theEnv, PCALL, dptr);
    else
#endif
#if DEFGENERIC_CONSTRUCT
    if (gfunc != nullptr)
        top = GenConstant(theEnv, GCALL, gfunc);
    else
#endif
    if (theFunction != nullptr)
        top = GenConstant(theEnv, FCALL, theFunction);
    else {
        PrintErrorID(theEnv, "EXPRNPSR", 3, true);
        WriteString(theEnv, STDERR, "Missing function declaration for '");
        WriteString(theEnv, STDERR, name);
        WriteString(theEnv, STDERR, "'.\n");
        return nullptr;
    }

    /*=======================================================*/
    /* Check to see if function has its own parsing routine. */
    /*=======================================================*/

    PushRtnBrkContexts(theEnv);
    ExpressionData(theEnv)->ReturnContext = false;
    ExpressionData(theEnv)->BreakContext = false;

#if DEFGENERIC_CONSTRUCT || DEFFUNCTION_CONSTRUCT
    if (top->type == FCALL)
#endif
    {
        if (theFunction->parser != nullptr) {
            top = (*theFunction->parser)(theEnv, top, logicalName);
            PopRtnBrkContexts(theEnv);
            if (top == nullptr) return nullptr;
            if (ReplaceSequenceExpansionOps(theEnv, top->argList, top, FindFunction(theEnv, "(expansion-call)"),
                                            FindFunction(theEnv, "expand$"))) {
                ReturnExpression(theEnv, top);
                return nullptr;
            }
            return (top);
        }
    }

    /*========================================*/
    /* Default parsing routine for functions. */
    /*========================================*/

    top = CollectArguments(theEnv, top, logicalName);
    PopRtnBrkContexts(theEnv);
    if (top == nullptr) return nullptr;

    if (ReplaceSequenceExpansionOps(theEnv, top->argList, top, FindFunction(theEnv, "(expansion-call)"),
                                    FindFunction(theEnv, "expand$"))) {
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    /*============================================================*/
    /* If the function call uses the sequence expansion operator, */
    /* its arguments cannot be checked until runtime.             */
    /*============================================================*/

    if (top->value == FindFunction(theEnv, "(expansion-call)")) { return (top); }

    /*============================*/
    /* Check for argument errors. */
    /*============================*/

    if (top->type == FCALL) {
        if (CheckExpressionAgainstRestrictions(theEnv, top, theFunction, name)) {
            ReturnExpression(theEnv, top);
            return nullptr;
        }
    }

#if DEFFUNCTION_CONSTRUCT
    else if (top->type == PCALL) {
        if (!CheckDeffunctionCall(theEnv, (Deffunction *) top->value, CountArguments(top->argList))) {
            ReturnExpression(theEnv, top);
            return nullptr;
        }
    }
#endif

    /*========================*/
    /* Return the expression. */
    /*========================*/

    return (top);
}

/***********************************************************************
  NAME         : ReplaceSequenceExpansionOps
  DESCRIPTION  : Replaces function calls which have multifield
                   references as arguments into a call to a
                   special function which expands the multifield
                   into single arguments at run-time.
                 Multifield references which are not function
                   arguments are errors
  INPUTS       : 1) The expression
                 2) The current function call
                 3) The address of the internal H/L function
                    (expansion-call)
                 4) The address of the H/L function expand$
  RETURNS      : False if OK, true on errors
  SIDE EFFECTS : Function call expressions modified, if necessary
  NOTES        : Function calls which truly want a multifield
                   to be passed need use only a single-field
                   refernce (i.e. ? instead of $? - the $ is
                   being treated as a special expansion operator)
 **********************************************************************/
bool ReplaceSequenceExpansionOps(
        const Environment::Ptr&theEnv,
        Expression *actions,
        Expression *fcallexp,
        void *expcall,
        void *expmult) {
    Expression *theExp;

    while (actions != nullptr) {
        if (!ExpressionData(theEnv)->SequenceOpMode &&
            ((actions->type == MF_VARIABLE) || (actions->type == MF_GBL_VARIABLE))) {
            if (actions->type == MF_VARIABLE) { actions->type = SF_VARIABLE; }
            else if (actions->type == MF_GBL_VARIABLE) { actions->type = GBL_VARIABLE; }
        }

        if ((actions->type == MF_VARIABLE) || (actions->type == MF_GBL_VARIABLE) ||
            (actions->value == expmult)) {
            if ((fcallexp->type != FCALL) ? false :
                !fcallexp->functionValue->sequenceuseok) {
                PrintErrorID(theEnv, "EXPRNPSR", 4, false);
                WriteString(theEnv, STDERR, "$ Sequence operator not a valid argument for function '");
                WriteString(theEnv, STDERR, fcallexp->functionValue->callFunctionName->contents);
                WriteString(theEnv, STDERR, "'.\n");
                return true;
            }
            if (fcallexp->value != expcall) {
                theExp = GenConstant(theEnv, fcallexp->type, fcallexp->value);
                theExp->argList = fcallexp->argList;
                theExp->nextArg = nullptr;
                fcallexp->type = FCALL;
                fcallexp->value = expcall;
                fcallexp->argList = theExp;
            }
            if (actions->value != expmult) {
                theExp = GenConstant(theEnv, SF_VARIABLE, actions->value);
                if (actions->type == MF_GBL_VARIABLE)
                    theExp->type = GBL_VARIABLE;
                actions->argList = theExp;
                actions->type = FCALL;
                actions->value = expmult;
            }
        }
        if (actions->argList != nullptr) {
            if ((actions->type == GCALL) ||
                (actions->type == PCALL) ||
                (actions->type == FCALL))
                theExp = actions;
            else
                theExp = fcallexp;
            if (ReplaceSequenceExpansionOps(theEnv, actions->argList, theExp, expcall, expmult))
                return true;
        }
        actions = actions->nextArg;
    }
    return false;
}

/*************************************************/
/* PushRtnBrkContexts: Saves the current context */
/*   for the break/return functions.             */
/*************************************************/
void PushRtnBrkContexts(
        const Environment::Ptr&theEnv) {
    SavedContexts *svtmp;

    svtmp = get_struct(theEnv, savedContexts);
    svtmp->rtn = ExpressionData(theEnv)->ReturnContext;
    svtmp->brk = ExpressionData(theEnv)->BreakContext;
    svtmp->nxt = ExpressionData(theEnv)->svContexts;
    ExpressionData(theEnv)->svContexts = svtmp;
}

/***************************************************/
/* PopRtnBrkContexts: Restores the current context */
/*   for the break/return functions.               */
/***************************************************/
void PopRtnBrkContexts(
        const Environment::Ptr&theEnv) {
    SavedContexts *svtmp;
    ExpressionData(theEnv)->ReturnContext = ExpressionData(theEnv)->svContexts->rtn;
    ExpressionData(theEnv)->BreakContext = ExpressionData(theEnv)->svContexts->brk;
    svtmp = ExpressionData(theEnv)->svContexts;
    ExpressionData(theEnv)->svContexts = ExpressionData(theEnv)->svContexts->nxt;
    rtn_struct(theEnv, savedContexts, svtmp);
}
#endif

#if STUBBING_INACTIVE
/**********************/
/* RestrictionExists: */
/**********************/
bool RestrictionExists(
        const char *restrictionString,
        int position) {
    int i = 0, currentPosition = 0;

    if (restrictionString == nullptr) { return false; }

    while (restrictionString[i] != '\0') {
        if (restrictionString[i] == ';') {
            if (currentPosition == position) return true;
            currentPosition++;
        }
        i++;
    }

    return position == currentPosition;

}


/*****************************************************************/
/* CheckExpressionAgainstRestrictions: Compares the arguments to */
/*   a function to the set of restrictions for that function to  */
/*   determine if any incompatibilities exist. If so, the value  */
/*   true is returned, otherwise false is returned.              */
/*****************************************************************/
FunctionArgumentsError CheckExpressionAgainstRestrictions(
        const Environment::Ptr&theEnv,
        Expression *theExpression,
        FunctionDefinition *theFunction,
        const char *functionName) {
    unsigned int j = 1;
    unsigned short number1, number2;
    unsigned short argCount;
    Expression *argPtr;
    const char *restrictions;
    unsigned defaultRestriction2, argRestriction2;

    if (theFunction->restrictions == nullptr) { restrictions = nullptr; }
    else { restrictions = theFunction->restrictions->contents; }

    /*=========================================*/
    /* Count the number of function arguments. */
    /*=========================================*/

    argCount = CountArguments(theExpression->argList);

    /*======================================*/
    /* Get the minimum number of arguments. */
    /*======================================*/

    number1 = theFunction->minArgs;

    /*======================================*/
    /* Get the maximum number of arguments. */
    /*======================================*/

    number2 = theFunction->maxArgs;

    /*============================================*/
    /* Check for the correct number of arguments. */
    /*============================================*/

    if ((number1 == UNBOUNDED) && (number2 == UNBOUNDED)) { /* Any number of arguments allowed. */ }
    else if (number1 == number2) {
        if (argCount != number1) {
            ExpectedCountError(theEnv, functionName, EXACTLY, number1);
            return FAE_COUNT_ERROR;
        }
    } else if (argCount < number1) {
        ExpectedCountError(theEnv, functionName, AT_LEAST, number1);
        return FAE_COUNT_ERROR;
    } else if ((number2 != UNBOUNDED) && (argCount > number2)) {
        ExpectedCountError(theEnv, functionName, NO_MORE_THAN, number2);
        return FAE_COUNT_ERROR;
    }

    /*===============================================*/
    /* Return if there are no argument restrictions. */
    /*===============================================*/

    if (restrictions == nullptr) return FAE_NO_ERROR;

    /*=======================================*/
    /* Check for the default argument types. */
    /*=======================================*/

    PopulateRestriction(theEnv, &defaultRestriction2, ANY_TYPE_BITS, restrictions, 0);

    /*======================*/
    /* Check each argument. */
    /*======================*/

    for (argPtr = theExpression->argList;
         argPtr != nullptr;
         argPtr = argPtr->nextArg) {
        PopulateRestriction(theEnv, &argRestriction2, defaultRestriction2, restrictions, j);

        if (CheckArgumentAgainstRestriction(theEnv, argPtr, argRestriction2)) {
            ExpectedTypeError0(theEnv, functionName, j);
            PrintTypesString(theEnv, STDERR, argRestriction2, true);
            return FAE_TYPE_ERROR;
        }

        j++;
    }

    return FAE_NO_ERROR;
}

/*******************************************************/
/* CollectArguments: Parses and groups together all of */
/*   the arguments for a function call expression.     */
/*******************************************************/
Expression *CollectArguments(
        const Environment::Ptr&theEnv,
        Expression *top,
        const char *logicalName) {
    bool errorFlag;
    Expression *lastOne, *nextOne;

    /*========================================*/
    /* Default parsing routine for functions. */
    /*========================================*/

    lastOne = nullptr;

    while (true) {
        SavePPBuffer(theEnv, " ");

        errorFlag = false;
        nextOne = ArgumentParse(theEnv, logicalName, &errorFlag);

        if (errorFlag) {
            ReturnExpression(theEnv, top);
            return nullptr;
        }

        if (nextOne == nullptr) {
            PPBackup(theEnv);
            PPBackup(theEnv);
            SavePPBuffer(theEnv, ")");
            return (top);
        }

        if (lastOne == nullptr) { top->argList = nextOne; }
        else { lastOne->nextArg = nextOne; }

        lastOne = nextOne;
    }
}

/********************************************/
/* ArgumentParse: Parses an argument within */
/*   a function call expression.            */
/********************************************/
Expression *ArgumentParse(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        bool *errorFlag) {
    Expression *top;
    struct token theToken;

    /*===============*/
    /* Grab a token. */
    /*===============*/

    GetToken(theEnv, logicalName, &theToken);

    /*============================*/
    /* ')' counts as no argument. */
    /*============================*/

    if (theToken.tknType == RIGHT_PARENTHESIS_TOKEN) { return nullptr; }

    /*================================*/
    /* Parse constants and variables. */
    /*================================*/

    if ((theToken.tknType == SF_VARIABLE_TOKEN) || (theToken.tknType == MF_VARIABLE_TOKEN) ||
        (theToken.tknType == SYMBOL_TOKEN) || (theToken.tknType == STRING_TOKEN) ||
        #if DEFGLOBAL_CONSTRUCT
        (theToken.tknType == GBL_VARIABLE_TOKEN) ||
        (theToken.tknType == MF_GBL_VARIABLE_TOKEN) ||
        #endif
        (theToken.tknType == INSTANCE_NAME_TOKEN) ||
        (theToken.tknType == FLOAT_TOKEN) || (theToken.tknType == INTEGER_TOKEN)) {
        return (GenConstant(theEnv, TokenTypeToType(theToken.tknType), theToken.value));
    }

    /*======================*/
    /* Parse function call. */
    /*======================*/

    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        PrintErrorID(theEnv, "EXPRNPSR", 2, true);
        WriteString(theEnv, STDERR, "Expected a constant, variable, or expression.\n");
        *errorFlag = true;
        return nullptr;
    }

    top = Function1Parse(theEnv, logicalName);
    if (top == nullptr) *errorFlag = true;
    return (top);
}

/************************************************************/
/* ParseAtomOrExpression: Parses an expression which may be */
/*   a function call, atomic value (string, symbol, etc.),  */
/*   or variable (local or global).                         */
/************************************************************/
Expression *ParseAtomOrExpression(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        struct token *useToken) {
    struct token theToken, *thisToken;
    Expression *rv;

    if (useToken == nullptr) {
        thisToken = &theToken;
        GetToken(theEnv, logicalName, thisToken);
    } else thisToken = useToken;

    if ((thisToken->tknType == SYMBOL_TOKEN) || (thisToken->tknType == STRING_TOKEN) ||
        (thisToken->tknType == INTEGER_TOKEN) || (thisToken->tknType == FLOAT_TOKEN) ||
        (thisToken->tknType == INSTANCE_NAME_TOKEN) ||
        #if DEFGLOBAL_CONSTRUCT
        (thisToken->tknType == GBL_VARIABLE_TOKEN) ||
        (thisToken->tknType == MF_GBL_VARIABLE_TOKEN) ||
        #endif
        (thisToken->tknType == SF_VARIABLE_TOKEN) || (thisToken->tknType == MF_VARIABLE_TOKEN)) {
        rv = GenConstant(theEnv, TokenTypeToType(thisToken->tknType), thisToken->value);
    } else if (thisToken->tknType == LEFT_PARENTHESIS_TOKEN) {
        rv = Function1Parse(theEnv, logicalName);
        if (rv == nullptr) return nullptr;
    } else {
        PrintErrorID(theEnv, "EXPRNPSR", 2, true);
        WriteString(theEnv, STDERR, "Expected a constant, variable, or expression.\n");
        return nullptr;
    }

    return (rv);
}

/*********************************************/
/* GroupActions: Groups together a series of */
/*   actions within a progn expression. Used */
/*   for example to parse the RHS of a rule. */
/*********************************************/
Expression *GroupActions(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        struct token *theToken,
        bool readFirstToken,
        const char *endWord,
        bool functionNameParsed) {
    Expression *top, *nextOne, *lastOne = nullptr;

    /*=============================*/
    /* Create the enclosing progn. */
    /*=============================*/

    top = GenConstant(theEnv, FCALL, FindFunction(theEnv, "progn"));

    /*========================================================*/
    /* Continue until all appropriate commands are processed. */
    /*========================================================*/

    while (true) {
        /*================================================*/
        /* Skip reading in the token if this is the first */
        /* pass and the initial token was already read    */
        /* before calling this function.                  */
        /*================================================*/

        if (readFirstToken) { GetToken(theEnv, logicalName, theToken); }
        else { readFirstToken = true; }

        /*=================================================*/
        /* Look to see if a symbol has terminated the list */
        /* of actions (such as "else" in an if function).  */
        /*=================================================*/

        if ((theToken->tknType == SYMBOL_TOKEN) &&
            (endWord != nullptr) &&
            (!functionNameParsed)) {
            if (strcmp(theToken->lexemeValue->contents, endWord) == 0) { return (top); }
        }

        /*====================================*/
        /* Process a function if the function */
        /* name has already been read.        */
        /*====================================*/

        if (functionNameParsed) {
            nextOne = Function2Parse(theEnv, logicalName, theToken->lexemeValue->contents);
            functionNameParsed = false;
        }

            /*========================================*/
            /* Process a constant or global variable. */
            /*========================================*/

        else if ((theToken->tknType == SYMBOL_TOKEN) || (theToken->tknType == STRING_TOKEN) ||
                 (theToken->tknType == INTEGER_TOKEN) || (theToken->tknType == FLOAT_TOKEN) ||
                 #if DEFGLOBAL_CONSTRUCT
                 (theToken->tknType == GBL_VARIABLE_TOKEN) ||
                 (theToken->tknType == MF_GBL_VARIABLE_TOKEN) ||
                 #endif
                 (theToken->tknType == INSTANCE_NAME_TOKEN) ||
                 (theToken->tknType == SF_VARIABLE_TOKEN) || (theToken->tknType == MF_VARIABLE_TOKEN)) {
            nextOne = GenConstant(theEnv, TokenTypeToType(theToken->tknType), theToken->value);
        }

            /*=============================*/
            /* Otherwise parse a function. */
            /*=============================*/

        else if (theToken->tknType == LEFT_PARENTHESIS_TOKEN) { nextOne = Function1Parse(theEnv, logicalName); }

            /*======================================*/
            /* Otherwise replace sequence expansion */
            /* variables and return the expression. */
            /*======================================*/

        else {
            if (ReplaceSequenceExpansionOps(theEnv, top, nullptr,
                                            FindFunction(theEnv, "(expansion-call)"),
                                            FindFunction(theEnv, "expand$"))) {
                ReturnExpression(theEnv, top);
                return nullptr;
            }

            return (top);
        }

        /*===========================*/
        /* Add the new action to the */
        /* list of progn arguments.  */
        /*===========================*/

        if (nextOne == nullptr) {
            theToken->tknType = UNKNOWN_VALUE_TOKEN;
            ReturnExpression(theEnv, top);
            return nullptr;
        }

        if (lastOne == nullptr) { top->argList = nextOne; }
        else { lastOne->nextArg = nextOne; }

        lastOne = nextOne;

        PPCRAndIndent(theEnv);
    }
}
#endif
/************************/
/* PopulateRestriction: */
/************************/
void PopulateRestriction(
        const Environment::Ptr&theEnv,
        unsigned *restriction,
        unsigned defaultRestriction,
        const std::string &restrictionString,
        unsigned int position) {
#if STUBBING_INACTIVE
    unsigned int i = 0, currentPosition = 0, valuesRead = 0;
    char buffer[2];

    *restriction = 0;

    if (restrictionString == nullptr) {
        *restriction = defaultRestriction;
        return;
    }

    while (restrictionString[i] != '\0') {
        char theChar = restrictionString[i];

        switch (theChar) {
            case ';':
                if (currentPosition == position) return;
                currentPosition++;
                *restriction = 0;
                valuesRead = 0;
                break;

            case 'l':
                *restriction |= INTEGER_BIT;
                valuesRead++;
                break;

            case 'd':
                *restriction |= FLOAT_BIT;
                valuesRead++;
                break;

            case 's':
                *restriction |= STRING_BIT;
                valuesRead++;
                break;

            case 'y':
                *restriction |= SYMBOL_BIT;
                valuesRead++;
                break;

            case 'n':
                *restriction |= INSTANCE_NAME_BIT;
                valuesRead++;
                break;

            case 'm':
                *restriction |= MULTIFIELD_BIT;
                valuesRead++;
                break;

            case 'f':
                *restriction |= FACT_ADDRESS_BIT;
                valuesRead++;
                break;

            case 'i':
                *restriction |= INSTANCE_ADDRESS_BIT;
                valuesRead++;
                break;

            case 'e':
                *restriction |= EXTERNAL_ADDRESS_BIT;
                valuesRead++;
                break;

            case 'v':
                *restriction |= VOID_BIT;
                valuesRead++;
                break;

            case 'b':
                *restriction |= BOOLEAN_BIT;
                valuesRead++;
                break;

            case '*':
                *restriction |= ANY_TYPE_BITS;
                valuesRead++;
                break;

            default:
                buffer[0] = theChar;
                buffer[1] = 0;
                WriteString(theEnv, STDERR, "Invalid argument type character ");
                WriteString(theEnv, STDERR, buffer);
                WriteString(theEnv, STDERR, "\n");
                valuesRead++;
                break;
        }

        i++;
    }

    if (position == currentPosition) {
        if (valuesRead == 0) { *restriction = defaultRestriction; }
        return;
    }

    *restriction = defaultRestriction;
#endif
}
#if STUBBING_INACTIVE
/*******************************************/
/* ParseConstantArguments: Parses a string */
/*    into a set of constant expressions.  */
/*******************************************/
Expression *ParseConstantArguments(
        const Environment::Ptr&theEnv,
        const char *argstr,
        bool *error) {
    Expression *top = nullptr, *bot = nullptr, *tmp;
    const char *router = "***FNXARGS***";
    struct token tkn;
    const char *oldRouter;
    const char *oldString;
    long oldIndex;

    *error = false;

    if (argstr == nullptr) return nullptr;

    /*=============================*/
    /* Use the fast router bypass. */
    /*=============================*/

    oldRouter = RouterData(theEnv)->FastCharGetRouter;
    oldString = RouterData(theEnv)->FastCharGetString;
    oldIndex = RouterData(theEnv)->FastCharGetIndex;

    RouterData(theEnv)->FastCharGetRouter = router;
    RouterData(theEnv)->FastCharGetString = argstr;
    RouterData(theEnv)->FastCharGetIndex = 0;

    /*======================*/
    /* Parse the constants. */
    /*======================*/

    GetToken(theEnv, router, &tkn);
    while (tkn.tknType != STOP_TOKEN) {
        if ((tkn.tknType != SYMBOL_TOKEN) && (tkn.tknType != STRING_TOKEN) &&
            (tkn.tknType != FLOAT_TOKEN) && (tkn.tknType != INTEGER_TOKEN) &&
            (tkn.tknType != INSTANCE_NAME_TOKEN)) {
            PrintErrorID(theEnv, "EXPRNPSR", 6, false);
            WriteString(theEnv, STDERR, "Only constant arguments allowed for external function call.\n");
            ReturnExpression(theEnv, top);
            *error = true;
            CloseStringSource(theEnv, router);
            return nullptr;
        }
        tmp = GenConstant(theEnv, TokenTypeToType(tkn.tknType), tkn.value);
        if (top == nullptr)
            top = tmp;
        else
            bot->nextArg = tmp;
        bot = tmp;
        GetToken(theEnv, router, &tkn);
    }

    /*===========================================*/
    /* Restore the old state of the fast router. */
    /*===========================================*/

    RouterData(theEnv)->FastCharGetRouter = oldRouter;
    RouterData(theEnv)->FastCharGetString = oldString;
    RouterData(theEnv)->FastCharGetIndex = oldIndex;

    /*=======================*/
    /* Return the arguments. */
    /*=======================*/

    return (top);
}

/************************/
/* RemoveUnneededProgn: */
/************************/
Expression *RemoveUnneededProgn(
        const Environment::Ptr&theEnv,
        Expression *theExpression) {
    FunctionDefinition *fptr;
    Expression *temp;

    if (theExpression == nullptr) return (theExpression);

    if (theExpression->type != FCALL) return (theExpression);

    fptr = theExpression->functionValue;

    if (fptr->functionPointer != PrognFunction) { return (theExpression); }

    if ((theExpression->argList != nullptr) &&
        (theExpression->argList->nextArg == nullptr)) {
        temp = theExpression;
        theExpression = theExpression->argList;
        temp->argList = nullptr;
        temp->nextArg = nullptr;
        ReturnExpression(theEnv, temp);
    }

    return (theExpression);
}
#endif
// from expression binary

#if (BLOAD_AND_BSAVE)
#include <cstdio>

#include "BinaryLoad.h"
#include "BinarySave.h"
#include "Construct.h"
#include "Deffacts.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"

#include "Network.h"

#if DEFGENERIC_CONSTRUCT
#include "GenericFunctionBinaryLoadSave.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "DeffunctionBinaryLoadSave.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "Fact.h"
#include "DeftemplateBinarySaveLoad.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "DefglobalBinary.h"
#endif

#include "ObjectBinaryLoadSave.h"
#include "InstanceFunctions.h"
#include "InstanceCommand.h"

#include "Expression.h"
#include "ReferenceCounted.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void UpdateExpression(const Environment::Ptr&, void *, unsigned long);

/***********************************************************/
/* AllocateExpressions: Determines the amount of space     */
/*   required for loading the binary image of expressions  */
/*   and allocates that amount of space.                   */
/***********************************************************/
void AllocateExpressions(
        const Environment::Ptr&theEnv) {
    size_t space;

    GenReadBinary(theEnv, &ExpressionData(theEnv)->NumberOfExpressions, sizeof(long));
    if (ExpressionData(theEnv)->NumberOfExpressions == 0L)
        ExpressionData(theEnv)->ExpressionArray = nullptr;
    else {
        space = ExpressionData(theEnv)->NumberOfExpressions * sizeof(Expression);
        ExpressionData(theEnv)->ExpressionArray = (Expression *) genalloc(theEnv, space);
    }
}

/**********************************************/
/* RefreshExpressions: Refreshes the pointers */
/*   used by the expression binary image.     */
/**********************************************/
void RefreshExpressions(
        const Environment::Ptr&theEnv) {
    if (ExpressionData(theEnv)->ExpressionArray == nullptr) return;

    BloadandRefresh(theEnv, ExpressionData(theEnv)->NumberOfExpressions,
                    sizeof(BSAVE_EXPRESSION), UpdateExpression);
}

/*********************************************************
  NAME         : UpdateExpression
  DESCRIPTION  : Given a bloaded expression buffer,
                   this routine refreshes the pointers
                   in the expression array
  INPUTS       : 1) a bloaded expression buffer
                 2) the index of the expression to refresh
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expression updated
  NOTES        : None
 *********************************************************/
static void UpdateExpression(
        const Environment::Ptr&theEnv,
        void *buf,
        unsigned long obji) {
    BSAVE_EXPRESSION *bexp;
    unsigned long theIndex;

    bexp = (BSAVE_EXPRESSION *) buf;
    ExpressionData(theEnv)->ExpressionArray[obji].type = bexp->type;
    switch (bexp->type) {
        case FCALL:
            ExpressionData(theEnv)->ExpressionArray[obji].value = BloadData(theEnv)->FunctionArray[bexp->value];
            break;

        case GCALL:
#if DEFGENERIC_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = GenericPointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case PCALL:
#if DEFFUNCTION_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = DeffunctionPointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = DeftemplatePointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case DEFCLASS_PTR:
            ExpressionData(theEnv)->ExpressionArray[obji].value = DefclassPointer(bexp->value);
            break;

        case DEFGLOBAL_PTR:

#if DEFGLOBAL_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = DefglobalPointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case INTEGER_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->IntegerArray[bexp->value];
            IncrementIntegerCount(ExpressionData(theEnv)->ExpressionArray[obji].integerValue);
            break;

        case FLOAT_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->FloatArray[bexp->value];
            IncrementFloatCount(ExpressionData(theEnv)->ExpressionArray[obji].floatValue);
            break;

        case INSTANCE_NAME_TYPE:
        case GBL_VARIABLE:
        case SYMBOL_TYPE:
        case STRING_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->SymbolArray[bexp->value];
            IncrementLexemeCount(ExpressionData(theEnv)->ExpressionArray[obji].lexemeValue);
            break;

#if DEFTEMPLATE_CONSTRUCT
        case FACT_ADDRESS_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = &FactData(theEnv)->DummyFact;
            RetainFact((Fact *) ExpressionData(theEnv)->ExpressionArray[obji].value);
            break;
#endif

        case INSTANCE_ADDRESS_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = &InstanceData(theEnv)->DummyInstance;
            RetainInstance((Instance *) ExpressionData(theEnv)->ExpressionArray[obji].value);
            break;

        case EXTERNAL_ADDRESS_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
            break;

        case VOID_TYPE:
            break;

        default:
            if (EvaluationData(theEnv)->PrimitivesArray[bexp->type] == nullptr) break;
            if (EvaluationData(theEnv)->PrimitivesArray[bexp->type]->bitMap) {
                ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->BitMapArray[bexp->value];
                IncrementBitMapCount((BitMap *) ExpressionData(theEnv)->ExpressionArray[obji].value);
            }
            break;
    }

    theIndex = bexp->nextArg;
    if (theIndex == ULONG_MAX) { ExpressionData(theEnv)->ExpressionArray[obji].nextArg = nullptr; }
    else { ExpressionData(theEnv)->ExpressionArray[obji].nextArg = (Expression *) &ExpressionData(theEnv)->ExpressionArray[theIndex]; }

    theIndex = bexp->argList;
    if (theIndex == ULONG_MAX) { ExpressionData(theEnv)->ExpressionArray[obji].argList = nullptr; }
    else { ExpressionData(theEnv)->ExpressionArray[obji].argList = (Expression *) &ExpressionData(theEnv)->ExpressionArray[theIndex]; }
}

/*********************************************/
/* ClearBloadedExpressions: Clears the space */
/*   utilized by an expression binary image. */
/*********************************************/
void ClearBloadedExpressions(
        const Environment::Ptr&theEnv) {
    unsigned long i;
    size_t space;

    /*===============================================*/
    /* Update the busy counts of atomic data values. */
    /*===============================================*/

    for (i = 0; i < ExpressionData(theEnv)->NumberOfExpressions; i++) {
        switch (ExpressionData(theEnv)->ExpressionArray[i].type) {
            case SYMBOL_TYPE          :
            case STRING_TYPE          :
            case INSTANCE_NAME_TYPE   :
            case GBL_VARIABLE    :
                ReleaseLexeme(theEnv, ExpressionData(theEnv)->ExpressionArray[i].lexemeValue);
                break;
            case FLOAT_TYPE           :
                ReleaseFloat(theEnv, ExpressionData(theEnv)->ExpressionArray[i].floatValue);
                break;
            case INTEGER_TYPE         :
                ReleaseInteger(theEnv, ExpressionData(theEnv)->ExpressionArray[i].integerValue);
                break;

#if DEFTEMPLATE_CONSTRUCT
            case FACT_ADDRESS_TYPE    :
                ReleaseFact((Fact *) ExpressionData(theEnv)->ExpressionArray[i].value);
                break;
#endif

            case INSTANCE_ADDRESS_TYPE :
                ReleaseInstance((Instance *) ExpressionData(theEnv)->ExpressionArray[i].value);
                break;
            case VOID_TYPE:
                break;

            default:
                if (EvaluationData(theEnv)->PrimitivesArray[ExpressionData(theEnv)->ExpressionArray[i].type] == nullptr) break;
                if (EvaluationData(theEnv)->PrimitivesArray[ExpressionData(
                        theEnv)->ExpressionArray[i].type]->bitMap) {
                    DecrementBitMapReferenceCount(theEnv, (BitMap *) ExpressionData(
                            theEnv)->ExpressionArray[i].value);
                }
                break;
        }
    }

    /*===================================*/
    /* Free the binary expression array. */
    /*===================================*/

    space = ExpressionData(theEnv)->NumberOfExpressions * sizeof(Expression);
    if (space != 0) genfree(theEnv, ExpressionData(theEnv)->ExpressionArray, space);
    ExpressionData(theEnv)->ExpressionArray = 0;
}

#if BLOAD_AND_BSAVE

/***************************************************
  NAME         : FindHashedExpressions
  DESCRIPTION  : Sets the bsave expression array
                 indices for hashed expression nodes
                 and marks the items needed by
                 these expressions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Atoms marked and ids set
  NOTES        : None
 ***************************************************/
void FindHashedExpressions(
        const Environment::Ptr&theEnv) {
    unsigned i;
    ExpressionHashNode *exphash;

    for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        for (exphash = ExpressionData(theEnv)->ExpressionHashTable[i]; exphash != nullptr; exphash = exphash->next) {
            MarkNeededItems(theEnv, exphash->exp);
            exphash->bsaveID = ExpressionData(theEnv)->ExpressionCount;
            ExpressionData(theEnv)->ExpressionCount += ExpressionSize(exphash->exp);
        }
}

/***************************************************
  NAME         : BsaveHashedExpressions
  DESCRIPTION  : Writes out hashed expressions
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions written
  NOTES        : None
 ***************************************************/
void BsaveHashedExpressions(
        const Environment::Ptr&theEnv,
        FILE *fp) {
    unsigned i;
    ExpressionHashNode *exphash;

    for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        for (exphash = ExpressionData(theEnv)->ExpressionHashTable[i]; exphash != nullptr; exphash = exphash->next)
            BsaveExpression(theEnv, exphash->exp, fp);
}

/***************************************************************/
/* BsaveConstructExpressions: Writes all expression needed by  */
/*   constructs for this binary image to the binary save file. */
/***************************************************************/
void BsaveConstructExpressions(
        const Environment::Ptr&theEnv,
        FILE *fp) {
    struct BinaryItem *biPtr;

    for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
         biPtr != nullptr;
         biPtr = biPtr->next) {
        if (biPtr->expressionFunction != nullptr) { (*biPtr->expressionFunction)(theEnv, fp); }
    }
}

/***************************************/
/* BsaveExpression: Recursively saves  */
/*   an expression to the binary file. */
/***************************************/
void BsaveExpression(
        const Environment::Ptr&theEnv,
        Expression *testPtr,
        FILE *fp) {
    BSAVE_EXPRESSION newTest;
    unsigned long newIndex;

    while (testPtr != nullptr) {
        ExpressionData(theEnv)->ExpressionCount++;

        /*================*/
        /* Copy the type. */
        /*================*/

        newTest.type = testPtr->type;

        /*=======================================*/
        /* Convert the argList slot to an index. */
        /*=======================================*/

        if (testPtr->argList == nullptr) { newTest.argList = ULONG_MAX; }
        else { newTest.argList = ExpressionData(theEnv)->ExpressionCount; }

        /*========================================*/
        /* Convert the nextArg slot to an index. */
        /*========================================*/

        if (testPtr->nextArg == nullptr) { newTest.nextArg = ULONG_MAX; }
        else {
            newIndex = ExpressionData(theEnv)->ExpressionCount + ExpressionSize(testPtr->argList);
            newTest.nextArg = newIndex;
        }

        /*=========================*/
        /* Convert the value slot. */
        /*=========================*/

        switch (testPtr->type) {
            case FLOAT_TYPE:
                newTest.value = testPtr->floatValue->bucket;
                break;

            case INTEGER_TYPE:
                newTest.value = testPtr->integerValue->bucket;
                break;

            case FCALL:
                newTest.value = testPtr->functionValue->bsaveIndex;
                break;

            case GCALL:
#if DEFGENERIC_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case PCALL:
#if DEFFUNCTION_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case DEFCLASS_PTR:
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
                    newTest.value = ULONG_MAX;
                break;

            case DEFGLOBAL_PTR:
#if DEFGLOBAL_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case INSTANCE_NAME_TYPE:
            case SYMBOL_TYPE:
            case GBL_VARIABLE:
            case STRING_TYPE:
                newTest.value = testPtr->lexemeValue->bucket;
                break;

            case FACT_ADDRESS_TYPE:
            case INSTANCE_ADDRESS_TYPE:
            case EXTERNAL_ADDRESS_TYPE:
                newTest.value = ULONG_MAX;
                break;

            case VOID_TYPE:
                break;

            default:
                if (EvaluationData(theEnv)->PrimitivesArray[testPtr->type] == nullptr) break;
                if (EvaluationData(
                        theEnv)->PrimitivesArray[testPtr->type]->bitMap) { newTest.value = ((BitMap *) testPtr->value)->bucket; }
                break;
        }

        /*===========================*/
        /* Write out the expression. */
        /*===========================*/

        GenWrite(&newTest, sizeof(BSAVE_EXPRESSION), fp);

        /*==========================*/
        /* Write out argument list. */
        /*==========================*/

        if (testPtr->argList != nullptr) {
            BsaveExpression(theEnv, testPtr->argList, fp);
        }

        testPtr = testPtr->nextArg;
    }
}

#endif /* BLOAD_AND_BSAVE */
#endif /* (BLOAD_AND_BSAVE) */

// from expression operations
#if STUBBING_INACTIVE
/************************************/
/* CheckArgumentAgainstRestriction: */
/************************************/
bool CheckArgumentAgainstRestriction(
        const Environment::Ptr&theEnv,
        Expression *theExpression,
        unsigned theRestriction) {
    CONSTRAINT_RECORD *cr1, *cr2, *cr3;

    /*=============================================*/
    /* Generate a constraint record for the actual */
    /* argument passed to the function.            */
    /*=============================================*/

    cr1 = ExpressionToConstraintRecord(theEnv, theExpression);

    /*================================================*/
    /* Generate a constraint record based on the type */
    /* of argument expected by the function.          */
    /*================================================*/

    cr2 = ArgumentTypeToConstraintRecord(theEnv, theRestriction);

    /*===============================================*/
    /* Intersect the two constraint records and then */
    /* discard them.                                 */
    /*===============================================*/

    cr3 = IntersectConstraints(theEnv, cr1, cr2);

    RemoveConstraint(theEnv, cr1);
    RemoveConstraint(theEnv, cr2);

    /*====================================================*/
    /* If the intersection of the two constraint records  */
    /* is empty, then the argument passed to the function */
    /* doesn't satisfy the restrictions for the argument. */
    /*====================================================*/

    if (UnmatchableConstraint(cr3)) {
        RemoveConstraint(theEnv, cr3);
        return true;
    }

    /*===================================================*/
    /* The argument satisfies the function restrictions. */
    /*===================================================*/

    RemoveConstraint(theEnv, cr3);
    return false;
}
#endif

/*****************************************************************************/
/* IdenticalExpression: Determines if two expressions are identical. Returns */
/*   true if the expressions are identical, otherwise false is returned.     */
/*****************************************************************************/
bool IdenticalExpression(
            Expression *firstList,
            Expression *secondList) {
#if STUBBING_INACTIVE
    /*==============================================*/
    /* Compare each argument in both expressions by */
    /* following the nextArg list.                  */
    /*==============================================*/

    for (;
            (firstList != nullptr) && (secondList != nullptr);
            firstList = firstList->nextArg, secondList = secondList->nextArg) {
        /*=========================*/
        /* Compare type and value. */
        /*=========================*/

        if (firstList->type != secondList->type) { return false; }

        if (firstList->value != secondList->value) { return false; }

        /*==============================*/
        /* Compare the arguments lists. */
        /*==============================*/

        if (!IdenticalExpression(firstList->argList, secondList->argList)) { return false; }
    }

    /*=====================================================*/
    /* If firstList and secondList aren't both nullptr, then  */
    /* one of the lists contains more expressions than the */
    /* other.                                              */
    /*=====================================================*/

    return firstList == secondList;

    /*============================*/
    /* Expressions are identical. */
    /*============================*/
#endif
    return false;

}

#if STUBBING_INACTIVE
/******************************************/
/* CopyExpresssion: Copies an expression. */
/******************************************/
Expression *CopyExpression(
        const Environment::Ptr&theEnv,
        Expression *original) {
    Expression *topLevel, *next, *last;

    if (original == nullptr) return nullptr;

    topLevel = GenConstant(theEnv, original->type, original->value);
    topLevel->argList = CopyExpression(theEnv, original->argList);

    last = topLevel;
    original = original->nextArg;
    while (original != nullptr) {
        next = GenConstant(theEnv, original->type, original->value);
        next->argList = CopyExpression(theEnv, original->argList);

        last->nextArg = next;
        last = next;
        original = original->nextArg;
    }

    return (topLevel);
}

/************************************************************/
/* ExpressionContainsVariables: Determines if an expression */
/*   contains any variables. Returns true if the expression */
/*   contains any variables, otherwise false is returned.   */
/************************************************************/
bool ExpressionContainsVariables(
        Expression *theExpression,
        bool globalsAreVariables) {
    while (theExpression != nullptr) {
        if (theExpression->argList != nullptr) {
            if (ExpressionContainsVariables(theExpression->argList, globalsAreVariables)) { return true; }
        }

        if ((theExpression->type == MF_VARIABLE) ||
            (theExpression->type == SF_VARIABLE) ||
            (((theExpression->type == GBL_VARIABLE) ||
              (theExpression->type == MF_GBL_VARIABLE)) &&
             globalsAreVariables)) { return true; }

        theExpression = theExpression->nextArg;
    }

    return false;
}
#endif
size_t
Expression::size() const noexcept {
    return 1 + _args.size();
}
#if STUBBING_INACTIVE
/*************************************************/
/* PrintExpression: Pretty prints an expression. */
/*************************************************/
void PrintExpression(
        const Environment::Ptr&theEnv,
        const char *fileid,
        Expression *theExpression) {
    Expression *oldExpression;

    if (theExpression == nullptr) { return; }

    while (theExpression != nullptr) {
        switch (theExpression->type) {
            case SF_VARIABLE:
            case GBL_VARIABLE:
                WriteString(theEnv, fileid, "?");
                WriteString(theEnv, fileid, theExpression->lexemeValue->contents);
                break;

            case MF_VARIABLE:
            case MF_GBL_VARIABLE:
                WriteString(theEnv, fileid, "$?");
                WriteString(theEnv, fileid, theExpression->lexemeValue->contents);
                break;

            case FCALL:
                WriteString(theEnv, fileid, "(");
                WriteString(theEnv, fileid, ExpressionFunctionCallName(theExpression)->contents);
                if (theExpression->argList != nullptr) { WriteString(theEnv, fileid, " "); }
                PrintExpression(theEnv, fileid, theExpression->argList);
                WriteString(theEnv, fileid, ")");
                break;

            default:
                oldExpression = EvaluationData(theEnv)->CurrentExpression;
                EvaluationData(theEnv)->CurrentExpression = theExpression;
                PrintAtom(theEnv, fileid, theExpression->type, theExpression->value);
                EvaluationData(theEnv)->CurrentExpression = oldExpression;
                break;
        }

        theExpression = theExpression->nextArg;
        if (theExpression != nullptr) WriteString(theEnv, fileid, " ");
    }

}

/*************************************************************************/
/* CombineExpressions: Combines two expressions into a single equivalent */
/*   expression. Mainly serves to merge expressions containing "and"     */
/*   and "or" expressions without unnecessary duplication of the "and"   */
/*   and "or" expressions (i.e., two "and" expressions can be merged by  */
/*   placing them as arguments within another "and" expression, but it   */
/*   is more efficient to add the arguments of one of the "and"          */
/*   expressions to the list of arguments for the other and expression). */
/*************************************************************************/
Expression *CombineExpressions(
        const Environment::Ptr&theEnv,
        Expression *expr1,
        Expression *expr2) {
    Expression *tempPtr;

    /*===========================================================*/
    /* If the 1st expression is nullptr, return the 2nd expression. */
    /*===========================================================*/

    if (expr1 == nullptr) return (expr2);

    /*===========================================================*/
    /* If the 2nd expression is nullptr, return the 1st expression. */
    /*===========================================================*/

    if (expr2 == nullptr) return (expr1);

    /*============================================================*/
    /* If the 1st expression is an "and" expression, and the 2nd  */
    /* expression is not an "and" expression, then include the    */
    /* 2nd expression in the argument list of the 1st expression. */
    /*============================================================*/

    if ((expr1->value == ExpressionData(theEnv)->PTR_AND) &&
        (expr2->value != ExpressionData(theEnv)->PTR_AND)) {
        tempPtr = expr1->argList;
        if (tempPtr == nullptr) {
            rtn_struct(theEnv, Expression, expr1);
            return (expr2);
        }

        while (tempPtr->nextArg != nullptr) { tempPtr = tempPtr->nextArg; }

        tempPtr->nextArg = expr2;
        return (expr1);
    }

    /*============================================================*/
    /* If the 2nd expression is an "and" expression, and the 1st  */
    /* expression is not an "and" expression, then include the    */
    /* 1st expression in the argument list of the 2nd expression. */
    /*============================================================*/

    if ((expr1->value != ExpressionData(theEnv)->PTR_AND) &&
        (expr2->value == ExpressionData(theEnv)->PTR_AND)) {
        tempPtr = expr2->argList;
        if (tempPtr == nullptr) {
            rtn_struct(theEnv, Expression, expr2);
            return (expr1);
        }

        expr2->argList = expr1;
        expr1->nextArg = tempPtr;

        return (expr2);
    }

    /*===========================================================*/
    /* If both expressions are "and" expressions, then add the   */
    /* 2nd expression to the argument list of the 1st expression */
    /* and throw away the extraneous "and" expression.           */
    /*===========================================================*/

    if ((expr1->value == ExpressionData(theEnv)->PTR_AND) &&
        (expr2->value == ExpressionData(theEnv)->PTR_AND)) {
        tempPtr = expr1->argList;
        if (tempPtr == nullptr) {
            rtn_struct(theEnv, Expression, expr1);
            return (expr2);
        }

        while (tempPtr->nextArg != nullptr) { tempPtr = tempPtr->nextArg; }

        tempPtr->nextArg = expr2->argList;
        rtn_struct(theEnv, Expression, expr2);

        return (expr1);
    }

    /*=====================================================*/
    /* If neither expression is an "and" expression, then  */
    /* create an "and" expression and add both expressions */
    /* to the argument list of that "and" expression.      */
    /*=====================================================*/

    tempPtr = GenConstant(theEnv, FCALL, ExpressionData(theEnv)->PTR_AND);
    tempPtr->argList = expr1;
    expr1->nextArg = expr2;
    return (tempPtr);
}

/*********************/
/* NegateExpression: */
/*********************/
Expression *NegateExpression(
        const Environment::Ptr&theEnv,
        Expression *theExpression) {
    Expression *tempPtr;

    /*=========================================*/
    /* If the expression is nullptr, return nullptr. */
    /*=========================================*/

    if (theExpression == nullptr) return nullptr;

    /*==================================================*/
    /* The expression is already wrapped within a "not" */
    /* function call, just remove the function call.    */
    /*==================================================*/

    if (theExpression->value == ExpressionData(theEnv)->PTR_NOT) {
        tempPtr = theExpression->argList;
        rtn_struct(theEnv, Expression, theExpression);
        return (tempPtr);
    }

    /*===================================================*/
    /* Wrap the expression within a "not" function call. */
    /*===================================================*/

    tempPtr = GenConstant(theEnv, FCALL, ExpressionData(theEnv)->PTR_NOT);
    tempPtr->argList = theExpression;

    return (tempPtr);
}

/********************************************************/
/* AppendExpressions: Attaches an expression to the end */
/*   of another expression's nextArg list.              */
/********************************************************/
Expression *AppendExpressions(
        Expression *expr1,
        Expression *expr2) {
    Expression *tempPtr;

    /*===========================================================*/
    /* If the 1st expression is nullptr, return the 2nd expression. */
    /*===========================================================*/

    if (expr1 == nullptr) return (expr2);

    /*===========================================================*/
    /* If the 2nd expression is nullptr, return the 1st expression. */
    /*===========================================================*/

    if (expr2 == nullptr) return (expr1);

    /*====================================*/
    /* Find the end of the 1st expression */
    /* and attach the 2nd expression.     */
    /*====================================*/

    tempPtr = expr1;
    while (tempPtr->nextArg != nullptr) tempPtr = tempPtr->nextArg;
    tempPtr->nextArg = expr2;

    /*===============================*/
    /* Return the merged expression. */
    /*===============================*/

    return (expr1);
}
#endif

} // end namespace maya
