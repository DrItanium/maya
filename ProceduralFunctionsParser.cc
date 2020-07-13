/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  02/19/20             */
/*                                                     */
/*          PROCEDURAL FUNCTIONS PARSER MODULE         */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.31: Fixed 'while' function bug with optional use   */
/*            of 'do' keyword.                               */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include <cstdio>

#include "Setup.h"

#include "ArgumentAccess.h"
#include "Constraint.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "DefmoduleUtility.h"
#include "Multifield.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Scanner.h"
#include "Utility.h"

#include "ProceduralFunctionsParser.h"

#if DEFGLOBAL_CONSTRUCT
#include "Defglobal.h"
#endif

constexpr auto PRCDRPSR_DATA = 12;

struct procedureParserData : public EnvironmentModule {
    BindInfo *ListOfParsedBindNames = nullptr;
};
RegisterEnvironmentModule(procedureParserData, PRCDRPSR_DATA, ProcedureParser);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
#if STUBBING_INACTIVE
static void DeallocateProceduralFunctionData(const Environment&);
static Expression *WhileParse(const Environment&, Expression *, const char *);
static Expression *LoopForCountParse(const Environment&, Expression *, const char *);
static void ReplaceLoopCountVars(const Environment&, CLIPSLexeme *, Expression *, int);
static Expression *IfParse(const Environment&, Expression *, const char *);
static Expression *PrognParse(const Environment&, Expression *, const char *);
static Expression *BindParse(const Environment&, Expression *, const char *);
static int AddBindName(const Environment&, CLIPSLexeme *, CONSTRAINT_RECORD *);
static Expression *ReturnParse(const Environment&, Expression *, const char *);
static Expression *BreakParse(const Environment&, Expression *, const char *);
static Expression *SwitchParse(const Environment&, Expression *, const char *);

/*****************************/
/* ProceduralFunctionParsers */
/*****************************/
void ProceduralFunctionParsers(
        const Environment&theEnv) {
    //AllocateEnvironmentData(theEnv, PRCDRPSR_DATA, sizeof(procedureParserData), DeallocateProceduralFunctionData);
    theEnv->allocateEnvironmentModule<procedureParserData>();

    AddFunctionParser(theEnv, "bind", BindParse);
    AddFunctionParser(theEnv, "progn", PrognParse);
    AddFunctionParser(theEnv, "if", IfParse);
    AddFunctionParser(theEnv, "while", WhileParse);
    AddFunctionParser(theEnv, "loop-for-count", LoopForCountParse);
    AddFunctionParser(theEnv, "return", ReturnParse);
    AddFunctionParser(theEnv, "break", BreakParse);
    AddFunctionParser(theEnv, "switch", SwitchParse);
}
#endif

/*************************************************************/
/* DeallocateProceduralFunctionData: Deallocates environment */
/*    data for procedural functions.                         */
/*************************************************************/
static void DeallocateProceduralFunctionData(
        const Environment&theEnv) {
    struct BindInfo *temp_bind;

    while (ProcedureParserData(theEnv)->ListOfParsedBindNames != nullptr) {
        temp_bind = ProcedureParserData(theEnv)->ListOfParsedBindNames->next;
        rtn_struct(theEnv, BindInfo, ProcedureParserData(theEnv)->ListOfParsedBindNames);
        ProcedureParserData(theEnv)->ListOfParsedBindNames = temp_bind;
    }
}

/***********************/
/* GetParsedBindNames: */
/***********************/
struct BindInfo *GetParsedBindNames(
        const Environment&theEnv) {
    return (ProcedureParserData(theEnv)->ListOfParsedBindNames);
}

/***********************/
/* SetParsedBindNames: */
/***********************/
void SetParsedBindNames(
        const Environment&theEnv,
        struct BindInfo *newValue) {
    ProcedureParserData(theEnv)->ListOfParsedBindNames = newValue;
}

/*************************/
/* ClearParsedBindNames: */
/*************************/
void ClearParsedBindNames(
        const Environment&theEnv) {
    struct BindInfo *temp_bind;

    while (ProcedureParserData(theEnv)->ListOfParsedBindNames != nullptr) {
        temp_bind = ProcedureParserData(theEnv)->ListOfParsedBindNames->next;
        RemoveConstraint(theEnv, ProcedureParserData(theEnv)->ListOfParsedBindNames->constraints);
        rtn_struct(theEnv, BindInfo, ProcedureParserData(theEnv)->ListOfParsedBindNames);
        ProcedureParserData(theEnv)->ListOfParsedBindNames = temp_bind;
    }
}

/*************************/
/* ParsedBindNamesEmpty: */
/*************************/
bool ParsedBindNamesEmpty(
        const Environment&theEnv) {
    return ProcedureParserData(theEnv)->ListOfParsedBindNames == nullptr;

}
#if STUBBING_INACTIVE
/*********************************************************/
/* WhileParse: purpose is to parse the while statement.  */
/*   The parse of the statement is the return value.     */
/*   Syntax: (while <expression> do <action>+)           */
/*********************************************************/
static Expression *WhileParse(
        const Environment&theEnv,
        Expression *parse,
        const char *infile) {
    struct token theToken;
    bool read_first_token;

    /*===============================*/
    /* Process the while expression. */
    /*===============================*/

    SavePPBuffer(theEnv, " ");

    parse->argList = ParseAtomOrExpression(theEnv, infile, nullptr);
    if (parse->argList == nullptr) {
        ReturnExpression(theEnv, parse);
        return nullptr;
    }

    /*====================================*/
    /* Process the do keyword if present. */
    /*====================================*/

    GetToken(theEnv, infile, &theToken);
    if ((theToken.tknType == SYMBOL_TOKEN) && (strcmp(theToken.lexemeValue->contents, "do") == 0)) {
        read_first_token = true;
        PPBackup(theEnv);
        SavePPBuffer(theEnv, " ");
        SavePPBuffer(theEnv, theToken.printForm);
        IncrementIndentDepth(theEnv, 3);
        PPCRAndIndent(theEnv);
    } else {
        read_first_token = false;
        PPBackup(theEnv);
        IncrementIndentDepth(theEnv, 3);
        PPCRAndIndent(theEnv);
        SavePPBuffer(theEnv, theToken.printForm);
    }

    /*============================*/
    /* Process the while actions. */
    /*============================*/

    if (ExpressionData(theEnv)->svContexts->rtn) { ExpressionData(theEnv)->ReturnContext = true; }
    ExpressionData(theEnv)->BreakContext = true;

    parse->argList->nextArg = GroupActions(theEnv, infile, &theToken, read_first_token, nullptr, false);

    if (parse->argList->nextArg == nullptr) {
        ReturnExpression(theEnv, parse);
        return nullptr;
    }

    PPBackup(theEnv);
    PPBackup(theEnv);
    SavePPBuffer(theEnv, theToken.printForm);

    /*=======================================================*/
    /* Check for the closing right parenthesis of the while. */
    /*=======================================================*/

    if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "while function");
        ReturnExpression(theEnv, parse);
        return nullptr;
    }

    DecrementIndentDepth(theEnv, 3);

    return parse;
}

/******************************************************************************************/
/* LoopForCountParse: purpose is to parse the loop-for-count statement.                   */
/*   The parse of the statement is the return value.                                      */
/*   Syntax: (loop-for-count <range> [do] <action>+)                                      */
/*           <range> ::= (<sf-var> [<start-integer-expression>] <end-integer-expression>) */
/******************************************************************************************/
static Expression *LoopForCountParse(
        const Environment&theEnv,
        Expression *parse,
        const char *infile) {
    struct token theToken;
    CLIPSLexeme *loopVar = nullptr;
    Expression *tmpexp;
    bool read_first_paren;
    struct BindInfo *oldBindList, *newBindList, *prev;

    /*======================================*/
    /* Process the loop counter expression. */
    /*======================================*/

    SavePPBuffer(theEnv, " ");
    GetToken(theEnv, infile, &theToken);

    /* ==========================================
       Simple form: loop-for-count <end> [do] ...
       ========================================== */
    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        parse->argList = GenConstant(theEnv, INTEGER_TYPE, CreateInteger(theEnv, 1LL));
        parse->argList->nextArg = ParseAtomOrExpression(theEnv, infile, &theToken);
        if (parse->argList->nextArg == nullptr) {
            ReturnExpression(theEnv, parse);
            return nullptr;
        }
    } else {
        GetToken(theEnv, infile, &theToken);
        if (theToken.tknType != SF_VARIABLE_TOKEN) {
            if (theToken.tknType != SYMBOL_TOKEN)
                goto LoopForCountParseError;
            parse->argList = GenConstant(theEnv, INTEGER_TYPE, CreateInteger(theEnv, 1LL));
            parse->argList->nextArg = Function2Parse(theEnv, infile, theToken.lexemeValue->contents);
            if (parse->argList->nextArg == nullptr) {
                ReturnExpression(theEnv, parse);
                return nullptr;
            }
        }

            /* =============================================================
               Complex form: loop-for-count (<var> [<start>] <end>) [do] ...
               ============================================================= */
        else {
            loopVar = theToken.lexemeValue;
            SavePPBuffer(theEnv, " ");
            parse->argList = ParseAtomOrExpression(theEnv, infile, nullptr);
            if (parse->argList == nullptr) {
                ReturnExpression(theEnv, parse);
                return nullptr;
            }

            if (CheckArgumentAgainstRestriction(theEnv, parse->argList, INTEGER_BIT))
                goto LoopForCountParseError;

            SavePPBuffer(theEnv, " ");
            GetToken(theEnv, infile, &theToken);
            if (theToken.tknType == RIGHT_PARENTHESIS_TOKEN) {
                PPBackup(theEnv);
                PPBackup(theEnv);
                SavePPBuffer(theEnv, theToken.printForm);
                tmpexp = GenConstant(theEnv, INTEGER_TYPE, CreateInteger(theEnv, 1LL));
                tmpexp->nextArg = parse->argList;
                parse->argList = tmpexp;
            } else {
                parse->argList->nextArg = ParseAtomOrExpression(theEnv, infile, &theToken);
                if (parse->argList->nextArg == nullptr) {
                    ReturnExpression(theEnv, parse);
                    return nullptr;
                }
                GetToken(theEnv, infile, &theToken);
                if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN)
                    goto LoopForCountParseError;
            }
            SavePPBuffer(theEnv, " ");
        }
    }

    if (CheckArgumentAgainstRestriction(theEnv, parse->argList->nextArg, INTEGER_BIT))
        goto LoopForCountParseError;

    /*====================================*/
    /* Process the do keyword if present. */
    /*====================================*/

    GetToken(theEnv, infile, &theToken);
    if ((theToken.tknType == SYMBOL_TOKEN) && (strcmp(theToken.lexemeValue->contents, "do") == 0)) {
        read_first_paren = true;
        PPBackup(theEnv);
        SavePPBuffer(theEnv, " ");
        SavePPBuffer(theEnv, theToken.printForm);
        IncrementIndentDepth(theEnv, 3);
        PPCRAndIndent(theEnv);
    } else if (theToken.tknType == LEFT_PARENTHESIS_TOKEN) {
        read_first_paren = false;
        PPBackup(theEnv);
        IncrementIndentDepth(theEnv, 3);
        PPCRAndIndent(theEnv);
        SavePPBuffer(theEnv, theToken.printForm);
    } else
        goto LoopForCountParseError;

    /*=====================================*/
    /* Process the loop-for-count actions. */
    /*=====================================*/
    if (ExpressionData(theEnv)->svContexts->rtn)
        ExpressionData(theEnv)->ReturnContext = true;
    ExpressionData(theEnv)->BreakContext = true;
    oldBindList = GetParsedBindNames(theEnv);
    SetParsedBindNames(theEnv, nullptr);
    parse->argList->nextArg->nextArg =
            GroupActions(theEnv, infile, &theToken, read_first_paren, nullptr, false);

    if (parse->argList->nextArg->nextArg == nullptr) {
        SetParsedBindNames(theEnv, oldBindList);
        ReturnExpression(theEnv, parse);
        return nullptr;
    }
    newBindList = GetParsedBindNames(theEnv);
    prev = nullptr;
    while (newBindList != nullptr) {
        if ((loopVar == nullptr) ? false :
            (strcmp(newBindList->name->contents, loopVar->contents) == 0)) {
            ClearParsedBindNames(theEnv);
            SetParsedBindNames(theEnv, oldBindList);
            PrintErrorID(theEnv, "PRCDRPSR", 1, true);
            WriteString(theEnv, STDERR, "Cannot rebind loop variable in function loop-for-count.\n");
            ReturnExpression(theEnv, parse);
            return nullptr;
        }
        prev = newBindList;
        newBindList = newBindList->next;
    }
    if (prev == nullptr)
        SetParsedBindNames(theEnv, oldBindList);
    else
        prev->next = oldBindList;
    if (loopVar != nullptr)
        ReplaceLoopCountVars(theEnv, loopVar, parse->argList->nextArg->nextArg, 0);
    PPBackup(theEnv);
    PPBackup(theEnv);
    SavePPBuffer(theEnv, theToken.printForm);

    /*================================================================*/
    /* Check for the closing right parenthesis of the loop-for-count. */
    /*================================================================*/

    if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "loop-for-count function");
        ReturnExpression(theEnv, parse);
        return nullptr;
    }

    DecrementIndentDepth(theEnv, 3);

    return (parse);

    LoopForCountParseError:
    SyntaxErrorMessage(theEnv, "loop-for-count function");
    ReturnExpression(theEnv, parse);
    return nullptr;
}

/*************************/
/* ReplaceLoopCountVars: */
/*************************/
static void ReplaceLoopCountVars(
        const Environment&theEnv,
        CLIPSLexeme *loopVar,
        Expression *theExp,
        int depth) {
    while (theExp != nullptr) {
        if ((theExp->type != SF_VARIABLE) ? false :
            (strcmp(theExp->lexemeValue->contents, loopVar->contents) == 0)) {
            theExp->type = FCALL;
            theExp->value = FindFunction(theEnv, "(get-loop-count)");
            theExp->argList = GenConstant(theEnv, INTEGER_TYPE, CreateInteger(theEnv, depth));
        } else if (theExp->argList != nullptr) {
            if ((theExp->type != FCALL) ? false :
                (theExp->value == (void *) FindFunction(theEnv, "loop-for-count")))
                ReplaceLoopCountVars(theEnv, loopVar, theExp->argList, depth + 1);
            else
                ReplaceLoopCountVars(theEnv, loopVar, theExp->argList, depth);
        }
        theExp = theExp->nextArg;
    }
}

/*********************************************************/
/* IfParse: purpose is to parse the if statement.  The  */
/*   parse of the statement is the return value.         */
/*   Syntax: (if <expression> then <action>+             */
/*               [ else <action>+ ] )                    */
/*********************************************************/
static Expression *IfParse(
        const Environment&theEnv,
        Expression *top,
        const char *infile) {
    struct token theToken;

    /*============================*/
    /* Process the if expression. */
    /*============================*/

    SavePPBuffer(theEnv, " ");

    top->argList = ParseAtomOrExpression(theEnv, infile, nullptr);

    if (top->argList == nullptr) {
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    /*========================================*/
    /* Keyword 'then' must follow expression. */
    /*========================================*/

    IncrementIndentDepth(theEnv, 3);
    PPCRAndIndent(theEnv);

    GetToken(theEnv, infile, &theToken);
    if ((theToken.tknType != SYMBOL_TOKEN) || (strcmp(theToken.lexemeValue->contents, "then") != 0)) {
        SyntaxErrorMessage(theEnv, "if function");
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    /*==============================*/
    /* Process the if then actions. */
    /*==============================*/

    PPCRAndIndent(theEnv);
    if (ExpressionData(theEnv)->svContexts->rtn)
        ExpressionData(theEnv)->ReturnContext = true;
    if (ExpressionData(theEnv)->svContexts->brk)
        ExpressionData(theEnv)->BreakContext = true;
    top->argList->nextArg = GroupActions(theEnv, infile, &theToken, true, "else", false);

    if (top->argList->nextArg == nullptr) {
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    top->argList->nextArg = RemoveUnneededProgn(theEnv, top->argList->nextArg);

    /*===========================================*/
    /* A ')' signals an if then without an else. */
    /*===========================================*/

    if (theToken.tknType == RIGHT_PARENTHESIS_TOKEN) {
        DecrementIndentDepth(theEnv, 3);
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, theToken.printForm);
        return (top);
    }

    /*=============================================*/
    /* Keyword 'else' must follow if then actions. */
    /*=============================================*/

    if ((theToken.tknType != SYMBOL_TOKEN) || (strcmp(theToken.lexemeValue->contents, "else") != 0)) {
        SyntaxErrorMessage(theEnv, "if function");
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    /*==============================*/
    /* Process the if else actions. */
    /*==============================*/

    PPCRAndIndent(theEnv);
    top->argList->nextArg->nextArg = GroupActions(theEnv, infile, &theToken, true, nullptr, false);

    if (top->argList->nextArg->nextArg == nullptr) {
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    top->argList->nextArg->nextArg = RemoveUnneededProgn(theEnv, top->argList->nextArg->nextArg);

    /*======================================================*/
    /* Check for the closing right parenthesis of the if. */
    /*======================================================*/

    if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "if function");
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    /*===========================================*/
    /* A ')' signals an if then without an else. */
    /*===========================================*/

    PPBackup(theEnv);
    PPBackup(theEnv);
    SavePPBuffer(theEnv, ")");
    DecrementIndentDepth(theEnv, 3);
    return (top);
}

/********************************************************/
/* PrognParse: purpose is to parse the progn statement. */
/*   The parse of the statement is the return value.    */
/*   Syntax:  (progn <expression>*)                     */
/********************************************************/
static Expression *PrognParse(
        const Environment&theEnv,
        Expression *top,
        const char *infile) {
    struct token tkn;
    Expression *tmp;

    ReturnExpression(theEnv, top);
    ExpressionData(theEnv)->BreakContext = ExpressionData(theEnv)->svContexts->brk;
    ExpressionData(theEnv)->ReturnContext = ExpressionData(theEnv)->svContexts->rtn;
    IncrementIndentDepth(theEnv, 3);
    PPCRAndIndent(theEnv);
    tmp = GroupActions(theEnv, infile, &tkn, true, nullptr, false);
    DecrementIndentDepth(theEnv, 3);
    PPBackup(theEnv);
    PPBackup(theEnv);
    SavePPBuffer(theEnv, tkn.printForm);
    return (tmp);
}

/***********************************************************/
/* BindParse: purpose is to parse the bind statement. The */
/*   parse of the statement is the return value.           */
/*   Syntax:  (bind ?var <expression>)                     */
/***********************************************************/
static Expression *BindParse(
        const Environment&theEnv,
        Expression *top,
        const char *infile) {
    struct token theToken;
    CLIPSLexeme *variableName;
    Expression *texp;
    CONSTRAINT_RECORD *theConstraint = nullptr;
#if DEFGLOBAL_CONSTRUCT
    Defglobal *theGlobal = nullptr;
    unsigned int count;
#endif

    SavePPBuffer(theEnv, " ");

    /*=============================================*/
    /* Next token must be the name of the variable */
    /* to be bound.                                */
    /*=============================================*/

    GetToken(theEnv, infile, &theToken);
    if ((theToken.tknType != SF_VARIABLE_TOKEN) &&
        (theToken.tknType != GBL_VARIABLE_TOKEN)) {
        if ((theToken.tknType != MF_VARIABLE_TOKEN) || ExpressionData(theEnv)->SequenceOpMode) {
            SyntaxErrorMessage(theEnv, "bind function");
            ReturnExpression(theEnv, top);
            return nullptr;
        }
    }

    /*==============================*/
    /* Process the bind expression. */
    /*==============================*/

    top->argList = GenConstant(theEnv, SYMBOL_TYPE, theToken.value);
    variableName = theToken.lexemeValue;

#if DEFGLOBAL_CONSTRUCT
    if ((theToken.tknType == GBL_VARIABLE_TOKEN) ?
        ((theGlobal = (Defglobal *)
                FindImportedConstruct(theEnv, "defglobal", nullptr, variableName->contents,
                                      &count, true, nullptr)) != nullptr) :
        false) {
        top->argList->type = DEFGLOBAL_PTR;
        top->argList->value = theGlobal;
    } else if (theToken.tknType == GBL_VARIABLE_TOKEN) {
        GlobalReferenceErrorMessage(theEnv, variableName->contents);
        ReturnExpression(theEnv, top);
        return nullptr;
    }
#endif

    texp = get_struct(theEnv, Expression);
    texp->argList = texp->nextArg = nullptr;
    if (CollectArguments(theEnv, texp, infile) == nullptr) {
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    top->argList->nextArg = texp->argList;
    rtn_struct(theEnv, Expression, texp);

#if DEFGLOBAL_CONSTRUCT
    if (top->argList->type == DEFGLOBAL_PTR) return (top);
#endif

    if (top->argList->nextArg != nullptr) { theConstraint = ExpressionToConstraintRecord(theEnv, top->argList->nextArg); }

    AddBindName(theEnv, variableName, theConstraint);

    return (top);
}

/********************************************/
/* ReturnParse: Parses the return function. */
/********************************************/
static Expression *ReturnParse(
        const Environment&theEnv,
        Expression *top,
        const char *infile) {
    bool error_flag = false;
    struct token theToken;

    if (ExpressionData(theEnv)->svContexts->rtn)
        ExpressionData(theEnv)->ReturnContext = true;
    if (!ExpressionData(theEnv)->ReturnContext) {
        PrintErrorID(theEnv, "PRCDRPSR", 2, true);
        WriteString(theEnv, STDERR, "The return function is not valid in this context.\n");
        ReturnExpression(theEnv, top);
        return nullptr;
    }
    ExpressionData(theEnv)->ReturnContext = false;

    SavePPBuffer(theEnv, " ");

    top->argList = ArgumentParse(theEnv, infile, &error_flag);
    if (error_flag) {
        ReturnExpression(theEnv, top);
        return nullptr;
    } else if (top->argList == nullptr) {
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, ")");
    } else {
        SavePPBuffer(theEnv, " ");
        GetToken(theEnv, infile, &theToken);
        if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
            SyntaxErrorMessage(theEnv, "return function");
            ReturnExpression(theEnv, top);
            return nullptr;
        }
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, ")");
    }
    return (top);
}

/***************/
/* BreakParse: */
/***************/
static Expression *BreakParse(
        const Environment&theEnv,
        Expression *top,
        const char *infile) {
    struct token theToken;

    if (!ExpressionData(theEnv)->svContexts->brk) {
        PrintErrorID(theEnv, "PRCDRPSR", 2, true);
        WriteString(theEnv, STDERR, "The break function not valid in this context.\n");
        ReturnExpression(theEnv, top);
        return nullptr;
    }

    SavePPBuffer(theEnv, " ");
    GetToken(theEnv, infile, &theToken);
    if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "break function");
        ReturnExpression(theEnv, top);
        return nullptr;
    }
    PPBackup(theEnv);
    PPBackup(theEnv);
    SavePPBuffer(theEnv, ")");
    return (top);
}

/****************/
/* SwitchParse: */
/****************/
static Expression *SwitchParse(
        const Environment&theEnv,
        Expression *top,
        const char *infile) {
    struct token theToken;
    Expression *theExp, *chk;
    int default_count = 0;

    /*============================*/
    /* Process the switch value   */
    /*============================*/
    IncrementIndentDepth(theEnv, 3);
    SavePPBuffer(theEnv, " ");
    top->argList = theExp = ParseAtomOrExpression(theEnv, infile, nullptr);
    if (theExp == nullptr)
        goto SwitchParseError;

    /*========================*/
    /* Parse case statements. */
    /*========================*/
    GetToken(theEnv, infile, &theToken);
    while (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        PPBackup(theEnv);
        PPCRAndIndent(theEnv);
        SavePPBuffer(theEnv, theToken.printForm);
        if (theToken.tknType != LEFT_PARENTHESIS_TOKEN)
            goto SwitchParseErrorAndMessage;
        GetToken(theEnv, infile, &theToken);
        SavePPBuffer(theEnv, " ");
        if ((theToken.tknType == SYMBOL_TOKEN) &&
            (strcmp(theToken.lexemeValue->contents, "case") == 0)) {
            if (default_count != 0)
                goto SwitchParseErrorAndMessage;
            theExp->nextArg = ParseAtomOrExpression(theEnv, infile, nullptr);
            SavePPBuffer(theEnv, " ");
            if (theExp->nextArg == nullptr)
                goto SwitchParseError;
            for (chk = top->argList->nextArg; chk != theExp->nextArg; chk = chk->nextArg) {
                if ((chk->type == theExp->nextArg->type) &&
                    (chk->value == theExp->nextArg->value) &&
                    IdenticalExpression(chk->argList, theExp->nextArg->argList)) {
                    PrintErrorID(theEnv, "PRCDRPSR", 3, true);
                    WriteString(theEnv, STDERR, "Duplicate case found in switch function.\n");
                    goto SwitchParseError;
                }
            }
            GetToken(theEnv, infile, &theToken);
            if ((theToken.tknType != SYMBOL_TOKEN) ? true :
                (strcmp(theToken.lexemeValue->contents, "then") != 0))
                goto SwitchParseErrorAndMessage;
        } else if ((theToken.tknType == SYMBOL_TOKEN) &&
                   (strcmp(theToken.lexemeValue->contents, "default") == 0)) {
            if (default_count)
                goto SwitchParseErrorAndMessage;
            theExp->nextArg = GenConstant(theEnv, VOID_TYPE, nullptr);
            default_count = 1;
        } else
            goto SwitchParseErrorAndMessage;
        theExp = theExp->nextArg;
        if (ExpressionData(theEnv)->svContexts->rtn)
            ExpressionData(theEnv)->ReturnContext = true;
        if (ExpressionData(theEnv)->svContexts->brk)
            ExpressionData(theEnv)->BreakContext = true;
        IncrementIndentDepth(theEnv, 3);
        PPCRAndIndent(theEnv);
        theExp->nextArg = GroupActions(theEnv, infile, &theToken, true, nullptr, false);
        DecrementIndentDepth(theEnv, 3);
        ExpressionData(theEnv)->ReturnContext = false;
        ExpressionData(theEnv)->BreakContext = false;
        if (theExp->nextArg == nullptr)
            goto SwitchParseError;
        theExp = theExp->nextArg;
        PPBackup(theEnv);
        PPBackup(theEnv);
        SavePPBuffer(theEnv, theToken.printForm);
        GetToken(theEnv, infile, &theToken);
    }
    DecrementIndentDepth(theEnv, 3);
    return (top);

    SwitchParseErrorAndMessage:
    SyntaxErrorMessage(theEnv, "switch function");
    SwitchParseError:
    ReturnExpression(theEnv, top);
    DecrementIndentDepth(theEnv, 3);
    return nullptr;
}

/**************************/
/* SearchParsedBindNames: */
/**************************/
unsigned short SearchParsedBindNames(
        const Environment&theEnv,
        CLIPSLexeme *name_sought) {
    struct BindInfo *var_ptr;
    unsigned short theIndex = 1;

    var_ptr = ProcedureParserData(theEnv)->ListOfParsedBindNames;
    while (var_ptr != nullptr) {
        if (var_ptr->name == name_sought) { return theIndex; }
        var_ptr = var_ptr->next;
        theIndex++;
    }

    return 0;
}

/************************/
/* FindBindConstraints: */
/************************/
struct constraintRecord *FindBindConstraints(
        const Environment&theEnv,
        CLIPSLexeme *nameSought) {
    struct BindInfo *theVariable;

    theVariable = ProcedureParserData(theEnv)->ListOfParsedBindNames;
    while (theVariable != nullptr) {
        if (theVariable->name == nameSought) { return (theVariable->constraints); }
        theVariable = theVariable->next;
    }

    return nullptr;
}

/********************************************************/
/* CountParsedBindNames: Counts the number of variables */
/*   names that have been bound using the bind function */
/*   in the current context (e.g. the RHS of a rule).   */
/********************************************************/
unsigned short CountParsedBindNames(
        const Environment&theEnv) {
    struct BindInfo *theVariable;
    unsigned short theIndex = 0;

    theVariable = ProcedureParserData(theEnv)->ListOfParsedBindNames;
    while (theVariable != nullptr) {
        theVariable = theVariable->next;
        theIndex++;
    }

    return theIndex;
}

/****************************************************************/
/* AddBindName: Adds a variable name used as the first argument */
/*   of the bind function to the list of variable names parsed  */
/*   within the current semantic context (e.g. RHS of a rule).  */
/****************************************************************/
static int AddBindName(
        const Environment&theEnv,
        CLIPSLexeme *variableName,
        CONSTRAINT_RECORD *theConstraint) {
    CONSTRAINT_RECORD *tmpConstraint;
    struct BindInfo *currentBind, *lastBind;
    int theIndex = 1;

    /*=========================================================*/
    /* Look for the variable name in the list of bind variable */
    /* names already parsed. If it is found, then return the   */
    /* index to the variable and union the new constraint      */
    /* information with the old constraint information.        */
    /*=========================================================*/

    lastBind = nullptr;
    currentBind = ProcedureParserData(theEnv)->ListOfParsedBindNames;
    while (currentBind != nullptr) {
        if (currentBind->name == variableName) {
            if (theConstraint != nullptr) {
                tmpConstraint = currentBind->constraints;
                currentBind->constraints = UnionConstraints(theEnv, theConstraint, currentBind->constraints);
                RemoveConstraint(theEnv, tmpConstraint);
                RemoveConstraint(theEnv, theConstraint);
            }

            return (theIndex);
        }
        lastBind = currentBind;
        currentBind = currentBind->next;
        theIndex++;
    }

    /*===============================================================*/
    /* If the variable name wasn't found, then add it to the list of */
    /* variable names and store the constraint information with it.  */
    /*===============================================================*/

    currentBind = get_struct(theEnv, BindInfo);
    currentBind->name = variableName;
    currentBind->constraints = theConstraint;
    currentBind->next = nullptr;

    if (lastBind == nullptr) ProcedureParserData(theEnv)->ListOfParsedBindNames = currentBind;
    else lastBind->next = currentBind;

    return (theIndex);
}

/*************************/
/* RemoveParsedBindName: */
/*************************/
void RemoveParsedBindName(
        const Environment&theEnv,
        CLIPSLexeme *bname) {
    struct BindInfo *prv, *tmp;

    prv = nullptr;
    tmp = ProcedureParserData(theEnv)->ListOfParsedBindNames;
    while ((tmp != nullptr) ? (tmp->name != bname) : false) {
        prv = tmp;
        tmp = tmp->next;
    }
    if (tmp != nullptr) {
        if (prv == nullptr)
            ProcedureParserData(theEnv)->ListOfParsedBindNames = tmp->next;
        else
            prv->next = tmp->next;

        RemoveConstraint(theEnv, tmp->constraints);
        rtn_struct(theEnv, BindInfo, tmp);
    }
}
#endif
