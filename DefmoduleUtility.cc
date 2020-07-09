/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  03/18/19             */
/*                                                     */
/*              DEFMODULE UTILITY MODULE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing module/construct   */
/*   names and searching through modules for specific        */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Used genstrncpy instead of strncpy.            */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Used strstr function to find module separator. */
/*                                                           */
/*            Disallowed use of extraneous module            */
/*            specifiers in a construct name.                */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include "Environment.h"
#include "Construct.h"
#include "MemoryAllocation.h"
#include "DefmoduleParser.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Router.h"
#include "SystemDependency.h"
#include "Watch.h"

#include "DefmoduleUtility.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static ConstructHeader *SearchImportedConstructModules(Environment *, CLIPSLexeme *, Defmodule *,
                                                       struct moduleItem *, CLIPSLexeme *,
                                                       unsigned int *, bool, Defmodule *);

/********************************************************************/
/* FindModuleSeparator: Finds the :: separator which delineates the */
/*   boundary between a module name and a construct name. The value */
/*   zero is returned if the separator is not found, otherwise the  */
/*   position of the second colon within the string is returned.    */
/********************************************************************/
unsigned FindModuleSeparator(
        const char *theString) {
    const char *sep;

    sep = strstr(theString, "::");

    if (sep == nullptr) { return 0; }

    return ((unsigned) (sep - theString) + 1);
}

/*******************************************************************/
/* ExtractModuleName: Given the position of the :: separator and a */
/*   module/construct name joined using the separator, returns a   */
/*   symbol reference to the module name (or nullptr if a module name */
/*   cannot be extracted).                                         */
/*******************************************************************/
CLIPSLexeme *ExtractModuleName(
        Environment *theEnv,
        unsigned thePosition,
        const char *theString) {
    char *newString;
    CLIPSLexeme *returnValue;

    /*=============================================*/
    /* Return nullptr if the :: is in a position such */
    /* that a module name can't be extracted.      */
    /*=============================================*/

    if (thePosition <= 1) return nullptr;

    /*==========================================*/
    /* Allocate storage for a temporary string. */
    /*==========================================*/

    newString = (char *) gm2(theEnv, thePosition);

    /*======================================================*/
    /* Copy the entire module/construct name to the string. */
    /*======================================================*/

    genstrncpy(newString, theString,
               (STD_SIZE) thePosition - 1);

    /*========================================================*/
    /* Place an end of string marker where the :: is located. */
    /*========================================================*/

    newString[thePosition - 1] = EOS;

    /*=====================================================*/
    /* Add the module name (the truncated module/construct */
    /* name) to the symbol table.                          */
    /*=====================================================*/

    returnValue = CreateSymbol(theEnv, newString);

    /*=============================================*/
    /* Return the storage of the temporary string. */
    /*=============================================*/

    rm(theEnv, newString, thePosition);

    /*=============================================*/
    /* Return a pointer to the module name symbol. */
    /*=============================================*/

    return returnValue;
}

/********************************************************************/
/* ExtractConstructName: Given the position of the :: separator and */
/*   a module/construct name joined using the separator, returns a  */
/*   symbol reference to the construct name (or nullptr if a construct */
/*   name cannot be extracted).                                     */
/********************************************************************/
CLIPSLexeme *ExtractConstructName(
        Environment *theEnv,
        unsigned thePosition,
        const char *theString,
        unsigned returnType) {
    size_t theLength;
    char *newString;
    CLIPSLexeme *returnValue;

    /*======================================*/
    /* Just return the string if it doesn't */
    /* contain the :: symbol.               */
    /*======================================*/

    if (thePosition == 0) return CreateSymbol(theEnv, theString);

    /*=====================================*/
    /* Determine the length of the string. */
    /*=====================================*/

    theLength = strlen(theString);

    /*=================================================*/
    /* Return nullptr if the :: is at the very end of the */
    /* string (and thus there is no construct name).   */
    /*=================================================*/

    if (theLength <= (thePosition + 1)) return nullptr;

    /*====================================*/
    /* Allocate a temporary string large  */
    /* enough to hold the construct name. */
    /*====================================*/

    newString = (char *) gm2(theEnv, theLength - thePosition);

    /*================================================*/
    /* Copy the construct name portion of the         */
    /* module/construct name to the temporary string. */
    /*================================================*/

    genstrncpy(newString, &theString[thePosition + 1],
               (STD_SIZE) theLength - thePosition);

    /*=============================================*/
    /* Add the construct name to the symbol table. */
    /*=============================================*/

    if (returnType == SYMBOL_TYPE) { returnValue = CreateSymbol(theEnv, newString); }
    else if (returnType == INSTANCE_NAME_TYPE) { returnValue = CreateInstanceName(theEnv, newString); }
    else { returnValue = CreateString(theEnv, newString); }

    /*=============================================*/
    /* Return the storage of the temporary string. */
    /*=============================================*/

    rm(theEnv, newString, theLength - thePosition);

    /*================================================*/
    /* Return a pointer to the construct name symbol. */
    /*================================================*/

    return returnValue;
}

/****************************************************/
/* ExtractModuleAndConstructName: Extracts both the */
/*   module and construct name from a string. Sets  */
/*   the current module to the specified module.    */
/****************************************************/
const char *ExtractModuleAndConstructName(
        Environment *theEnv,
        const char *theName) {
    unsigned separatorPosition;
    CLIPSLexeme *moduleName, *shortName;
    Defmodule *theModule;

    /*========================*/
    /* Find the :: separator. */
    /*========================*/

    separatorPosition = FindModuleSeparator(theName);
    if (!separatorPosition) return (theName);

    /*==========================*/
    /* Extract the module name. */
    /*==========================*/

    moduleName = ExtractModuleName(theEnv, separatorPosition, theName);
    if (moduleName == nullptr) return nullptr;

    /*====================================*/
    /* Check to see if the module exists. */
    /*====================================*/

    theModule = FindDefmodule(theEnv, moduleName->contents);
    if (theModule == nullptr) return nullptr;

    /*============================*/
    /* Change the current module. */
    /*============================*/

    SetCurrentModule(theEnv, theModule);

    /*=============================*/
    /* Extract the construct name. */
    /*=============================*/

    shortName = ExtractConstructName(theEnv, separatorPosition, theName, SYMBOL_TYPE);
    if (shortName == nullptr) return nullptr;
    return shortName->contents;
}

/************************************************************/
/* FindImportedConstruct: High level routine which searches */
/*   a module and other modules from which it imports       */
/*   constructs for a specified construct.                  */
/************************************************************/
ConstructHeader *FindImportedConstruct(
        Environment *theEnv,
        const char *constructName,
        Defmodule *matchModule,
        const char *findName,
        unsigned int *count,
        bool searchCurrent,
        Defmodule *notYetDefinedInModule) {
    ConstructHeader *rv;
    struct moduleItem *theModuleItem;

    /*=============================================*/
    /* Set the number of references found to zero. */
    /*=============================================*/

    *count = 0;

    /*===============================*/
    /* The :: should not be included */
    /* in the construct's name.      */
    /*===============================*/

    if (FindModuleSeparator(findName)) return nullptr;

    /*=============================================*/
    /* Remember the current module since we'll be  */
    /* changing it during the search and will want */
    /* to restore it once the search is completed. */
    /*=============================================*/

    SaveCurrentModule(theEnv);

    /*==========================================*/
    /* Find the module related access functions */
    /* for the construct type being sought.     */
    /*==========================================*/

    if ((theModuleItem = FindModuleItem(theEnv, constructName)) == nullptr) {
        RestoreCurrentModule(theEnv);
        return nullptr;
    }

    /*===========================================*/
    /* If the construct type doesn't have a find */
    /* function, then we can't look for it.      */
    /*===========================================*/

    if (theModuleItem->findFunction == nullptr) {
        RestoreCurrentModule(theEnv);
        return nullptr;
    }

    /*==================================*/
    /* Initialize the search by marking */
    /* all modules as unvisited.        */
    /*==================================*/

    MarkModulesAsUnvisited(theEnv);

    /*===========================*/
    /* Search for the construct. */
    /*===========================*/

    rv = SearchImportedConstructModules(theEnv, CreateSymbol(theEnv, constructName),
                                        matchModule, theModuleItem,
                                        CreateSymbol(theEnv, findName), count,
                                        searchCurrent, notYetDefinedInModule);

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);

    /*====================================*/
    /* Return a pointer to the construct. */
    /*====================================*/

    return rv;
}

/*********************************************************/
/* AmbiguousReferenceErrorMessage: Error message printed */
/*   when a reference to a specific construct can be     */
/*   imported from more than one module.                 */
/*********************************************************/
void AmbiguousReferenceErrorMessage(
        Environment *theEnv,
        const char *constructName,
        const char *findName) {
    WriteString(theEnv, STDERR, "Ambiguous reference to ");
    WriteString(theEnv, STDERR, constructName);
    WriteString(theEnv, STDERR, " ");
    WriteString(theEnv, STDERR, findName);
    WriteString(theEnv, STDERR, ".\nIt is imported from more than one module.\n");
}

/****************************************************/
/* MarkModulesAsUnvisited: Used for initializing a  */
/*   search through the module heirarchies. Sets    */
/*   the visited flag of each module to false.      */
/****************************************************/
void MarkModulesAsUnvisited(
        Environment *theEnv) {
    Defmodule *theModule;

    DefmoduleData(theEnv)->CurrentModule->visitedFlag = false;
    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) { theModule->visitedFlag = false; }
}

/***********************************************************/
/* SearchImportedConstructModules: Low level routine which */
/*   searches a module and other modules from which it     */
/*   imports constructs for a specified construct.         */
/***********************************************************/
static ConstructHeader *SearchImportedConstructModules(
        Environment *theEnv,
        CLIPSLexeme *constructType,
        Defmodule *matchModule,
        struct moduleItem *theModuleItem,
        CLIPSLexeme *findName,
        unsigned int *count,
        bool searchCurrent,
        Defmodule *notYetDefinedInModule) {
    Defmodule *theModule;
    struct portItem *theImportList, *theExportList;
    ConstructHeader *rv, *arv = nullptr;
    bool searchModule, exported;
    Defmodule *currentModule;

    /*=========================================*/
    /* Start the search in the current module. */
    /* If the current module has already been  */
    /* visited, then return.                   */
    /*=========================================*/

    currentModule = GetCurrentModule(theEnv);
    if (currentModule->visitedFlag) return nullptr;

    /*=======================================================*/
    /* The searchCurrent flag indicates whether the current  */
    /* module should be included in the search. In addition, */
    /* if matchModule is non-nullptr, the current module will   */
    /* only be searched if it is the specific module from    */
    /* which we want the construct imported.                 */
    /*=======================================================*/

    if ((searchCurrent) &&
        ((matchModule == nullptr) || (currentModule == matchModule))) {
        /*===============================================*/
        /* Look for the construct in the current module. */
        /*===============================================*/

        rv = (*theModuleItem->findFunction)(theEnv, findName->contents);

        /*========================================================*/
        /* If we're in the process of defining the construct in   */
        /* the module we're searching then go ahead and increment */
        /* the count indicating the number of modules in which    */
        /* the construct was found.                               */
        /*========================================================*/

        if (notYetDefinedInModule == currentModule) {
            (*count)++;
            arv = rv;
        }

            /*=========================================================*/
            /* Otherwise, if the construct is in the specified module, */
            /* increment the count only if the construct actually      */
            /* belongs to the module. [Some constructs, like the COOL  */
            /* system classes, can be found in any module, but they    */
            /* actually belong to the MAIN module.]                    */
            /*=========================================================*/

        else if (rv != nullptr) {
            if (rv->whichModule->theModule == currentModule) { (*count)++; }
            arv = rv;
        }
    }

    /*=====================================*/
    /* Mark the current module as visited. */
    /*=====================================*/

    currentModule->visitedFlag = true;

    /*===================================*/
    /* Search through all of the modules */
    /* imported by the current module.   */
    /*===================================*/

    theModule = GetCurrentModule(theEnv);
    theImportList = theModule->importList;

    while (theImportList != nullptr) {
        /*===================================================*/
        /* Determine if the module should be searched (based */
        /* upon whether the entire module, all constructs of */
        /* a specific type, or specifically named constructs */
        /* are imported).                                    */
        /*===================================================*/

        searchModule = false;
        if ((theImportList->constructType == nullptr) ||
            (theImportList->constructType == constructType)) {
            if ((theImportList->constructName == nullptr) ||
                (theImportList->constructName == findName)) { searchModule = true; }
        }

        /*=================================*/
        /* Determine if the module exists. */
        /*=================================*/

        if (searchModule) {
            theModule = FindDefmodule(theEnv, theImportList->moduleName->contents);
            if (theModule == nullptr) searchModule = false;
        }

        /*=======================================================*/
        /* Determine if the construct is exported by the module. */
        /*=======================================================*/

        if (searchModule) {
            exported = false;
            theExportList = theModule->exportList;
            while ((theExportList != nullptr) && (!exported)) {
                if ((theExportList->constructType == nullptr) ||
                    (theExportList->constructType == constructType)) {
                    if ((theExportList->constructName == nullptr) ||
                        (theExportList->constructName == findName)) { exported = true; }
                }

                theExportList = theExportList->next;
            }

            if (!exported) searchModule = false;
        }

        /*=================================*/
        /* Search in the specified module. */
        /*=================================*/

        if (searchModule) {
            SetCurrentModule(theEnv, theModule);
            if ((rv = SearchImportedConstructModules(theEnv, constructType, matchModule,
                                                     theModuleItem, findName,
                                                     count, true,
                                                     notYetDefinedInModule)) != nullptr) { arv = rv; }
        }

        /*====================================*/
        /* Move on to the next imported item. */
        /*====================================*/

        theImportList = theImportList->next;
    }

    /*=========================*/
    /* Return a pointer to the */
    /* last construct found.   */
    /*=========================*/

    return arv;
}

/**************************************************************/
/* ConstructExported: Returns true if the specified construct */
/*   is exported from the specified module.                   */
/**************************************************************/
bool ConstructExported(
        Environment *theEnv,
        const char *constructTypeStr,
        CLIPSLexeme *moduleName,
        CLIPSLexeme *findName) {
    CLIPSLexeme *constructType;
    Defmodule *theModule;
    struct portItem *theExportList;

    constructType = FindSymbolHN(theEnv, constructTypeStr, SYMBOL_BIT);
    theModule = FindDefmodule(theEnv, moduleName->contents);

    if ((constructType == nullptr) || (theModule == nullptr) || (findName == nullptr)) { return false; }

    theExportList = theModule->exportList;
    while (theExportList != nullptr) {
        if ((theExportList->constructType == nullptr) ||
            (theExportList->constructType == constructType)) {
            if ((theExportList->constructName == nullptr) ||
                (theExportList->constructName == findName)) { return true; }
        }

        theExportList = theExportList->next;
    }

    return false;
}

/*********************************************************/
/* AllImportedModulesVisited: Returns true if all of the */
/*   imported modules for a module have been visited.    */
/*********************************************************/
bool AllImportedModulesVisited(
        Environment *theEnv,
        Defmodule *theModule) {
    struct portItem *theImportList;
    Defmodule *theImportModule;

    theImportList = theModule->importList;
    while (theImportList != nullptr) {
        theImportModule = FindDefmodule(theEnv, theImportList->moduleName->contents);

        if (!theImportModule->visitedFlag) return false;

        theImportList = theImportList->next;
    }

    return true;
}

/***************************************/
/* ListItemsDriver: Driver routine for */
/*   listing items in a module.        */
/***************************************/
void ListItemsDriver(
        Environment *theEnv,
        const char *logicalName,
        Defmodule *theModule,
        const char *singleName,
        const char *pluralName,
        GetNextItemFunction *nextFunction,
        const char *(*nameFunction)(void *),
        PrintItemFunction *printFunction,
        bool (*doItFunction)(void *)) {
    void *constructPtr;
    const char *constructName;
    unsigned long count = 0;
    bool allModules = false;
    bool doIt;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*======================*/
    /* Print out the items. */
    /*======================*/

    if (theModule == nullptr) {
        theModule = GetNextDefmodule(theEnv, nullptr);
        allModules = true;
    }

    while (theModule != nullptr) {
        if (allModules) {
            WriteString(theEnv, logicalName, DefmoduleName(theModule));
            WriteString(theEnv, logicalName, ":\n");
        }

        SetCurrentModule(theEnv, theModule);
        constructPtr = (*nextFunction)(theEnv, nullptr);
        while (constructPtr != nullptr) {
            if (EvaluationData(theEnv)->HaltExecution) return;

            if (doItFunction == nullptr) doIt = true;
            else doIt = (*doItFunction)(constructPtr);

            if (!doIt) {}
            else if (nameFunction != nullptr) {
                constructName = (*nameFunction)(constructPtr);
                if (constructName != nullptr) {
                    if (allModules) WriteString(theEnv, logicalName, "   ");
                    WriteString(theEnv, logicalName, constructName);
                    WriteString(theEnv, logicalName, "\n");
                }
            } else if (printFunction != nullptr) {
                if (allModules) WriteString(theEnv, logicalName, "   ");
                (*printFunction)(theEnv, logicalName, constructPtr);
                WriteString(theEnv, logicalName, "\n");
            }

            constructPtr = (*nextFunction)(theEnv, constructPtr);
            count++;
        }

        if (allModules) theModule = GetNextDefmodule(theEnv, theModule);
        else theModule = nullptr;
    }

    /*=================================================*/
    /* Print the tally and restore the current module. */
    /*=================================================*/

    if (singleName != nullptr) PrintTally(theEnv, logicalName, count, singleName, pluralName);

    RestoreCurrentModule(theEnv);
}

/********************************************************/
/* DoForAllModules: Executes an action for all modules. */
/********************************************************/
long DoForAllModules(
        Environment *theEnv,
        void (*actionFunction)(Defmodule *, void *),
        int interruptable,
        void *userBuffer) {
    Defmodule *theModule;
    long moduleCount = 0L;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*==================================*/
    /* Loop through all of the modules. */
    /*==================================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule), moduleCount++) {
        SetCurrentModule(theEnv, theModule);

        if ((interruptable) && GetHaltExecution(theEnv)) {
            RestoreCurrentModule(theEnv);
            return (-1L);
        }

        (*actionFunction)(theModule, userBuffer);
    }

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);

    /*=========================================*/
    /* Return the number of modules traversed. */
    /*=========================================*/

    return (moduleCount);
}


/****************************************/
/* RemoveConstructFromModule: Removes a */
/*   construct from its module's list   */
/****************************************/
void RemoveConstructFromModule(
        Environment *theEnv,
        ConstructHeader *theConstruct) {
    ConstructHeader *lastConstruct, *currentConstruct;

    /*==============================*/
    /* Find the specified construct */
    /* in the module's list.        */
    /*==============================*/

    lastConstruct = nullptr;
    currentConstruct = theConstruct->whichModule->firstItem;
    while (currentConstruct != theConstruct) {
        lastConstruct = currentConstruct;
        currentConstruct = currentConstruct->next;
    }

    /*========================================*/
    /* If it wasn't there, something's wrong. */
    /*========================================*/

    if (currentConstruct == nullptr) {
        SystemError(theEnv, "CSTRCPSR", 1);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    /*==========================*/
    /* Remove it from the list. */
    /*==========================*/

    if (lastConstruct == nullptr) { theConstruct->whichModule->firstItem = theConstruct->next; }
    else { lastConstruct->next = theConstruct->next; }

    /*=================================================*/
    /* Update the pointer to the last item in the list */
    /* if the construct just deleted was at the end.   */
    /*=================================================*/

    if (theConstruct == theConstruct->whichModule->lastItem) { theConstruct->whichModule->lastItem = lastConstruct; }
}

/*********************************************************/
/* GetConstructNameAndComment: Get the name and comment  */
/*   field of a construct. Returns name of the construct */
/*   if no errors are detected, otherwise returns nullptr.  */
/*********************************************************/
CLIPSLexeme *GetConstructNameAndComment(
        Environment *theEnv,
        const char *readSource,
        struct token *inputToken,
        const char *constructName,
        FindConstructFunction *findFunction,
        DeleteConstructFunction *deleteFunction,
        const char *constructSymbol,
        bool fullMessageCR,
        bool getComment,
        bool moduleNameAllowed,
        bool ignoreRedefinition) {
#if (MAC_XCD) && (!DEBUGGING_FUNCTIONS)
#pragma unused(fullMessageCR)
#endif
    CLIPSLexeme *name, *moduleName;
    bool redefining = false;
    ConstructHeader *theConstruct;
    unsigned separatorPosition;
    Defmodule *theModule;

    /*==========================*/
    /* Next token should be the */
    /* name of the construct.   */
    /*==========================*/

    GetToken(theEnv, readSource, inputToken);
    if (inputToken->tknType != SYMBOL_TOKEN) {
        PrintErrorID(theEnv, "CSTRCPSR", 2, true);
        WriteString(theEnv, STDERR, "Missing name for ");
        WriteString(theEnv, STDERR, constructName);
        WriteString(theEnv, STDERR, " construct.\n");
        return nullptr;
    }

    name = inputToken->lexemeValue;

    /*===============================*/
    /* Determine the current module. */
    /*===============================*/

    separatorPosition = FindModuleSeparator(name->contents);
    if (separatorPosition) {
        if (!moduleNameAllowed) {
            SyntaxErrorMessage(theEnv, "module specifier");
            return nullptr;
        }

        moduleName = ExtractModuleName(theEnv, separatorPosition, name->contents);
        if (moduleName == nullptr) {
            SyntaxErrorMessage(theEnv, "construct name");
            return nullptr;
        }

        theModule = FindDefmodule(theEnv, moduleName->contents);
        if (theModule == nullptr) {
            CantFindItemErrorMessage(theEnv, "defmodule", moduleName->contents, true);
            return nullptr;
        }

        SetCurrentModule(theEnv, theModule);
        name = ExtractConstructName(theEnv, separatorPosition, name->contents, SYMBOL_TYPE);
        if (name == nullptr) {
            SyntaxErrorMessage(theEnv, "construct name");
            return nullptr;
        }

        if (FindModuleSeparator(name->contents) != 0) {
            SyntaxErrorMessage(theEnv, "module specifier");
            return nullptr;
        }
    }

        /*=====================================================*/
        /* If the module was not specified, record the current */
        /* module name as part of the pretty-print form.       */
        /*=====================================================*/

    else {
        theModule = GetCurrentModule(theEnv);
        if (moduleNameAllowed) {
            PPBackup(theEnv);
            SavePPBuffer(theEnv, DefmoduleName(theModule));
            SavePPBuffer(theEnv, "::");
            SavePPBuffer(theEnv, name->contents);
        }
    }

    /*==================================================================*/
    /* Check for import/export conflicts from the construct definition. */
    /*==================================================================*/

#if DEFMODULE_CONSTRUCT
    if (FindImportExportConflict(theEnv, constructName, theModule, name->contents)) {
        ImportExportConflictMessage(theEnv, constructName, name->contents, nullptr, nullptr);
        return nullptr;
    }
#endif

    /*========================================================*/
    /* Remove the construct if it is already in the knowledge */
    /* base and we're not just checking syntax.               */
    /*========================================================*/

    if ((findFunction != nullptr) && (!ConstructData(theEnv)->CheckSyntaxMode)) {
        theConstruct = (*findFunction)(theEnv, name->contents);
        if (theConstruct != nullptr) {
            redefining = true;
            if (deleteFunction != nullptr) {
                RetainLexeme(theEnv, name);
                if (!(*deleteFunction)(theConstruct, theEnv)) {
                    PrintErrorID(theEnv, "CSTRCPSR", 4, true);
                    WriteString(theEnv, STDERR, "Cannot redefine ");
                    WriteString(theEnv, STDERR, constructName);
                    WriteString(theEnv, STDERR, " '");
                    WriteString(theEnv, STDERR, name->contents);
                    WriteString(theEnv, STDERR, "' while it is in use.\n");
                    ReleaseLexeme(theEnv, name);
                    return nullptr;
                }
                ReleaseLexeme(theEnv, name);
            }
        }
    }

    /*=============================================*/
    /* If compilations are being watched, indicate */
    /* that a construct is being compiled.         */
    /*=============================================*/

#if DEBUGGING_FUNCTIONS
    if ((GetWatchItem(theEnv, "compilations") == 1) &&
        GetPrintWhileLoading(theEnv) && (!ConstructData(theEnv)->CheckSyntaxMode)) {
        const char *outRouter = STDOUT;
        if (redefining && (!ignoreRedefinition)) {
            outRouter = STDWRN;
            PrintWarningID(theEnv, "CSTRCPSR", 1, true);
            WriteString(theEnv, outRouter, "Redefining ");
        } else WriteString(theEnv, outRouter, "Defining ");

        WriteString(theEnv, outRouter, constructName);
        WriteString(theEnv, outRouter, ": ");
        WriteString(theEnv, outRouter, name->contents);

        if (fullMessageCR) WriteString(theEnv, outRouter, "\n");
        else WriteString(theEnv, outRouter, " ");
    } else
#endif
    {
        if (GetPrintWhileLoading(theEnv) && (!ConstructData(theEnv)->CheckSyntaxMode)) { WriteString(theEnv, STDOUT, constructSymbol); }
    }

    /*===============================*/
    /* Get the comment if it exists. */
    /*===============================*/

    GetToken(theEnv, readSource, inputToken);
    if ((inputToken->tknType == STRING_TOKEN) && getComment) {
        PPBackup(theEnv);
        SavePPBuffer(theEnv, " ");
        SavePPBuffer(theEnv, inputToken->printForm);
        GetToken(theEnv, readSource, inputToken);
        if (inputToken->tknType != RIGHT_PARENTHESIS_TOKEN) {
            PPBackup(theEnv);
            SavePPBuffer(theEnv, "\n   ");
            SavePPBuffer(theEnv, inputToken->printForm);
        }
    } else if (inputToken->tknType != RIGHT_PARENTHESIS_TOKEN) {
        PPBackup(theEnv);
        SavePPBuffer(theEnv, "\n   ");
        SavePPBuffer(theEnv, inputToken->printForm);
    }

    /*===================================*/
    /* Return the name of the construct. */
    /*===================================*/

    return (name);
}



