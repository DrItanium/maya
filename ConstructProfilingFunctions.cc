/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/25/16             */
/*                                                     */
/*         CONSTRUCT PROFILING FUNCTIONS MODULE        */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for profiling the amount of    */
/*   time spent in constructs and user defined functions.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modified OutputProfileInfo to allow a before   */
/*            and after prefix so that a string buffer does  */
/*            not need to be created to contain the entire   */
/*            prefix. This allows a buffer overflow problem  */
/*            to be corrected. DR0857.                       */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_TBC).         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*************************************************************/

#include "Setup.h"

#if PROFILING_FUNCTIONS

#include "ArgumentAccess.h"
#include "ClassCommands.h"
#include "Deffunction.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "GenericFunctionCommands.h"
#include "GenericFunction.h"
#include "MemoryAllocation.h"
#include "DefmessageHandlerCommands.h"
#include "Router.h"
#include "SystemDependency.h"

#include "ConstructProfilingFunctions.h"

#include <cstring>

constexpr auto NO_PROFILE      = 0;
constexpr auto USER_FUNCTIONS  = 1;
constexpr auto CONSTRUCTS_CODE = 2;

#define OUTPUT_STRING "%-40s %7ld %15.6f  %8.2f%%  %15.6f  %8.2f%%\n"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static bool OutputProfileInfo(const Environment&, const char *, struct constructProfileInfo *,
                              const char *, const char *, const char *, const char **);
static void OutputUserFunctionsInfo(const Environment&);
static void OutputConstructsCodeInfo(const Environment&);
static void ProfileClearFunction(const Environment&, void *);

/******************************************************/
/* ConstructProfilingFunctionDefinitions: Initializes */
/*   the construct profiling functions.               */
/******************************************************/
void ConstructProfilingFunctionDefinitions(
        const Environment&theEnv) {
    struct userDataRecord profileDataInfo = {0, CreateProfileData, DeleteProfileData};

    //AllocateEnvironmentData(theEnv, PROFLFUN_DATA, sizeof(profileFunctionData));
    theEnv->allocateEnvironmentModule<profileFunctionData>();

    ProfileFunctionData(theEnv)->ProfileDataInfo = profileDataInfo;

    ProfileFunctionData(theEnv)->LastProfileInfo = NO_PROFILE;
    ProfileFunctionData(theEnv)->PercentThreshold = 0.0;
    ProfileFunctionData(theEnv)->OutputString = OUTPUT_STRING;

    AddUDF(theEnv, "profile", "v", 1, 1, "y", ProfileCommand);
    AddUDF(theEnv, "profile-info", "v", 0, 0, nullptr, ProfileInfoCommand);
    AddUDF(theEnv, "profile-reset", "v", 0, 0, nullptr, ProfileResetCommand);

    AddUDF(theEnv, "set-profile-percent-threshold", "d", 1, 1, "ld", SetProfilePercentThresholdCommand);
    AddUDF(theEnv, "get-profile-percent-threshold", "d", 0, 0, nullptr, GetProfilePercentThresholdCommand);

    ProfileFunctionData(theEnv)->ProfileDataID = InstallUserDataRecord(theEnv, &ProfileFunctionData(theEnv)->ProfileDataInfo);

    AddClearFunction(theEnv, "profile", ProfileClearFunction, 0, nullptr);
}

/**********************************/
/* CreateProfileData: Allocates a */
/*   profile user data structure. */
/**********************************/
void *CreateProfileData(
        const Environment&theEnv) {
    struct constructProfileInfo *theInfo;

    theInfo = (constructProfileInfo *)
            genalloc(theEnv, sizeof(constructProfileInfo));

    theInfo->numberOfEntries = 0;
    theInfo->childCall = false;
    theInfo->startTime = 0.0;
    theInfo->totalSelfTime = 0.0;
    theInfo->totalWithChildrenTime = 0.0;

    return (theInfo);
}

/**************************************/
/* DeleteProfileData:          */
/**************************************/
void DeleteProfileData(
        const Environment&theEnv,
        void *theData) {
    genfree(theEnv, theData, sizeof(constructProfileInfo));
}

/**************************************/
/* ProfileCommand: H/L access routine */
/*   for the profile command.         */
/**************************************/
void ProfileCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *argument;
    UDFValue theValue;

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theValue)) return;
    argument = theValue.lexemeValue->contents;

    if (!Profile(theEnv, argument)) {
        UDFInvalidArgumentMessage(context, "symbol with value constructs, user-functions, or off");
        return;
    }

    return;
}

/******************************/
/* Profile: C access routine  */
/*   for the profile command. */
/******************************/
bool Profile(
        const Environment&theEnv,
        const char *argument) {
    /*======================================================*/
    /* If the argument is the symbol "user-functions", then */
    /* user-defined functions should be profiled. If the    */
    /* argument is the symbol "constructs", then            */
    /* deffunctions, generic functions, message-handlers,   */
    /* and rule RHS actions are profiled.                   */
    /*======================================================*/

    if (strcmp(argument, "user-functions") == 0) {
        ProfileFunctionData(theEnv)->ProfileStartTime = gentime();
        ProfileFunctionData(theEnv)->ProfileUserFunctions = true;
        ProfileFunctionData(theEnv)->ProfileConstructs = false;
        ProfileFunctionData(theEnv)->LastProfileInfo = USER_FUNCTIONS;
    } else if (strcmp(argument, "constructs") == 0) {
        ProfileFunctionData(theEnv)->ProfileStartTime = gentime();
        ProfileFunctionData(theEnv)->ProfileUserFunctions = false;
        ProfileFunctionData(theEnv)->ProfileConstructs = true;
        ProfileFunctionData(theEnv)->LastProfileInfo = CONSTRUCTS_CODE;
    }

        /*======================================================*/
        /* Otherwise, if the argument is the symbol "off", then */
        /* don't profile constructs and user-defined functions. */
        /*======================================================*/

    else if (strcmp(argument, "off") == 0) {
        ProfileFunctionData(theEnv)->ProfileEndTime = gentime();
        ProfileFunctionData(theEnv)->ProfileTotalTime += (ProfileFunctionData(theEnv)->ProfileEndTime -
                                                          ProfileFunctionData(theEnv)->ProfileStartTime);
        ProfileFunctionData(theEnv)->ProfileUserFunctions = false;
        ProfileFunctionData(theEnv)->ProfileConstructs = false;
    }

        /*=====================================================*/
        /* Otherwise, generate an error since the only allowed */
        /* arguments are "on" or "off."                        */
        /*=====================================================*/

    else { return false; }

    return true;
}

/******************************************/
/* ProfileInfoCommand: H/L access routine */
/*   for the profile-info command.        */
/******************************************/
void ProfileInfoCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    char buffer[512];

    /*==================================*/
    /* If code is still being profiled, */
    /* update the profile end time.     */
    /*==================================*/

    if (ProfileFunctionData(theEnv)->ProfileUserFunctions || ProfileFunctionData(theEnv)->ProfileConstructs) {
        ProfileFunctionData(theEnv)->ProfileEndTime = gentime();
        ProfileFunctionData(theEnv)->ProfileTotalTime += (ProfileFunctionData(theEnv)->ProfileEndTime -
                                                          ProfileFunctionData(theEnv)->ProfileStartTime);
    }

    /*==================================*/
    /* Print the profiling information. */
    /*==================================*/

    if (ProfileFunctionData(theEnv)->LastProfileInfo != NO_PROFILE) {
        gensprintf(buffer, "Profile elapsed time = %g seconds\n",
                   ProfileFunctionData(theEnv)->ProfileTotalTime);
        WriteString(theEnv, STDOUT, buffer);

        if (ProfileFunctionData(theEnv)->LastProfileInfo == USER_FUNCTIONS) {
            WriteString(theEnv, STDOUT, "Function Name                            ");
        } else if (ProfileFunctionData(theEnv)->LastProfileInfo == CONSTRUCTS_CODE) {
            WriteString(theEnv, STDOUT, "Construct Name                           ");
        }

        WriteString(theEnv, STDOUT, "Entries         Time           %          Time+Kids     %+Kids\n");

        if (ProfileFunctionData(theEnv)->LastProfileInfo == USER_FUNCTIONS) {
            WriteString(theEnv, STDOUT, "-------------                            ");
        } else if (ProfileFunctionData(theEnv)->LastProfileInfo == CONSTRUCTS_CODE) {
            WriteString(theEnv, STDOUT, "--------------                           ");
        }

        WriteString(theEnv, STDOUT, "-------        ------        -----        ---------     ------\n");
    }

    if (ProfileFunctionData(theEnv)->LastProfileInfo == USER_FUNCTIONS) OutputUserFunctionsInfo(theEnv);
    if (ProfileFunctionData(theEnv)->LastProfileInfo == CONSTRUCTS_CODE) OutputConstructsCodeInfo(theEnv);
}

/**********************************************/
/* StartProfile: Initiates bookkeeping needed */
/*   to profile a construct or function.      */
/**********************************************/
void StartProfile(
        const Environment&theEnv,
        struct profileFrameInfo *theFrame,
        struct userData **theList,
        bool checkFlag) {
    double startTime, addTime;
    struct constructProfileInfo *profileInfo;

    if (!checkFlag) {
        theFrame->profileOnExit = false;
        return;
    }

    profileInfo = (constructProfileInfo *) FetchUserData(theEnv, ProfileFunctionData(theEnv)->ProfileDataID, theList);

    theFrame->profileOnExit = true;
    theFrame->parentCall = false;

    startTime = gentime();
    theFrame->oldProfileFrame = ProfileFunctionData(theEnv)->ActiveProfileFrame;

    if (ProfileFunctionData(theEnv)->ActiveProfileFrame != nullptr) {
        addTime = startTime - ProfileFunctionData(theEnv)->ActiveProfileFrame->startTime;
        ProfileFunctionData(theEnv)->ActiveProfileFrame->totalSelfTime += addTime;
    }

    ProfileFunctionData(theEnv)->ActiveProfileFrame = profileInfo;

    ProfileFunctionData(theEnv)->ActiveProfileFrame->numberOfEntries++;
    ProfileFunctionData(theEnv)->ActiveProfileFrame->startTime = startTime;

    if (!ProfileFunctionData(theEnv)->ActiveProfileFrame->childCall) {
        theFrame->parentCall = true;
        theFrame->parentStartTime = startTime;
        ProfileFunctionData(theEnv)->ActiveProfileFrame->childCall = true;
    }
}

/*******************************************/
/* EndProfile: Finishes bookkeeping needed */
/*   to profile a construct or function.   */
/*******************************************/
void EndProfile(
        const Environment&theEnv,
        struct profileFrameInfo *theFrame) {
    double endTime, addTime;

    if (!theFrame->profileOnExit) return;

    endTime = gentime();

    if (theFrame->parentCall) {
        addTime = endTime - theFrame->parentStartTime;
        ProfileFunctionData(theEnv)->ActiveProfileFrame->totalWithChildrenTime += addTime;
        ProfileFunctionData(theEnv)->ActiveProfileFrame->childCall = false;
    }

    ProfileFunctionData(theEnv)->ActiveProfileFrame->totalSelfTime += (endTime -
                                                                       ProfileFunctionData(theEnv)->ActiveProfileFrame->startTime);

    if (theFrame->oldProfileFrame != nullptr) { theFrame->oldProfileFrame->startTime = endTime; }

    ProfileFunctionData(theEnv)->ActiveProfileFrame = theFrame->oldProfileFrame;
}

/******************************************/
/* OutputProfileInfo: Prints out a single */
/*   line of profile information.         */
/******************************************/
static bool OutputProfileInfo(
        const Environment&theEnv,
        const char *itemName,
        struct constructProfileInfo *profileInfo,
        const char *printPrefixBefore,
        const char *printPrefix,
        const char *printPrefixAfter,
        const char **banner) {
    double percent = 0.0, percentWithKids = 0.0;
    char buffer[512];

    if (profileInfo == nullptr) return false;

    if (profileInfo->numberOfEntries == 0) return false;

    if (ProfileFunctionData(theEnv)->ProfileTotalTime != 0.0) {
        percent = (profileInfo->totalSelfTime * 100.0) / ProfileFunctionData(theEnv)->ProfileTotalTime;
        if (percent < 0.005) percent = 0.0;
        percentWithKids = (profileInfo->totalWithChildrenTime * 100.0) / ProfileFunctionData(theEnv)->ProfileTotalTime;
        if (percentWithKids < 0.005) percentWithKids = 0.0;
    }

    if (percent < ProfileFunctionData(theEnv)->PercentThreshold) return false;

    if ((banner != nullptr) && (*banner != nullptr)) {
        WriteString(theEnv, STDOUT, *banner);
        *banner = nullptr;
    }

    if (printPrefixBefore != nullptr) { WriteString(theEnv, STDOUT, printPrefixBefore); }

    if (printPrefix != nullptr) { WriteString(theEnv, STDOUT, printPrefix); }

    if (printPrefixAfter != nullptr) { WriteString(theEnv, STDOUT, printPrefixAfter); }

    if (strlen(itemName) >= 40) {
        WriteString(theEnv, STDOUT, itemName);
        WriteString(theEnv, STDOUT, "\n");
        itemName = "";
    }

    gensprintf(buffer, ProfileFunctionData(theEnv)->OutputString,
               itemName,
               (long) profileInfo->numberOfEntries,

               (double) profileInfo->totalSelfTime,
               (double) percent,

               (double) profileInfo->totalWithChildrenTime,
               (double) percentWithKids);
    WriteString(theEnv, STDOUT, buffer);

    return true;
}

/*******************************************/
/* ProfileResetCommand: H/L access routine */
/*   for the profile-reset command.        */
/*******************************************/
void ProfileResetCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    FunctionDefinition *theFunction;
    int i;
#if DEFFUNCTION_CONSTRUCT
    Deffunction *theDeffunction;
#endif
    Defrule *theDefrule;
#if DEFGENERIC_CONSTRUCT
    Defgeneric *theDefgeneric;
    unsigned short methodIndex;
    Defmethod *theMethod;
#endif
    Defclass *theDefclass;
    DefmessageHandler *theHandler;
    unsigned handlerIndex;

    ProfileFunctionData(theEnv)->ProfileStartTime = 0.0;
    ProfileFunctionData(theEnv)->ProfileEndTime = 0.0;
    ProfileFunctionData(theEnv)->ProfileTotalTime = 0.0;
    ProfileFunctionData(theEnv)->LastProfileInfo = NO_PROFILE;

    for (theFunction = GetFunctionList(theEnv);
         theFunction != nullptr;
         theFunction = theFunction->next) {
        ResetProfileInfo((constructProfileInfo *)
                                 TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theFunction->usrData));
    }

    for (i = 0; i < MAXIMUM_PRIMITIVES; i++) {
        if (EvaluationData(theEnv)->PrimitivesArray[i] != nullptr) {
            ResetProfileInfo((constructProfileInfo *)
                                     TestUserData(ProfileFunctionData(theEnv)->ProfileDataID,
                                                  EvaluationData(theEnv)->PrimitivesArray[i]->usrData));
        }
    }

#if DEFFUNCTION_CONSTRUCT
    for (theDeffunction = GetNextDeffunction(theEnv, nullptr);
         theDeffunction != nullptr;
         theDeffunction = GetNextDeffunction(theEnv, theDeffunction)) {
        ResetProfileInfo((constructProfileInfo *)
                                 TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theDeffunction->header.usrData));
    }
#endif

    for (theDefrule = GetNextDefrule(theEnv, nullptr);
         theDefrule != nullptr;
         theDefrule = GetNextDefrule(theEnv, theDefrule)) {
        ResetProfileInfo((constructProfileInfo *)
                                 TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theDefrule->header.usrData));
    }

#if DEFGENERIC_CONSTRUCT
    for (theDefgeneric = GetNextDefgeneric(theEnv, nullptr);
         theDefgeneric != nullptr;
         theDefgeneric = GetNextDefgeneric(theEnv, theDefgeneric)) {
        ResetProfileInfo((constructProfileInfo *)
                                 TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theDefgeneric->header.usrData));

        for (methodIndex = GetNextDefmethod(theDefgeneric, 0);
             methodIndex != 0;
             methodIndex = GetNextDefmethod(theDefgeneric, methodIndex)) {
            theMethod = GetDefmethodPointer(theDefgeneric, methodIndex);
            ResetProfileInfo((constructProfileInfo *)
                                     TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theMethod->header.usrData));
        }
    }
#endif

    for (theDefclass = GetNextDefclass(theEnv, nullptr);
         theDefclass != nullptr;
         theDefclass = GetNextDefclass(theEnv, theDefclass)) {
        ResetProfileInfo((constructProfileInfo *)
                                 TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theDefclass->header.usrData));
        for (handlerIndex = GetNextDefmessageHandler(theDefclass, 0);
             handlerIndex != 0;
             handlerIndex = GetNextDefmessageHandler(theDefclass, handlerIndex)) {
            theHandler = GetDefmessageHandlerPointer(theDefclass, handlerIndex);
            ResetProfileInfo((constructProfileInfo *)
                                     TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theHandler->header.usrData));
        }
    }
}

/*************************************************/
/* ResetProfileInfo: Sets the initial values for */
/*   a constructProfileInfo data structure.      */
/*************************************************/
void ResetProfileInfo(
        struct constructProfileInfo *profileInfo) {
    if (profileInfo == nullptr) return;

    profileInfo->numberOfEntries = 0;
    profileInfo->childCall = false;
    profileInfo->startTime = 0.0;
    profileInfo->totalSelfTime = 0.0;
    profileInfo->totalWithChildrenTime = 0.0;
}

/****************************/
/* OutputUserFunctionsInfo: */
/****************************/
static void OutputUserFunctionsInfo(
        const Environment&theEnv) {
    FunctionDefinition *theFunction;
    int i;

    for (theFunction = GetFunctionList(theEnv);
         theFunction != nullptr;
         theFunction = theFunction->next) {
        OutputProfileInfo(theEnv, theFunction->callFunctionName->contents,
                          (constructProfileInfo *)
                                  TestUserData(ProfileFunctionData(theEnv)->ProfileDataID,
                                               theFunction->usrData),
                          nullptr, nullptr, nullptr, nullptr);
    }

    for (i = 0; i < MAXIMUM_PRIMITIVES; i++) {
        if (EvaluationData(theEnv)->PrimitivesArray[i] != nullptr) {
            OutputProfileInfo(theEnv, EvaluationData(theEnv)->PrimitivesArray[i]->name,
                              (constructProfileInfo *)
                                      TestUserData(ProfileFunctionData(theEnv)->ProfileDataID,
                                                   EvaluationData(theEnv)->PrimitivesArray[i]->usrData),
                              nullptr, nullptr, nullptr, nullptr);
        }
    }
}

/*****************************/
/* OutputConstructsCodeInfo: */
/*****************************/
static void OutputConstructsCodeInfo(
        const Environment&theEnv) {
#if DEFFUNCTION_CONSTRUCT
    Deffunction *theDeffunction;
#endif
    Defrule *theDefrule;
#if DEFGENERIC_CONSTRUCT
    Defgeneric *theDefgeneric;
    Defmethod *theMethod;
    unsigned short methodIndex;
    StringBuilder *theSB;
#endif
    Defclass *theDefclass;
    DefmessageHandler *theHandler;
    unsigned handlerIndex;
    const char *prefix, *prefixBefore, *prefixAfter;
    const char *banner;

    banner = "\n*** Deffunctions ***\n\n";

#if DEFFUNCTION_CONSTRUCT
    for (theDeffunction = GetNextDeffunction(theEnv, nullptr);
         theDeffunction != nullptr;
         theDeffunction = GetNextDeffunction(theEnv, theDeffunction)) {
        OutputProfileInfo(theEnv, DeffunctionName(theDeffunction),
                          (constructProfileInfo *)
                                  TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theDeffunction->header.usrData),
                          nullptr, nullptr, nullptr, &banner);
    }
#endif

    banner = "\n*** Defgenerics ***\n";
#if DEFGENERIC_CONSTRUCT
    theSB = CreateStringBuilder(theEnv, 512);
    for (theDefgeneric = GetNextDefgeneric(theEnv, nullptr);
         theDefgeneric != nullptr;
         theDefgeneric = GetNextDefgeneric(theEnv, theDefgeneric)) {
        prefixBefore = "\n";
        prefix = DefgenericName(theDefgeneric);
        prefixAfter = "\n";

        for (methodIndex = GetNextDefmethod(theDefgeneric, 0);
             methodIndex != 0;
             methodIndex = GetNextDefmethod(theDefgeneric, methodIndex)) {
            theMethod = GetDefmethodPointer(theDefgeneric, methodIndex);

            DefmethodDescription(theDefgeneric, methodIndex, theSB);
            if (OutputProfileInfo(theEnv, theSB->contents,
                                  (constructProfileInfo *)
                                          TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theMethod->header.usrData),
                                  prefixBefore, prefix, prefixAfter, &banner)) {
                prefixBefore = nullptr;
                prefix = nullptr;
                prefixAfter = nullptr;
            }
        }
    }
    SBDispose(theSB);
#endif

    banner = "\n*** Defclasses ***\n";
    for (theDefclass = GetNextDefclass(theEnv, nullptr);
         theDefclass != nullptr;
         theDefclass = GetNextDefclass(theEnv, theDefclass)) {
        prefixAfter = "\n";
        prefix = DefclassName(theDefclass);
        prefixBefore = "\n";

        for (handlerIndex = GetNextDefmessageHandler(theDefclass, 0);
             handlerIndex != 0;
             handlerIndex = GetNextDefmessageHandler(theDefclass, handlerIndex)) {
            theHandler = GetDefmessageHandlerPointer(theDefclass, handlerIndex);
            if (OutputProfileInfo(theEnv, DefmessageHandlerName(theDefclass, handlerIndex),
                                  (constructProfileInfo *)
                                          TestUserData(ProfileFunctionData(theEnv)->ProfileDataID,
                                                       theHandler->header.usrData),
                                  prefixBefore, prefix, prefixAfter, &banner)) {
                prefixBefore = nullptr;
                prefix = nullptr;
                prefixAfter = nullptr;
            }
        }

    }

    banner = "\n*** Defrules ***\n\n";

    for (theDefrule = GetNextDefrule(theEnv, nullptr);
         theDefrule != nullptr;
         theDefrule = GetNextDefrule(theEnv, theDefrule)) {
        OutputProfileInfo(theEnv, DefruleName(theDefrule),
                          (constructProfileInfo *)
                                  TestUserData(ProfileFunctionData(theEnv)->ProfileDataID, theDefrule->header.usrData),
                          nullptr, nullptr, nullptr, &banner);
    }

}

/*********************************************************/
/* SetProfilePercentThresholdCommand: H/L access routine */
/*   for the set-profile-percent-threshold command.      */
/*********************************************************/
void SetProfilePercentThresholdCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theValue;
    double newThreshold;

    if (!UDFFirstArgument(context, NUMBER_BITS, &theValue)) { return; }

    newThreshold = CVCoerceToFloat(&theValue);

    if ((newThreshold < 0.0) || (newThreshold > 100.0)) {
        UDFInvalidArgumentMessage(context, "number in the range 0 to 100");
        returnValue->floatValue = CreateFloat(theEnv, -1.0);
    } else { returnValue->floatValue = CreateFloat(theEnv, SetProfilePercentThreshold(theEnv, newThreshold)); }
}

/****************************************************/
/* SetProfilePercentThreshold: C access routine for */
/*   the set-profile-percent-threshold command.     */
/****************************************************/
double SetProfilePercentThreshold(
        const Environment&theEnv,
        double value) {
    double oldPercentThreshhold;

    if ((value < 0.0) || (value > 100.0)) { return (-1.0); }

    oldPercentThreshhold = ProfileFunctionData(theEnv)->PercentThreshold;

    ProfileFunctionData(theEnv)->PercentThreshold = value;

    return (oldPercentThreshhold);
}

/*********************************************************/
/* GetProfilePercentThresholdCommand: H/L access routine */
/*   for the get-profile-percent-threshold command.      */
/*********************************************************/
void GetProfilePercentThresholdCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    returnValue->floatValue = CreateFloat(theEnv, ProfileFunctionData(theEnv)->PercentThreshold);
}

/****************************************************/
/* GetProfilePercentThreshold: C access routine for */
/*   the get-profile-percent-threshold command.     */
/****************************************************/
double GetProfilePercentThreshold(
        const Environment&theEnv) {
    return (ProfileFunctionData(theEnv)->PercentThreshold);
}

/**********************************************************/
/* SetProfileOutputString: Sets the output string global. */
/**********************************************************/
const char *SetProfileOutputString(
        const Environment&theEnv,
        const char *value) {
    const char *oldOutputString;

    if (value == nullptr) { return (ProfileFunctionData(theEnv)->OutputString); }

    oldOutputString = ProfileFunctionData(theEnv)->OutputString;

    ProfileFunctionData(theEnv)->OutputString = value;

    return (oldOutputString);
}

/******************************************************************/
/* ProfileClearFunction: Profiling clear routine for use with the */
/*   clear command. Removes user data attached to user functions. */
/******************************************************************/
static void ProfileClearFunction(
        const Environment&theEnv,
        void *context) {
    FunctionDefinition *theFunction;
    int i;

    for (theFunction = GetFunctionList(theEnv);
         theFunction != nullptr;
         theFunction = theFunction->next) {
        theFunction->usrData =
                DeleteUserData(theEnv, ProfileFunctionData(theEnv)->ProfileDataID, theFunction->usrData);
    }

    for (i = 0; i < MAXIMUM_PRIMITIVES; i++) {
        if (EvaluationData(theEnv)->PrimitivesArray[i] != nullptr) {
            EvaluationData(theEnv)->PrimitivesArray[i]->usrData =
                    DeleteUserData(theEnv, ProfileFunctionData(theEnv)->ProfileDataID, EvaluationData(theEnv)->PrimitivesArray[i]->usrData);
        }
    }
}

#endif /* PROFILING_FUNCTIONS */

