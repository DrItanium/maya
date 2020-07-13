/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*      CONSTRUCT PROFILING FUNCTIONS HEADER FILE      */
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
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_proflfun

#pragma once

#define _H_proflfun

#include "UserData.h"

struct constructProfileInfo {
    struct userData usrData;
    long numberOfEntries;
    bool childCall: 1;
    double startTime;
    double totalSelfTime;
    double totalWithChildrenTime;
};

struct profileFrameInfo {
    bool parentCall: 1;
    bool profileOnExit: 1;
    double parentStartTime;
    struct constructProfileInfo *oldProfileFrame;
};

constexpr auto PROFLFUN_DATA = 15;

struct profileFunctionData : public EnvironmentModule {
    double ProfileStartTime;
    double ProfileEndTime;
    double ProfileTotalTime;
    int LastProfileInfo;
    double PercentThreshold;
    struct userDataRecord ProfileDataInfo;
    unsigned char ProfileDataID;
    bool ProfileUserFunctions;
    bool ProfileConstructs;
    struct constructProfileInfo *ActiveProfileFrame;
    const char *OutputString;
};
RegisterEnvironmentModule(profileFunctionData, PROFLFUN_DATA, ProfileFunction);

void ConstructProfilingFunctionDefinitions(const Environment::Ptr&);
void ProfileCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ProfileInfoCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void StartProfile(const Environment::Ptr&, struct profileFrameInfo *,
                  struct userData **, bool);
void EndProfile(const Environment::Ptr&, struct profileFrameInfo *);
void ProfileResetCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ResetProfileInfo(constructProfileInfo *);

void SetProfilePercentThresholdCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
double SetProfilePercentThreshold(const Environment::Ptr&, double);
void GetProfilePercentThresholdCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
double GetProfilePercentThreshold(const Environment::Ptr&);
bool Profile(const Environment::Ptr&, const char *);
void DeleteProfileData(const Environment::Ptr&, void *);
void *CreateProfileData(const Environment::Ptr&);
const char *SetProfileOutputString(const Environment::Ptr&, const char *);

#endif /* _H_proflfun */


