/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*                USER DATA HEADER FILE                */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for attaching user data to constructs,  */
/*   facts, instances, user functions, etc.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_userdata

#pragma once

#define _H_userdata

struct userData {
    unsigned char dataID;
    userData *next;
};

typedef struct userData USER_DATA;
typedef struct userData *USER_DATA_PTR;
typedef void* CreateUserDataFunction(Environment*);
typedef void DeleteUserDataFunction(Environment*, void*);
struct userDataRecord {
    unsigned char dataID;
    CreateUserDataFunction* createUserData;
    DeleteUserDataFunction* deleteUserData;
};

typedef userDataRecord USER_DATA_RECORD;
typedef userDataRecord *USER_DATA_RECORD_PTR;

constexpr auto MAXIMUM_USER_DATA_RECORDS = 100;
constexpr auto USER_DATA_DATA = 56;

struct userDataData {
    userDataRecord *UserDataRecordArray[MAXIMUM_USER_DATA_RECORDS];
    unsigned char UserDataRecordCount;
};

#define UserDataData(theEnv) ((userDataData *) GetEnvironmentData(theEnv,USER_DATA_DATA))

void InitializeUserDataData(Environment *);
unsigned char InstallUserDataRecord(Environment *, userDataRecord *);
userData *FetchUserData(Environment *, unsigned char, userData **);
userData *TestUserData(unsigned char, userData *);
void ClearUserDataList(Environment *, userData *);
userData *DeleteUserData(Environment *, unsigned char, userData *);

#endif

