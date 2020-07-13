/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/30/16             */
/*                                                     */
/*                  USER DATA MODULE                   */
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
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include <cstdlib>

#include "Setup.h"

#include "Environment.h"

#include "UserData.h"

/*************************************************/
/* InitializeUserDataData: Allocates environment */
/*    data for user data routines.               */
/*************************************************/
void InitializeUserDataData(
        const Environment::Ptr&theEnv) {
    //AllocateEnvironmentData(theEnv, USER_DATA_DATA, sizeof(userDataData));
    theEnv->allocateEnvironmentModule<userDataData>();
}

/******************************************************/
/* InstallUserDataRecord: Installs a user data record */
/*   in the user data record array and returns the    */
/*   integer data ID associated with the record.      */
/******************************************************/
unsigned char InstallUserDataRecord(
        const Environment::Ptr&theEnv,
        struct userDataRecord *theRecord) {
    theRecord->dataID = UserDataData(theEnv)->UserDataRecordCount;
    UserDataData(theEnv)->UserDataRecordArray[UserDataData(theEnv)->UserDataRecordCount] = theRecord;
    return (UserDataData(theEnv)->UserDataRecordCount++);
}

/*****************************************************/
/* FetchUserData: Searches for user data information */
/*   from a list of user data structures. A new user */
/*   data structure is created if one is not found.  */
/*****************************************************/
struct userData *FetchUserData(
        const Environment::Ptr&theEnv,
        unsigned char userDataID,
        struct userData **theList) {
    struct userData *theData;

    for (theData = *theList;
         theData != nullptr;
         theData = theData->next) {
        if (theData->dataID == userDataID) { return (theData); }
    }

    theData = (userData *) (*UserDataData(theEnv)->UserDataRecordArray[userDataID]->createUserData)(theEnv);
    theData->dataID = userDataID;
    theData->next = *theList;
    *theList = theData;

    return (theData);
}

/*****************************************************/
/* TestUserData: Searches for user data information  */
/*   from a list of user data structures. nullptr is    */
/*   returned if the appropriate user data structure */
/*   is not found.                                   */
/*****************************************************/
struct userData *TestUserData(
        unsigned char userDataID,
        struct userData *theList) {
    struct userData *theData;

    for (theData = theList;
         theData != nullptr;
         theData = theData->next) {
        if (theData->dataID == userDataID) { return (theData); }
    }

    return nullptr;
}

/***************************************************************/
/* ClearUserDataList: Deallocates a linked list of user data.  */
/***************************************************************/
void ClearUserDataList(
        const Environment::Ptr&theEnv,
        struct userData *theList) {
    struct userData *nextData;

    while (theList != nullptr) {
        nextData = theList->next;
        (*UserDataData(theEnv)->UserDataRecordArray[theList->dataID]->deleteUserData)(theEnv, theList);
        theList = nextData;
    }
}

/*************************************************/
/* DeleteUserData: Removes user data information */
/*   from a list of user data structures.        */
/*************************************************/
struct userData *DeleteUserData(
        const Environment::Ptr&theEnv,
        unsigned char userDataID,
        struct userData *theList) {
    struct userData *theData, *lastData = nullptr;

    for (theData = theList;
         theData != nullptr;
         theData = theData->next) {
        if (theData->dataID == userDataID) {
            if (lastData == nullptr) { theList = theData->next; }
            else { lastData->next = theData->next; }

            (*UserDataData(theEnv)->UserDataRecordArray[userDataID]->deleteUserData)(theEnv, theData);
            return (theList);
        }

        lastData = theData;
    }

    return (theList);
}

