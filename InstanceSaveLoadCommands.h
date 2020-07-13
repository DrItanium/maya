/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  05/03/19            */
/*                                                     */
/*                                                     */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_insfile

#pragma once

#define _H_insfile

#include "Expression.h"

constexpr auto INSTANCE_FILE_DATA = 30;

#if BLOAD_INSTANCES || BSAVE_INSTANCES
struct instanceFileData : public EnvironmentModule {
    const char *InstanceBinaryPrefixID;
    const char *InstanceBinaryVersionID;
};
RegisterEnvironmentModule(instanceFileData, INSTANCE_FILE_DATA, InstanceFile);

#endif /* BLOAD_INSTANCES || BSAVE_INSTANCES */

void SetupInstanceFileCommands(const Environment::Ptr&);
void SaveInstancesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LoadInstancesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RestoreInstancesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
long SaveInstancesDriver(const Environment::Ptr&, const char *, SaveScope, Expression *, bool);
long SaveInstances(const Environment::Ptr&, const char *, SaveScope);
#if BSAVE_INSTANCES
void BinarySaveInstancesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
long BinarySaveInstancesDriver(const Environment::Ptr&, const char *, SaveScope, Expression *, bool);
long BinarySaveInstances(const Environment::Ptr&, const char *, SaveScope);
#endif
#if BLOAD_INSTANCES
void BinaryLoadInstancesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
long BinaryLoadInstances(const Environment::Ptr&, const char *);
#endif
long LoadInstances(const Environment::Ptr&, const char *);
long LoadInstancesFromString(const Environment::Ptr&, const char *, size_t);
long RestoreInstances(const Environment::Ptr&, const char *);
long RestoreInstancesFromString(const Environment::Ptr&, const char *, size_t);

#endif /* _H_insfile */



