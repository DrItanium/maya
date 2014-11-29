   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.30  08/02/14          */
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
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*************************************************************/

#ifndef _H_inscom
#define _H_inscom

#ifndef _H_object
#include "object.h"
#endif

#ifndef _H_insfun
#include "insfun.h"
#endif

#define INSTANCE_DATA 29

struct instanceData
  { 
   INSTANCE_TYPE DummyInstance;
   INSTANCE_TYPE **InstanceTable;
   int MaintainGarbageInstances;
   int MkInsMsgPass;
   int ChangesToInstances;
   IGARBAGE *InstanceGarbageList;
   struct patternEntityRecord InstanceInfo;
   INSTANCE_TYPE *InstanceList;  
   unsigned long GlobalNumberOfInstances;
   INSTANCE_TYPE *CurrentInstance;
   INSTANCE_TYPE *InstanceListBottom;
   intBool ObjectModDupMsgValid;
  };

#define InstanceData(theEnv) ((struct instanceData *) GetEnvironmentData(theEnv,INSTANCE_DATA))

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _INSCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           SetupInstances(void *);
   LOCALE intBool                        EnvDeleteInstance(void *,void *);
   LOCALE intBool                        EnvUnmakeInstance(void *,void *);
#if DEBUGGING_FUNCTIONS
   LOCALE void                           InstancesCommand(void *);
   LOCALE void                           PPInstanceCommand(void *);
   LOCALE void                           EnvInstances(void *,const char *,void *,const char *,int);
#endif
   LOCALE void                          *EnvMakeInstance(void *,const char *);
   LOCALE void                          *EnvCreateRawInstance(void *,void *,const char *);
   LOCALE void                          *EnvFindInstance(void *,void *,const char *,unsigned);
   LOCALE int                            EnvValidInstanceAddress(void *,void *);
   LOCALE void                           EnvDirectGetSlot(void *,void *,const char *,DATA_OBJECT *);
   LOCALE int                            EnvDirectPutSlot(void *,void *,const char *,DATA_OBJECT *);
   LOCALE char                          *EnvGetInstanceName(void *,void *);
   LOCALE void                          *EnvGetInstanceClass(void *,void *);
   LOCALE unsigned long GetGlobalNumberOfInstances(void *);
   LOCALE void                          *EnvGetNextInstance(void *,void *);
   LOCALE void                          *GetNextInstanceInScope(void *,void *);
   LOCALE void                          *EnvGetNextInstanceInClass(void *,void *,void *);
   LOCALE void                          *EnvGetNextInstanceInClassAndSubclasses(void *,void **,void *,DATA_OBJECT *);
   LOCALE void                           EnvGetInstancePPForm(void *,char *,unsigned,void *);
   LOCALE void                           ClassCommand(void *,DATA_OBJECT *);
   LOCALE intBool                        DeleteInstanceCommand(void *);
   LOCALE intBool                        UnmakeInstanceCommand(void *);
   LOCALE void                           SymbolToInstanceName(void *,DATA_OBJECT *);
   LOCALE void                          *InstanceNameToSymbol(void *);
   LOCALE void                           InstanceAddressCommand(void *,DATA_OBJECT *);
   LOCALE void                           InstanceNameCommand(void *,DATA_OBJECT *);
   LOCALE intBool                        InstanceAddressPCommand(void *);
   LOCALE intBool                        InstanceNamePCommand(void *);
   LOCALE intBool                        InstancePCommand(void *);
   LOCALE intBool                        InstanceExistPCommand(void *);
   LOCALE intBool                        CreateInstanceHandler(void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   LOCALE char                          *GetInstanceName(void *);
   LOCALE void                          *CreateRawInstance(void *,const char *);
   LOCALE intBool                        DeleteInstance(void *);
   LOCALE void                           DirectGetSlot(void *,const char *,DATA_OBJECT *);
   LOCALE int                            DirectPutSlot(void *,const char *,DATA_OBJECT *);
   LOCALE void                          *FindInstance(void *,const char *,unsigned);
   LOCALE void                          *GetInstanceClass(void *);
   LOCALE void                           GetInstancePPForm(char *,unsigned,void *);
   LOCALE void                          *GetNextInstance(void *);
   LOCALE void                          *GetNextInstanceInClass(void *,void *);
   LOCALE void                          *GetNextInstanceInClassAndSubclasses(void **,void *,DATA_OBJECT *);
   LOCALE void                           Instances(const char *,void *,const char *,int);
#if DEBUGGING_FUNCTIONS
   LOCALE void                          *MakeInstance(const char *);
#endif
   LOCALE intBool                        UnmakeInstance(void *);
   LOCALE int                            ValidInstanceAddress(void *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_inscom */





