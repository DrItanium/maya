   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.30  08/16/14          */
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
/*            Fixed linkage issue when DEBUGGING_FUNCTIONS   */
/*            is set to 0 and PROFILING_FUNCTIONS is set to  */
/*            1.                                             */
/*                                                           */
/*************************************************************/

#ifndef _H_genrccom
#define _H_genrccom

#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_genrcfun
#include "genrcfun.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _GENRCCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE void                           SetupGenericFunctions(void *);
   LOCALE void                          *EnvFindDefgeneric(void *,const char *);
   LOCALE DEFGENERIC                    *LookupDefgenericByMdlOrScope(void *,const char *);
   LOCALE DEFGENERIC                    *LookupDefgenericInScope(void *,const char *);
   LOCALE void                          *EnvGetNextDefgeneric(void *,void *);
   LOCALE long                           EnvGetNextDefmethod(void *,void *,long);
   LOCALE int                            EnvIsDefgenericDeletable(void *,void *);
   LOCALE int                            EnvIsDefmethodDeletable(void *,void *,long);
   LOCALE void                           UndefgenericCommand(void *);
   LOCALE void                          *GetDefgenericModuleCommand(void *);
   LOCALE void                           UndefmethodCommand(void *);
   LOCALE DEFMETHOD                     *GetDefmethodPointer(void *,long);
   LOCALE intBool                        EnvUndefgeneric(void *,void *);
   LOCALE intBool                        EnvUndefmethod(void *,void *,long);
#if ! OBJECT_SYSTEM
   LOCALE void                           TypeCommand(void *,DATA_OBJECT *);
#endif
#if DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS
   LOCALE void                           EnvGetDefmethodDescription(void *,char *,int,void *,long);
#endif
#if DEBUGGING_FUNCTIONS
   LOCALE unsigned                       EnvGetDefgenericWatch(void *,void *);
   LOCALE void                           EnvSetDefgenericWatch(void *,unsigned,void *);
   LOCALE unsigned                       EnvGetDefmethodWatch(void *,void *,long);
   LOCALE void                           EnvSetDefmethodWatch(void *,unsigned,void *,long);
   LOCALE void                           PPDefgenericCommand(void *);
   LOCALE void                           PPDefmethodCommand(void *);
   LOCALE void                           ListDefmethodsCommand(void *);
   LOCALE char                          *EnvGetDefmethodPPForm(void *,void *,long);
   LOCALE void                           ListDefgenericsCommand(void *);
   LOCALE void                           EnvListDefgenerics(void *,const char *,struct defmodule *);
   LOCALE void                           EnvListDefmethods(void *,const char *,void *);
#endif
   LOCALE void                           GetDefgenericListFunction(void *,DATA_OBJECT *);
   LOCALE void                           EnvGetDefgenericList(void *,DATA_OBJECT *,struct defmodule *);
   LOCALE void                           GetDefmethodListCommand(void *,DATA_OBJECT *);
   LOCALE void                           EnvGetDefmethodList(void *,void *,DATA_OBJECT *);
   LOCALE void                           GetMethodRestrictionsCommand(void *,DATA_OBJECT *);
   LOCALE void                           EnvGetMethodRestrictions(void *,void *,long,DATA_OBJECT *);
   LOCALE SYMBOL_HN                     *GetDefgenericNamePointer(void *);
   LOCALE void                           SetNextDefgeneric(void *,void *);
   LOCALE char                          *EnvDefgenericModule(void *,void *);
   LOCALE char                          *EnvGetDefgenericName(void *,void *);
   LOCALE char                          *EnvGetDefgenericPPForm(void *,void *);
   LOCALE SYMBOL_HN                     *EnvGetDefgenericNamePointer(void *,void *);
   LOCALE void                           EnvSetDefgenericPPForm(void *,void *,char *);

#if ALLOW_ENVIRONMENT_GLOBALS

   LOCALE void                           SetDefgenericPPForm(void *,char *);
   LOCALE char                          *DefgenericModule(void *);
   LOCALE void                          *FindDefgeneric(const char *);
   LOCALE void                           GetDefgenericList(DATA_OBJECT *,struct defmodule *);
   LOCALE char                          *GetDefgenericName(void *);
   LOCALE char                          *GetDefgenericPPForm(void *);
   LOCALE void                          *GetNextDefgeneric(void *);
   LOCALE int                            IsDefgenericDeletable(void *);
   LOCALE intBool                        Undefgeneric(void *);
   LOCALE void                           GetDefmethodList(void *,DATA_OBJECT_PTR);
   LOCALE void                           GetMethodRestrictions(void *,long,DATA_OBJECT *);
   LOCALE long                           GetNextDefmethod(void *,long );
   LOCALE int                            IsDefmethodDeletable(void *,long );
   LOCALE intBool                        Undefmethod(void *,long );
#if DEBUGGING_FUNCTIONS
   LOCALE unsigned                       GetDefgenericWatch(void *);
   LOCALE void                           ListDefgenerics(const char *,struct defmodule *);
   LOCALE void                           SetDefgenericWatch(unsigned,void *);
   LOCALE char                          *GetDefmethodPPForm(void *,long);
   LOCALE unsigned                       GetDefmethodWatch(void *,long);
   LOCALE void                           ListDefmethods(const char *,void *);
   LOCALE void                           SetDefmethodWatch(unsigned,void *,long);
#endif /* DEBUGGING_FUNCTIONS */
#if DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS
   LOCALE void                           GetDefmethodDescription(char *,int,void *,long );
#endif /* DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS */

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_genrccom */





