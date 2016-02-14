   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  01/06/16             */
   /*                                                     */
   /*              FACT FUNCTIONS HEADER FILE             */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Added ppfact function.                         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*************************************************************/

#ifndef _H_factfun

#pragma once

#define _H_factfun

#include "factmngr.h"

   void                           FactFunctionDefinitions(void *);
   void                           FactRelationFunction(UDFContext *,CLIPSValue *);
   void                          *FactRelation(void *);
   void                          *EnvFactDeftemplate(void *,void *);
   void                           FactExistpFunction(UDFContext *,CLIPSValue *);
   bool                           EnvFactExistp(void *,void *);
   void                           FactSlotValueFunction(UDFContext *,CLIPSValue *);
   void                           FactSlotValue(void *,void *,const char *,CLIPSValue *);
   void                           FactSlotNamesFunction(UDFContext *,CLIPSValue *);
   void                           EnvFactSlotNames(void *,void *,DATA_OBJECT *);
   void                           GetFactListFunction(UDFContext *,CLIPSValue *);
   void                           EnvGetFactList(void *,DATA_OBJECT *,void *);
   void                           PPFactFunction(UDFContext *,CLIPSValue *);
   void                           EnvPPFact(void *,void *,const char *,bool);
   struct fact                   *GetFactAddressOrIndexArgument(UDFContext *,bool);

#endif /* _H_factfun */

