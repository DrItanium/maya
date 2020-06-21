   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/17/19             */
   /*                                                     */
   /*                   API HEADER FILE                   */
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
/*      6.24: Added filertr.h and tmpltfun.h to include      */
/*            list.                                          */
/*                                                           */
/*      6.30: Added classpsr.h, iofun.h, and strngrtr.h to   */
/*            include list.                                  */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_CLIPS_API

#pragma once

#define _H_CLIPS_API

#include <stdio.h>

#include "setup.h"
#include "argacces.h"
#include "constant.h"
#include "memalloc.h"
#include "cstrcpsr.h"
#include "fileutil.h"
#include "strngfun.h"
#include "environment.h"
#include "CommandLine.h"
#include "symbol.h"

#include "prntutil.h"
#include "router.h"
#include "filertr.h"
#include "strngrtr.h"

#include "iofun.h"

#include "sysdep.h"
#include "bmathfun.h"
#include "expression.h"
#include "expression.h"
#include "evaluation.h"
#include "miscfun.h"
#include "constrct.h"
#include "utility.h"
#include "watch.h"
#include "modulbsc.h"

#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif

#if BLOAD_AND_BSAVE
#include "bsave.h"
#endif

#if DEFRULE_CONSTRUCT
#include "ruledef.h"
#include "rulebsc.h"
#include "engine.h"
#include "drive.h"
#include "incrrset.h"
#include "rulecom.h"
#include "crstrtgy.h"
#endif

#if DEFFACTS_CONSTRUCT
#include "dffctdef.h"
#include "dffctbsc.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltdef.h"
#include "tmpltbsc.h"
#include "tmpltfun.h"
#include "fact.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#include "globlbsc.h"
#include "globlcom.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#include "genrcfun.h"
#endif

#if OBJECT_SYSTEM
#include "classcom.h"
#include "classexm.h"
#include "classfun.h"
#include "classinf.h"
#include "classini.h"
#include "classpsr.h"
#include "defins.h"
#include "inscom.h"
#include "insfile.h"
#include "insfun.h"
#include "insmngr.h"
#include "msgcom.h"
#include "msgpass.h"
#include "objrtmch.h"
#endif

#endif /* _H_CLIPS_API */
