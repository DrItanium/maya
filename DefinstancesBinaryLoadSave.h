/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
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
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_dfinsbin

#pragma once

#define _H_dfinsbin

#if DEFINSTANCES_CONSTRUCT && (BLOAD_AND_BSAVE)

#include "Definstances.h"

constexpr auto DFINSBIN_DATA = 25;

struct definstancesBinaryData : public EnvironmentModule {
    Definstances *DefinstancesArray;
    unsigned long DefinstancesCount;
    unsigned long ModuleCount;
    DEFINSTANCES_MODULE *ModuleArray;
};
RegisterEnvironmentModule(definstancesBinaryData, DFINSBIN_DATA, DefinstancesBinary);

void SetupDefinstancesBload(const Environment::Ptr&);
void *BloadDefinstancesModuleRef(const Environment::Ptr&, unsigned long);

#endif /* DEFINSTANCES_CONSTRUCT && (BLOAD_AND_BSAVE) */

#endif /* _H_dfinsbin */



