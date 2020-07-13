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
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_objbin

#pragma once

#define _H_objbin

#include "Object.h"

constexpr auto OBJECTBIN_DATA = 33;

struct objectBinaryData : public EnvironmentModule {
    Defclass *DefclassArray;
    unsigned long ModuleCount;
    unsigned long ClassCount;
    unsigned long LinkCount;
    unsigned long SlotCount;
    unsigned long SlotNameCount;
    unsigned long TemplateSlotCount;
    unsigned long SlotNameMapCount;
    unsigned long HandlerCount;
    DEFCLASS_MODULE *ModuleArray;
    Defclass **LinkArray;
    SlotDescriptor *SlotArray;
    SlotDescriptor **TmpslotArray;
    SLOT_NAME *SlotNameArray;
    unsigned *MapslotArray;
    DefmessageHandler *HandlerArray;
    unsigned *MaphandlerArray;
};
RegisterEnvironmentModule(objectBinaryData, OBJECTBIN_DATA, ObjectBinary);

#define DefclassPointer(i) (((i) == ULONG_MAX) ? nullptr : &ObjectBinaryData(theEnv)->DefclassArray[i])
#define DefclassIndex(cls) (((cls) == nullptr) ? ULONG_MAX : ((ConstructHeader *) cls)->bsaveID)

void SetupObjectsBload(const Environment::Ptr&);
void *BloadDefclassModuleReference(const Environment::Ptr&, unsigned long);

#endif /* _H_objbin */



