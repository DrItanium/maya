/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  12/02/19            */
/*                                                     */
/*    CONSTRAINT BLOAD/BSAVE/CONSTRUCTS-TO-C HEADER    */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    constraint records.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnbin

#pragma once

#define _H_cstrnbin

#include <cstdio>

#include "Evaluation.h"
#include "Constraint.h"

#define ConstraintIndex(theConstraint) (((! GetDynamicConstraintChecking(theEnv)) || (theConstraint == nullptr)) ? ULONG_MAX : (theConstraint->getBSaveID()))
#define ConstraintPointer(i) (((i) == ULONG_MAX) ? nullptr : (CONSTRAINT_RECORD *) &ConstraintData(theEnv)->ConstraintArray[i])

#if BLOAD_AND_BSAVE
void WriteNeededConstraints(Environment *, FILE *);
#endif
void ReadNeededConstraints(Environment *);
void ClearBloadedConstraints(Environment *);

#endif /* _H_cstrnbin */


