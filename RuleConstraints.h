/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*             RULE CONSTRAINTS HEADER FILE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for detecting constraint       */
/*   conflicts in the LHS and RHS of rules.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
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
/*************************************************************/

#ifndef _H_rulecstr

#pragma once

#define _H_rulecstr

struct lhsParseNode *GetExpressionVarConstraints(const Environment::Ptr&, struct lhsParseNode *);
struct lhsParseNode *DeriveVariableConstraints(const Environment::Ptr&, struct lhsParseNode *);
bool ProcessConnectedConstraints(const Environment::Ptr&, struct lhsParseNode *, struct lhsParseNode *, struct lhsParseNode *);
void ConstraintReferenceErrorMessage(const Environment::Ptr&,
                                     CLIPSLexeme *,
                                     struct lhsParseNode *,
                                     int, int,
                                     CLIPSLexeme *,
                                     int);
bool CheckRHSForConstraintErrors(const Environment::Ptr&, Expression *, struct lhsParseNode *);

#endif /* _H_rulecstr */

