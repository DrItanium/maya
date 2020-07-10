/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*                ANALYSIS HEADER FILE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Analyzes LHS patterns to check for semantic      */
/*   errors and to determine variable comparisons and other  */
/*   tests which must be performed either in the pattern or  */
/*   join networks.                                          */
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
/*      6.30: Join network rework and optimizations.         */
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

#ifndef _H_analysis

#pragma once

#define _H_analysis

#include "Environment.h"
#include "Expression.h"
#include "Reorder.h"

/*****************************************************/
/* nandFrame structure: Stores information about the */
/*   current position in the nesting of not/and CEs  */
/*   as the patterns of a rule are analyzed.         */
/*****************************************************/
struct nandFrame {
private:
    int depth;
    lhsParseNode *nandCE;
    nandFrame *next;
public:
    nandFrame(int d, lhsParseNode* nce, nandFrame* nxt = nullptr);
    constexpr auto getDepth() const noexcept { return depth; }
    void setDepth(int value) noexcept { depth = value; }
    auto getNandCE() const noexcept { return nandCE; }
    void setNandCE(lhsParseNode* value) noexcept { nandCE = value; }
    auto getNext() const noexcept { return next; }
    void setNext(nandFrame* value) noexcept { next = value; }
};

bool VariableAnalysis(const Environment&, struct lhsParseNode *);

#endif

