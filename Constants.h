/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  04/22/20             */
/*                                                     */
/*                CONSTANTS HEADER FILE                */
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
/*      6.30: Moved default type constants (NO_DEFAULT,      */
/*            STATIC_DEFAULT, and DYNAMIC_DEFAULT) to        */
/*            constant.h                                     */
/*                                                           */
/*            Added DATA_OBJECT_ARRAY primitive type.        */
/*                                                           */
/*            Added NESTED_RHS constant.                     */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*************************************************************/

#ifndef _H_constant

#pragma once

#define _H_constant


#include <cstdint>
#include <climits>

#define X(name, idx) constexpr auto name = idx
X(EXACTLY, 0);
X(AT_LEAST, 1);
X(NO_MORE_THAN, 2);
X(RANGE, 3);
X(UNBOUNDED, USHRT_MAX);
X(CLIPS_LHS, 0);
X(CLIPS_RHS, 1);
X(NESTED_RHS, 2);
X(NEGATIVE, 0);
X(POSITIVE, 1);
X(EOS, '\0');
X(INSIDE, 0);
X(OUTSIDE, 1);
X(LESS_THAN, 0);
X(GREATER_THAN, 1);
X(EQUAL, 2);
#undef X

enum SaveScope{
    LOCAL_SAVE,
    VISIBLE_SAVE
};

enum DefaultType {
    NO_DEFAULT,
    STATIC_DEFAULT,
    DYNAMIC_DEFAULT
} ;

enum PutSlotError {
    PSE_NO_ERROR = 0,
    PSE_nullptr_POINTER_ERROR,
    PSE_INVALID_TARGET_ERROR,
    PSE_SLOT_NOT_FOUND_ERROR,
    PSE_TYPE_ERROR,
    PSE_RANGE_ERROR,
    PSE_ALLOWED_VALUES_ERROR,
    PSE_CARDINALITY_ERROR,
    PSE_ALLOWED_CLASSES_ERROR,
    PSE_EVALUATION_ERROR,
    PSE_RULE_NETWORK_ERROR
};

enum GetSlotError {
    GSE_NO_ERROR = 0,
    GSE_nullptr_POINTER_ERROR,
    GSE_INVALID_TARGET_ERROR,
    GSE_SLOT_NOT_FOUND_ERROR
};

#ifndef APPLICATION_NAME
#define APPLICATION_NAME "CLIPS"
#endif

#ifndef COMMAND_PROMPT
#define COMMAND_PROMPT "CLIPS> "
#endif

#ifndef VERSION_STRING
#define VERSION_STRING "6.40"
#endif

#ifndef CREATION_DATE_STRING
#define CREATION_DATE_STRING "4/22/20"
#endif

#ifndef BANNER_STRING
#define BANNER_STRING "         CLIPS (Cypher Beta 4/22/20)\n"
#endif

/*************************/
/* TOKEN AND TYPE VALUES */
/*************************/

#define OBJECT_TYPE_NAME               "OBJECT"
#define USER_TYPE_NAME                 "USER"
#define PRIMITIVE_TYPE_NAME            "PRIMITIVE"
#define NUMBER_TYPE_NAME               "NUMBER"
#define INTEGER_TYPE_NAME              "INTEGER"
#define FLOAT_TYPE_NAME                "FLOAT"
#define SYMBOL_TYPE_NAME               "SYMBOL"
#define STRING_TYPE_NAME               "STRING"
#define MULTIFIELD_TYPE_NAME           "MULTIFIELD"
#define LEXEME_TYPE_NAME               "LEXEME"
#define ADDRESS_TYPE_NAME              "ADDRESS"
#define EXTERNAL_ADDRESS_TYPE_NAME     "EXTERNAL-ADDRESS"
#define FACT_ADDRESS_TYPE_NAME         "FACT-ADDRESS"
#define INSTANCE_TYPE_NAME             "INSTANCE"
#define INSTANCE_NAME_TYPE_NAME        "INSTANCE-NAME"
#define INSTANCE_ADDRESS_TYPE_NAME     "INSTANCE-ADDRESS"

/*************************************************************************/
/* The values of these constants should not be changed.  They are set to */
/* start after the primitive type codes in CONSTANT.H.  These codes are  */
/* used to let the generic function bsave image be used whether COOL is  */
/* present or not.                                                       */
/*************************************************************************/
#define X(name, code) constexpr auto name = code
X(OBJECT_TYPE_CODE, 9);
X(PRIMITIVE_TYPE_CODE, 10);
X(NUMBER_TYPE_CODE, 11);
X(LEXEME_TYPE_CODE, 12);
X(ADDRESS_TYPE_CODE, 13);
X(INSTANCE_TYPE_CODE, 14);
#undef X

enum CLIPSType {
    FLOAT_BIT = (1 << 0),
    INTEGER_BIT = (1 << 1),
    SYMBOL_BIT = (1 << 2),
    STRING_BIT = (1 << 3),
    MULTIFIELD_BIT = (1 << 4),
    EXTERNAL_ADDRESS_BIT = (1 << 5),
    FACT_ADDRESS_BIT = (1 << 6),
    INSTANCE_ADDRESS_BIT = (1 << 7),
    INSTANCE_NAME_BIT = (1 << 8),
    VOID_BIT = (1 << 9),
    BOOLEAN_BIT = (1 << 10),
};

constexpr auto NUMBER_BITS = (INTEGER_BIT | FLOAT_BIT);
constexpr auto LEXEME_BITS = (SYMBOL_BIT | STRING_BIT | BOOLEAN_BIT);
constexpr auto ADDRESS_BITS = (EXTERNAL_ADDRESS_BIT | FACT_ADDRESS_BIT | INSTANCE_ADDRESS_BIT);
constexpr auto INSTANCE_BITS = (INSTANCE_ADDRESS_BIT | INSTANCE_NAME_BIT);
constexpr auto SINGLEFIELD_BITS = (NUMBER_BITS | LEXEME_BITS | ADDRESS_BITS | INSTANCE_NAME_BIT);
constexpr auto ANY_TYPE_BITS = (VOID_BIT | SINGLEFIELD_BITS | MULTIFIELD_BIT);

/****************************************************/
/* The first 9 primitive types need to retain their */
/* values!! Sorted arrays depend on their values!!  */
/****************************************************/


#define X(name, code) constexpr auto name = code
X(FLOAT_TYPE                     , 0);
X(INTEGER_TYPE                   , 1);
X(SYMBOL_TYPE                    , 2);
X(STRING_TYPE                    , 3);
X(MULTIFIELD_TYPE                , 4);
X(EXTERNAL_ADDRESS_TYPE          , 5);
X(FACT_ADDRESS_TYPE              , 6);
X(INSTANCE_ADDRESS_TYPE          , 7);
X(INSTANCE_NAME_TYPE             , 8);

X( VOID_TYPE                     ,  9);
X( BITMAP_TYPE                   , 11);

X( FCALL                         , 30);
X( GCALL                         , 31);
X( PCALL                         , 32);
X( GBL_VARIABLE                  , 33);
X( MF_GBL_VARIABLE               , 34);

X( SF_VARIABLE                   , 35);
X( MF_VARIABLE                   , 36);
X( BITMAPARRAY                   , 39);

X( FACT_PN_CMP1                  , 50);
X( FACT_JN_CMP1                  , 51);
X( FACT_JN_CMP2                  , 52);
X( FACT_SLOT_LENGTH              , 53);
X( FACT_PN_VAR1                  , 54);
X( FACT_PN_VAR2                  , 55);
X( FACT_PN_VAR3                  , 56);
X( FACT_JN_VAR1                  , 57);
X( FACT_JN_VAR2                  , 58);
X( FACT_JN_VAR3                  , 59);
X( FACT_PN_CONSTANT1             , 60);
X( FACT_PN_CONSTANT2             , 61);
X( FACT_STORE_MULTIFIELD         , 62);
X( DEFTEMPLATE_PTR               , 63);

X( OBJ_GET_SLOT_PNVAR1           , 70);
X( OBJ_GET_SLOT_PNVAR2           , 71);
X( OBJ_GET_SLOT_JNVAR1           , 72);
X( OBJ_GET_SLOT_JNVAR2           , 73);
X( OBJ_SLOT_LENGTH               , 74);
X( OBJ_PN_CONSTANT               , 75);
X( OBJ_PN_CMP1                   , 76);
X( OBJ_JN_CMP1                   , 77);
X( OBJ_PN_CMP2                   , 78);
X( OBJ_JN_CMP2                   , 79);
X( OBJ_PN_CMP3                   , 80);
X( OBJ_JN_CMP3                   , 81);
X( DEFCLASS_PTR                  , 82);
X( HANDLER_GET                   , 83);
X( HANDLER_PUT                   , 84);

X( DEFGLOBAL_PTR                 , 90);

X( PROC_PARAM                    , 95);
X( PROC_WILD_PARAM               , 96);
X( PROC_GET_BIND                 , 97);
X( PROC_BIND                     , 98);

X( UNKNOWN_VALUE                 ,173);

X( INTEGER_OR_FLOAT              ,180);
X( SYMBOL_OR_STRING              ,181);
X( INSTANCE_OR_INSTANCE_NAME     ,182);
#undef X
constexpr bool isVariableType(unsigned short value) noexcept {
    switch (value) {
        case MF_VARIABLE:
        case SF_VARIABLE:
        case GBL_VARIABLE:
        case MF_GBL_VARIABLE:
            return true;
        default:
            return false;
    }
}
constexpr bool isConstantType(unsigned short value) noexcept {
    switch (value) {
        case SYMBOL_TYPE:
        case STRING_TYPE:
        case INTEGER_TYPE:
        case FLOAT_TYPE:
        case INSTANCE_NAME_TYPE:
        case INSTANCE_ADDRESS_TYPE:
            return true;
        default:
            return false;
    }
}
#endif
