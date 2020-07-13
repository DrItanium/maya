/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/11/17            */
/*                                                     */
/*            MEMORY ALLOCATION HEADER FILE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Memory allocation routines.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed HaltExecution check from the           */
/*            EnvReleaseMem function. DR0863                 */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems.                   */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Removed genlongalloc/genlongfree functions.    */
/*                                                           */
/*            Added get_mem and rtn_mem macros.              */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Removed deallocating message parameter from    */
/*            EnvReleaseMem.                                 */
/*                                                           */
/*            Removed support for BLOCK_MEMORY.              */
/*                                                           */
/*      6.31: Fix for get_mem macro.                         */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*************************************************************/

#ifndef _H_memalloc

#pragma once

#define _H_memalloc

#include <cstring>

#ifndef MEM_TABLE_SIZE
#define MEM_TABLE_SIZE 0
#endif


template<typename T, typename ... Args>
std::shared_ptr<T> getStruct(const Environment::Ptr&, Args&& ... ctorArgs) {
    return std::make_shared<T>(std::forward<Args>(ctorArgs)...);
}
/*
 * Debug case (routes all memory management through genalloc/genfree to take advantage of
 * platform, memory debugging aids)
 */
//#define get_struct(theEnv,type) ((struct type *) genalloc(theEnv,sizeof(struct type)))
#define get_struct(theEnv, type) (getStruct<type>(theEnv))

#define rtn_struct(theEnv,type,struct_ptr) (genfree(theEnv,struct_ptr,sizeof(struct type)))

#define rtn_sized_struct(theEnv,size,struct_ptr) (genfree(theEnv,struct_ptr,size))

#define get_var_struct(theEnv,type,vsize) ((struct type *) genalloc(theEnv,(sizeof(struct type) + vsize)))

#define rtn_var_struct(theEnv,type,vsize,struct_ptr) (genfree(theEnv,struct_ptr,sizeof(struct type)+vsize))

#define get_mem(theEnv,size) ((struct type *) genalloc(theEnv,(size_t) (size)))

#define rtn_mem(theEnv,size,ptr) (genfree(theEnv,ptr,size))

#define GenCopyMemory(type, cnt, dst, src) \
   memcpy((void *) (dst),(void *) (src),sizeof(type) * (size_t) (cnt))

void *genalloc(const Environment::Ptr&, size_t);
void genfree(const Environment::Ptr&, void *, size_t);
void *genrealloc(const Environment::Ptr&, void *, size_t, size_t);
void *gm1(const Environment::Ptr&, size_t);
void *gm2(const Environment::Ptr&, size_t);
void rm(const Environment::Ptr&, void *, size_t);

#endif /* _H_memalloc */






