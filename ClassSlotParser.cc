/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  04/03/19             */
/*                                                     */
/*                 CLASS PARSER MODULE                 */
/*******************************************************/

/**************************************************************/
/* Purpose: Parsing Routines for Defclass Construct           */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Dantes                                       */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to          */
/*            DEFRULE_CONSTRUCT.                              */
/*                                                            */
/*            Renamed BOOLEAN macro type to intBool.          */
/*                                                            */
/*      6.30: Changed integer type/precision.                 */
/*                                                            */
/*            Support for long long integers.                 */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
/*      6.31: Changed allocation of multifield slot default   */
/*            from ephemeral to explicit deallocation.        */
/*                                                            */
/*      6.40: Pragma once and other inclusion changes.        */
/*                                                            */
/*            Added support for booleans with <stdbool.h>.    */
/*                                                            */
/*            Removed use of void pointers for specific       */
/*            data structures.                                */
/*                                                            */
/*            Static constraint checking is always enabled.   */
/*                                                            */
/*            UDF redesign.                                   */
/*                                                            */
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "Setup.h"


#include <cstring>

#include "ClassCommands.h"
#include "ClassFunctions.h"
#include "ConstraintChecking.h"
#include "ConstraintParser.h"
#include "ConstraintUtilities.h"
#include "DefaultAttribute.h"
#include "Environment.h"
#include "InstanceFunctions.h"
#include "MemoryAllocation.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Scanner.h"

#include "ClassSlotParser.h"
#include "ReferenceCounted.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define DEFAULT_FACET         "default"
#define DYNAMIC_FACET         "default-dynamic"
#define VARIABLE_VAR          "VARIABLE"

#define STORAGE_FACET         "storage"
#define SLOT_SHARE_RLN        "shared"
#define SLOT_LOCAL_RLN        "local"

#define ACCESS_FACET          "access"
#define SLOT_RDONLY_RLN       "read-only"
#define SLOT_RDWRT_RLN        "read-write"
#define SLOT_INIT_RLN         "initialize-only"

#define PROPAGATION_FACET     "propagation"
#define SLOT_NO_INH_RLN       "no-inherit"
#define SLOT_INH_RLN          "inherit"

#define SOURCE_FACET          "source"
#define SLOT_COMPOSITE_RLN    "composite"
#define SLOT_EXCLUSIVE_RLN    "exclusive"

#define MATCH_FACET           MATCH_RLN
#define SLOT_REACTIVE_RLN     REACTIVE_RLN
#define SLOT_NONREACTIVE_RLN  NONREACTIVE_RLN

#define VISIBILITY_FACET      "visibility"
#define SLOT_PUBLIC_RLN       "public"
#define SLOT_PRIVATE_RLN      "private"

#define CREATE_ACCESSOR_FACET "create-accessor"
#define SLOT_READ_RLN         "read"
#define SLOT_WRITE_RLN        "write"
#define SLOT_NONE_RLN         "NONE"

#define OVERRIDE_MSG_FACET    "override-message"
#define SLOT_DEFAULT_RLN      "DEFAULT"

constexpr auto STORAGE_BIT           = 0;
constexpr auto FIELD_BIT             = 1;
constexpr auto ACCESS_BIT            = 2;
constexpr auto PROPAGATION_BIT       = 3;
constexpr auto SOURCE_BIT            = 4;
constexpr auto MATCH_BIT             = 5;
constexpr auto DEFAULT_BIT           = 6;
constexpr auto DEFAULT_DYNAMIC_BIT   = 7;
constexpr auto VISIBILITY_BIT        = 8;
constexpr auto CREATE_ACCESSOR_BIT   = 9;
constexpr auto OVERRIDE_MSG_BIT      = 10;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static SlotDescriptor *NewSlot(const Environment&, CLIPSLexeme *);
static TEMP_SLOT_LINK *InsertSlot(const Environment&, const char *, TEMP_SLOT_LINK *, SlotDescriptor *);
static int ParseSimpleFacet(const Environment&, const char *, SlotDescriptor *, char *, const char *, int, const char *,
                            const char *, const char *, const char *, CLIPSLexeme **);
static bool ParseDefaultFacet(const Environment&, const char *, char *, SlotDescriptor *);
static void BuildCompositeFacets(const Environment&, SlotDescriptor *, PACKED_CLASS_LINKS *, const char *,
                                 CONSTRAINT_PARSE_RECORD *);
static bool CheckForFacetConflicts(const Environment&, SlotDescriptor *, CONSTRAINT_PARSE_RECORD *);
static bool EvaluateSlotDefaultValue(const Environment&, SlotDescriptor *, const char *);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/************************************************************
  NAME         : ParseSlot
  DESCRIPTION  : Parses slot definitions for a
                   defclass statement
  INPUTS       : 1) The logical name of the input source
                 2) The current slot list
                 3) The class precedence list for the class
                    to which this slot is being attached
                    (used to find facets for composite slots)
                 4) A flag indicating if this is a multifield
                    slot or not
                 5) A flag indicating if the type of slot
                    (single or multi) was explicitly
                    specified or not
  RETURNS      : The address of the list of slots,
                   nullptr if there was an error
  SIDE EFFECTS : The slot list is allocated
  NOTES        : Assumes "(slot" has already been parsed.
 ************************************************************/
TEMP_SLOT_LINK *ParseSlot(
        const Environment&theEnv,
        const char *readSource,
        const char *className,
        TEMP_SLOT_LINK *slist,
        PACKED_CLASS_LINKS *preclist,
        bool multiSlot) {
    SlotDescriptor *slot;
    CONSTRAINT_PARSE_RECORD parsedConstraint;
    char specbits[2];
    int rtnCode;
    CLIPSLexeme *newOverrideMsg;

    /* ===============================================================
       Bits in specbits are when slot qualifiers are specified so that
       duplicate or conflicting qualifiers can be detected.

       Shared/local                          bit-0
       Single/multiple                       bit-1
       Read-only/Read-write/Initialize-Only  bit-2
       Inherit/No-inherit                    bit-3
       Composite/Exclusive                   bit-4
       Reactive/Nonreactive                  bit-5
       Default                               bit-6
       Default-dynamic                       bit-7
       Visibility                            bit-8
       Override-message                      bit-9
       =============================================================== */
    SavePPBuffer(theEnv, " ");
    specbits[0] = specbits[1] = '\0';
    GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
    if (DefclassData(theEnv)->ObjectParseToken.tknType != SYMBOL_TOKEN) {
        DeleteSlots(theEnv, slist);
        SyntaxErrorMessage(theEnv, "defclass slot");
        return nullptr;
    }
    if ((DefclassData(theEnv)->ObjectParseToken.value == (void *) DefclassData(theEnv)->ISA_SYMBOL) ||
        (DefclassData(theEnv)->ObjectParseToken.value == (void *) DefclassData(theEnv)->NAME_SYMBOL)) {
        DeleteSlots(theEnv, slist);
        SyntaxErrorMessage(theEnv, "defclass slot");
        return nullptr;
    }
    slot = NewSlot(theEnv, DefclassData(theEnv)->ObjectParseToken.lexemeValue);
    slist = InsertSlot(theEnv, className, slist, slot);
    if (slist == nullptr)
        return nullptr;
    if (multiSlot) {
        slot->multiple = true;
        SetBitMap(specbits, FIELD_BIT);
    }

    GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
    IncrementIndentDepth(theEnv, 3);
    InitializeConstraintParseRecord(&parsedConstraint);
    while (DefclassData(theEnv)->ObjectParseToken.tknType == LEFT_PARENTHESIS_TOKEN) {
        PPBackup(theEnv);
        PPCRAndIndent(theEnv);
        SavePPBuffer(theEnv, "(");
        GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
        if (DefclassData(theEnv)->ObjectParseToken.tknType != SYMBOL_TOKEN) {
            SyntaxErrorMessage(theEnv, "defclass slot");
            goto ParseSlotError;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, DEFAULT_FACET) == 0) {
            if (!ParseDefaultFacet(theEnv, readSource, specbits, slot))
                goto ParseSlotError;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, DYNAMIC_FACET) == 0) {
            SetBitMap(specbits, DEFAULT_DYNAMIC_BIT);
            if (!ParseDefaultFacet(theEnv, readSource, specbits, slot))
                goto ParseSlotError;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, ACCESS_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, ACCESS_FACET, ACCESS_BIT,
                                       SLOT_RDWRT_RLN, SLOT_RDONLY_RLN, SLOT_INIT_RLN,
                                       nullptr, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            else if (rtnCode == 1)
                slot->noWrite = 1;
            else if (rtnCode == 2)
                slot->initializeOnly = 1;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, STORAGE_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, STORAGE_FACET, STORAGE_BIT,
                                       SLOT_LOCAL_RLN, SLOT_SHARE_RLN, nullptr, nullptr, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            slot->shared = (rtnCode == 0) ? false : true;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, PROPAGATION_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, PROPAGATION_FACET, PROPAGATION_BIT,
                                       SLOT_INH_RLN, SLOT_NO_INH_RLN, nullptr, nullptr, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            slot->noInherit = (rtnCode == 0) ? false : true;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, SOURCE_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, SOURCE_FACET, SOURCE_BIT,
                                       SLOT_EXCLUSIVE_RLN, SLOT_COMPOSITE_RLN, nullptr, nullptr, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            slot->composite = (rtnCode == 0) ? false : true;
        }
        else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, MATCH_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, MATCH_FACET, MATCH_BIT,
                                       SLOT_NONREACTIVE_RLN, SLOT_REACTIVE_RLN, nullptr, nullptr, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            slot->reactive = (rtnCode == 0) ? false : true;
        }
        else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, VISIBILITY_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, VISIBILITY_FACET, VISIBILITY_BIT,
                                       SLOT_PRIVATE_RLN, SLOT_PUBLIC_RLN, nullptr, nullptr, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            slot->publicVisibility = (rtnCode == 0) ? false : true;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, CREATE_ACCESSOR_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, CREATE_ACCESSOR_FACET,
                                       CREATE_ACCESSOR_BIT,
                                       SLOT_READ_RLN, SLOT_WRITE_RLN, SLOT_RDWRT_RLN,
                                       SLOT_NONE_RLN, nullptr);
            if (rtnCode == -1)
                goto ParseSlotError;
            if ((rtnCode == 0) || (rtnCode == 2))
                slot->createReadAccessor = true;
            if ((rtnCode == 1) || (rtnCode == 2))
                slot->createWriteAccessor = true;
        } else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, OVERRIDE_MSG_FACET) == 0) {
            rtnCode = ParseSimpleFacet(theEnv, readSource, slot, specbits, OVERRIDE_MSG_FACET, OVERRIDE_MSG_BIT,
                                       nullptr, nullptr, nullptr, SLOT_DEFAULT_RLN, &newOverrideMsg);
            if (rtnCode == -1)
                goto ParseSlotError;
            if (rtnCode == 4) {
                ReleaseLexeme(theEnv, slot->overrideMessage);
                slot->overrideMessage = newOverrideMsg;
                IncrementLexemeCount(slot->overrideMessage);
            }
            slot->overrideMessageSpecified = true;
        } else if (StandardConstraint(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents)) {
            if (!ParseStandardConstraint(theEnv, readSource, DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents,
                                         slot->constraint, &parsedConstraint, true))
                goto ParseSlotError;
        } else {
            SyntaxErrorMessage(theEnv, "defclass slot");
            goto ParseSlotError;
        }
        GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
    }
    if (DefclassData(theEnv)->ObjectParseToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "defclass slot");
        goto ParseSlotError;
    }

    if (DefclassData(theEnv)->ClassDefaultsModeValue == CONVENIENCE_MODE) {
        if (!TestBitMap(specbits, CREATE_ACCESSOR_BIT)) {
            slot->createReadAccessor = true;

            if (!slot->noWrite) { slot->createWriteAccessor = true; }
        }
    }

    if (slot->composite)
        BuildCompositeFacets(theEnv, slot, preclist, specbits, &parsedConstraint);
    if (!CheckForFacetConflicts(theEnv, slot, &parsedConstraint))
        goto ParseSlotError;
    if (!CheckConstraintParseConflicts(theEnv, slot->constraint))
        goto ParseSlotError;
    if (!EvaluateSlotDefaultValue(theEnv, slot, specbits))
        goto ParseSlotError;
    if ((slot->dynamicDefault == 0) && (slot->noWrite == 1) &&
        (slot->initializeOnly == 0))
        slot->shared = 1;
    slot->constraint = AddConstraint(theEnv, slot->constraint);
    DecrementIndentDepth(theEnv, 3);
    return (slist);

    ParseSlotError:
    DecrementIndentDepth(theEnv, 3);
    DeleteSlots(theEnv, slist);
    return nullptr;
}

/***************************************************
  NAME         : DeleteSlots
  DESCRIPTION  : Deallocates a list of slots and
                   their values
  INPUTS       : The address of the slot list
  RETURNS      : Nothing useful
  SIDE EFFECTS : The slot list is destroyed
  NOTES        : None
 ***************************************************/
void DeleteSlots(
        const Environment&theEnv,
        TEMP_SLOT_LINK *slots) {
    TEMP_SLOT_LINK *stmp;

    while (slots != nullptr) {
        stmp = slots;
        slots = slots->getNext();
        DeleteSlotName(theEnv, stmp->getDescription()->slotName);
        ReleaseLexeme(theEnv, stmp->getDescription()->overrideMessage);
        RemoveConstraint(theEnv, stmp->getDescription()->constraint);
        if (stmp->getDescription()->dynamicDefault == 1) {
            ExpressionDeinstall(theEnv, (Expression *) stmp->getDescription()->defaultValue);
            ReturnPackedExpression(theEnv, (Expression *) stmp->getDescription()->defaultValue);
        } else if (stmp->getDescription()->defaultValue != nullptr) {
            UDFValue *theValue = (UDFValue *) stmp->getDescription()->defaultValue;
            ReleaseUDFV(theEnv, theValue);
            if (theValue->header->type == MULTIFIELD_TYPE) { ReturnMultifield(theEnv, theValue->multifieldValue); }
            rtn_struct(theEnv, UDFValue, theValue);
        }
        rtn_struct(theEnv, slotDescriptor, stmp->getDescription());
        rtn_struct(theEnv, tempSlotLink, stmp);
    }
}

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/**************************************************************
  NAME         : NewSlot
  DESCRIPTION  : Allocates and initalizes a new slot structure
  INPUTS       : The symbolic name of the new slot
  RETURNS      : The address of the new slot
  SIDE EFFECTS : None
  NOTES        : Also adds symbols of the form get-<name> and
                   put-<name> for slot accessors
 **************************************************************/
static SlotDescriptor *NewSlot(
        const Environment&theEnv,
        CLIPSLexeme *name) {
    SlotDescriptor *slot;

    slot = get_struct(theEnv, slotDescriptor);
    slot->dynamicDefault = 1;
    slot->defaultSpecified = 0;
    slot->noDefault = 0;
    slot->reactive = 1;
    slot->noInherit = 0;
    slot->noWrite = 0;
    slot->initializeOnly = 0;
    slot->shared = 0;
    slot->multiple = 0;
    slot->composite = 0;
    slot->sharedCount = 0;
    slot->publicVisibility = 0;
    slot->createReadAccessor = false;
    slot->createWriteAccessor = false;
    slot->overrideMessageSpecified = 0;
    slot->cls = nullptr;
    slot->defaultValue = nullptr;
    slot->constraint = GetConstraintRecord(theEnv);
    slot->slotName = AddSlotName(theEnv, name, 0, false);
    slot->overrideMessage = slot->slotName->putHandlerName;
    IncrementLexemeCount(slot->overrideMessage);
    return (slot);
}

/**********************************************************
  NAME         : InsertSlot
  DESCRIPTION  : Inserts a slot into the list of slots
  INPUTS       : 1) The current head of the slot list
                 2) The slot to be inserted
  RETURNS      : The head of the slot list
  SIDE EFFECTS : The slot is inserted if no errors,
                   otherwise the original list and the
                   new slot are destroyed
  NOTES        : None
 **********************************************************/
static TEMP_SLOT_LINK *InsertSlot(
        const Environment&theEnv,
        const char *className,
        TEMP_SLOT_LINK *slist,
        SlotDescriptor *slot) {
    TEMP_SLOT_LINK *stmp, *sprv, *tmp;

    tmp = get_struct(theEnv, tempSlotLink);
    tmp->setDescription(slot);
    tmp->setNext(nullptr);
    if (slist == nullptr)
        slist = tmp;
    else {
        stmp = slist;
        sprv = nullptr;
        while (stmp != nullptr) {
            if (stmp->getDescription()->slotName == slot->slotName) {
                tmp->setNext(slist);
                DeleteSlots(theEnv, tmp);
                PrintErrorID(theEnv, "CLSLTPSR", 1, false);
                WriteString(theEnv, STDERR, "The '");
                WriteString(theEnv, STDERR, slot->slotName->name->contents);
                WriteString(theEnv, STDERR, "' slot for class '");
                WriteString(theEnv, STDERR, className);
                WriteString(theEnv, STDERR, "' is already specified.\n");
                return nullptr;
            }
            sprv = stmp;
            stmp = stmp->getNext();
        }
        sprv->setNext(tmp);
    }
    return (slist);
}

/****************************************************************
  NAME         : ParseSimpleFacet
  DESCRIPTION  : Parses the following facets for a slot:
                   access, source, propagation, storage,
                   pattern-match, visibility and override-message
  INPUTS       : 1) The input logical name
                 2) The bitmap indicating which facets have
                    already been parsed
                 3) The name of the facet
                 4) The bit to test/set in arg #2 for this facet
                 5) The facet value string which indicates the
                    facet should be false
                 6) The facet value string which indicates the
                    facet should be true
                 7) An alternate value string for use when the
                    first two don't match (can be nullptr)
                 7) An alternate value string for use when the
                    first three don't match (can be nullptr)
                    (will be an SF_VARIABLE type)
                 9) A buffer to hold the facet value symbol
                    (can be nullptr - only set if args #5 and #6
                     are both nullptr)
  RETURNS      : -1 on errors
                  0 if first value string matched
                  1 if second value string matched
                  2 if alternate value string matched
                  3 if variable value string matched
                  4 if facet value buffer was set
  SIDE EFFECTS : Messages printed on errors
                 Bitmap marked indicating facet was parsed
                 Facet value symbol buffer set, if appropriate
  NOTES        : None
 *****************************************************************/
static int ParseSimpleFacet(
        const Environment&theEnv,
        const char *readSource,
        SlotDescriptor *slot,
        char *specbits,
        const char *facetName,
        int testBit,
        const char *clearRelation,
        const char *setRelation,
        const char *alternateRelation,
        const char *varRelation,
        CLIPSLexeme **facetSymbolicValue) {
    int rtnCode;

    if (TestBitMap(specbits, testBit)) {
        PrintErrorID(theEnv, "CLSLTPSR", 2, false);
        WriteString(theEnv, STDERR, "The '");
        WriteString(theEnv, STDERR, facetName);
        WriteString(theEnv, STDERR, "' facet for slot '");
        WriteString(theEnv, STDERR, slot->slotName->name->contents);
        WriteString(theEnv, STDERR, "' is already specified.\n");
        return -1;
    }
    SetBitMap(specbits, testBit);
    SavePPBuffer(theEnv, " ");
    GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);

    /* ===============================
       Check for the variable relation
       =============================== */
    if (DefclassData(theEnv)->ObjectParseToken.tknType == SF_VARIABLE_TOKEN) {
        if ((varRelation == nullptr) ? false :
            (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, varRelation) == 0))
            rtnCode = 3;
        else
            goto ParseSimpleFacetError;
    } else {
        if (DefclassData(theEnv)->ObjectParseToken.tknType != SYMBOL_TOKEN)
            goto ParseSimpleFacetError;

        /* ===================================================
           If the facet value buffer is non-nullptr
           simply get the value and do not check any relations
           =================================================== */
        if (facetSymbolicValue == nullptr) {
            if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, clearRelation) == 0)
                rtnCode = 0;
            else if (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, setRelation) == 0)
                rtnCode = 1;
            else if ((alternateRelation == nullptr) ? false :
                     (strcmp(DefclassData(theEnv)->ObjectParseToken.lexemeValue->contents, alternateRelation) == 0))
                rtnCode = 2;
            else
                goto ParseSimpleFacetError;
        } else {
            rtnCode = 4;
            *facetSymbolicValue = DefclassData(theEnv)->ObjectParseToken.lexemeValue;
        }
    }
    GetToken(theEnv, readSource, &DefclassData(theEnv)->ObjectParseToken);
    if (DefclassData(theEnv)->ObjectParseToken.tknType != RIGHT_PARENTHESIS_TOKEN)
        goto ParseSimpleFacetError;
    return (rtnCode);

    ParseSimpleFacetError:
    SyntaxErrorMessage(theEnv, "slot facet");
    return (-1);
}

/*************************************************************
  NAME         : ParseDefaultFacet
  DESCRIPTION  : Parses the facet for a slot
  INPUTS       : 1) The input logical name
                 2) The bitmap indicating which facets have
                    already been parsed
                 3) The slot descriptor to set
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : Slot  set and parsed facet bitmap set
  NOTES        : Syntax: (default ?NONE|<expression>*)
                         (default-dynamic <expression>*)
 *************************************************************/
static bool ParseDefaultFacet(
        const Environment&theEnv,
        const char *readSource,
        char *specbits,
        SlotDescriptor *slot) {
    Expression *tmp;
    bool error, noneSpecified, deriveSpecified;

    if (TestBitMap(specbits, DEFAULT_BIT)) {
        PrintErrorID(theEnv, "CLSLTPSR", 2, false);
        WriteString(theEnv, STDERR, "The 'default' facet for slot '");
        WriteString(theEnv, STDERR, slot->slotName->name->contents);
        WriteString(theEnv, STDERR, "' is already specified.\n");
        return false;
    }
    SetBitMap(specbits, DEFAULT_BIT);
    error = false;
    tmp = ParseDefault(theEnv, readSource, true, TestBitMap(specbits, DEFAULT_DYNAMIC_BIT),
                       false, &noneSpecified, &deriveSpecified, &error);
    if (error)
        return false;
    if (noneSpecified || deriveSpecified) {
        if (noneSpecified) {
            slot->noDefault = 1;
            slot->defaultSpecified = 1;
        } else
            ClearBitMap(specbits, DEFAULT_BIT);
    } else {
        slot->defaultValue = PackExpression(theEnv, tmp);
        ReturnExpression(theEnv, tmp);
        ExpressionInstall(theEnv, (Expression *) slot->defaultValue);
        slot->defaultSpecified = 1;
    }
    return true;
}

/**************************************************************************
  NAME         : BuildCompositeFacets
  DESCRIPTION  : Composite slots are ones that get their facets
                   from more than one class.  By default, the most
                   specific class in object's precedence list specifies
                   the complete set of facets for a slot.  The composite
                   facet in a slot allows facets that are not overridden
                   by the most specific class to be obtained from other
                   classes.

                 Since all superclasses are predetermined before creating
                   a new class based on them, this routine need only
                   examine the immediately next most specific class for
                   extra facets.  Even if that slot is also composite, the
                   other facets have already been filtered down.  If the
                   slot is no-inherit, the next most specific class must
                   be examined.
  INPUTS       : 1) The slot descriptor
                 2) The class precedence list
                 3) The bitmap marking which facets were specified in
                    the original slot definition
  RETURNS      : Nothing useful
  SIDE EFFECTS : Composite slot is updated to reflect facets from
                   a less specific class
  NOTES        : Assumes slot is composite
 *************************************************************************/
static void BuildCompositeFacets(
        const Environment&theEnv,
        SlotDescriptor *sd,
        PACKED_CLASS_LINKS *preclist,
        const char *specbits,
        CONSTRAINT_PARSE_RECORD *parsedConstraint) {
    SlotDescriptor *compslot = nullptr;
    unsigned long i;

    for (i = 1; i < preclist->classCount; i++) {
        compslot = FindClassSlot(preclist->classArray[i], sd->slotName->name);
        if ((compslot != nullptr) ? (compslot->noInherit == 0) : false)
            break;
    }
    if (compslot != nullptr) {
        if ((sd->defaultSpecified == 0) && (compslot->defaultSpecified == 1)) {
            sd->dynamicDefault = compslot->dynamicDefault;
            sd->noDefault = compslot->noDefault;
            sd->defaultSpecified = 1;
            if (compslot->defaultValue != nullptr) {
                if (sd->dynamicDefault) {
                    sd->defaultValue = PackExpression(theEnv, (Expression *) compslot->defaultValue);
                    ExpressionInstall(theEnv, (Expression *) sd->defaultValue);
                } else {
                    UDFValue *newValue;
                    UDFValue *oldValue = (UDFValue *) compslot->defaultValue;
                    sd->defaultValue = get_struct(theEnv, UDFValue);
                    GenCopyMemory(UDFValue, 1, sd->defaultValue, oldValue);
                    newValue = (UDFValue *) sd->defaultValue;
                    if (oldValue->header->type == MULTIFIELD_TYPE) {
                        newValue->multifieldValue = CopyMultifield(theEnv, oldValue->multifieldValue);
                    }
                    RetainUDFV(theEnv, newValue);
                }
            }
        }
        if (!TestBitMap(specbits, FIELD_BIT))
            sd->multiple = compslot->multiple;
        if (!TestBitMap(specbits, STORAGE_BIT))
            sd->shared = compslot->shared;
        if (!TestBitMap(specbits, ACCESS_BIT)) {
            sd->noWrite = compslot->noWrite;
            sd->initializeOnly = compslot->initializeOnly;
        }
        if (!TestBitMap(specbits, MATCH_BIT))
            sd->reactive = compslot->reactive;
        if (!TestBitMap(specbits, VISIBILITY_BIT))
            sd->publicVisibility = compslot->publicVisibility;
        if (!TestBitMap(specbits, CREATE_ACCESSOR_BIT)) {
            sd->createReadAccessor = compslot->createReadAccessor;
            sd->createWriteAccessor = compslot->createWriteAccessor;
        }
        if ((!TestBitMap(specbits, OVERRIDE_MSG_BIT)) &&
            compslot->overrideMessageSpecified) {
            ReleaseLexeme(theEnv, sd->overrideMessage);
            sd->overrideMessage = compslot->overrideMessage;
            IncrementLexemeCount(sd->overrideMessage);
            sd->overrideMessageSpecified = true;
        }
        OverlayConstraint(theEnv, parsedConstraint, sd->constraint, compslot->constraint);
    }
}

/***************************************************
  NAME         : CheckForFacetConflicts
  DESCRIPTION  : Determines if all facets specified
                 (and inherited) for a slot are
                 consistent
  INPUTS       : 1) The slot descriptor
                 2) The parse record for the
                    type constraints on the slot
  RETURNS      : True if all OK,
                 false otherwise
  SIDE EFFECTS : Min and Max fields replaced in
                 constraint for single-field slot
  NOTES        : None
 ***************************************************/
static bool CheckForFacetConflicts(
        const Environment&theEnv,
        SlotDescriptor *sd,
        CONSTRAINT_PARSE_RECORD *parsedConstraint) {
    if (sd->multiple == 0) {
        if (parsedConstraint->cardinality) {
            PrintErrorID(theEnv, "CLSLTPSR", 3, true);
            WriteString(theEnv, STDERR, "The 'cardinality' facet can only be used with multifield slots.\n");
            return false;
        } else {
            ReturnExpression(theEnv, sd->constraint->getMinFields());
            ReturnExpression(theEnv, sd->constraint->getMaxFields());
            sd->constraint->setMinFields(GenConstant(theEnv, INTEGER_TYPE, CreateInteger(theEnv, 1LL)));
            sd->constraint->setMaxFields(GenConstant(theEnv, INTEGER_TYPE, CreateInteger(theEnv, 1LL)));
        }
    }
    if (sd->noDefault && sd->noWrite) {
        PrintErrorID(theEnv, "CLSLTPSR", 4, true);
        WriteString(theEnv, STDERR, "Slots with an 'access' facet value of 'read-only' must have a default value.\n");
        return false;
    }
    if (sd->noWrite && (sd->createWriteAccessor || sd->overrideMessageSpecified)) {
        PrintErrorID(theEnv, "CLSLTPSR", 5, true);
        WriteString(theEnv, STDERR, "Slots with an 'access' facet value of 'read-only' cannot have a write accessor.\n");
        return false;
    }
    if (sd->noInherit && sd->publicVisibility) {
        PrintErrorID(theEnv, "CLSLTPSR", 6, true);
        WriteString(theEnv, STDERR,
                    "Slots with a 'propagation' facet value of 'no-inherit' cannot have a 'visibility' facet value of 'public'.\n");
        return false;
    }
    return true;
}

/********************************************************************
  NAME         : EvaluateSlotDefaultValue
  DESCRIPTION  : Checks the default value against the slot
                 constraints and evaluates static default values
  INPUTS       : 1) The slot descriptor
                 2) The bitmap marking which facets were specified in
                    the original slot definition
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : Static default value expressions deleted and
                 replaced with data object evaluation
  NOTES        : On errors, slot is marked as dynamix so that
                 DeleteSlots() will erase the slot expression
 ********************************************************************/
static bool EvaluateSlotDefaultValue(
        const Environment&theEnv,
        SlotDescriptor *sd,
        const char *specbits) {
    UDFValue temp;
    bool oldce, olddcc, vPass;
    ConstraintViolationType vCode;

    /* ===================================================================
       Slot default value expression is marked as dynamic until now so
       that DeleteSlots() would erase in the event of an error.  The delay
       was so that the evaluation of a static default value could be
       delayed until all the constraints were parsed.
       =================================================================== */
    if (!TestBitMap(specbits, DEFAULT_DYNAMIC_BIT))
        sd->dynamicDefault = 0;

    if (sd->noDefault)
        return true;

    if (sd->dynamicDefault == 0) {
        if (TestBitMap(specbits, DEFAULT_BIT)) {
            oldce = ExecutingConstruct(theEnv);
            SetExecutingConstruct(theEnv, true);
            olddcc = SetDynamicConstraintChecking(theEnv, true);
            vPass = EvaluateAndStoreInDataObject(theEnv, sd->multiple,
                                                 (Expression *) sd->defaultValue, &temp, true);
            if (vPass)
                vPass = (ValidSlotValue(theEnv, &temp, sd, nullptr, "the 'default' facet") == PSE_NO_ERROR);
            SetDynamicConstraintChecking(theEnv, olddcc);
            SetExecutingConstruct(theEnv, oldce);
            if (vPass) {
                UDFValue *newValue;
                ExpressionDeinstall(theEnv, (Expression *) sd->defaultValue);
                ReturnPackedExpression(theEnv, (Expression *) sd->defaultValue);
                sd->defaultValue = get_struct(theEnv, UDFValue);
                newValue = (UDFValue *) sd->defaultValue;
                GenCopyMemory(UDFValue, 1, sd->defaultValue, &temp);
                if (temp.header->type == MULTIFIELD_TYPE) { newValue->multifieldValue = CopyMultifield(theEnv, temp.multifieldValue); }
                RetainUDFV(theEnv, (UDFValue *) sd->defaultValue);
            } else {
                sd->dynamicDefault = 1;
                return false;
            }
        } else if (sd->defaultSpecified == 0) {
            sd->defaultValue = get_struct(theEnv, UDFValue);
            DeriveDefaultFromConstraints(theEnv, sd->constraint,
                                         (UDFValue *) sd->defaultValue, sd->multiple, false);
            RetainUDFV(theEnv, (UDFValue *) sd->defaultValue);
        }
    } else {
        vCode = ConstraintCheckExpressionChain(theEnv, (Expression *) sd->defaultValue, sd->constraint);
        if (vCode != NO_VIOLATION) {
            PrintErrorID(theEnv, "CSTRNCHK", 1, false);
            WriteString(theEnv, STDERR, "Expression for ");
            PrintSlot(theEnv, STDERR, sd, nullptr, "dynamic default value");
            ConstraintViolationErrorMessage(theEnv, nullptr, nullptr, 0, 0, nullptr, 0,
                                            vCode, sd->constraint, false);
            return false;
        }
    }
    return true;
}

