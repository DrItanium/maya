/**
 * @brief Symbol related operations
 */
#ifndef _H_symbol

#pragma once

#define _H_symbol
#include <map>
#include <memory>
#include "Entities.hxx"

namespace maya {

    class SymbolMatch {
    public:
        using Self = SymbolMatch;
        using Ptr = std::shared_ptr<Self>;
    public:
        SymbolMatch(Lexeme::Ptr match) : _match(match) { }
        Lexeme::Ptr getMatch() const noexcept { return _match; }
    private:
        Lexeme::Ptr _match;
    };
#if STUBBING_INACTIVE

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

CLIPSLexeme *FindSymbolHN(const Environment::Ptr&, const char *, unsigned short);
CLIPSExternalAddress::Ptr CreateExternalAddress(const Environment::Ptr& theEnv, void * ctx, unsigned short kind);
struct symbolMatch *FindSymbolMatches(const Environment::Ptr&, const char *, unsigned *, size_t *);
CLIPSLexeme *GetNextSymbolMatch(const Environment::Ptr&, const char *, size_t, CLIPSLexeme *, bool, size_t *);
void ClearBitString(void *, size_t);
void SetAtomicValueIndices(const Environment::Ptr&, bool);
void RestoreAtomicValueBuckets(const Environment::Ptr&);
void EphemerateValue(const Environment::Ptr&, void *);
bool BitStringHasBitsSet(void *, unsigned);

void MarkNeededAtomicValues(Environment);
void WriteNeededAtomicValues(const Environment::Ptr&, FILE *);
void ReadNeededAtomicValues(const Environment::Ptr&);
void InitAtomicValueNeededFlags(const Environment::Ptr&);
void FreeAtomicValueStorage(const Environment::Ptr&);
void WriteNeededSymbols(const Environment::Ptr&, FILE *);
void WriteNeededFloats(const Environment::Ptr&, FILE *);
void WriteNeededIntegers(const Environment::Ptr&, FILE *);
void ReadNeededSymbols(const Environment::Ptr&);
void ReadNeededFloats(const Environment::Ptr&);
void ReadNeededIntegers(const Environment::Ptr&);
#endif
}
#endif /* _H_symbol */



