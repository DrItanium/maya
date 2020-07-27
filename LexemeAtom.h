//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_LEXEMEATOM_H
#define MAYA_LEXEMEATOM_H
#include "Atom.h"
#include "Constants.h"
#include "TransferEvaluable.h"
#include <memory>
#include <string>
namespace maya {
    class TreatAsString {
    };

    class TreatAsSymbol {
    };

    class TreatAsInstanceName {
    };

    /**
     * @brief Represents a symbol, string, instance name, or any other string based type
     */
    class Lexeme : public EphemeralAtom, public TransferEvaluable<Lexeme> {
    public:
        using Self = Lexeme;
        using Ptr = std::shared_ptr<Self>;
    public:
        Lexeme(Environment &parent, TreatAsSymbol) : EphemeralAtom(parent, SYMBOL_TYPE) {}
        Lexeme(Environment &parent, TreatAsString) : EphemeralAtom(parent, STRING_TYPE) {}
        Lexeme(Environment &parent, TreatAsInstanceName) : EphemeralAtom(parent, INSTANCE_NAME_TYPE) {}
        Lexeme(Environment &parent, const std::string &contents, TreatAsSymbol) : EphemeralAtom(parent, SYMBOL_TYPE),
                                                                                        _contents(contents) {}
        Lexeme(Environment &parent, const std::string &contents, TreatAsString) : EphemeralAtom(parent, STRING_TYPE),
                                                                                        _contents(contents) {}
        Lexeme(Environment &parent, const std::string &contents, TreatAsInstanceName) : EphemeralAtom(parent,
                                                                                                            INSTANCE_NAME_TYPE),
                                                                                              _contents(contents) {}
        Lexeme(Environment &parent, const std::string &contents, unsigned short type) : EphemeralAtom(parent, type),
                                                                                              _contents(contents) {}
        std::string getContents() const noexcept { return _contents; }
        ~Lexeme() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string &logicalName) override;
    private:
        std::string _contents;
    };

}
std::ostream& operator<<(std::ostream& os, const maya::Lexeme& lexeme);
#endif //MAYA_LEXEMEATOM_H
