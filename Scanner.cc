#include "Environment.h"
#include "Token.h"
#include "CommandLine.h"

namespace maya {
    constexpr auto isWhiteSpace(char value) noexcept {
        return (value == ' ') || (value == '\n') || (value == '\f') ||
               (value == '\r') || (value == ';') || (value == '\t');
    }
    static_assert(isWhiteSpace(' '));
    static_assert(isWhiteSpace('\n'));
    static_assert(isWhiteSpace('\f'));
    static_assert(isWhiteSpace('\r'));
    static_assert(isWhiteSpace('\t'));
    static_assert(isWhiteSpace(';'));
    static_assert(!isWhiteSpace('a'));

    constexpr auto isCommentCharacter(char value) noexcept { return value == ';'; }
    static_assert(isCommentCharacter(';'));
    static_assert(!isCommentCharacter('a'));
    constexpr auto isDefglobalSymbol(char input) noexcept {
        return input == '*';
    }
    bool startsVariableToken(char input) noexcept {
        return isalpha(input) || IsUTF8MultiByteStart(input) || isDefglobalSymbol(input);
    }
    Token
    Environment::scanQuestionMarkVariable(const std::string &logicalName) {
        auto inchar = readRouter(logicalName);
        if (startsVariableToken(inchar)) {
            unreadRouter(logicalName, inchar);
            auto targetToken = scanSymbol(logicalName, 0);
            targetToken.setType(Token::Type::SFVariable);
            // defglobal support
            if (std::holds_alternative<Lexeme::Ptr>(targetToken.getContents())) {
                auto lexemeValue = std::get<Lexeme::Ptr>(targetToken.getContents());
                std::stringstream gen;
                gen << "?" << lexemeValue->getContents();
                auto printString = gen.str();
                targetToken.setPrintForm(printString);
                if (auto theContents = lexemeValue->getContents(); theContents.length() > 1 && theContents.front() == '*' && theContents.back() == '*') {
                    targetToken.setType(Token::Type::GlobalVariable);
                    // eliminate the * at front and back and commit that as the underlying token contents
                    targetToken.setContents(createSymbol(theContents.substr(1, theContents.length() - 1)));
                }
                return targetToken;
            } else {
                /// @todo flesh this out
                throw "Illegal state entered, got a symbol back which was not a symbol!";
            }
        } else {
            unreadRouter(logicalName, inchar);
            return { Token::Type::SFWildcard, "?", createSymbol("?") };
        }
    }
    Token
    Environment::getToken(const std::string& logicalName) {
        Token targetToken;
        auto inchar = readRouter(logicalName);
        // remove all whitespace before processing the actual request
        while (isWhiteSpace(inchar)) {
            // remove comment lines
            if (isCommentCharacter(inchar)) {
                inchar = readRouter(logicalName);
                while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF)) {
                    inchar = readRouter(logicalName);
                }
            }
            inchar = readRouter(logicalName);
        }
        // process symbolic tokens
        if (isalpha(inchar) || IsUTF8MultiByteStart(inchar)) {
            unreadRouter(logicalName, inchar);
            targetToken = scanSymbol(logicalName, 0);
        } else if (isdigit(inchar)) {
            // process number tokens beginning with a digit
            unreadRouter(logicalName, inchar);
            targetToken = scanNumber(logicalName);
        } else {
            switch (inchar) {
                // process string tokens
                case '"':
                    targetToken = scanString(logicalName);
                    break;
                    // process tokens that might be numbers
                case '-':
                case '.':
                case '+':
                    unreadRouter(logicalName, inchar);
                    targetToken = scanNumber(logicalName);
                    break;
                    // process ? and ?<variable> tokens
                case '?':
                    targetToken = scanQuestionMarkVariable(logicalName);
                    break;
                case '$':
                    targetToken = scanDollarQuestionMarkVariable(logicalName);
                    break;
                    // symbols beginning with '<'
                case '<':
                    _globalStream << "<";
                    targetToken = scanSymbol(logicalName, 1);
                    break;
                    // process "(", ")", "~", "|" and "&" tokens
                case '(':
                    targetToken = { Token::Type::LeftParen, "(", createString("(") };
                    break;
                case ')':
                    targetToken = { Token::Type::RightParen, ")", createString(")") };
                    break;
                case '~':
                    targetToken = { Token::Type::NotConstraint, "~", createString("~") };
                    break;
                case '|':
                    targetToken = { Token::Type::OrConstraint, "|", createString("|") };
                    break;
                case '&':
                    targetToken = { Token::Type::AndConstraint, "&", createString("&") };
                    break;
                    // process end-of-file token
                case EOF:
                case 0:
                case 3:
                    targetToken = {Token::Type::Stop, "", createSymbol("stop") };
                    break;
                    //process other tokens
                default: {
                    if (isprint(inchar)) {
                        unreadRouter(logicalName, inchar);
                        targetToken = scanSymbol(logicalName, 0);
                    } else {
                        targetToken.setPrintForm("<<<unprintable character>>>");
                    }
                    break;
                }
            }
        }
        // put the new token into the pretty print buffer :)
        if (targetToken.getType() == Token::Type::InstanceName) {
            saveMultipleToPrettyPrintBuffer("[", targetToken.getPrintForm(), "]");
        } else {
            saveToPrettyPrintBuffer(targetToken.getPrintForm());
        }
        // clear the global stream
        _globalStream.str("");
        return targetToken;
    }
    Token
    Environment::scanDollarQuestionMarkVariable(const std::string &logicalName) {
        if (auto inchar = readRouter(logicalName); inchar == '?') {
            inchar = readRouter(logicalName);
            if (startsVariableToken(inchar)) {
                unreadRouter(logicalName, inchar);
                if (auto targetToken = scanSymbol(logicalName, 0); std::holds_alternative<Lexeme::Ptr>(targetToken.getContents())) {
                    targetToken.setType(Token::Type::MFVariable);
                    const auto& lexemeValue = std::get<Lexeme::Ptr>(targetToken.getContents());
                    std::stringstream gen;
                    gen << "$?" << lexemeValue->getContents();
                    auto str = gen.str();
                    targetToken.setPrintForm(str);
                    if (auto theContents = lexemeValue->getContents(); theContents.length() > 1 && theContents.front() == '*' && theContents.back() == '*') {
                        targetToken.setType(Token::Type::MFGlobalVariable);
                        // remove the starting and ending *
                        targetToken.setContents(createSymbol(theContents.substr(1, theContents.length() - 1)));
                    }
                    return targetToken;
                } else {
                    throw "target token did not hold expected type!" ;
                }
            } else {
                unreadRouter(logicalName, inchar);
                return {Token::Type::MFWildcard, "$?", createSymbol("$?")};
            }
        } else {
            _globalStream << "$";
            unreadRouter(logicalName, inchar);
            return scanSymbol(logicalName, 1) ;
        }
    }
    constexpr bool isReservedCharacter(char c) noexcept {
        switch(c) {
            case '<':
            case '"':
            case '(':
            case ')':
            case '&':
            case '|':
            case '~':
            case ' ':
            case ';':
                return true;
            default:
                return false;
        }
    }
    static_assert(isReservedCharacter('<'));
    static_assert(isReservedCharacter('"'));
    static_assert(isReservedCharacter('('));
    static_assert(isReservedCharacter(')'));
    static_assert(isReservedCharacter('&'));
    static_assert(isReservedCharacter('|'));
    static_assert(isReservedCharacter('~'));
    static_assert(isReservedCharacter(' '));
    static_assert(isReservedCharacter(';'));
    static_assert(!isReservedCharacter('a'));
    bool hasInstanceMarkers(const std::string& str) noexcept {
        return (!str.empty()) && (str.length() > 2) && (str.at(0) == '[') && (str.at(str.length() - 1) == ']');
    }
    std::string stripOffInstanceMarkers(const std::string& str) noexcept {
        if (hasInstanceMarkers(str)) {
            return str.substr(1, str.length() - 2);
        } else {
            return str;
        }
    }
    Token
    Environment::scanSymbol(const std::string& logicalName, int count) {
        // scan characters and add them to the symbol until a delimiter is found.
        auto inchar = readRouter(logicalName);
        while ((!isReservedCharacter(inchar)) && (IsUTF8MultiByteStart(inchar) || IsUTF8MultiByteContinuation(inchar) || isprint(inchar))) {
            _globalStream << inchar;
            ++count;
            inchar = readRouter(logicalName);
        }
        // return the last character scanned (the delimiter) to the input stream so it will scanned as part of the next token
        unreadRouter(logicalName, inchar);

        // add the symbol to the symbol table and return the symbol table address of the symbol.
        // Symbols of the form [<symbol>] are instance names, so the type returned is InstanceName rather than Symbol
        auto str = _globalStream.str();
        if (auto str = _globalStream.str(); count > 2) {
            if (hasInstanceMarkers(str)) {
                auto actualInstanceName = stripOffInstanceMarkers(str);
                auto ptr = createInstanceName(actualInstanceName);
                return {Token::Type::InstanceName, actualInstanceName, ptr};
            } else {
                return {Token::Type::Symbol, str, createSymbol(str)};
            }
        } else {
            return {Token::Type::Symbol, str, createSymbol(str)};
        }
    }
    constexpr bool isExponentMark(int c) noexcept {
        return (c == 'E') || (c == 'e');
    }
    constexpr bool isSign(int c) noexcept {
        return (c == '-') || (c == '+');
    }
    constexpr bool isDot(int c) noexcept {
        return c == '.';
    }
    Token
    Environment::scanNumber(const std::string& logicalName) {
        enum class ScanNumberPhase {
            Sign, // -1
            Integral, // 0
            Decimal, // 1
            ExponentBegin, // 2
            ExponentValue, // 3
            Done, // 5
            Error // 9
        };
        size_t count = 0;
        ScanNumberPhase currentPhase = ScanNumberPhase::Sign;
        auto inChar = readRouter(logicalName);
        decltype(inChar) previousChar = 0;
        bool digitFound = false;
        bool processFloat = false;
        while ((currentPhase != ScanNumberPhase::Done) && (currentPhase != ScanNumberPhase::Error)) {
            if (currentPhase == ScanNumberPhase::Sign) {
                if (isdigit(inChar)) {
                    currentPhase = ScanNumberPhase::Integral;
                    digitFound = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                } else if (isSign(inChar)) {
                    currentPhase = ScanNumberPhase::Integral;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                } else if (isDot(inChar)) {
                    processFloat = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::Decimal;
                } else if (isExponentMark(inChar)) {
                    processFloat = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::ExponentBegin;
                } else if (isReservedCharacter(inChar) || (isprint(inChar) == 0)) {
                    currentPhase = ScanNumberPhase::Done;
                } else {
                    currentPhase = ScanNumberPhase::Error;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                }
            } else if (currentPhase == ScanNumberPhase::Integral) {
                if (isdigit(inChar)) {
                    digitFound = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                } else if (isDot(inChar)) {
                    processFloat = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::Decimal;
                } else if (isExponentMark(inChar)) {
                    processFloat = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::ExponentBegin;
                } else if (isReservedCharacter(inChar) || (!IsUTF8MultiByteStart(inChar) && (isprint(inChar) == 0)) ) {
                    currentPhase = ScanNumberPhase::Done;
                } else {
                    currentPhase = ScanNumberPhase::Error;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                }
            } else if (currentPhase == ScanNumberPhase::Decimal) {
                if (isdigit(inChar)) {
                    digitFound = true;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                } else if (isExponentMark(inChar)) {
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::ExponentBegin;
                } else if (isReservedCharacter(inChar) || (!IsUTF8MultiByteStart(inChar) && (isprint(inChar) == 0)) ) {
                    currentPhase = ScanNumberPhase::Done;
                } else {
                    currentPhase = ScanNumberPhase::Error;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                }
            } else if (currentPhase == ScanNumberPhase::ExponentBegin) {
                if (isdigit(inChar)) {
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::ExponentValue;
                } else if (isSign(inChar)) {
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                    currentPhase = ScanNumberPhase::ExponentValue;
                } else if (isReservedCharacter(inChar) || (!IsUTF8MultiByteStart(inChar) && (isprint(inChar) == 0))) {
                    currentPhase = ScanNumberPhase::Done;
                    digitFound = false;
                } else {
                    currentPhase = ScanNumberPhase::Error;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                }
            } else if (currentPhase == ScanNumberPhase::ExponentValue) {
                if (isdigit(inChar)) {
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                } else if (isReservedCharacter(inChar) || (!IsUTF8MultiByteStart(inChar) && (isprint(inChar) == 0))) {
                    if (isSign(previousChar)) {
                        digitFound = false;
                    }
                    currentPhase = ScanNumberPhase::Done;
                } else {
                    currentPhase = ScanNumberPhase::Error;
                    _globalStream << static_cast<char>(inChar);
                    ++count;
                }
            }
            if (currentPhase != ScanNumberPhase::Done || currentPhase != ScanNumberPhase::Error) {
                previousChar = inChar;
                inChar = readRouter(logicalName);
            }
        }
        if (currentPhase == ScanNumberPhase::Error) {
            return scanSymbol(logicalName, count);
        }

        // stuff last character back into buffer and return the number
        unreadRouter(logicalName, inChar);

        if (!digitFound) {
            auto str = _globalStream.str();
            auto target = createSymbol(str);
            return Token(Token::Type::Symbol, target->getContents(), target);
        }
        std::stringstream printFormGenerator;
        if (auto str = _globalStream.str(); processFloat) {
            auto fvalue = std::stod(str);
            auto target = createFloat(fvalue);
            /// @todo implement support for FloatToString as clips defines it
            printFormGenerator << fvalue;
            auto printForm = printFormGenerator.str();
            return Token(Token::Type::Float, printForm, target);
        } else {
            int64_t ivalue = std::stoll(str);
            auto target = createInteger(ivalue);
            /// @todo implement support for LongIntegerToString as clips defines it
            printFormGenerator << ivalue;
            auto printForm = printFormGenerator.str();
            return Token(Token::Type::Integer, printForm, target);
        }
    }

    constexpr bool isEscapeCharacter(char c) noexcept {
        return c == '\\' ;
    }
    static_assert(isEscapeCharacter('\\'));
    static_assert(!isEscapeCharacter('a'));
    constexpr bool isDoubleQuoteCharacter(char c) noexcept {
        return c == '"';
    }
    static_assert(isDoubleQuoteCharacter('"'));
    static_assert(!isDoubleQuoteCharacter('a'));
    constexpr bool isEOFSymbol(char c) noexcept {
        return c == EOF;
    }
    static_assert(isEOFSymbol(EOF));
    static_assert(!isEOFSymbol('a'));
    Token
    Environment::scanString(const std::string& logicalName) {
        auto inchar = readRouter(logicalName);
        // scan characters and add them to the string until the " delimiter is found.
        while (!isDoubleQuoteCharacter(inchar) && !isEOFSymbol(inchar)) {
            if (isEscapeCharacter(inchar)) {
                inchar = readRouter(logicalName);
            }
            _globalStream << inchar;
            inchar = readRouter(logicalName);
        }
        if (isEOFSymbol(inchar) && !_ignoreCompletionErrors) {
            printErrorID("SCANNER", 1, true);
            writeStringRouter(STDERR(), "Encountered End-Of-File while scanning a string\n");
        }
        // make a string out of this
        auto str = _globalStream.str();
        auto lex = createString(str.empty() ? "" : str);
        return {Token::Type::String, stringPrintForm(lex->getContents()), lex };
    }
    std::string
    Environment::stringPrintForm(const std::string &str) {
        std::stringstream container;
        /// @todo figure out if there is a better c++ way to do this
        for (const auto& c : str) {
            if (c == '"' || c == '\\')  {
                container << '\\';
            }
            container << c;
        }
        auto out = container.str();
        return out;
    }

} // end namespace maya
