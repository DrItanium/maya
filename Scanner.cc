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
    Token
    Environment::getToken(const std::string& logicalName) {
        Token targetToken;
        _globalPos = 0;
        _globalMax = 0;
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
            }
        }
        return targetToken;
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
        std::stringstream ss; /// @todo replace this with an instance stringstream
        // scan characters and add them to the symbol until a delimiter is found.
        auto inchar = readRouter(logicalName);
        while ((!isReservedCharacter(inchar)) && (IsUTF8MultiByteStart(inchar) || IsUTF8MultiByteContinuation(inchar) || isprint(inchar))) {
            ss << inchar;
            ++count;
            inchar = readRouter(logicalName);
        }
        // return the last character scanned (the delimiter) to the input stream so it will scanned as part of the next token
        unreadRouter(logicalName, inchar);

        // add the symbol to the symbol table and return the symbol table address of the symbol.
        // Symbols of the form [<symbol>] are instance names, so the type returned is InstanceName rather than Symbol
        auto str = ss.str();
        if (auto str = ss.str(); count > 2) {
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
#if 0
    void GetToken( const Environment::Ptr&theEnv, const char *logicalName, struct token *theToken) {
    } else
        switch (inchar) {
            /*========================*/
            /* Process String Tokens. */
            /*========================*/

            case '"':
                theToken->lexemeValue = ScanString(theEnv, logicalName);
                theToken->tknType = STRING_TOKEN;
                theToken->printForm = StringPrintForm(theEnv, theToken->lexemeValue->contents);
                break;

                /*=======================================*/
                /* Process Tokens that might be numbers. */
                /*=======================================*/

            case '-':
            case '.':
            case '+':
                UnreadRouter(theEnv, logicalName, inchar);
                ScanNumber(theEnv, logicalName, theToken);
                break;

                /*===================================*/
                /* Process ? and ?<variable> Tokens. */
                /*===================================*/

            case '?':
                inchar = ReadRouter(theEnv, logicalName);
                if (isalpha(inchar) || IsUTF8MultiByteStart(inchar)
                    #if DEFGLOBAL_CONSTRUCT
                    || (inchar == '*'))
#else
                    )
#endif
                {
                    UnreadRouter(theEnv, logicalName, inchar);
                    theToken->lexemeValue = ScanSymbol(theEnv, logicalName, 0, &type);
                    theToken->tknType = SF_VARIABLE_TOKEN;
#if DEFGLOBAL_CONSTRUCT
                    if ((theToken->lexemeValue->contents[0] == '*') &&
                        ((strlen(theToken->lexemeValue->contents)) > 1) &&
                        (theToken->lexemeValue->contents[strlen(theToken->lexemeValue->contents) - 1] == '*')) {
                        size_t count;

                        theToken->tknType = GBL_VARIABLE_TOKEN;
                        theToken->printForm = AppendStrings(theEnv, "?", theToken->lexemeValue->contents);
                        count = strlen(ScannerData(theEnv)->GlobalString);
                        ScannerData(theEnv)->GlobalString[count - 1] = EOS;
                        theToken->lexemeValue = CreateSymbol(theEnv, ScannerData(theEnv)->GlobalString + 1);
                        ScannerData(theEnv)->GlobalString[count - 1] = (char) inchar;

                    } else
#endif
                        theToken->printForm = AppendStrings(theEnv, "?", theToken->lexemeValue->contents);
                } else {
                    theToken->tknType = SF_WILDCARD_TOKEN;
                    theToken->lexemeValue = CreateSymbol(theEnv, "?");
                    UnreadRouter(theEnv, logicalName, inchar);
                    theToken->printForm = "?";
                }
                break;

                /*=====================================*/
                /* Process $? and $?<variable> Tokens. */
                /*=====================================*/

            case '$':
                if ((inchar = ReadRouter(theEnv, logicalName)) == '?') {
                    inchar = ReadRouter(theEnv, logicalName);
                    if (isalpha(inchar) || IsUTF8MultiByteStart(inchar)
                        #if DEFGLOBAL_CONSTRUCT
                        || (inchar == '*'))
#else
                        )
#endif
                    {
                        UnreadRouter(theEnv, logicalName, inchar);
                        theToken->lexemeValue = ScanSymbol(theEnv, logicalName, 0, &type);
                        theToken->tknType = MF_VARIABLE_TOKEN;
#if DEFGLOBAL_CONSTRUCT
                        if ((theToken->lexemeValue->contents[0] == '*') &&
                            (strlen(theToken->lexemeValue->contents) > 1) &&
                            (theToken->lexemeValue->contents[strlen(theToken->lexemeValue->contents) - 1] == '*')) {
                            size_t count;

                            theToken->tknType = MF_GBL_VARIABLE_TOKEN;
                            theToken->printForm = AppendStrings(theEnv, "$?", theToken->lexemeValue->contents);
                            count = strlen(ScannerData(theEnv)->GlobalString);
                            ScannerData(theEnv)->GlobalString[count - 1] = EOS;
                            theToken->lexemeValue = CreateSymbol(theEnv, ScannerData(theEnv)->GlobalString + 1);
                            ScannerData(theEnv)->GlobalString[count - 1] = (char) inchar;
                        } else
#endif
                            theToken->printForm = AppendStrings(theEnv, "$?", theToken->lexemeValue->contents);
                    } else {
                        theToken->tknType = MF_WILDCARD_TOKEN;
                        theToken->lexemeValue = CreateSymbol(theEnv, "$?");
                        theToken->printForm = "$?";
                        UnreadRouter(theEnv, logicalName, inchar);
                    }
                } else {
                    theToken->tknType = SYMBOL_TOKEN;
                    ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, '$', ScannerData(theEnv)->GlobalString,
                                                                             &ScannerData(theEnv)->GlobalPos,
                                                                             &ScannerData(theEnv)->GlobalMax,
                                                                             ScannerData(theEnv)->GlobalMax + 80);
                    UnreadRouter(theEnv, logicalName, inchar);
                    theToken->lexemeValue = ScanSymbol(theEnv, logicalName, 1, &type);
                    theToken->printForm = theToken->lexemeValue->contents;
                }
                break;

                /*============================*/
                /* Symbols beginning with '<' */
                /*============================*/

            case '<':
                theToken->tknType = SYMBOL_TOKEN;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, '<', ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                theToken->lexemeValue = ScanSymbol(theEnv, logicalName, 1, &type);
                theToken->printForm = theToken->lexemeValue->contents;
                break;

                /*=============================================*/
                /* Process "(", ")", "~", "|", and "&" Tokens. */
                /*=============================================*/

            case '(':
                theToken->tknType = LEFT_PARENTHESIS_TOKEN;
                theToken->lexemeValue = CreateString(theEnv, "(");
                theToken->printForm = "(";
                break;

            case ')':
                theToken->tknType = RIGHT_PARENTHESIS_TOKEN;
                theToken->lexemeValue = CreateString(theEnv, ")");
                theToken->printForm = ")";
                break;

            case '~':
                theToken->tknType = NOT_CONSTRAINT_TOKEN;
                theToken->lexemeValue = CreateString(theEnv, "~");
                theToken->printForm = "~";
                break;

            case '|':
                theToken->tknType = OR_CONSTRAINT_TOKEN;
                theToken->lexemeValue = CreateString(theEnv, "|");
                theToken->printForm = "|";
                break;

            case '&':
                theToken->tknType = AND_CONSTRAINT_TOKEN;
                theToken->lexemeValue = CreateString(theEnv, "&");
                theToken->printForm = "&";
                break;

                /*============================*/
                /* Process End-of-File Token. */
                /*============================*/

            case EOF:
            case 0:
            case 3:
                theToken->tknType = STOP_TOKEN;
                theToken->lexemeValue = CreateSymbol(theEnv, "stop");
                theToken->printForm = "";
                break;

                /*=======================*/
                /* Process Other Tokens. */
                /*=======================*/

            default:
                if (isprint(inchar)) {
                    UnreadRouter(theEnv, logicalName, inchar);
                    theToken->lexemeValue = ScanSymbol(theEnv, logicalName, 0, &type);
                    theToken->tknType = type;
                    theToken->printForm = theToken->lexemeValue->contents;
                } else { theToken->printForm = "<<<unprintable character>>>"; }
                break;
        }

    /*===============================================*/
    /* Put the new token in the pretty print buffer. */
    /*===============================================*/

    if (theToken->tknType == INSTANCE_NAME_TOKEN) {
        SavePPBuffer(theEnv, "[");
        SavePPBuffer(theEnv, theToken->printForm);
        SavePPBuffer(theEnv, "]");
    } else { SavePPBuffer(theEnv, theToken->printForm); }

    /*=========================================================*/
    /* Return the temporary memory used in scanning the token. */
    /*=========================================================*/

    if (ScannerData(theEnv)->GlobalString != nullptr) {
        rm(theEnv, ScannerData(theEnv)->GlobalString, ScannerData(theEnv)->GlobalMax);
        ScannerData(theEnv)->GlobalString = nullptr;
        ScannerData(theEnv)->GlobalMax = 0;
        ScannerData(theEnv)->GlobalPos = 0;
    }

    return;
}
#endif
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
    if (processFloat) {
       double fvalue = 0.0;
       _globalStream >> fvalue;
       auto target = createFloat(fvalue);
       printFormGenerator << fvalue;
       auto printForm = printFormGenerator.str();
       return Token(Token::Type::Float, printForm, target);
    } else {
        int64_t ivalue = 0;
        _globalStream >> ivalue;
        auto target = createInteger(ivalue);
        printFormGenerator << ivalue;
        auto printForm = printFormGenerator.str();
        return Token(Token::Type::Integer, printForm, target);
    }
}

#if 0
/**************************************/
/* ScanNumber: Scans a numeric token. */
/**************************************/
static void ScanNumber(const Environment::Ptr&theEnv, const char *logicalName, struct token *theToken) {
    int count = 0; int inchar, phase;
    bool digitFound = false; bool processFloat = false;
    double fvalue; long long lvalue; TokenType type;

    inchar = ReadRouter(theEnv, logicalName);
    phase = -1;

    while ((phase != 5) && (phase != 9)) {
        if (phase == -1) {
            if (isdigit(inchar)) {
                phase = 0;
                digitFound = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            } else if ((inchar == '+') || (inchar == '-')) {
                phase = 0;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            } else if (inchar == '.') {
                processFloat = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 1;
            } else if ((inchar == 'E') || (inchar == 'e')) {
                processFloat = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 2;
            } else if ((inchar == '<') || (inchar == '"') ||
                       (inchar == '(') || (inchar == ')') ||
                       (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                       (inchar == ' ') || (inchar == ';') ||
                       (isprint(inchar) == 0)) { phase = 5; }
            else {
                phase = 9;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            }
        } else if (phase == 0) {
            if (isdigit(inchar)) {
                digitFound = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            } else if (inchar == '.') {
                processFloat = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 1;
            } else if ((inchar == 'E') || (inchar == 'e')) {
                processFloat = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 2;
            } else if ((inchar == '<') || (inchar == '"') ||
                       (inchar == '(') || (inchar == ')') ||
                       (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                       (inchar == ' ') || (inchar == ';') ||
                       ((!IsUTF8MultiByteStart(inchar) && (isprint(inchar) == 0)))) { phase = 5; }
            else {
                phase = 9;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            }
        } else if (phase == 1) {
            if (isdigit(inchar)) {
                digitFound = true;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            } else if ((inchar == 'E') || (inchar == 'e')) {
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 2;
            } else if ((inchar == '<') || (inchar == '"') ||
                       (inchar == '(') || (inchar == ')') ||
                       (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                       (inchar == ' ') || (inchar == ';') ||
                       ((!IsUTF8MultiByteStart(inchar)) && (isprint(inchar) == 0))) { phase = 5; }
            else {
                phase = 9;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            }
        } else if (phase == 2) {
            if (isdigit(inchar)) {
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 3;
            } else if ((inchar == '+') || (inchar == '-')) {
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
                phase = 3;
            } else if ((inchar == '<') || (inchar == '"') ||
                       (inchar == '(') || (inchar == ')') ||
                       (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                       (inchar == ' ') || (inchar == ';') ||
                       ((!IsUTF8MultiByteStart(inchar)) && (isprint(inchar) == 0))) {
                digitFound = false;
                phase = 5;
            } else {
                phase = 9;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            }
        } else if (phase == 3) {
            if (isdigit(inchar)) {
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            } else if ((inchar == '<') || (inchar == '"') ||
                       (inchar == '(') || (inchar == ')') ||
                       (inchar == '&') || (inchar == '|') || (inchar == '~') ||
                       (inchar == ' ') || (inchar == ';') ||
                       ((!IsUTF8MultiByteStart(inchar)) && (isprint(inchar) == 0))) {
                if ((ScannerData(theEnv)->GlobalString[count - 1] == '+') ||
                    (ScannerData(theEnv)->GlobalString[count - 1] == '-')) { digitFound = false; }
                phase = 5;
            } else {
                phase = 9;
                ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                         &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                         ScannerData(theEnv)->GlobalMax + 80);
                count++;
            }
        }

        if ((phase != 5) && (phase != 9)) { inchar = ReadRouter(theEnv, logicalName); }
    }

    if (phase == 9) {
        theToken->lexemeValue = ScanSymbol(theEnv, logicalName, count, &type);
        theToken->tknType = type;
        theToken->printForm = theToken->lexemeValue->contents;
        return;
    }

    /*=======================================*/
    /* Stuff last character back into buffer */
    /* and return the number.                */
    /*=======================================*/

    UnreadRouter(theEnv, logicalName, inchar);

    if (!digitFound) {
        theToken->tknType = SYMBOL_TOKEN;
        theToken->lexemeValue = CreateSymbol(theEnv, ScannerData(theEnv)->GlobalString);
        theToken->printForm = theToken->lexemeValue->contents;
        return;
    }

    if (processFloat) {
        fvalue = atof(ScannerData(theEnv)->GlobalString);
        theToken->tknType = FLOAT_TOKEN;
        theToken->floatValue = CreateFloat(theEnv, fvalue);
        theToken->printForm = FloatToString(theEnv, theToken->floatValue->contents);
    } else {
        errno = 0;
#if WIN_MVC
        lvalue = _strtoi64(ScannerData(theEnv)->GlobalString,nullptr,10);
#else
        lvalue = strtoll(ScannerData(theEnv)->GlobalString, nullptr, 10);
#endif
        if (errno) {
            PrintWarningID(theEnv, "SCANNER", 1, false);
            WriteString(theEnv, STDWRN, "Over or underflow of long long integer.\n");
        }
        theToken->tknType = INTEGER_TOKEN;
        theToken->integerValue = CreateInteger(theEnv, lvalue);
        theToken->printForm = LongIntegerToString(theEnv, theToken->integerValue->contents);
    }

    return;
    }
#endif
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
        std::stringstream theString;
        auto inchar = readRouter(logicalName);
        // scan characters and add them to the string until the " delimiter is found.
        while (!isDoubleQuoteCharacter(inchar) && !isEOFSymbol(inchar)) {
            if (isEscapeCharacter(inchar)) {
                inchar = readRouter(logicalName);
            }
            theString << inchar;
            inchar = readRouter(logicalName);
        }
        if (isEOFSymbol(inchar) && !_ignoreCompletionErrors) {
            printErrorID("SCANNER", 1, true);
            writeStringRouter(STDERR(), "Encountered End-Of-File while scanning a string\n");
        }
        // make a string out of this
        auto str = theString.str();
        auto lex = createString(str.empty() ? "" : str);
        return {Token::Type::String, stringPrintForm(lex->getContents()), lex };
    }

} // end namespace maya
