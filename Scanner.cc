#include "Environment.h"
#include "Token.h"
#include "CommandLine.h"

namespace maya {
    constexpr auto isWhiteSpace(char value) noexcept {
        return (value == ' ') || (value == '\n') || (value == '\f') ||
               (value == '\r') || (value == ';') || (value == '\t');
    }
    constexpr auto isCommentCharacter(char value) noexcept { return value == ';'; }
    Token
    Environment::getToken(const std::string& logicalName) {
        auto targetType = Token::Type::Unknown;
        std::string printForm("unknown");
        Token::Contents storage = nullptr;
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
           targetType = Token::Type::Symbol;
           unreadRouter(logicalName, inchar);
           auto symbol = scanSymbol(logicalName, 0, targetType);
           storage = symbol;
           printForm = symbol->getContents();
        } else if (isdigit(inchar)) {
            unreadRouter(logicalName, inchar);
            //scanNumber(logicalName, )
        }
        return {targetType, printForm, storage};
    }
} // end namespace maya
#if 0

void GetToken( const Environment::Ptr&theEnv, const char *logicalName, struct token *theToken) {
    if (isalpha(inchar) || IsUTF8MultiByteStart(inchar)) {
        theToken->tknType = SYMBOL_TOKEN;
        UnreadRouter(theEnv, logicalName, inchar);
        theToken->lexemeValue = ScanSymbol(theEnv, logicalName, 0, &type);
        theToken->printForm = theToken->lexemeValue->contents;
    }

        /*===============================================*/
        /* Process Number Tokens beginning with a digit. */
        /*===============================================*/

    else if (isdigit(inchar)) {
        UnreadRouter(theEnv, logicalName, inchar);
        ScanNumber(theEnv, logicalName, theToken);
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

/*************************************/
/* ScanSymbol: Scans a symbol token. */
/*************************************/
static CLIPSLexeme *ScanSymbol(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        int count,
        TokenType *type) {
#if STUBBING_INACTIVE
    int inchar;
    CLIPSLexeme *symbol;

    /*=====================================*/
    /* Scan characters and add them to the */
    /* symbol until a delimiter is found.  */
    /*=====================================*/

    inchar = ReadRouter(theEnv, logicalName);
    while ((inchar != '<') && (inchar != '"') &&
           (inchar != '(') && (inchar != ')') &&
           (inchar != '&') && (inchar != '|') && (inchar != '~') &&
           (inchar != ' ') && (inchar != ';') &&
           (IsUTF8MultiByteStart(inchar) ||
            IsUTF8MultiByteContinuation(inchar) ||
            isprint(inchar))) {
        ScannerData(theEnv)->GlobalString = ExpandStringWithChar(theEnv, inchar, ScannerData(theEnv)->GlobalString,
                                                                 &ScannerData(theEnv)->GlobalPos, &ScannerData(theEnv)->GlobalMax,
                                                                 ScannerData(theEnv)->GlobalMax + 80);

        count++;
        inchar = ReadRouter(theEnv, logicalName);
    }

    /*===================================================*/
    /* Return the last character scanned (the delimiter) */
    /* to the input stream so it will be scanned as part */
    /* of the next token.                                */
    /*===================================================*/

    UnreadRouter(theEnv, logicalName, inchar);

    /*====================================================*/
    /* Add the symbol to the symbol table and return the  */
    /* symbol table address of the symbol. Symbols of the */
    /* form [<symbol>] are instance names, so the type    */
    /* returned is INSTANCE_NAME_TYPE rather than SYMBOL_TYPE.      */
    /*====================================================*/

    if (count > 2) {
        if ((ScannerData(theEnv)->GlobalString[0] == '[') ? (ScannerData(theEnv)->GlobalString[count - 1] == ']') : false) {
            *type = INSTANCE_NAME_TOKEN;
            inchar = ']';
        } else {
            *type = SYMBOL_TOKEN;
            return CreateSymbol(theEnv, ScannerData(theEnv)->GlobalString);
        }
        ScannerData(theEnv)->GlobalString[count - 1] = EOS;
        symbol = CreateInstanceName(theEnv, ScannerData(theEnv)->GlobalString + 1);
        ScannerData(theEnv)->GlobalString[count - 1] = (char) inchar;
        return symbol;
    } else {
        *type = SYMBOL_TOKEN;
        return CreateSymbol(theEnv, ScannerData(theEnv)->GlobalString);
    }
#endif
    return nullptr;
}
#if STUBBING_INACTIVE
/*************************************/
/* ScanString: Scans a string token. */
/*************************************/
static CLIPSLexeme *ScanString(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    int inchar;
    size_t pos = 0;
    size_t max = 0;
    char *theString = nullptr;
    CLIPSLexeme *thePtr;

    /*============================================*/
    /* Scan characters and add them to the string */
    /* until the " delimiter is found.            */
    /*============================================*/

    inchar = ReadRouter(theEnv, logicalName);
    while ((inchar != '"') && (inchar != EOF)) {
        if (inchar == '\\') { inchar = ReadRouter(theEnv, logicalName); }

        theString = ExpandStringWithChar(theEnv, inchar, theString, &pos, &max, max + 80);
        inchar = ReadRouter(theEnv, logicalName);
    }

    if ((inchar == EOF) && !ScannerData(theEnv)->IgnoreCompletionErrors) {
        PrintErrorID(theEnv, "SCANNER", 1, true);
        WriteString(theEnv, STDERR, "Encountered End-Of-File while scanning a string\n");
    }

    /*===============================================*/
    /* Add the string to the symbol table and return */
    /* the symbol table address of the string.       */
    /*===============================================*/

    if (theString == nullptr) { thePtr = CreateString(theEnv, ""); }
    else {
        thePtr = CreateString(theEnv, theString);
        rm(theEnv, theString, max);
    }

    return thePtr;
}
#endif

/**************************************/
/* ScanNumber: Scans a numeric token. */
/**************************************/
static void ScanNumber(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        struct token *theToken) {
#if STUBBING_INACTIVE
    int count = 0;
    int inchar, phase;
    bool digitFound = false;
    bool processFloat = false;
    double fvalue;
    long long lvalue;
    TokenType type;

    /* Phases:              */
    /*  -1 = sign           */
    /*   0 = integral       */
    /*   1 = decimal        */
    /*   2 = exponent-begin */
    /*   3 = exponent-value */
    /*   5 = done           */
    /*   9 = error          */

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
#endif
}

#endif
