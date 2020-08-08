#include "CommandLine.h"
#include "Environment.h"
#include "Expression.h"
namespace maya {
    constexpr auto NO_SWITCH         = 0;
    constexpr auto BATCH_SWITCH      = 1;
    constexpr auto BATCH_STAR_SWITCH = 2;
    constexpr auto LOAD_SWITCH       = 3;
    namespace {
        int doString(const std::string &str, int i, bool &);
        int doComment(const std::string &str, int i);
        int doWhitespace(const std::string &str, int i);
    } // end namespace
    void
    Environment::rerouteStdin(int argc, char **argv) {
        // if there are not enough arguments for the -f argument then return
        if (argc < 3) {
            return;
        }
        // we were not given argv so just return
        if (!argv) {
            return;
        }
        for (int i = 1; i < argc; ++i) {
            auto theSwitch = NO_SWITCH;
            std::string currentArgv(argv[i]);
            if (currentArgv == "-f") {
                theSwitch = BATCH_SWITCH;
            } else if (currentArgv == "-f2") {
                theSwitch = BATCH_STAR_SWITCH;
            } else if (currentArgv == "-l") {
                theSwitch = LOAD_SWITCH;
            } else if (theSwitch == NO_SWITCH) {
                printErrorID("SYSDEP", 2, false);
                writeStringsRouter(STDERR(), "Invalid option '", argv[i], "'.\n");
            }
            if (i > (argc - 1)) {
                printErrorID("SYSDEP", 1, false);
                writeStringRouter(STDERR(), "No file found for '");
                switch (theSwitch) {
                    case BATCH_SWITCH:
                        writeStringRouter(STDERR(), "-f");
                        break;
                    case BATCH_STAR_SWITCH:
                        writeStringRouter(STDERR(), "-f2");
                        break;
                    case LOAD_SWITCH:
                        writeStringRouter(STDERR(), "-l");
                        break;
                }
                writeStringRouter(STDERR(), "' option.\n");
                return;
            }
            std::string str(argv[++i]);
            switch (theSwitch) {
                case BATCH_SWITCH:
                    openBatch(str, true);
                    break;
                case BATCH_STAR_SWITCH:
                    batchStar(str);
                    break;
                case LOAD_SWITCH:
                    load(str);
                    break;
            }
        }
    }
    std::string
    Environment::getCommandString() const noexcept {
        std::string str = _commandStream.str();
        return str;
    }
    void
    Environment::setCommandString(const std::string &value) noexcept {
        resetCommandStringTracking();
        _commandStream.str(value);
        _maximumCharacters += value.length();
        _commandBufferInputCount += value.length();

    }
    bool
    Environment::expandCommandString(char inchar) {
        _commandStream << inchar;
        return _commandStream.fail();
    }

    void
    Environment::flushCommandString() {
        static std::string empty;
        setCommandString(empty);
    }
    void
    Environment::resetCommandStringTracking() {
        _maximumCharacters = 0;
        _commandBufferInputCount = 0;
        _inputUngets = 0;
        _awaitingInput = true;
    }

    void
    Environment::appendCommandString(const std::string &str) {
        _commandStream << str;
    }
    void
    Environment::commandLoop() {
        int inchar = 0;
        writeStringRouter(STDOUT(), _bannerString);
        _executionHalted = false;
        _evaluationError = false;

        // cleanCurrentGarbageFrame(nullptr);
        // callPeriodicTasks();
        printPrompt();
        _commandBufferInputCount = 0;
        _inputUngets = 0;
        _awaitingInput = true;
        while (true) {
            // if a batch file is active, grab the command input directly from the batch file, otherwise call
            // the event function
            if (batchActive()) {
                inchar = llgetcBatch(STDIN(), true) ;
                if (inchar == EOF) {
                    doEventCallback();
                } else {
                    expandCommandString(static_cast<char>(inchar));
                }
            } else {
                doEventCallback();
            }
            // if execution was halted, then remove everything from the command buffer
            if (executionHalted()) {
                _executionHalted = false;
                _evaluationError = false;
                flushCommandString();
                writeStringRouter(STDOUT(), "\n");
                printPrompt();
            }
            // if a complete command is in the command buffer, then execute it
            executeIfCommandComplete();
        }
    }
    void
    Environment::commandLoopBatch() {
        _executionHalted = false;
        _evaluationError = false;

        // cleanCurrentGarbageFrame(nullptr);
        // callPeriodicTasks();

        printPrompt();
        _commandBufferInputCount = 0;
        _inputUngets = 0;
        _awaitingInput = true;
        commandLoopBatchDriver();
    }
    void
    Environment::commandLoopOnceThenBatch() {
        if (!executeIfCommandComplete()) {
            return;
        }
        commandLoopBatchDriver();
    }
    void
    Environment::commandLoopBatchDriver() {
        int inchar = 0;

        while (true) {
            if (isHaltCommandLoopBatch())  {
                closeAllBatchSources();
                setHaltCommandLoopBatch(false);
            }

            // if a batch file is active, grab the command input directly from the batch file,
            // otherwise call the event function.

            if (batchActive()) {
                inchar = llgetcBatch(STDIN(), true);
                if (inchar == EOF) {
                    return;
                } else {
                    expandCommandString(static_cast<char>(inchar));
                }
            } else {
                return;
            }
            // if execution was halted, then remove everything from the command buffer.
            if (executionHalted()) {
                _executionHalted = false;
                _evaluationError = false;
                flushCommandString();
                writeStringRouter(STDOUT(), "\n");
                printPrompt();
            }
            // if a complete command is in the command buffer, then execute it
            executeIfCommandComplete();
        }
    }
    bool
    Environment::executeIfCommandComplete() {
        auto cmdString = getCommandString();
        if (cmdString.empty() || (_commandBufferInputCount == 0) || !_awaitingInput) {
            return false;
        }

        if (_beforeCommandExecutionCallback) {
            if (!_beforeCommandExecutionCallback(*this)) {
                return false;
            }
        }
        //flushPPBuffer();
        // setPPBufferStatus(false);
        _commandBufferInputCount = 0;
        _inputUngets = 0;
        _awaitingInput = false;
        routeCommand(cmdString, true);
        // flushPPBuffer();
        // flushParsingMessages();
        _executionHalted = false;
        _evaluationError = false;
        flushCommandString();
        // cleanCurrentGarbageFrame(nullptr);
        // callPeriodicTasks();
        printPrompt();
        return true;
    }
    bool
    Environment::commandCompleteAndNotEmpty() {
        auto cmdStr = getCommandString();
        return ! ((isCompleteCommand(cmdStr) == CommandCompletionStatus::Incomplete) || (_commandBufferInputCount == 0) || !_awaitingInput);
    }
    void
    Environment::printPrompt() {
        writeStringRouter(STDOUT(), COMMAND_PROMPT);
        if (_afterPromptCallback) {
            _afterPromptCallback(*this);
        }
    }
    void
    Environment::printBanner() {
        writeStringRouter(STDOUT(), _bannerString);
    }
    void
    Environment::doEventCallback() {
        // if we have an event callback then use it, otherwise do the default implementation
        if (_eventCallback) {
            _eventCallback(*this);
        } else {
            // default implementation
            auto inchar = readRouter(STDIN());
            if (inchar == EOF) inchar = '\n';
            expandCommandString(static_cast<char>(inchar));
        }
    }
    bool
    Environment::routeCommand(const std::string &command, bool printResult) {
        if (command.empty()) {
            return false;
        }
        openStringSource("command", command);
        auto theToken = getToken("command");
        // evaluate constants
        if (theToken.isConstant()) {
            closeStringSource("command");
            if (printResult) {
                theToken.write("stdout");
                writeStringRouter("stdout", "\n");
            }
            return true;
        } else if (theToken.isVariable()) {
            closeStringSource("command");
            auto top = genConstant<Lexeme>(theToken.getGlobalType(), std::get<Lexeme::Ptr>(theToken.getContents()));
            auto result = top->evaluate();
            if (printResult) {
            }
            /// @todo continue implementing here
        }
        /// @todo continue implementation
        return true;
    }
#if 0

bool RouteCommand(
        const Environment::Ptr&theEnv,
        const char *command,
        bool printResult) {
    UDFValue returnValue;
    Expression *top;
    const char *commandName;
    struct token theToken;
    int danglingConstructs;

    if (command == nullptr) { return false; }

    /*========================================*/
    /* Open a string input source and get the */
    /* first token from that source.          */
    /*========================================*/

    OpenStringSource(theEnv, "command", command, 0);

    GetToken(theEnv, "command", &theToken);

    /*=====================*/
    /* Evaluate constants. */
    /*=====================*/

    if ((theToken.tknType == SYMBOL_TOKEN) || (theToken.tknType == STRING_TOKEN) ||
        (theToken.tknType == FLOAT_TOKEN) || (theToken.tknType == INTEGER_TOKEN) ||
        (theToken.tknType == INSTANCE_NAME_TOKEN)) {
        CloseStringSource(theEnv, "command");
        if (printResult) {
            PrintAtom(theEnv, STDOUT, TokenTypeToType(theToken.tknType), theToken.value);
            WriteString(theEnv, STDOUT, "\n");
        }
        return true;
    }

    /*=====================*/
    /* Evaluate variables. */
    /*=====================*/

    if ((theToken.tknType == GBL_VARIABLE_TOKEN) ||
        (theToken.tknType == MF_GBL_VARIABLE_TOKEN) ||
        (theToken.tknType == SF_VARIABLE_TOKEN) ||
        (theToken.tknType == MF_VARIABLE_TOKEN)) {
        CloseStringSource(theEnv, "command");
        top = GenConstant(theEnv, TokenTypeToType(theToken.tknType), theToken.value);
        EvaluateExpression(theEnv, top, &returnValue);
        rtn_struct(theEnv, Expression, top);
        if (printResult) {
            WriteUDFValue(theEnv, STDOUT, &returnValue);
            WriteString(theEnv, STDOUT, "\n");
        }
        return true;
    }

    /*========================================================*/
    /* If the next token isn't the beginning left parenthesis */
    /* of a command or construct, then whatever was entered   */
    /* cannot be evaluated at the command prompt.             */
    /*========================================================*/

    if (theToken.tknType != LEFT_PARENTHESIS_TOKEN) {
        PrintErrorID(theEnv, "COMMLINE", 1, false);
        WriteString(theEnv, STDERR, "Expected a '(', constant, or variable.\n");
        CloseStringSource(theEnv, "command");
        return false;
    }

    /*===========================================================*/
    /* The next token must be a function name or construct type. */
    /*===========================================================*/

    GetToken(theEnv, "command", &theToken);
    if (theToken.tknType != SYMBOL_TOKEN) {
        PrintErrorID(theEnv, "COMMLINE", 2, false);
        WriteString(theEnv, STDERR, "Expected a command.\n");
        CloseStringSource(theEnv, "command");
        return false;
    }

    commandName = theToken.lexemeValue->contents;

    /*======================*/
    /* Evaluate constructs. */
    /*======================*/

    {
        BuildError errorFlag;

        errorFlag = ParseConstruct(theEnv, commandName, "command");
        if (errorFlag != BE_CONSTRUCT_NOT_FOUND_ERROR) {
            CloseStringSource(theEnv, "command");
            if (errorFlag == BE_PARSING_ERROR) {
                WriteString(theEnv, STDERR, "\nERROR:\n");
                WriteString(theEnv, STDERR, GetPPBuffer(theEnv));
                WriteString(theEnv, STDERR, "\n");
            }
            DestroyPPBuffer(theEnv);

            SetWarningFileName(theEnv, nullptr);
            SetErrorFileName(theEnv, nullptr);

            return errorFlag == BE_NO_ERROR;
        }
    }

    /*========================*/
    /* Parse a function call. */
    /*========================*/

    danglingConstructs = ConstructData(theEnv)->DanglingConstructs;
    CommandLineData(theEnv)->ParsingTopLevelCommand = true;
    top = Function2Parse(theEnv, "command", commandName);
    CommandLineData(theEnv)->ParsingTopLevelCommand = false;
    ClearParsedBindNames(theEnv);

    /*================================*/
    /* Close the string input source. */
    /*================================*/

    CloseStringSource(theEnv, "command");

    /*=========================*/
    /* Evaluate function call. */
    /*=========================*/

    if (top == nullptr) {
        SetWarningFileName(theEnv, nullptr);
        SetErrorFileName(theEnv, nullptr);
        ConstructData(theEnv)->DanglingConstructs = danglingConstructs;
        return false;
    }

    ExpressionInstall(theEnv, top);

    CommandLineData(theEnv)->EvaluatingTopLevelCommand = true;
    CommandLineData(theEnv)->CurrentCommand = top;
    EvaluateExpression(theEnv, top, &returnValue);
    CommandLineData(theEnv)->CurrentCommand = nullptr;
    CommandLineData(theEnv)->EvaluatingTopLevelCommand = false;

    ExpressionDeinstall(theEnv, top);
    ReturnExpression(theEnv, top);
    ConstructData(theEnv)->DanglingConstructs = danglingConstructs;

    SetWarningFileName(theEnv, nullptr);
    SetErrorFileName(theEnv, nullptr);

    /*=================================================*/
    /* Print the return value of the function/command. */
    /*=================================================*/

    if ((returnValue.header->type != VOID_TYPE) && printResult) {
        WriteUDFValue(theEnv, STDOUT, &returnValue);
        WriteString(theEnv, STDOUT, "\n");
    }

    return true;
}



/***********************************************************/
/* GetCommandCompletionString: Returns the last token in a */
/*   string if it is a valid token for command completion. */
/***********************************************************/
const char *GetCommandCompletionString(
        const Environment::Ptr&theEnv,
        const char *theString,
        size_t maxPosition) {
#if STUBBING_INACTIVE
    struct token lastToken;
    struct token theToken;
    char lastChar;
    const char *rs;
    size_t length;

    /*=========================*/
    /* Get the command string. */
    /*=========================*/

    if (theString == nullptr) return ("");

    /*=========================================================================*/
    /* If the last character in the command string is a space, character       */
    /* return, or quotation mark, then the command completion can be anything. */
    /*=========================================================================*/

    lastChar = theString[maxPosition - 1];
    if ((lastChar == ' ') || (lastChar == '"') ||
        (lastChar == '\t') || (lastChar == '\f') ||
        (lastChar == '\n') || (lastChar == '\r')) { return (""); }

    /*============================================*/
    /* Find the last token in the command string. */
    /*============================================*/

    OpenTextSource(theEnv, "CommandCompletion", theString, 0, maxPosition);
    ScannerData(theEnv)->IgnoreCompletionErrors = true;
    GetToken(theEnv, "CommandCompletion", &theToken);
    CopyToken(&lastToken, &theToken);
    while (theToken.tknType != STOP_TOKEN) {
        CopyToken(&lastToken, &theToken);
        GetToken(theEnv, "CommandCompletion", &theToken);
    }
    CloseStringSource(theEnv, "CommandCompletion");
    ScannerData(theEnv)->IgnoreCompletionErrors = false;

    /*===============================================*/
    /* Determine if the last token can be completed. */
    /*===============================================*/

    if (lastToken.tknType == SYMBOL_TOKEN) {
        rs = lastToken.lexemeValue->contents;
        if (rs[0] == '[') return (&rs[1]);
        return lastToken.lexemeValue->contents;
    } else if (lastToken.tknType == SF_VARIABLE_TOKEN) { return lastToken.lexemeValue->contents; }
    else if (lastToken.tknType == MF_VARIABLE_TOKEN) { return lastToken.lexemeValue->contents; }
    else if ((lastToken.tknType == GBL_VARIABLE_TOKEN) ||
             (lastToken.tknType == MF_GBL_VARIABLE_TOKEN) ||
             (lastToken.tknType == INSTANCE_NAME_TOKEN)) { return nullptr; }
    else if (lastToken.tknType == STRING_TOKEN) {
        length = strlen(lastToken.lexemeValue->contents);
        return GetCommandCompletionString(theEnv, lastToken.lexemeValue->contents, length);
    } else if ((lastToken.tknType == FLOAT_TOKEN) ||
               (lastToken.tknType == INTEGER_TOKEN)) { return nullptr; }

#endif
    return ("");
}

#endif
    Environment::CommandCompletionStatus
    Environment::isCompleteCommand(const std::string &str) noexcept {
/// @todo finish implementing isCompleteCommand
#if 0
        if (str.empty()) {
            return CommandCompletionStatus::Incomplete;
        }
        auto error = false;
        auto complete = false;
        auto moreThanZero = false;
        auto depth = 0;
        // iterate through each character
        for (size_t index = 0; index < str.length(); ++index) {
            switch (char targetCharacter = str.at(index); targetCharacter) {
                /// If a carriage return or line feed is found there is at least
                /// one completed token in the command buffer,
                /// and parentheses are balanced, then a complete command has been found.
                /// Otherwise, remove all whitespace beginning with the current character
                case '\n':
                case '\r':
                    if (error) {
                        return CommandCompletionStatus::Error;
                    }
                    if (moreThanZero && (depth == 0)) {
                        return CommandCompletionStatus::Complete;
                    }
                    index = doWhitespace(str, index);
                    break;
                    /// Remove whitespace
                case ' ':
                case '\f':
                case '\t':
                    index = doWhitespace(str, index);
                    break;
                    /// If the opening quotation of a string is encountered, determine if the closing quotation of the string is  in the command buffer.
                    /// Until the closing quotation is found, a complete command can not be made.
                case '"' :
                    index = doString(str, index, complete);
                    if ((depth == 0) && complete) {
                        moreThanZero = true;
                    }
                    break;

                case ';' : /// process a comment
                    index = doComment(str, index);
                    if (moreThanZero && (depth == 0) && (str.at(index) != EOS)) {
                        return error ? CommandCompletionStatus::Error : CommandCompletionStatus::Complete;
                    } else if (str.at(index) != EOS) {
                        index++;
                    }
                    break;

                    /// a left paren "(" increases the nesting depth of the current command by 1.
                    /// Do not increment the count if the first token encountered was not a parenthesis (e.g. "donuts (+ 3 6", the symbol
                    /// donuts already forms a complete command, so the next carriage return will cause evaluation of red--the closing paren
                    /// for "(+ 3 6" does not have to be found)
                case '(' :
                    if ((depth > 0) || !moreThanZero) {
                        depth++;
                        moreThanZero = true;
                    }
                    break;
                    /// A right paren decreases the nesting depth of the current command by 1. If the parenthesis is
                    /// the first token of the command, then an error is generated.
                case ')' :
                    if (depth > 0) {
                        depth--;
                    } else if (!moreThanZero) {
                        error = true;
                    }
                    break;
                    /// if the command begins with any other character and an opening paren hasn't been found, then
                    /// skip all chars on the same line. If a carriage return or line feed is found, then a complete command
                    /// exists.
                default:
                    if (depth == 0) {
                        if (IsUTF8MultiByteStart(targetCharacter) || isprint(targetCharacter)) {
                            while ((targetCharacter = str.at(index++)) != EOS) {
                                if ((targetCharacter == '\n') || (targetCharacter == '\r')) {
                                    if (error) return CommandCompletionStatus::Error;
                                    else return CommandCompletionStatus::Complete;
                                }
                            }
                            return CommandCompletionStatus::Incomplete;
                        }
                    }
                    break;
            }
        }
        // if we got here then there is no complete command to be found
#endif
        return CommandCompletionStatus::Incomplete;
    }
#if 0
    namespace {
        int
        doString(const std::string &str, int position, bool& complete) {
            try {
                char targetCharacter = str.at(position);
                while (targetCharacter != '"') {
                    // if we find a \ then the next character is ignored even if it is a closing quote mark
                    if (targetCharacter == '\\') {
                        ++position;
                        targetCharacter = str.at(position);
                    }
                }
            } catch (std::out_of_range&) {
                // we've gone out of bounds
            }
        }
    }

/***********************************************************/
/* DoString: Skips over a string contained within a string */
/*   until the closing quotation mark is encountered.      */
/***********************************************************/
    static int DoString(
            const char *str,
            int pos,
            bool *complete) {
        int inchar;

        /*=================================================*/
        /* Process the string character by character until */
        /* the closing quotation mark is found.            */
        /*=================================================*/

        inchar = str[pos];
        while (inchar != '"') {
            /*=====================================================*/
            /* If a \ is found, then the next character is ignored */
            /* even if it is a closing quotation mark.             */
            /*=====================================================*/

            if (inchar == '\\') {
                pos++;
                inchar = str[pos];
            }

            /*===================================================*/
            /* If the end of input is reached before the closing */
            /* quotation mark is found, the return the last      */
            /* position that was reached and indicate that a     */
            /* complete string was not found.                    */
            /*===================================================*/

            if (inchar == EOS) {
                *complete = false;
                return (pos);
            }

            /*================================*/
            /* Move on to the next character. */
            /*================================*/

            pos++;
            inchar = str[pos];
        }

        /*======================================================*/
        /* Indicate that a complete string was found and return */
        /* the position of the closing quotation mark.          */
        /*======================================================*/

        pos++;
        *complete = true;
        return (pos);
    }

/*************************************************************/
/* DoComment: Skips over a comment contained within a string */
/*   until a line feed or carriage return is encountered.    */
/*************************************************************/
    static int DoComment(
            const char *str,
            int pos) {
        int inchar;

        inchar = str[pos];
        while ((inchar != '\n') && (inchar != '\r')) {
            if (inchar == EOS) { return (pos); }

            pos++;
            inchar = str[pos];
        }

        return (pos);
    }

/**************************************************************/
/* DoWhiteSpace: Skips over white space consisting of spaces, */
/*   tabs, and form feeds that is contained within a string.  */
/**************************************************************/
    static int DoWhiteSpace(
            const char *str,
            int pos) {
        int inchar;

        inchar = str[pos];
        while ((inchar == ' ') || (inchar == '\f') || (inchar == '\t')) {
            pos++;
            inchar = str[pos];
        }

        return (pos);
    }
#endif

} // end namespace maya