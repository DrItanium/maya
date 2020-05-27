#include "environmentthread.h"
#include <QDialog>
#include <QMessageBox>
#include <QApplication>
#include <QTextStream>

template<typename T>
void setupQTRouters(Environment* env, void* context) {
    using K = std::remove_pointer_t<std::decay_t<T>>;
    ::AddRouter(env, "qtstdout", 20,
                [](Environment*,
                const char* logicalName,
                void*)
    {
        QString str(logicalName);
        return str == STDOUT;
    },
    [](Environment*, const char*,
                    const char* str,
                    void* context) {
        QString message(str);
        auto self = static_cast<K*>(context);
        self->writeOut(message);
    },
    nullptr,
                    nullptr,
                    nullptr,
                    context);

    ::AddRouter(env, "qtstderr", 20,
                [](Environment*,
                const char* logicalName,
                void*)
    {
        QString str(logicalName);
        return str == STDERR || str == "werror" || str == "wwarning";
    },
    [](Environment*,
                    const char*,
                    const char* str,
                    void* context) {
        QString message(str);
        auto self = static_cast<K*>(context);
        self->writeOut(message);
    },
    nullptr,
                    nullptr,
                    nullptr,
                    context);
}
EnvironmentThread::EnvironmentThread(QObject* parent) : QObject(parent)
{
    _env = ::CreateEnvironment();
    if (!_env) {
        QMessageBox::critical(nullptr, "Environment Allocation Problem", "Could not allocate the backing clips environment! Terminating!");
        QCoreApplication::quit();
    }
    setupQTRouters<decltype(this)>(_env, this);
    ::AddClearFunction(_env,
                       "qtrouters",
                       setupQTRouters<decltype(this)>,
                       0,
                       this);
}
EnvironmentThread::~EnvironmentThread() {
    ::DestroyEnvironment(_env);
}
void
EnvironmentThread::writeOut(const QString& str)  {
    emit ioRouterWrite(str);
}

void
EnvironmentThread::processCommand()
{
    // now that we have added the line to our command string, we need to process it
    // make a copy of the string
    _mutex.lock();
    auto cmdString(_commandString);
    auto cmd = cmdString.toLocal8Bit().data();
    auto result = ::CompleteCommand(cmd);
    if (result != 0) {
        _commandString.clear();
    }
    _mutex.unlock();
    // at this point we can unlock because we have a safe copy of the input stream
    switch (result) {
    case 0:
        // do nothing here just process the command
        break;
    case -1:
        emit ioRouterWrite("An error occurred in the command stream... clearing out");
        break;
    case 1:
        [this, cmd]() {
            SetEvaluationError(_env, false);
            SetHaltExecution(_env, false);
            FlushPPBuffer(_env);
            SetPPBufferStatus(_env,false);
            RouteCommand(_env, cmd, true);
            FlushPPBuffer(_env);
#if (! BLOAD_ONLY)
            FlushParsingMessages(_env);
#endif
            SetHaltExecution(_env,false);
            SetEvaluationError(_env,false);
            FlushCommandString(_env);
            CleanCurrentGarbageFrame(_env, nullptr);
           }();
        break;
    default:
        emit ioRouterWrite("Unknown parsing state... clearing command");
        break;
    }
}
void
EnvironmentThread::parseLine(const QString& str) {

    _mutex.lock();
    QTextStream commandStream;
    commandStream.setString(&_commandString);
    commandStream << str << endl;
    _mutex.unlock();
    processCommand();
}
