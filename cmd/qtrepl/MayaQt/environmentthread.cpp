#include "environmentthread.h"
#include <QDialog>
#include <QMessageBox>
#include <QApplication>

template<typename T>
void setupQTRouters(Environment* env, void* context) {
    using K = std::remove_pointer_t<std::decay_t<T>>;
    ::AddRouter(env, "qtstdout", 20,
                [](Environment* env,
                const char* logicalName,
                void*)
    {
        QString str(logicalName);
        return str == STDOUT;
    },
    [](Environment* env,
                    const char* logicalName,
                    const char* str,
                    void* context) {
        QString message(str);
        auto self = (K*)(context);
        self->writeOut(message);
    },
    nullptr,
                    nullptr,
                    nullptr,
                    context);

    ::AddRouter(env, "qtstderr", 20,
                [](Environment* env,
                const char* logicalName,
                void*)
    {
        QString str(logicalName);
        return str == STDERR;
    },
    [](Environment* env,
                    const char* logicalName,
                    const char* str,
                    void* context) {
        QString message(str);
        auto self = (K*)(context);
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
    emit writeOut("CLIPS Environment Successfully Created");
    emit writeOut("---------------------------------------");
    setupQTRouters<decltype(this)>(_env, this);
    ::AddClearFunction(_env,
                       "qtrouters",
                       setupQTRouters<decltype(this)>,
                       0,
                       this);
}
void
EnvironmentThread::writeOut(const QString& str)  {
    emit writeOut(str);
}
void
EnvironmentThread::parseLine(const QString& str) {
#if 0
    QTextStream commandStream;
    commandStream.setString(&_commandString);
    commandStream << _currentLine << endl;
#endif
}
EnvironmentThread::~EnvironmentThread() {

#if 0
    // now that we have added the text to the command stream we must see if CLIPS
    // considers it to be a complete command
    const char* cmd = _commandString.toLocal8Bit().data();
    switch (::CompleteCommand(cmd)) {
    case 0:
        // more input required so just return
        ui->textEdit->append("&&");
        break;
    case -1:
        ui->textEdit->append("An error occurred in the command stream... clearing out");
        _commandString.clear();
        break;
    case 1:
        [this, cmd](){
            QTextStream commandStream;
            commandStream.setString(&_commandString);
            commandStream << endl;
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
            _commandString.clear();
        }();
        break;
    }
#endif
    ::DestroyEnvironment(_env);
}
