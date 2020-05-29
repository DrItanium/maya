#include "environmentthread.h"
#include <QDialog>
#include <QMessageBox>
#include <QApplication>
#include <QTextStream>
#include <QMutexLocker>
#include <QByteArray>

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
    ::AddRouter(env, "qtstdin", 20,
                [](Environment*,
                const char* logicalName,
                void*) {
      QString str(logicalName);
      return str == STDIN;
    },
    nullptr,
    [](Environment*,
       const char*,
            void* context) {
        auto self = static_cast<K*>(context);
        return self->getChar();
    },
    [](Environment*,
       const char*,
       int ch,
       void* context)
    {
        auto self = static_cast<K*>(context);
        return self->putChar(ch);
    },
    nullptr,
    context);
    // hack to make sure that the worker thread does not call genexit,
    // we intercept it before hand and link it to the application's exit routine.
    // Since the priority is below 0, all file handles and any other structures
    // held open by any other routers will be cleaned up by this point (at least in theory).
    ::AddRouter(env, "qtexit",
                -1, // makes sure that this router always comes _last_
                nullptr,
                nullptr,
                nullptr,
                nullptr,
                [](Environment*,
                   int code,
                   void* context)
    {
        auto self = static_cast<K*>(context);
        self->transmitExitSignal(code);
    },
    context);
}
template<typename T>
void transmitClearToGui(Environment*, void* context) {
    using K = std::remove_pointer_t<std::decay_t<T>>;
    auto self = static_cast<K*>(context);
    self->transmitClearSignal();
}

template<typename T>
void transmitResetToGui(Environment*, void* context) {
    using K = std::remove_pointer_t<std::decay_t<T>>;
    auto self = static_cast<K*>(context);
    self->transmitResetSignal();
}

EnvironmentThread::EnvironmentThread(QObject* parent) : QThread(parent)
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
                       1, // this has higher priority
                       this);
    ::AddClearFunction(_env,
                       "sendClearToQtGui",
                       transmitClearToGui<decltype(this)>,
                       0,
                       this);
    ::AddResetFunction(_env,
                       "sendResetToQtGui",
                       transmitResetToGui<decltype(this)>,
                       0,
                       this);
}
EnvironmentThread::~EnvironmentThread() {
    _mutex.lock();
    ::DestroyEnvironment(_env);
    _env = nullptr;
    _cond.wakeOne();
    _mutex.unlock();
}

void
EnvironmentThread::run()
{
    ::CommandLoop(_env);
}
void
EnvironmentThread::parseLine(const QString& str) {
    QMutexLocker locker(&_mutex);
    QTextStream commandStream;
    commandStream.setString(&_commandString);
    commandStream << str << endl;
    if (!isRunning()) {
        start(LowPriority);
    } else {
        _cond.wakeOne();
    }
}

void
EnvironmentThread::writeOut(const QString& str)  {
    _mutex.lock();
    emit ioRouterWrite(str);
    _cond.wakeOne();
    _mutex.unlock();
}

void
EnvironmentThread::transmitClearSignal()
{
    _mutex.lock();
    emit clearInvoked();
    _cond.wakeOne();
    _mutex.unlock();
}

void
EnvironmentThread::transmitResetSignal()
{
    _mutex.lock();
    emit resetInvoked();
    _cond.wakeOne();
    _mutex.unlock();
}

int
EnvironmentThread::getChar()
{
    // pull a character out of the input stream
    _mutex.lock();
    if (_commandString.isEmpty()) {
        _cond.wait(&_mutex);
    }
    char firstChar = _commandString.toLocal8Bit().front();
    _commandString.remove(0, 1); // strip the first character off
    _mutex.unlock();
    return firstChar;
}

int
EnvironmentThread::putChar(int ch)
{
    _mutex.lock();
    _commandString.push_front(ch);
    _mutex.unlock();
    return ch;
}
