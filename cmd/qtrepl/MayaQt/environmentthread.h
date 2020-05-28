#ifndef ENVIRONMENTTHREAD_H
#define ENVIRONMENTTHREAD_H
#include <QMutex>
#include <QThread>
#include <QWaitCondition>

// parasoft-begin-suppress ALL "clips uses the phrase slots which conflicts with Qt"
#undef slots
// parasoft-end-suppress ALL "clips uses the phrase slots"
extern "C"
{
#include "clips.h"
#include "pprint.h"
}
// parasoft-begin-suppress ALL "clips uses the phrase slots which conflicts with Qt"
#define slots Q_SLOTS
// parasoft-end-suppress ALL "clips uses the phrase slots"

class EnvironmentThread : public QThread
{
    Q_OBJECT
    void run() override;
public:
    EnvironmentThread(QObject* parent = nullptr);
    ~EnvironmentThread() override;
signals:
    void ioRouterWrite(const QString& str);
    void clearInvoked();
    void resetInvoked();
public slots:
    void parseLine(const QString&);
public: // used to bridge CLIPS -> C++ since I can't capture this in a lambda and pass that to C code
   void writeOut(const QString& str);
   void transmitClearSignal();
   void transmitResetSignal();
private:
    ::Environment* _env;
    QString _commandString;
    QMutex _mutex;
    QWaitCondition _cond;
};

#endif // ENVIRONMENTTHREAD_H
