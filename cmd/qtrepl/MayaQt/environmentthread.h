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

class EnvironmentThread : public QObject
{
    Q_OBJECT
public:
    EnvironmentThread(QObject* parent = nullptr);
    ~EnvironmentThread() override;
signals:
    void ioRouterWrite(const QString& str);
public slots:
    void parseCommand(const QString&);
private:
   void writeOut(const QString& str);
private:
    ::Environment* _env;
    QString _currentLine;
    QString _commandString;

};

#endif // ENVIRONMENTTHREAD_H
