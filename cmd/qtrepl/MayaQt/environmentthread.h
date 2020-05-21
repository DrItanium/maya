#ifndef ENVIRONMENTTHREAD_H
#define ENVIRONMENTTHREAD_H
#include <QMutex>
#include <QThread>

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
public:
    EnvironmentThread(QObject* parent = nullptr);
    ~EnvironmentThread();

public slots:
    void parseCommand(const QString&);
};

#endif // ENVIRONMENTTHREAD_H
