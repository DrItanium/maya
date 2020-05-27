#ifndef COMMANDSTORAGEHELPER_H
#define COMMANDSTORAGEHELPER_H

#include <QObject>
#include <QWidget>
#include <QThread>

class CommandStorageHelper : public QThread
{
    Q_OBJECT
public:
    CommandStorageHelper(QObject* parent = nullptr);
};

#endif // COMMANDSTORAGEHELPER_H
