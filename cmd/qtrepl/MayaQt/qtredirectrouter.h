#ifndef QTREDIRECTROUTER_H
#define QTREDIRECTROUTER_H

#include <QObject>

class QtRedirectRouter : public QObject
{
    Q_OBJECT
public:
    explicit QtRedirectRouter(QObject *parent = nullptr);

signals:

};

#endif // QTREDIRECTROUTER_H
