#ifndef CAPTURESTDIO_H
#define CAPTURESTDIO_H

#include <QObject>

class CaptureStdIO : public QObject
{
    Q_OBJECT
public:
    explicit CaptureStdIO(QObject *parent = nullptr);

signals:

};

#endif // CAPTURESTDIO_H
