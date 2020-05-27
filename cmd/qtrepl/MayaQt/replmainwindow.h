#ifndef REPLMAINWINDOW_H
#define REPLMAINWINDOW_H

#include <QMainWindow>
#include <QString>
#include <QTextStream>
#include "environmentthread.h"

QT_BEGIN_NAMESPACE
namespace Ui { class REPLMainWindow; }
QT_END_NAMESPACE

class REPLMainWindow : public QMainWindow
{
    // parasoft-begin-suppress ALL "Q_OBJECT inserts code that is from Qt"
    Q_OBJECT
    // parasoft-end-suppress ALL "Q_OBJECT's expansion needs to be ignored, end of the block"

public:
    REPLMainWindow(QWidget *parent = nullptr);
    ~REPLMainWindow();
signals:
    void sendCommand(const QString&);
public slots:
    void moveToBottomOfLog();
    void println(const QString& str);
    void print(const QString& str);
private slots:
    void on_actionAbout_triggered();

    void on_actionExit_triggered();

    void on_actionSave_triggered();

    void on_actionClear_Console_triggered();

    void on_lineEdit_returnPressed();

private:
    QString extractCurrentLineFromInput();
    void processCommand();
private:
    Ui::REPLMainWindow *ui;
    EnvironmentThread _env;
};
#endif // REPLMAINWINDOW_H
