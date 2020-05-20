#ifndef REPLMAINWINDOW_H
#define REPLMAINWINDOW_H

#include <QMainWindow>
#include <QString>
#include <QTextStream>
// undefine slots temporarily
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
public slots:
    void printoutToConsole(const QString& str);
    void printoutToErrorStream(const QString& str);
    void moveToBottomOfLog();
private slots:
    void on_actionAbout_triggered();

    void on_actionExit_triggered();

    void on_actionSave_triggered();

    void on_actionClear_Console_triggered();

    void on_lineEdit_returnPressed();

private:
    void extractCurrentLineFromInput();
    void transferTextToConsole();
    void addTextToCommand();
    void processCommand();
private:
    Ui::REPLMainWindow *ui;
    ::Environment* _env;
    QString _currentLine;
    QString _commandString;
};
#endif // REPLMAINWINDOW_H
