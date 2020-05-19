#ifndef REPLMAINWINDOW_H
#define REPLMAINWINDOW_H

#include <QMainWindow>
#include <QString>
// undefine slots for temporarily
#undef slots
extern "C"
{
#include "clips.h"
}
#define slots Q_SLOTS

QT_BEGIN_NAMESPACE
namespace Ui { class REPLMainWindow; }
QT_END_NAMESPACE

class REPLMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    REPLMainWindow(QWidget *parent = nullptr);
    ~REPLMainWindow();

private slots:
    void on_actionAbout_triggered();

    void on_actionExit_triggered();

    void on_submitLine_clicked();

    void on_actionSave_triggered();

    void on_actionClear_Console_triggered();

    void on_lineEdit_returnPressed();
private:
    void transferTextToConsole();

private:
    Ui::REPLMainWindow *ui;
    ::Environment* _env;
};
#endif // REPLMAINWINDOW_H
