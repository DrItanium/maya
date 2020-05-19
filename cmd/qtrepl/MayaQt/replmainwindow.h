#ifndef REPLMAINWINDOW_H
#define REPLMAINWINDOW_H

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class REPLMainWindow; }
QT_END_NAMESPACE

class REPLMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    REPLMainWindow(QWidget *parent = nullptr);
    ~REPLMainWindow();

private:
    Ui::REPLMainWindow *ui;
};
#endif // REPLMAINWINDOW_H
