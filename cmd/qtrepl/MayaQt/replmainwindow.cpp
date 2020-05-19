#include "replmainwindow.h"
#include "ui_replmainwindow.h"
#include <QApplication>

REPLMainWindow::REPLMainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::REPLMainWindow)
{
    ui->setupUi(this);
}

REPLMainWindow::~REPLMainWindow()
{
    delete ui;
}


void REPLMainWindow::on_actionAbout_triggered()
{
    QCoreApplication::quit();
}
