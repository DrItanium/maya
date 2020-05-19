#include "replmainwindow.h"
#include "ui_replmainwindow.h"
#include <QApplication>
#include <QMessageBox>
#include <QFileDialog>

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
   QMessageBox::about(this, "About", "Simple CLIPS REPL (C) 2020 Josh Scoggins");
}

void REPLMainWindow::on_actionExit_triggered()
{
    QCoreApplication::quit();
}

void REPLMainWindow::on_submitLine_clicked()
{
    if (ui->lineEdit->isModified()) {
        auto text = ui->lineEdit->text();
        ui->plainTextEdit->appendPlainText(text);
        ui->lineEdit->clear();
        ui->lineEdit->setText("");
    }
}

void REPLMainWindow::on_actionSave_triggered()
{

}
