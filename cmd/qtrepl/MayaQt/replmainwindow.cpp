#include "replmainwindow.h"
#include "ui_replmainwindow.h"
#include <QApplication>
#include <QMessageBox>
#include <QFileDialog>
#include <QDir>
#include <QSaveFile>
#include <QTextStream>

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
void REPLMainWindow::transferTextToConsole()
{
    if (ui->lineEdit->isModified()) {
        auto text = ui->lineEdit->text();
        ui->plainTextEdit->appendPlainText(text);
        ui->lineEdit->clear();
        ui->lineEdit->setText("");
    }
}
void REPLMainWindow::on_submitLine_clicked()
{
    transferTextToConsole();
}

void REPLMainWindow::on_actionSave_triggered()
{
    auto homeLocation = QDir::homePath();
    auto path = QFileDialog::getSaveFileName(this, tr("Save console to"), homeLocation);
    QSaveFile file(path);
    if (!file.open(QIODevice::WriteOnly)) {
        QMessageBox::information(this, tr("Unable to open file for writing"),
                                 file.errorString());
        return;
    } else {
        QTextStream ts(&file);
        ts << ui->plainTextEdit->toPlainText();
        file.commit();
    }
}

void REPLMainWindow::on_actionClear_Console_triggered()
{
   ui->plainTextEdit->clear();
}

void REPLMainWindow::on_lineEdit_returnPressed()
{
    transferTextToConsole();
}
