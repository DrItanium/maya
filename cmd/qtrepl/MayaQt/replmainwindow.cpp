#include "replmainwindow.h"
#include "ui_replmainwindow.h"
#include <QApplication>
#include <QMessageBox>
#include <QFileDialog>
#include <QDir>
#include <QSaveFile>
#include <QTextStream>
#include <QScrollBar>
#include <type_traits>


REPLMainWindow::REPLMainWindow(QWidget *parent)
        : QMainWindow(parent)
        ,ui(new Ui::REPLMainWindow)
{
    ui->setupUi(this);
    ui->textEdit->append("UI Setup...");
    ui->textEdit->append("Creating a CLIPS Environment");
    connect(&_env, &EnvironmentThread::ioRouterWrite,
            this, &REPLMainWindow::println);
    connect(this, &REPLMainWindow::sendCommand,
            &_env, &EnvironmentThread::parseLine);
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
QString
REPLMainWindow::extractCurrentLineFromInput()
{
    auto tmp = ui->lineEdit->text();
    ui->lineEdit->clear();
    ui->lineEdit->setText("");
    return tmp;
}

void
REPLMainWindow::print(const QString& str)
{
    ui->textEdit->append(str);
}

void
REPLMainWindow::println(const QString& str)
{
    print(str);
    ui->textEdit->append(""); // make sure that we newline things correctly
}

void REPLMainWindow::processCommand()
{
    // get the input line
    auto str = extractCurrentLineFromInput();
    emit sendCommand(str);
    // update the console
    println(str);
    moveToBottomOfLog();
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
        ts << ui->textEdit->toPlainText();
        file.commit();
    }
}

void REPLMainWindow::on_actionClear_Console_triggered()
{
    ui->textEdit->clear();
}

void REPLMainWindow::on_lineEdit_returnPressed()
{
    if (ui->lineEdit->isModified()) {
        processCommand();
    }
}

void REPLMainWindow::moveToBottomOfLog()
{
    auto scrollBar = ui->textEdit->verticalScrollBar();
    scrollBar->setValue(scrollBar->maximum());
}
