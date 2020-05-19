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
    ,ui(new Ui::REPLMainWindow)
{
    ui->setupUi(this);
    ui->plainTextEdit->appendPlainText("UI Setup...");
    ui->plainTextEdit->appendPlainText("Creating a CLIPS Environment");
    _env = ::CreateEnvironment();
    if (!_env) {
        QMessageBox::critical(this, "Environment Allocation Problem", "Could not allocate the backing clips environment! Terminating!");
        QCoreApplication::quit();
    }
    ui->plainTextEdit->appendPlainText("CLIPS Environment Successfully Created");
    ui->plainTextEdit->appendPlainText("---------------------------------------");
}

REPLMainWindow::~REPLMainWindow()
{
    delete ui;
    ::DestroyEnvironment(_env);
}


void REPLMainWindow::on_actionAbout_triggered()
{
   QMessageBox::about(this, "About", "Simple CLIPS REPL (C) 2020 Josh Scoggins");
}

void REPLMainWindow::on_actionExit_triggered()
{
    QCoreApplication::quit();
}
void REPLMainWindow::extractCurrentLineFromInput()
{
    _currentLine = ui->lineEdit->text();
    ui->lineEdit->clear();
    ui->lineEdit->setText("");
}
void REPLMainWindow::addTextToCommand()
{
    QTextStream commandStream;
    commandStream.setString(&_commandString);
    commandStream << _currentLine << endl;
}
void REPLMainWindow::processCommand()
{
    // get the input line
    extractCurrentLineFromInput();
    // update the console
    transferTextToConsole();
    addTextToCommand();
    // now that we have added the text to the command stream we must see if CLIPS
    // considers it to be a complete command
    const char* cmd = _commandString.toLocal8Bit().data();
    switch (::CompleteCommand(cmd)) {
    case 0:
        // more input required so just return
        ui->plainTextEdit->appendPlainText("&&");
        break;
    case -1:
        ui->plainTextEdit->appendPlainText("An error occurred in the command stream... clearing out");
        _commandString.clear();
        break;
    case 1:
        [this, cmd](){
        QTextStream commandStream;
        commandStream.setString(&_commandString);
        commandStream << endl;
        FlushPPBuffer(_env);
        SetPPBufferStatus(_env,false);
        RouteCommand(_env, cmd, true);
        FlushPPBuffer(_env);
#if (! BLOAD_ONLY)
        FlushParsingMessages(_env);
#endif
        SetHaltExecution(_env,false);
        SetEvaluationError(_env,false);
        FlushCommandString(_env);
        CleanCurrentGarbageFrame(_env, nullptr);
        _commandString.clear();
    }();
        break;
    }
}
void REPLMainWindow::transferTextToConsole()
{
    ui->plainTextEdit->appendPlainText(_currentLine);
}
void REPLMainWindow::on_submitLine_clicked()
{
    if (ui->lineEdit->isModified()) {
        processCommand();
    }
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
    if (ui->lineEdit->isModified()) {
        processCommand();
    }
}
