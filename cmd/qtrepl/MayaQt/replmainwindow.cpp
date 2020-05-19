#include "replmainwindow.h"
#include "ui_replmainwindow.h"

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

