#include "replmainwindow.h"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    REPLMainWindow w;
    w.show();
    return a.exec();
}
