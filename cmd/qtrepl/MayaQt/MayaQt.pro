QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

CONFIG += c++17

# The following define makes your compiler emit warnings if you use
# any Qt feature that has been marked deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if it uses deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

SOURCES += \
    ../../../agenda.c \
    ../../../analysis.c \
    ../../../argacces.c \
    ../../../bload.c \
    ../../../bmathfun.c \
    ../../../boost.cc \
    ../../../bsave.c \
    ../../../classcom.c \
    ../../../classexm.c \
    ../../../classfun.c \
    ../../../classinf.c \
    ../../../classini.c \
    ../../../classpsr.c \
    ../../../clsltpsr.c \
    ../../../commline.c \
    ../../../conscomp.c \
    ../../../constrct.c \
    ../../../constrnt.c \
    ../../../crstrtgy.c \
    ../../../cstrcbin.c \
    ../../../cstrccom.c \
    ../../../cstrcpsr.c \
    ../../../cstrnbin.c \
    ../../../cstrnchk.c \
    ../../../cstrncmp.c \
    ../../../cstrnops.c \
    ../../../cstrnpsr.c \
    ../../../cstrnutl.c \
    ../../../default.c \
    ../../../defins.c \
    ../../../developr.c \
    ../../../dffctbin.c \
    ../../../dffctbsc.c \
    ../../../dffctcmp.c \
    ../../../dffctdef.c \
    ../../../dffctpsr.c \
    ../../../dffnxbin.c \
    ../../../dffnxcmp.c \
    ../../../dffnxexe.c \
    ../../../dffnxfun.c \
    ../../../dffnxpsr.c \
    ../../../dfinsbin.c \
    ../../../dfinscmp.c \
    ../../../drive.c \
    ../../../emathfun.c \
    ../../../engine.c \
    ../../../envrnbld.c \
    ../../../envrnmnt.c \
    ../../../evaluatn.c \
    ../../../expressn.c \
    ../../../exprnbin.c \
    ../../../exprnops.c \
    ../../../exprnpsr.c \
    ../../../extnfunc.c \
    ../../../factbin.c \
    ../../../factbld.c \
    ../../../factcmp.c \
    ../../../factcom.c \
    ../../../factfun.c \
    ../../../factgen.c \
    ../../../facthsh.c \
    ../../../factlhs.c \
    ../../../factmch.c \
    ../../../factmngr.c \
    ../../../factprt.c \
    ../../../factqpsr.c \
    ../../../factqury.c \
    ../../../factrete.c \
    ../../../factrhs.c \
    ../../../filecom.c \
    ../../../filertr.c \
    ../../../fileutil.c \
    ../../../functional.cpp \
    ../../../generate.c \
    ../../../genrcbin.c \
    ../../../genrccmp.c \
    ../../../genrccom.c \
    ../../../genrcexe.c \
    ../../../genrcfun.c \
    ../../../genrcpsr.c \
    ../../../globlbin.c \
    ../../../globlbsc.c \
    ../../../globlcmp.c \
    ../../../globlcom.c \
    ../../../globldef.c \
    ../../../globlpsr.c \
    ../../../immthpsr.c \
    ../../../incrrset.c \
    ../../../inherpsr.c \
    ../../../inscom.c \
    ../../../insfile.c \
    ../../../insfun.c \
    ../../../insmngr.c \
    ../../../insmoddp.c \
    ../../../insmult.c \
    ../../../inspsr.c \
    ../../../insquery.c \
    ../../../insqypsr.c \
    ../../../iofun.c \
    ../../../lgcldpnd.c \
    ../../../maya.c \
    ../../../memalloc.c \
    ../../../miscfun.c \
    ../../../modulbin.c \
    ../../../modulbsc.c \
    ../../../modulcmp.c \
    ../../../moduldef.c \
    ../../../modulpsr.c \
    ../../../modulutl.c \
    ../../../msgcom.c \
    ../../../msgfun.c \
    ../../../msgpass.c \
    ../../../msgpsr.c \
    ../../../multifld.c \
    ../../../multifun.c \
    ../../../objbin.c \
    ../../../objcmp.c \
    ../../../objrtbin.c \
    ../../../objrtbld.c \
    ../../../objrtcmp.c \
    ../../../objrtfnx.c \
    ../../../objrtgen.c \
    ../../../objrtmch.c \
    ../../../parsefun.c \
    ../../../pattern.c \
    ../../../pprint.c \
    ../../../prccode.c \
    ../../../prcdrfun.c \
    ../../../prcdrpsr.c \
    ../../../prdctfun.c \
    ../../../prntutil.c \
    ../../../proflfun.c \
    ../../../reorder.c \
    ../../../reteutil.c \
    ../../../retract.c \
    ../../../router.c \
    ../../../rulebin.c \
    ../../../rulebld.c \
    ../../../rulebsc.c \
    ../../../rulecmp.c \
    ../../../rulecom.c \
    ../../../rulecstr.c \
    ../../../ruledef.c \
    ../../../ruledlt.c \
    ../../../rulelhs.c \
    ../../../rulepsr.c \
    ../../../scanner.c \
    ../../../sortfun.c \
    ../../../strngfun.c \
    ../../../strngrtr.c \
    ../../../symblbin.c \
    ../../../symblcmp.c \
    ../../../symbol.c \
    ../../../sysdep.c \
    ../../../taglib_interface.cc \
    ../../../textpro.c \
    ../../../tmpltbin.c \
    ../../../tmpltbsc.c \
    ../../../tmpltcmp.c \
    ../../../tmpltdef.c \
    ../../../tmpltfun.c \
    ../../../tmpltlhs.c \
    ../../../tmpltpsr.c \
    ../../../tmpltrhs.c \
    ../../../tmpltutl.c \
    ../../../userdata.c \
    ../../../userfunctions.c \
    ../../../utility.c \
    ../../../watch.c \
    main.cpp \
    replmainwindow.cpp

HEADERS += \
    ../../../agenda.h \
    ../../../analysis.h \
    ../../../argacces.h \
    ../../../bload.h \
    ../../../bmathfun.h \
    ../../../boost.h \
    ../../../bsave.h \
    ../../../classcom.h \
    ../../../classexm.h \
    ../../../classfun.h \
    ../../../classinf.h \
    ../../../classini.h \
    ../../../classpsr.h \
    ../../../clips.h \
    ../../../clsltpsr.h \
    ../../../commline.h \
    ../../../conscomp.h \
    ../../../constant.h \
    ../../../constrct.h \
    ../../../constrnt.h \
    ../../../crstrtgy.h \
    ../../../cstrcbin.h \
    ../../../cstrccmp.h \
    ../../../cstrccom.h \
    ../../../cstrcpsr.h \
    ../../../cstrnbin.h \
    ../../../cstrnchk.h \
    ../../../cstrncmp.h \
    ../../../cstrnops.h \
    ../../../cstrnpsr.h \
    ../../../cstrnutl.h \
    ../../../default.h \
    ../../../defins.h \
    ../../../developr.h \
    ../../../dffctbin.h \
    ../../../dffctbsc.h \
    ../../../dffctcmp.h \
    ../../../dffctdef.h \
    ../../../dffctpsr.h \
    ../../../dffnxbin.h \
    ../../../dffnxcmp.h \
    ../../../dffnxexe.h \
    ../../../dffnxfun.h \
    ../../../dffnxpsr.h \
    ../../../dfinsbin.h \
    ../../../dfinscmp.h \
    ../../../drive.h \
    ../../../emathfun.h \
    ../../../engine.h \
    ../../../entities.h \
    ../../../envrnbld.h \
    ../../../envrnmnt.h \
    ../../../evaluatn.h \
    ../../../expressn.h \
    ../../../exprnbin.h \
    ../../../exprnops.h \
    ../../../exprnpsr.h \
    ../../../extnfunc.h \
    ../../../factbin.h \
    ../../../factbld.h \
    ../../../factcmp.h \
    ../../../factcom.h \
    ../../../factfun.h \
    ../../../factgen.h \
    ../../../facthsh.h \
    ../../../factlhs.h \
    ../../../factmch.h \
    ../../../factmngr.h \
    ../../../factprt.h \
    ../../../factqpsr.h \
    ../../../factqury.h \
    ../../../factrete.h \
    ../../../factrhs.h \
    ../../../filecom.h \
    ../../../filertr.h \
    ../../../fileutil.h \
    ../../../functional.h \
    ../../../generate.h \
    ../../../genrcbin.h \
    ../../../genrccmp.h \
    ../../../genrccom.h \
    ../../../genrcexe.h \
    ../../../genrcfun.h \
    ../../../genrcpsr.h \
    ../../../globlbin.h \
    ../../../globlbsc.h \
    ../../../globlcmp.h \
    ../../../globlcom.h \
    ../../../globldef.h \
    ../../../globlpsr.h \
    ../../../immthpsr.h \
    ../../../incrrset.h \
    ../../../inherpsr.h \
    ../../../inscom.h \
    ../../../insfile.h \
    ../../../insfun.h \
    ../../../insmngr.h \
    ../../../insmoddp.h \
    ../../../insmult.h \
    ../../../inspsr.h \
    ../../../insquery.h \
    ../../../insqypsr.h \
    ../../../iofun.h \
    ../../../lgcldpnd.h \
    ../../../match.h \
    ../../../maya.h \
    ../../../maya.ino \
    ../../../mayasetup.h \
    ../../../memalloc.h \
    ../../../miscfun.h \
    ../../../modulbin.h \
    ../../../modulbsc.h \
    ../../../modulcmp.h \
    ../../../moduldef.h \
    ../../../modulpsr.h \
    ../../../modulutl.h \
    ../../../msgcom.h \
    ../../../msgfun.h \
    ../../../msgpass.h \
    ../../../msgpsr.h \
    ../../../multifld.h \
    ../../../multifun.h \
    ../../../network.h \
    ../../../objbin.h \
    ../../../objcmp.h \
    ../../../object.h \
    ../../../objrtbin.h \
    ../../../objrtbld.h \
    ../../../objrtcmp.h \
    ../../../objrtfnx.h \
    ../../../objrtgen.h \
    ../../../objrtmch.h \
    ../../../os_shim.h \
    ../../../parsefun.h \
    ../../../pattern.h \
    ../../../platform.h \
    ../../../pprint.h \
    ../../../prccode.h \
    ../../../prcdrfun.h \
    ../../../prcdrpsr.h \
    ../../../prdctfun.h \
    ../../../prntutil.h \
    ../../../proflfun.h \
    ../../../reorder.h \
    ../../../reteutil.h \
    ../../../retract.h \
    ../../../router.h \
    ../../../rulebin.h \
    ../../../rulebld.h \
    ../../../rulebsc.h \
    ../../../rulecmp.h \
    ../../../rulecom.h \
    ../../../rulecstr.h \
    ../../../ruledef.h \
    ../../../ruledlt.h \
    ../../../rulelhs.h \
    ../../../rulepsr.h \
    ../../../scanner.h \
    ../../../setup.h \
    ../../../sortfun.h \
    ../../../strngfun.h \
    ../../../strngrtr.h \
    ../../../symblbin.h \
    ../../../symblcmp.h \
    ../../../symbol.h \
    ../../../sysdep.h \
    ../../../taglib_interface.h \
    ../../../textpro.h \
    ../../../tmpltbin.h \
    ../../../tmpltbsc.h \
    ../../../tmpltcmp.h \
    ../../../tmpltdef.h \
    ../../../tmpltfun.h \
    ../../../tmpltlhs.h \
    ../../../tmpltpsr.h \
    ../../../tmpltrhs.h \
    ../../../tmpltutl.h \
    ../../../userdata.h \
    ../../../usrsetup.h \
    ../../../utility.h \
    ../../../watch.h \
    replmainwindow.h

FORMS += \
    replmainwindow.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

DISTFILES += \
    ../../../LICENSE \
    ../../../LICENSE.CLIPS \
    ../../../Makefile \
    ../../../README \
    ../../../config.mk

unix|win32: LIBS += -lboost_system

unix|win32: LIBS += -lboost_filesystem

unix|win32: LIBS += -ltag
