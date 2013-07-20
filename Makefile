########### MAKEFILE FOR ELECTRON ###########
export progroot ?= $(CURDIR)
export srcroot ?= $(progroot)
CC := cc
LD := cc
OUTPUT = electron

ifeq ($(CC),9c)
    $(error Use mk to enable plan9port features)
endif

ifeq ($(LD),9l)
   $(error Use mk to enable plan9port features)
endif

OBJS = agenda.o analysis.o argacces.o bload.o bmathfun.o bsave.o \
 	classcom.o classexm.o classfun.o classinf.o classini.o \
	classpsr.o clsltpsr.o commline.o conscomp.o constrct.o \
 	constrnt.o crstrtgy.o cstrcbin.o cstrccom.o cstrcpsr.o \
 	cstrnbin.o cstrnchk.o cstrncmp.o cstrnops.o cstrnpsr.o \
 	cstrnutl.o default.o defins.o developr.o dffctbin.o dffctbsc.o \
 	dffctcmp.o dffctdef.o dffctpsr.o dffnxbin.o dffnxcmp.o \
	dffnxexe.o dffnxfun.o dffnxpsr.o dfinsbin.o dfinscmp.o drive.o \
	emathfun.o engine.o envrnmnt.o evaluatn.o expressn.o exprnbin.o exprnops.o \
 	exprnpsr.o extnfunc.o factbin.o factbld.o factcmp.o factcom.o \
 	factfun.o factgen.o facthsh.o factlhs.o factmch.o factmngr.o \
 	factprt.o factqpsr.o factqury.o factrete.o factrhs.o filecom.o \
 	filertr.o generate.o genrcbin.o genrccmp.o genrccom.o genrcexe.o \
 	genrcfun.o genrcpsr.o globlbin.o globlbsc.o globlcmp.o globlcom.o \
 	globldef.o globlpsr.o immthpsr.o incrrset.o inherpsr.o \
 	inscom.o insfile.o insfun.o insmngr.o insmoddp.o insmult.o \
 	inspsr.o insquery.o insqypsr.o iofun.o lgcldpnd.o \
 	memalloc.o miscfun.o modulbin.o modulbsc.o modulcmp.o moduldef.o \
 	modulpsr.o modulutl.o msgcom.o msgfun.o msgpass.o msgpsr.o \
 	multifld.o multifun.o objbin.o objcmp.o objrtbin.o objrtbld.o \
 	objrtcmp.o objrtfnx.o objrtgen.o objrtmch.o parsefun.o pattern.o \
 	pprint.o prccode.o prcdrfun.o prcdrpsr.o prdctfun.o prntutil.o \
 	proflfun.o reorder.o reteutil.o retract.o router.o rulebin.o \
 	rulebld.o rulebsc.o rulecmp.o rulecom.o rulecstr.o ruledef.o \
 	ruledlt.o rulelhs.o rulepsr.o scanner.o sortfun.o strngfun.o \
 	strngrtr.o symblbin.o symblcmp.o symbol.o sysdep.o textpro.o \
 	tmpltbin.o tmpltbsc.o tmpltcmp.o tmpltdef.o tmpltfun.o tmpltlhs.o \
 	tmpltpsr.o tmpltrhs.o tmpltutl.o userdata.o userfunctions.o \
 	utility.o watch.o main.o binops.o ArchitectureDetection.o \
 	OSDetection.o HardwareDetection.o Platform.o ShellVariables.o


all: $(OBJS)
	$(LD) $(LDFLAGS) -o $(OUTPUT) $(OBJS) -lm -lncurses

.c.o :
	$(CC) -c $(CFLAGS) -DALLOW_ENVIRONMENT_GLOBALS=0 -D_POSIX_C_SOURCE=200112L \
		-std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit $<

agenda.o: agenda.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h crstrtgy.h agenda.h ruledef.h \
  constrnt.h cstrccom.h network.h match.h pattern.h reorder.h engine.h \
  lgcldpnd.h retract.h memalloc.h modulutl.h multifld.h reteutil.h \
  router.h prntutil.h rulebsc.h strngrtr.h sysdep.h watch.h

analysis.o: analysis.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h exprnpsr.h extnfunc.h expressn.h exprnops.h userdata.h \
  scanner.h pprint.h reorder.h ruledef.h conscomp.h constrct.h moduldef.h \
  modulpsr.h evaluatn.h utility.h symblcmp.h constrnt.h cstrccom.h \
  agenda.h match.h network.h pattern.h generate.h router.h prntutil.h \
  cstrnchk.h cstrnutl.h cstrnops.h rulecstr.h modulutl.h analysis.h \
  globldef.h

argacces.o: argacces.c setup.h envrnmnt.h symbol.h usrsetup.h extnfunc.h \
  expressn.h exprnops.h exprnpsr.h scanner.h pprint.h userdata.h router.h \
  prntutil.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h utility.h cstrnchk.h constrnt.h insfun.h object.h \
  multifld.h match.h network.h ruledef.h cstrccom.h agenda.h pattern.h \
  reorder.h factmngr.h facthsh.h tmpltdef.h factbld.h sysdep.h argacces.h

bload.o: bload.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h bsave.h cstrnbin.h constrnt.h \
  memalloc.h router.h prntutil.h bload.h exprnbin.h sysdep.h symblbin.h

bmathfun.o: bmathfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h router.h prntutil.h bmathfun.h

bsave.o: bsave.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h bload.h exprnbin.h sysdep.h symblbin.h \
  cstrnbin.h constrnt.h memalloc.h router.h prntutil.h bsave.h

classcom.o: classcom.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h argacces.h \
  evaluatn.h constant.h moduldef.h conscomp.h constrct.h symblcmp.h \
  modulpsr.h classfun.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h cstrccom.h agenda.h pattern.h reorder.h classini.h modulutl.h \
  msgcom.h msgpass.h router.h prntutil.h classcom.h

classexm.o: classexm.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h classini.h insfun.h memalloc.h msgcom.h msgpass.h \
  msgfun.h router.h prntutil.h strngrtr.h sysdep.h classexm.h

classfun.o: classfun.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h agenda.h pattern.h reorder.h classini.h cstrcpsr.h inscom.h \
  insfun.h insmngr.h memalloc.h modulutl.h msgfun.h msgpass.h router.h \
  prntutil.h classfun.h

classinf.o: classinf.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classexm.h classfun.h classini.h memalloc.h insfun.h msgcom.h \
  msgpass.h msgfun.h prntutil.h classinf.h

classini.o: classini.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classexm.h \
  classfun.h classinf.h classpsr.h cstrcpsr.h inscom.h insfun.h \
  memalloc.h modulutl.h msgcom.h msgpass.h watch.h defins.h insquery.h \
  bload.h exprnbin.h sysdep.h symblbin.h objbin.h objcmp.h objrtbld.h \
  objrtfnx.h objrtmch.h classini.h

classpsr.o: classpsr.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h agenda.h pattern.h reorder.h classfun.h clsltpsr.h cstrcpsr.h \
  inherpsr.h memalloc.h modulutl.h msgpsr.h router.h prntutil.h \
  classpsr.h

clsltpsr.o: clsltpsr.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  cstrnchk.h cstrnpsr.h cstrnutl.h default.h insfun.h memalloc.h \
  prntutil.h router.h clsltpsr.h

commline.o: commline.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  argacces.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h evaluatn.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h cstrcpsr.h filecom.h memalloc.h \
  prcdrfun.h prcdrpsr.h constrnt.h router.h prntutil.h strngrtr.h \
  sysdep.h commline.h

conscomp.o: conscomp.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  constant.h exprnpsr.h extnfunc.h expressn.h exprnops.h userdata.h \
  scanner.h pprint.h cstrccom.h moduldef.h conscomp.h constrct.h \
  evaluatn.h symblcmp.h modulpsr.h utility.h argacces.h cstrncmp.h \
  constrnt.h router.h prntutil.h sysdep.h modulcmp.h network.h match.h \
  pattern.h reorder.h ruledef.h agenda.h dffnxcmp.h dffnxfun.h tmpltcmp.h \
  globlcmp.h genrccmp.h genrcfun.h object.h multifld.h objcmp.h

constrct.o: constrct.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h watch.h prcdrfun.h \
  prcdrpsr.h constrnt.h argacces.h multifld.h sysdep.h commline.h \
  ruledef.h cstrccom.h agenda.h match.h network.h pattern.h reorder.h

constrnt.o: constrnt.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h multifld.h router.h \
  prntutil.h constrnt.h

crstrtgy.o: crstrtgy.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  pattern.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h match.h network.h ruledef.h conscomp.h \
  constrct.h moduldef.h modulpsr.h utility.h symblcmp.h constrnt.h \
  cstrccom.h agenda.h reorder.h reteutil.h argacces.h memalloc.h \
  crstrtgy.h

cstrcbin.o: cstrcbin.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h moduldef.h \
  conscomp.h constrct.h evaluatn.h constant.h symblcmp.h modulpsr.h \
  cstrcbin.h

cstrccom.o: cstrccom.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h pprint.h \
  symblcmp.h modulpsr.h utility.h argacces.h multifld.h modulutl.h \
  router.h prntutil.h commline.h sysdep.h bload.h exprnbin.h symblbin.h \
  cstrcpsr.h cstrccom.h

cstrcpsr.o: cstrcpsr.c setup.h envrnmnt.h symbol.h usrsetup.h router.h \
  prntutil.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h watch.h prcdrpsr.h constrnt.h \
  modulutl.h sysdep.h cstrcpsr.h

cstrnbin.o: cstrnbin.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h bload.h exprnbin.h \
  sysdep.h symblbin.h bsave.h cstrnbin.h constrnt.h

cstrnchk.o: cstrnchk.c setup.h envrnmnt.h symbol.h usrsetup.h router.h \
  prntutil.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h multifld.h cstrnutl.h \
  constrnt.h inscom.h object.h match.h network.h ruledef.h cstrccom.h \
  agenda.h pattern.h reorder.h insfun.h classcom.h classexm.h cstrnchk.h

cstrncmp.o: cstrncmp.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h memalloc.h router.h prntutil.h sysdep.h cstrncmp.h \
  constrnt.h

cstrnops.o: cstrnops.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h multifld.h \
  constrnt.h cstrnchk.h cstrnutl.h cstrnops.h

cstrnpsr.o: cstrnpsr.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h cstrnutl.h \
  constrnt.h cstrnchk.h sysdep.h cstrnpsr.h

cstrnutl.o: cstrnutl.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h multifld.h \
  argacces.h cstrnutl.h constrnt.h

default.o: default.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  constrnt.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h cstrnchk.h multifld.h inscom.h object.h \
  constrct.h moduldef.h conscomp.h symblcmp.h modulpsr.h utility.h \
  match.h network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h \
  insfun.h router.h prntutil.h factmngr.h facthsh.h tmpltdef.h factbld.h \
  cstrnutl.h default.h

defins.o: defins.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h dfinsbin.h defins.h \
  conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h constant.h \
  symblcmp.h cstrccom.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h agenda.h pattern.h reorder.h dfinscmp.h argacces.h classcom.h \
  classfun.h cstrcpsr.h insfun.h inspsr.h memalloc.h router.h prntutil.h

developr.o: developr.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h inscom.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h \
  insfun.h modulutl.h router.h prntutil.h tmpltdef.h factbld.h factmngr.h \
  facthsh.h classcom.h classfun.h objrtmch.h developr.h

dffctbin.o: dffctbin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  dffctdef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h cstrccom.h bload.h exprnbin.h \
  sysdep.h symblbin.h bsave.h dffctbin.h modulbin.h cstrcbin.h

dffctbsc.o: dffctbsc.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h router.h prntutil.h \
  cstrccom.h factrhs.h factmngr.h facthsh.h pattern.h match.h network.h \
  ruledef.h constrnt.h agenda.h reorder.h multifld.h tmpltdef.h factbld.h \
  cstrcpsr.h dffctpsr.h dffctdef.h dffctbin.h modulbin.h cstrcbin.h \
  dffctcmp.h dffctbsc.h

dffctcmp.o: dffctcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h dffctdef.h cstrccom.h dffctcmp.h

dffctdef.o: dffctdef.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  dffctpsr.h dffctbsc.h evaluatn.h constant.h expressn.h exprnops.h \
  exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h bload.h utility.h \
  exprnbin.h sysdep.h symblbin.h dffctbin.h modulbin.h moduldef.h \
  conscomp.h constrct.h symblcmp.h modulpsr.h cstrcbin.h dffctcmp.h \
  dffctdef.h cstrccom.h

dffctpsr.o: dffctpsr.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h cstrcpsr.h factrhs.h \
  factmngr.h facthsh.h pattern.h match.h network.h ruledef.h constrnt.h \
  cstrccom.h agenda.h reorder.h multifld.h tmpltdef.h factbld.h bload.h \
  exprnbin.h sysdep.h symblbin.h dffctdef.h dffctbsc.h dffctpsr.h

dffnxbin.o: dffnxbin.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h memalloc.h \
  cstrcbin.h constrct.h moduldef.h conscomp.h symblcmp.h modulpsr.h \
  evaluatn.h constant.h modulbin.h dffnxbin.h dffnxfun.h cstrccom.h

dffnxcmp.o: dffnxcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h dffnxcmp.h dffnxfun.h cstrccom.h

dffnxexe.o: dffnxexe.c setup.h envrnmnt.h symbol.h usrsetup.h constrct.h \
  moduldef.h conscomp.h extnfunc.h expressn.h exprnops.h exprnpsr.h \
  scanner.h pprint.h userdata.h symblcmp.h modulpsr.h evaluatn.h \
  constant.h utility.h prcdrfun.h prccode.h proflfun.h router.h \
  prntutil.h watch.h dffnxexe.h dffnxfun.h cstrccom.h

dffnxfun.o: dffnxfun.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h dffnxbin.h \
  dffnxfun.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h symblcmp.h cstrccom.h dffnxcmp.h cstrcpsr.h dffnxpsr.h \
  dffnxexe.h watch.h argacces.h memalloc.h router.h prntutil.h

dffnxpsr.o: dffnxpsr.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h network.h match.h \
  evaluatn.h constant.h pattern.h reorder.h ruledef.h conscomp.h \
  constrct.h moduldef.h modulpsr.h symblcmp.h constrnt.h cstrccom.h \
  agenda.h genrccom.h genrcfun.h object.h multifld.h cstrcpsr.h \
  dffnxfun.h memalloc.h prccode.h router.h prntutil.h dffnxpsr.h

dfinsbin.o: dfinsbin.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h memalloc.h \
  cstrcbin.h constrct.h moduldef.h conscomp.h symblcmp.h modulpsr.h \
  evaluatn.h constant.h defins.h cstrccom.h object.h constrnt.h \
  multifld.h match.h network.h ruledef.h agenda.h pattern.h reorder.h \
  modulbin.h dfinsbin.h

dfinscmp.o: dfinscmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h defins.h cstrccom.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h dfinscmp.h

drive.o: drive.c setup.h envrnmnt.h symbol.h usrsetup.h agenda.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h cstrccom.h network.h \
  match.h pattern.h reorder.h engine.h lgcldpnd.h retract.h memalloc.h \
  prntutil.h reteutil.h router.h incrrset.h drive.h

emathfun.o: emathfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h router.h prntutil.h emathfun.h


engine.o: engine.c setup.h envrnmnt.h symbol.h usrsetup.h agenda.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h cstrccom.h network.h \
  match.h pattern.h reorder.h argacces.h factmngr.h facthsh.h multifld.h \
  tmpltdef.h factbld.h inscom.h object.h insfun.h memalloc.h modulutl.h \
  prccode.h prcdrfun.h proflfun.h reteutil.h retract.h router.h \
  prntutil.h ruledlt.h sysdep.h watch.h engine.h lgcldpnd.h

envrnmnt.o: envrnmnt.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  prntutil.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h router.h engine.h lgcldpnd.h \
  match.h network.h ruledef.h constrnt.h cstrccom.h agenda.h pattern.h \
  reorder.h retract.h sysdep.h

evaluatn.o: evaluatn.c setup.h envrnmnt.h symbol.h usrsetup.h commline.h \
  constant.h memalloc.h router.h prntutil.h moduldef.h conscomp.h \
  constrct.h userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h scanner.h pprint.h symblcmp.h modulpsr.h utility.h \
  prcdrfun.h multifld.h factmngr.h facthsh.h pattern.h match.h network.h \
  ruledef.h constrnt.h cstrccom.h agenda.h reorder.h tmpltdef.h factbld.h \
  proflfun.h sysdep.h dffnxfun.h genrccom.h genrcfun.h object.h

expressn.o: expressn.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h memalloc.h router.h \
  prntutil.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h

exprnbin.o: exprnbin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  dffctdef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h cstrccom.h bload.h exprnbin.h \
  sysdep.h symblbin.h bsave.h network.h match.h pattern.h reorder.h \
  ruledef.h constrnt.h agenda.h genrcbin.h genrcfun.h object.h multifld.h \
  dffnxbin.h dffnxfun.h tmpltbin.h cstrcbin.h modulbin.h tmpltdef.h \
  factbld.h factmngr.h facthsh.h globlbin.h globldef.h objbin.h insfun.h \
  inscom.h

exprnops.o: exprnops.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h cstrnchk.h \
  constrnt.h cstrnutl.h cstrnops.h

exprnpsr.o: exprnpsr.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h strngrtr.h memalloc.h \
  argacces.h cstrnchk.h constrnt.h modulutl.h prcdrfun.h network.h \
  match.h pattern.h reorder.h ruledef.h cstrccom.h agenda.h genrccom.h \
  genrcfun.h object.h multifld.h dffnxfun.h

extnfunc.o: extnfunc.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h memalloc.h

factbin.o: factbin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  tmpltdef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h factbld.h pattern.h \
  match.h network.h ruledef.h cstrccom.h agenda.h reorder.h factmngr.h \
  facthsh.h multifld.h bload.h exprnbin.h sysdep.h symblbin.h bsave.h \
  reteutil.h rulebin.h modulbin.h cstrcbin.h factbin.h

factbld.o: factbld.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  reteutil.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h match.h network.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h pattern.h reorder.h router.h prntutil.h \
  factcmp.h factmch.h factmngr.h facthsh.h multifld.h tmpltdef.h \
  factbld.h factgen.h factlhs.h argacces.h modulutl.h

factcmp.o: factcmp.c setup.h envrnmnt.h symbol.h usrsetup.h factbld.h \
  pattern.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h match.h network.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h reorder.h factcmp.h tmpltdef.h \
  factmngr.h facthsh.h multifld.h

factcom.o: factcom.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  exprnpsr.h extnfunc.h expressn.h exprnops.h userdata.h scanner.h \
  pprint.h factmngr.h facthsh.h conscomp.h constrct.h moduldef.h \
  modulpsr.h evaluatn.h constant.h utility.h symblcmp.h pattern.h match.h \
  network.h ruledef.h constrnt.h cstrccom.h agenda.h reorder.h multifld.h \
  tmpltdef.h factbld.h argacces.h router.h prntutil.h factrhs.h factmch.h \
  tmpltpsr.h tmpltutl.h modulutl.h strngrtr.h tmpltfun.h sysdep.h bload.h \
  exprnbin.h symblbin.h factcom.h

factfun.o: factfun.c setup.h envrnmnt.h symbol.h usrsetup.h extnfunc.h \
  expressn.h exprnops.h exprnpsr.h scanner.h pprint.h userdata.h \
  argacces.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h prntutil.h tmpltutl.h factmngr.h \
  facthsh.h pattern.h match.h network.h ruledef.h constrnt.h cstrccom.h \
  agenda.h reorder.h multifld.h tmpltdef.h factbld.h router.h sysdep.h \
  factfun.h

factgen.o: factgen.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h network.h match.h \
  pattern.h reorder.h ruledef.h constrnt.h cstrccom.h agenda.h reteutil.h \
  factmch.h factmngr.h facthsh.h multifld.h tmpltdef.h factbld.h \
  factrete.h factprt.h tmpltlhs.h factgen.h

facthsh.o: facthsh.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h sysdep.h lgcldpnd.h \
  match.h network.h ruledef.h constrnt.h cstrccom.h agenda.h pattern.h \
  reorder.h facthsh.h factmngr.h multifld.h tmpltdef.h factbld.h

factlhs.o: factlhs.c setup.h envrnmnt.h symbol.h usrsetup.h cstrcpsr.h \
  evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h constrct.h moduldef.h conscomp.h \
  symblcmp.h modulpsr.h utility.h pattern.h match.h network.h ruledef.h \
  constrnt.h cstrccom.h agenda.h reorder.h router.h prntutil.h tmpltpsr.h \
  tmpltdef.h factbld.h factmngr.h facthsh.h multifld.h tmpltlhs.h \
  tmpltutl.h modulutl.h factlhs.h

factmch.o: factmch.c setup.h envrnmnt.h symbol.h usrsetup.h drive.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h match.h evaluatn.h constant.h network.h ruledef.h conscomp.h \
  constrct.h moduldef.h modulpsr.h utility.h symblcmp.h constrnt.h \
  cstrccom.h agenda.h pattern.h reorder.h engine.h lgcldpnd.h retract.h \
  factgen.h factrete.h incrrset.h memalloc.h reteutil.h router.h \
  prntutil.h sysdep.h tmpltdef.h factbld.h factmngr.h facthsh.h \
  multifld.h factmch.h

factmngr.o: factmngr.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h exprnpsr.h extnfunc.h expressn.h exprnops.h userdata.h \
  scanner.h pprint.h argacces.h evaluatn.h moduldef.h conscomp.h \
  constrct.h symblcmp.h modulpsr.h utility.h router.h prntutil.h \
  strngrtr.h match.h network.h ruledef.h constrnt.h cstrccom.h agenda.h \
  pattern.h reorder.h factbld.h factqury.h factmngr.h facthsh.h \
  multifld.h tmpltdef.h reteutil.h retract.h factcmp.h filecom.h \
  factfun.h factcom.h factrhs.h factmch.h watch.h factbin.h default.h \
  commline.h sysdep.h engine.h lgcldpnd.h drive.h ruledlt.h tmpltbsc.h \
  tmpltutl.h tmpltfun.h

factprt.o: factprt.c setup.h envrnmnt.h symbol.h usrsetup.h router.h \
  prntutil.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h factgen.h reorder.h ruledef.h \
  constrnt.h cstrccom.h agenda.h match.h network.h pattern.h factprt.h

factqpsr.o: factqpsr.c setup.h envrnmnt.h symbol.h usrsetup.h exprnpsr.h \
  extnfunc.h expressn.h exprnops.h userdata.h scanner.h pprint.h \
  factqury.h factmngr.h facthsh.h conscomp.h constrct.h moduldef.h \
  modulpsr.h evaluatn.h constant.h utility.h symblcmp.h pattern.h match.h \
  network.h ruledef.h constrnt.h cstrccom.h agenda.h reorder.h multifld.h \
  tmpltdef.h factbld.h modulutl.h prcdrpsr.h prntutil.h router.h \
  strngrtr.h factqpsr.h

factqury.o: factqury.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h modulutl.h tmpltutl.h \
  factmngr.h facthsh.h pattern.h match.h network.h ruledef.h constrnt.h \
  cstrccom.h agenda.h reorder.h multifld.h tmpltdef.h factbld.h insfun.h \
  object.h factqpsr.h prcdrfun.h router.h prntutil.h factqury.h

factrete.o: factrete.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h pprint.h \
  userdata.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  evaluatn.h constant.h symblcmp.h modulpsr.h utility.h incrrset.h \
  ruledef.h constrnt.h cstrccom.h agenda.h match.h network.h pattern.h \
  reorder.h reteutil.h drive.h engine.h lgcldpnd.h retract.h factgen.h \
  factmch.h factmngr.h facthsh.h multifld.h tmpltdef.h factbld.h \
  factrete.h

factrhs.o: factrhs.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h pprint.h \
  userdata.h modulutl.h moduldef.h conscomp.h constrct.h evaluatn.h \
  symblcmp.h modulpsr.h utility.h pattern.h match.h network.h ruledef.h \
  constrnt.h cstrccom.h agenda.h reorder.h prntutil.h cstrcpsr.h bload.h \
  exprnbin.h sysdep.h symblbin.h tmpltpsr.h tmpltdef.h factbld.h \
  factmngr.h facthsh.h multifld.h tmpltrhs.h tmpltutl.h strngrtr.h \
  router.h factrhs.h

filecom.o: filecom.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h commline.h cstrcpsr.h memalloc.h \
  prcdrfun.h router.h prntutil.h strngrtr.h sysdep.h filecom.h bsave.h \
  bload.h exprnbin.h symblbin.h

filertr.o: filertr.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h sysdep.h filertr.h

generate.o: generate.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h exprnpsr.h extnfunc.h expressn.h exprnops.h userdata.h \
  scanner.h pprint.h argacces.h evaluatn.h moduldef.h conscomp.h \
  constrct.h symblcmp.h modulpsr.h utility.h router.h prntutil.h \
  ruledef.h constrnt.h cstrccom.h agenda.h match.h network.h pattern.h \
  reorder.h generate.h globlpsr.h

genrcbin.o: genrcbin.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h bload.h utility.h extnfunc.h expressn.h exprnops.h \
  exprnpsr.h scanner.h pprint.h userdata.h exprnbin.h sysdep.h symblbin.h \
  bsave.h cstrcbin.h constrct.h moduldef.h conscomp.h symblcmp.h \
  modulpsr.h evaluatn.h objbin.h object.h constrnt.h multifld.h match.h \
  network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h genrccom.h \
  genrcfun.h modulbin.h genrcbin.h router.h prntutil.h

genrccmp.o: genrccmp.c setup.h envrnmnt.h symbol.h usrsetup.h network.h \
  match.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h pattern.h reorder.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h genrccom.h genrcfun.h object.h \
  multifld.h objcmp.h genrccmp.h

genrccom.o: genrccom.c setup.h envrnmnt.h symbol.h usrsetup.h network.h \
  match.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h pattern.h reorder.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h bload.h exprnbin.h sysdep.h symblbin.h \
  genrcbin.h genrcfun.h object.h multifld.h genrccmp.h genrcpsr.h \
  classcom.h inscom.h insfun.h watch.h argacces.h cstrcpsr.h genrcexe.h \
  memalloc.h router.h prntutil.h genrccom.h

genrcexe.o: genrcexe.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  insfun.h argacces.h genrccom.h genrcfun.h prcdrfun.h prccode.h \
  proflfun.h router.h prntutil.h genrcexe.h

genrcfun.o: genrcfun.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h agenda.h pattern.h reorder.h classfun.h argacces.h cstrcpsr.h \
  genrccom.h genrcfun.h genrcexe.h memalloc.h prccode.h router.h \
  prntutil.h

genrcpsr.o: genrcpsr.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h dffnxfun.h \
  conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h constant.h \
  symblcmp.h cstrccom.h classfun.h object.h constrnt.h multifld.h match.h \
  network.h ruledef.h agenda.h pattern.h reorder.h classcom.h memalloc.h \
  cstrcpsr.h genrccom.h genrcfun.h immthpsr.h modulutl.h prcdrpsr.h \
  prccode.h router.h prntutil.h genrcpsr.h

globlbin.o: globlbin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  multifld.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h globldef.h conscomp.h \
  constrct.h moduldef.h modulpsr.h utility.h symblcmp.h cstrccom.h \
  bload.h exprnbin.h sysdep.h symblbin.h bsave.h globlbsc.h globlbin.h \
  modulbin.h cstrcbin.h

globlbsc.o: globlbsc.c setup.h envrnmnt.h symbol.h usrsetup.h constrct.h \
  moduldef.h conscomp.h extnfunc.h expressn.h exprnops.h exprnpsr.h \
  scanner.h pprint.h userdata.h symblcmp.h modulpsr.h evaluatn.h \
  constant.h utility.h watch.h globlcom.h globldef.h cstrccom.h \
  globlbin.h modulbin.h cstrcbin.h globlcmp.h globlbsc.h

globlcmp.o: globlcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h globldef.h cstrccom.h globlcmp.h

globlcom.o: globlcom.c setup.h envrnmnt.h symbol.h usrsetup.h extnfunc.h \
  expressn.h exprnops.h exprnpsr.h scanner.h pprint.h userdata.h \
  argacces.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h prntutil.h router.h globldef.h \
  cstrccom.h globlcom.h

globldef.o: globldef.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  modulpsr.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h moduldef.h conscomp.h \
  constrct.h symblcmp.h utility.h multifld.h router.h prntutil.h \
  strngrtr.h modulutl.h globlbsc.h globlpsr.h globlcom.h commline.h \
  bload.h exprnbin.h sysdep.h symblbin.h globlbin.h modulbin.h cstrcbin.h \
  globldef.h cstrccom.h globlcmp.h

globlpsr.o: globlpsr.c setup.h envrnmnt.h symbol.h usrsetup.h pprint.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h symblcmp.h modulpsr.h utility.h memalloc.h multifld.h watch.h \
  modulutl.h cstrcpsr.h globldef.h cstrccom.h globlbsc.h bload.h \
  exprnbin.h sysdep.h symblbin.h globlpsr.h

immthpsr.o: immthpsr.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  memalloc.h cstrnutl.h genrcpsr.h genrcfun.h prccode.h immthpsr.h

incrrset.o: incrrset.c setup.h envrnmnt.h symbol.h usrsetup.h agenda.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h cstrccom.h network.h \
  match.h pattern.h reorder.h argacces.h drive.h engine.h lgcldpnd.h \
  retract.h router.h prntutil.h reteutil.h incrrset.h

inherpsr.o: inherpsr.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  memalloc.h modulutl.h router.h prntutil.h inherpsr.h

inscom.o: inscom.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h classinf.h insfile.h insfun.h insmngr.h insmoddp.h \
  insmult.h inspsr.h lgcldpnd.h memalloc.h msgcom.h msgpass.h msgfun.h \
  router.h prntutil.h strngrtr.h sysdep.h commline.h inscom.h

insfile.o: insfile.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h memalloc.h inscom.h insfun.h insmngr.h inspsr.h \
  router.h prntutil.h strngrtr.h symblbin.h sysdep.h factmngr.h facthsh.h \
  tmpltdef.h factbld.h insfile.h

insfun.o: insfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h cstrnchk.h engine.h lgcldpnd.h retract.h inscom.h \
  insfun.h insmngr.h memalloc.h modulutl.h msgcom.h msgpass.h msgfun.h \
  prccode.h router.h prntutil.h drive.h objrtmch.h

insmngr.o: insmngr.c setup.h envrnmnt.h symbol.h usrsetup.h network.h \
  match.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h pattern.h reorder.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h drive.h objrtmch.h object.h multifld.h \
  lgcldpnd.h classcom.h classfun.h engine.h retract.h memalloc.h insfun.h \
  modulutl.h msgcom.h msgpass.h msgfun.h prccode.h router.h prntutil.h \
  sysdep.h insmngr.h inscom.h watch.h

insmoddp.o: insmoddp.c setup.h envrnmnt.h symbol.h usrsetup.h network.h \
  match.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h pattern.h reorder.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h objrtmch.h object.h multifld.h \
  argacces.h memalloc.h inscom.h insfun.h insmngr.h inspsr.h miscfun.h \
  msgcom.h msgpass.h msgfun.h prccode.h router.h prntutil.h insmoddp.h

insmult.o: insmult.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h insfun.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h \
  msgfun.h msgpass.h multifun.h router.h prntutil.h insmult.h

inspsr.o: inspsr.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  classinf.h prntutil.h router.h inspsr.h

insquery.o: insquery.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h memalloc.h insfun.h insmngr.h insqypsr.h \
  prcdrfun.h router.h prntutil.h insquery.h

insqypsr.o: insqypsr.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h insquery.h \
  prcdrpsr.h prntutil.h router.h strngrtr.h insqypsr.h

iofun.o: iofun.c setup.h envrnmnt.h symbol.h usrsetup.h router.h \
  prntutil.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h strngrtr.h filertr.h \
  argacces.h memalloc.h commline.h sysdep.h iofun.h

lgcldpnd.o: lgcldpnd.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h engine.h lgcldpnd.h \
  match.h network.h ruledef.h constrnt.h cstrccom.h agenda.h pattern.h \
  reorder.h retract.h reteutil.h argacces.h factmngr.h facthsh.h \
  multifld.h tmpltdef.h factbld.h insfun.h object.h
  
main.o: main.c setup.h envrnmnt.h symbol.h usrsetup.h clips.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h cstrcpsr.h filecom.h \
  strngfun.h commline.h router.h prntutil.h filertr.h sysdep.h bmathfun.h \
  watch.h modulbsc.h bload.h exprnbin.h symblbin.h bsave.h ruledef.h \
  constrnt.h cstrccom.h agenda.h match.h network.h pattern.h reorder.h \
  rulebsc.h engine.h lgcldpnd.h retract.h drive.h incrrset.h rulecom.h \
  crstrtgy.h dffctdef.h dffctbsc.h tmpltdef.h factbld.h factmngr.h \
  facthsh.h multifld.h tmpltbsc.h tmpltfun.h factcom.h factfun.h \
  globldef.h globlbsc.h globlcom.h dffnxfun.h genrccom.h genrcfun.h \
  object.h classcom.h classexm.h classinf.h classini.h defins.h inscom.h \
  insfun.h insfile.h msgcom.h msgpass.h objrtmch.h

memalloc.o: memalloc.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h

miscfun.o: miscfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h multifld.h router.h \
  prntutil.h sysdep.h dffnxfun.h cstrccom.h miscfun.h

modulbin.o: modulbin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  constrct.h moduldef.h conscomp.h extnfunc.h expressn.h exprnops.h \
  exprnpsr.h scanner.h pprint.h userdata.h symblcmp.h modulpsr.h \
  evaluatn.h constant.h utility.h bload.h exprnbin.h sysdep.h symblbin.h \
  bsave.h modulbin.h

modulbsc.o: modulbsc.c setup.h envrnmnt.h symbol.h usrsetup.h constrct.h \
  moduldef.h conscomp.h extnfunc.h expressn.h exprnops.h exprnpsr.h \
  scanner.h pprint.h userdata.h symblcmp.h modulpsr.h evaluatn.h \
  constant.h utility.h modulbin.h prntutil.h modulcmp.h router.h \
  argacces.h bload.h exprnbin.h sysdep.h symblbin.h modulbsc.h

modulcmp.o: modulcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h sysdep.h modulcmp.h

moduldef.o: moduldef.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  constant.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h argacces.h \
  modulcmp.h modulbsc.h bload.h exprnbin.h sysdep.h symblbin.h modulbin.h

modulpsr.o: modulpsr.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  constant.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h argacces.h \
  cstrcpsr.h modulutl.h bload.h exprnbin.h sysdep.h symblbin.h

modulutl.o: modulutl.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h sysdep.h modulutl.h

msgcom.o: msgcom.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h classinf.h insfun.h insmoddp.h msgfun.h msgpass.h \
  memalloc.h prccode.h router.h prntutil.h bload.h exprnbin.h sysdep.h \
  symblbin.h msgpsr.h watch.h msgcom.h

msgfun.o: msgfun.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  memalloc.h insfun.h msgcom.h msgpass.h prccode.h router.h prntutil.h \
  msgfun.h

msgpass.o: msgpass.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h classfun.h memalloc.h insfun.h msgcom.h msgpass.h msgfun.h \
  prcdrfun.h prccode.h proflfun.h router.h prntutil.h strngfun.h \
  commline.h inscom.h

msgpsr.o: msgpsr.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h agenda.h pattern.h reorder.h classfun.h memalloc.h cstrcpsr.h \
  cstrnchk.h insfun.h msgcom.h msgpass.h msgfun.h prccode.h router.h \
  prntutil.h strngrtr.h msgpsr.h

multifld.o: multifld.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h router.h prntutil.h moduldef.h conscomp.h \
  constrct.h symblcmp.h modulpsr.h utility.h strngrtr.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h cstrccom.h agenda.h \
  pattern.h reorder.h

multifun.o: multifun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h multifld.h multifun.h \
  prcdrpsr.h constrnt.h prcdrfun.h router.h prntutil.h object.h match.h \
  network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h

objbin.o: objbin.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h evaluatn.h constant.h \
  symblcmp.h modulpsr.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h agenda.h pattern.h reorder.h classfun.h classini.h cstrcbin.h \
  cstrnbin.h insfun.h memalloc.h modulbin.h msgcom.h msgpass.h msgfun.h \
  prntutil.h router.h objbin.h

objcmp.o: objcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h classcom.h cstrccom.h object.h constrnt.h \
  multifld.h match.h network.h ruledef.h agenda.h pattern.h reorder.h \
  classfun.h classini.h cstrncmp.h objrtfnx.h objrtmch.h sysdep.h \
  objcmp.h

objrtbin.o: objrtbin.c setup.h envrnmnt.h symbol.h usrsetup.h bload.h \
  utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h memalloc.h \
  insfun.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h object.h constrnt.h multifld.h match.h network.h \
  ruledef.h cstrccom.h agenda.h pattern.h reorder.h objrtmch.h reteutil.h \
  rulebin.h modulbin.h cstrcbin.h objrtbin.h

objrtbld.o: objrtbld.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  cstrnutl.h cstrnchk.h cstrnops.h drive.h inscom.h insfun.h insmngr.h \
  memalloc.h reteutil.h rulepsr.h objrtmch.h objrtgen.h objrtfnx.h \
  router.h prntutil.h objrtcmp.h objrtbin.h objrtbld.h

objrtcmp.o: objrtcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h objrtfnx.h object.h constrnt.h multifld.h match.h \
  network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h objrtmch.h \
  sysdep.h objrtcmp.h

objrtfnx.o: objrtfnx.c setup.h envrnmnt.h symbol.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h object.h constrnt.h multifld.h \
  match.h network.h ruledef.h agenda.h pattern.h reorder.h classfun.h \
  bload.h exprnbin.h sysdep.h symblbin.h drive.h engine.h lgcldpnd.h \
  retract.h memalloc.h objrtmch.h reteutil.h router.h prntutil.h \
  objrtfnx.h

objrtgen.o: objrtgen.c setup.h envrnmnt.h symbol.h usrsetup.h classfun.h \
  object.h constrct.h moduldef.h conscomp.h extnfunc.h expressn.h \
  exprnops.h exprnpsr.h scanner.h pprint.h userdata.h symblcmp.h \
  modulpsr.h evaluatn.h constant.h utility.h constrnt.h multifld.h \
  match.h network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h \
  objrtfnx.h objrtmch.h objrtgen.h

objrtmch.o: objrtmch.c setup.h envrnmnt.h symbol.h usrsetup.h classfun.h \
  object.h constrct.h moduldef.h conscomp.h extnfunc.h expressn.h \
  exprnops.h exprnpsr.h scanner.h pprint.h userdata.h symblcmp.h \
  modulpsr.h evaluatn.h constant.h utility.h constrnt.h multifld.h \
  match.h network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h \
  memalloc.h drive.h engine.h lgcldpnd.h retract.h incrrset.h reteutil.h \
  ruledlt.h router.h prntutil.h objrtfnx.h objrtmch.h insmngr.h

parsefun.o: parsefun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h cstrcpsr.h memalloc.h multifld.h \
  prcdrpsr.h constrnt.h router.h prntutil.h strngrtr.h parsefun.h

pattern.o: pattern.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  constrnt.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h cstrnchk.h cstrnutl.h match.h network.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h utility.h \
  symblcmp.h cstrccom.h agenda.h pattern.h reorder.h memalloc.h \
  reteutil.h router.h prntutil.h rulecmp.h

pprint.o: pprint.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h sysdep.h utility.h pprint.h

prccode.o: prccode.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  constant.h globlpsr.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h multifld.h evaluatn.h object.h constrct.h \
  moduldef.h conscomp.h symblcmp.h modulpsr.h utility.h constrnt.h \
  match.h network.h ruledef.h cstrccom.h agenda.h pattern.h reorder.h \
  prcdrpsr.h router.h prntutil.h prccode.h

prcdrfun.o: prcdrfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h constrnt.h cstrnchk.h cstrnops.h \
  memalloc.h multifld.h prcdrpsr.h router.h prntutil.h prcdrfun.h \
  globldef.h cstrccom.h

prcdrpsr.o: prcdrpsr.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h constrnt.h cstrnchk.h cstrnops.h \
  cstrnutl.h memalloc.h modulutl.h multifld.h router.h prntutil.h \
  prcdrpsr.h globldef.h cstrccom.h globlpsr.h

prdctfun.o: prdctfun.c setup.h envrnmnt.h symbol.h usrsetup.h exprnpsr.h \
  extnfunc.h expressn.h exprnops.h userdata.h scanner.h pprint.h \
  argacces.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h multifld.h router.h prntutil.h \
  prdctfun.h

prntutil.o: prntutil.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  utility.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h argacces.h moduldef.h conscomp.h \
  constrct.h symblcmp.h modulpsr.h router.h prntutil.h multifun.h \
  factmngr.h facthsh.h pattern.h match.h network.h ruledef.h constrnt.h \
  cstrccom.h agenda.h reorder.h multifld.h tmpltdef.h factbld.h inscom.h \
  object.h insfun.h insmngr.h memalloc.h sysdep.h

proflfun.o: proflfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h classcom.h cstrccom.h object.h \
  constrnt.h multifld.h match.h network.h ruledef.h agenda.h pattern.h \
  reorder.h dffnxfun.h genrccom.h genrcfun.h memalloc.h msgcom.h \
  msgpass.h router.h prntutil.h sysdep.h proflfun.h

reorder.o: reorder.c setup.h envrnmnt.h symbol.h usrsetup.h cstrnutl.h \
  constrnt.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h memalloc.h pattern.h match.h \
  network.h ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h \
  utility.h symblcmp.h cstrccom.h agenda.h reorder.h prntutil.h router.h \
  rulelhs.h

reteutil.o: reteutil.c setup.h envrnmnt.h symbol.h usrsetup.h drive.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h match.h evaluatn.h constant.h network.h ruledef.h conscomp.h \
  constrct.h moduldef.h modulpsr.h utility.h symblcmp.h constrnt.h \
  cstrccom.h agenda.h pattern.h reorder.h engine.h lgcldpnd.h retract.h \
  incrrset.h memalloc.h router.h prntutil.h reteutil.h

retract.o: retract.c setup.h envrnmnt.h symbol.h usrsetup.h agenda.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h cstrccom.h network.h \
  match.h pattern.h reorder.h argacces.h drive.h engine.h lgcldpnd.h \
  retract.h memalloc.h reteutil.h router.h prntutil.h

router.o: router.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h filertr.h memalloc.h strngrtr.h \
  sysdep.h router.h prntutil.h

rulebin.o: rulebin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  bload.h utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h reteutil.h \
  evaluatn.h constant.h match.h network.h ruledef.h conscomp.h constrct.h \
  moduldef.h modulpsr.h symblcmp.h constrnt.h cstrccom.h agenda.h \
  pattern.h reorder.h engine.h lgcldpnd.h retract.h rulebsc.h rulebin.h \
  modulbin.h cstrcbin.h

rulebld.o: rulebld.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  constrct.h moduldef.h conscomp.h extnfunc.h expressn.h exprnops.h \
  exprnpsr.h scanner.h pprint.h userdata.h symblcmp.h modulpsr.h \
  evaluatn.h utility.h drive.h match.h network.h ruledef.h constrnt.h \
  cstrccom.h agenda.h pattern.h reorder.h incrrset.h memalloc.h \
  reteutil.h router.h prntutil.h rulebld.h watch.h

rulebsc.o: rulebsc.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h router.h prntutil.h watch.h ruledef.h \
  constrnt.h cstrccom.h agenda.h match.h network.h pattern.h reorder.h \
  engine.h lgcldpnd.h retract.h drive.h reteutil.h rulebin.h modulbin.h \
  cstrcbin.h rulecmp.h rulebsc.h

rulecmp.o: rulecmp.c setup.h envrnmnt.h symbol.h usrsetup.h factbld.h \
  pattern.h evaluatn.h constant.h expressn.h exprnops.h exprnpsr.h \
  extnfunc.h userdata.h scanner.h pprint.h match.h network.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h reorder.h reteutil.h rulecmp.h

rulecom.o: rulecom.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h crstrtgy.h agenda.h ruledef.h \
  constrnt.h cstrccom.h network.h match.h pattern.h reorder.h engine.h \
  lgcldpnd.h retract.h incrrset.h memalloc.h reteutil.h router.h \
  prntutil.h ruledlt.h sysdep.h watch.h rulebin.h modulbin.h cstrcbin.h \
  rulecom.h

rulecstr.o: rulecstr.c setup.h envrnmnt.h symbol.h usrsetup.h analysis.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h reorder.h ruledef.h conscomp.h constrct.h moduldef.h \
  modulpsr.h evaluatn.h constant.h utility.h symblcmp.h constrnt.h \
  cstrccom.h agenda.h match.h network.h pattern.h cstrnchk.h cstrnops.h \
  cstrnutl.h prcdrpsr.h router.h prntutil.h rulepsr.h rulecstr.h

ruledef.o: ruledef.c setup.h envrnmnt.h symbol.h usrsetup.h agenda.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h cstrccom.h network.h \
  match.h pattern.h reorder.h drive.h engine.h lgcldpnd.h retract.h \
  memalloc.h reteutil.h rulebsc.h rulecom.h rulepsr.h ruledlt.h bload.h \
  exprnbin.h sysdep.h symblbin.h rulebin.h modulbin.h cstrcbin.h \
  rulecmp.h

ruledlt.o: ruledlt.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  engine.h lgcldpnd.h match.h evaluatn.h constant.h expressn.h exprnops.h \
  exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h network.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h utility.h symblcmp.h \
  constrnt.h cstrccom.h agenda.h pattern.h reorder.h retract.h reteutil.h \
  drive.h bload.h exprnbin.h sysdep.h symblbin.h ruledlt.h

rulelhs.o: rulelhs.c setup.h envrnmnt.h symbol.h usrsetup.h agenda.h \
  ruledef.h conscomp.h constrct.h moduldef.h modulpsr.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h utility.h symblcmp.h constrnt.h cstrccom.h network.h \
  match.h pattern.h reorder.h argacces.h cstrnchk.h memalloc.h router.h \
  prntutil.h rulelhs.h

rulepsr.o: rulepsr.c setup.h envrnmnt.h symbol.h usrsetup.h analysis.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h reorder.h ruledef.h conscomp.h constrct.h moduldef.h \
  modulpsr.h evaluatn.h constant.h utility.h symblcmp.h constrnt.h \
  cstrccom.h agenda.h match.h network.h pattern.h cstrcpsr.h cstrnchk.h \
  cstrnops.h engine.h lgcldpnd.h retract.h incrrset.h memalloc.h \
  prccode.h prcdrpsr.h router.h prntutil.h rulebld.h rulebsc.h rulecstr.h \
  ruledlt.h rulelhs.h watch.h tmpltfun.h factmngr.h facthsh.h multifld.h \
  tmpltdef.h factbld.h bload.h exprnbin.h sysdep.h symblbin.h rulepsr.h

scanner.o: scanner.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  router.h prntutil.h moduldef.h conscomp.h constrct.h userdata.h \
  evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h memalloc.h

sortfun.o: sortfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h dffnxfun.h cstrccom.h memalloc.h \
  multifld.h sysdep.h sortfun.h

strngfun.o: strngfun.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h commline.h cstrcpsr.h engine.h \
  lgcldpnd.h match.h network.h ruledef.h constrnt.h cstrccom.h agenda.h \
  pattern.h reorder.h retract.h memalloc.h prcdrpsr.h router.h prntutil.h \
  strngrtr.h sysdep.h drive.h strngfun.h

strngrtr.o: strngrtr.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h sysdep.h strngrtr.h

symblbin.o: symblbin.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h bload.h exprnbin.h sysdep.h symblbin.h \
  bsave.h cstrnbin.h constrnt.h memalloc.h router.h prntutil.h

symblcmp.o: symblcmp.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  constant.h exprnpsr.h extnfunc.h expressn.h exprnops.h userdata.h \
  scanner.h pprint.h cstrccom.h moduldef.h conscomp.h constrct.h \
  evaluatn.h symblcmp.h modulpsr.h utility.h argacces.h cstrncmp.h \
  constrnt.h router.h prntutil.h sysdep.h

symbol.o: symbol.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h argacces.h sysdep.h

sysdep.o: sysdep.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h bmathfun.h commline.h constrnt.h \
  cstrcpsr.h emathfun.h filecom.h iofun.h memalloc.h miscfun.h multifld.h \
  multifun.h parsefun.h prccode.h prdctfun.h proflfun.h prcdrfun.h \
  router.h prntutil.h sortfun.h strngfun.h textpro.h watch.h sysdep.h \
  dffctdef.h cstrccom.h ruledef.h agenda.h match.h network.h pattern.h \
  reorder.h genrccom.h genrcfun.h object.h dffnxfun.h globldef.h \
  tmpltdef.h factbld.h factmngr.h facthsh.h classini.h

textpro.o: textpro.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h commline.h memalloc.h router.h \
  prntutil.h sysdep.h textpro.h

tmpltbin.o: tmpltbin.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  bload.h utility.h extnfunc.h expressn.h exprnops.h exprnpsr.h scanner.h \
  pprint.h userdata.h exprnbin.h sysdep.h symblbin.h bsave.h factbin.h \
  factbld.h pattern.h evaluatn.h constant.h match.h network.h ruledef.h \
  conscomp.h constrct.h moduldef.h modulpsr.h symblcmp.h constrnt.h \
  cstrccom.h agenda.h reorder.h cstrnbin.h factmngr.h facthsh.h \
  multifld.h tmpltdef.h tmpltpsr.h tmpltutl.h tmpltbin.h cstrcbin.h \
  modulbin.h

tmpltbsc.o: tmpltbsc.c setup.h envrnmnt.h symbol.h usrsetup.h argacces.h \
  expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h evaluatn.h constant.h moduldef.h conscomp.h constrct.h \
  symblcmp.h modulpsr.h utility.h memalloc.h router.h prntutil.h \
  cstrccom.h factrhs.h factmngr.h facthsh.h pattern.h match.h network.h \
  ruledef.h constrnt.h agenda.h reorder.h multifld.h tmpltdef.h factbld.h \
  cstrcpsr.h tmpltpsr.h tmpltbin.h cstrcbin.h modulbin.h tmpltcmp.h \
  tmpltutl.h tmpltbsc.h

tmpltcmp.o: tmpltcmp.c setup.h envrnmnt.h symbol.h usrsetup.h conscomp.h \
  constrct.h moduldef.h modulpsr.h evaluatn.h constant.h expressn.h \
  exprnops.h exprnpsr.h extnfunc.h userdata.h scanner.h pprint.h \
  utility.h symblcmp.h factcmp.h pattern.h match.h network.h ruledef.h \
  constrnt.h cstrccom.h agenda.h reorder.h cstrncmp.h tmpltdef.h \
  factbld.h factmngr.h facthsh.h multifld.h tmpltcmp.h

tmpltdef.o: tmpltdef.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  exprnops.h expressn.h exprnpsr.h extnfunc.h userdata.h scanner.h \
  pprint.h cstrccom.h moduldef.h conscomp.h constrct.h evaluatn.h \
  constant.h symblcmp.h modulpsr.h utility.h network.h match.h pattern.h \
  reorder.h ruledef.h constrnt.h agenda.h tmpltpsr.h tmpltdef.h factbld.h \
  factmngr.h facthsh.h multifld.h tmpltbsc.h tmpltutl.h tmpltfun.h \
  router.h prntutil.h modulutl.h cstrnchk.h bload.h exprnbin.h sysdep.h \
  symblbin.h tmpltbin.h cstrcbin.h modulbin.h tmpltcmp.h

tmpltfun.o: tmpltfun.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h scanner.h pprint.h exprnpsr.h extnfunc.h expressn.h \
  exprnops.h userdata.h argacces.h evaluatn.h moduldef.h conscomp.h \
  constrct.h symblcmp.h modulpsr.h utility.h router.h prntutil.h \
  cstrnchk.h constrnt.h default.h factmngr.h facthsh.h pattern.h match.h \
  network.h ruledef.h cstrccom.h agenda.h reorder.h multifld.h tmpltdef.h \
  factbld.h commline.h factrhs.h modulutl.h sysdep.h tmpltlhs.h \
  tmpltutl.h tmpltrhs.h tmpltfun.h

tmpltlhs.o: tmpltlhs.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h scanner.h pprint.h exprnpsr.h extnfunc.h expressn.h \
  exprnops.h userdata.h router.h prntutil.h moduldef.h conscomp.h \
  constrct.h evaluatn.h symblcmp.h modulpsr.h utility.h constrnt.h \
  reorder.h ruledef.h cstrccom.h agenda.h match.h network.h pattern.h \
  factrhs.h factmngr.h facthsh.h multifld.h tmpltdef.h factbld.h \
  modulutl.h tmpltutl.h tmpltlhs.h

tmpltpsr.o: tmpltpsr.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h scanner.h pprint.h exprnpsr.h extnfunc.h expressn.h \
  exprnops.h userdata.h router.h prntutil.h moduldef.h conscomp.h \
  constrct.h evaluatn.h symblcmp.h modulpsr.h utility.h factmngr.h \
  facthsh.h pattern.h match.h network.h ruledef.h constrnt.h cstrccom.h \
  agenda.h reorder.h multifld.h tmpltdef.h factbld.h cstrnchk.h \
  cstrnpsr.h cstrcpsr.h bload.h exprnbin.h sysdep.h symblbin.h default.h \
  watch.h cstrnutl.h tmpltbsc.h tmpltpsr.h

tmpltrhs.o: tmpltrhs.c setup.h envrnmnt.h symbol.h usrsetup.h memalloc.h \
  prntutil.h moduldef.h conscomp.h constrct.h userdata.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h scanner.h \
  pprint.h symblcmp.h modulpsr.h utility.h router.h tmpltfun.h factmngr.h \
  facthsh.h pattern.h match.h network.h ruledef.h constrnt.h cstrccom.h \
  agenda.h reorder.h multifld.h tmpltdef.h factbld.h factrhs.h modulutl.h \
  default.h tmpltutl.h tmpltlhs.h tmpltrhs.h

tmpltutl.o: tmpltutl.c setup.h envrnmnt.h symbol.h usrsetup.h extnfunc.h \
  expressn.h exprnops.h exprnpsr.h scanner.h pprint.h userdata.h \
  memalloc.h constrct.h moduldef.h conscomp.h symblcmp.h modulpsr.h \
  evaluatn.h constant.h utility.h router.h prntutil.h argacces.h \
  cstrnchk.h constrnt.h tmpltfun.h factmngr.h facthsh.h pattern.h match.h \
  network.h ruledef.h cstrccom.h agenda.h reorder.h multifld.h tmpltdef.h \
  factbld.h tmpltpsr.h modulutl.h watch.h sysdep.h tmpltbsc.h tmpltutl.h

userdata.o: userdata.c setup.h envrnmnt.h symbol.h usrsetup.h userdata.h

userfunctions.o: userfunctions.c clips.h setup.h envrnmnt.h symbol.h \
  usrsetup.h argacces.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  userdata.h scanner.h pprint.h evaluatn.h constant.h moduldef.h \
  conscomp.h constrct.h symblcmp.h modulpsr.h utility.h memalloc.h \
  cstrcpsr.h filecom.h strngfun.h commline.h router.h prntutil.h \
  filertr.h sysdep.h bmathfun.h watch.h modulbsc.h bload.h exprnbin.h \
  symblbin.h bsave.h ruledef.h constrnt.h cstrccom.h agenda.h match.h \
  network.h pattern.h reorder.h rulebsc.h engine.h lgcldpnd.h retract.h \
  drive.h incrrset.h rulecom.h crstrtgy.h dffctdef.h dffctbsc.h \
  tmpltdef.h factbld.h factmngr.h facthsh.h multifld.h tmpltbsc.h \
  tmpltfun.h factcom.h factfun.h globldef.h globlbsc.h globlcom.h \
  dffnxfun.h genrccom.h genrcfun.h object.h classcom.h classexm.h \
  classinf.h classini.h defins.h inscom.h insfun.h insfile.h msgcom.h \
  msgpass.h objrtmch.h

utility.o: utility.c setup.h envrnmnt.h symbol.h usrsetup.h evaluatn.h \
  constant.h expressn.h exprnops.h exprnpsr.h extnfunc.h userdata.h \
  scanner.h pprint.h facthsh.h factmngr.h conscomp.h constrct.h \
  moduldef.h modulpsr.h utility.h symblcmp.h pattern.h match.h network.h \
  ruledef.h constrnt.h cstrccom.h agenda.h reorder.h multifld.h \
  tmpltdef.h factbld.h memalloc.h prntutil.h sysdep.h

watch.o: watch.c setup.h envrnmnt.h symbol.h usrsetup.h constant.h \
  memalloc.h router.h prntutil.h moduldef.h conscomp.h constrct.h \
  userdata.h evaluatn.h expressn.h exprnops.h exprnpsr.h extnfunc.h \
  scanner.h pprint.h symblcmp.h modulpsr.h utility.h argacces.h watch.h

binops.o: binops.c clips.h binops.h

ArchitectureDetection.o: ArchitectureDetection.c ArchitectureDetection.h

OSDetection.o: OSDetection.c OSDetection.h

HardwareDetection.o: HardwareDetection.c HardwareDetection.h

Platform.o: Platform.c Platform.h

ShellVariables.o: ShellVariables.c ShellVariables.h

.PHONY: clean 


clean: 
	rm -f *.o
	rm -f $(OUTPUT)
