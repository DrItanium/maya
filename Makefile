########### MAKEFILE FOR MAYA ###########
include config.mk
OBJECTS = $(patsubst %.c,%.o, $(wildcard *.c))
CXX_OBJECTS = $(patsubst %.cc,%.o, $(wildcard *.cc))
CFLAGS += -DBANNER_STRING=${BANNER_STRING} -DCOMMAND_PROMPT='$(COMMAND_PROMPT)'
OBJS = ${OBJECTS} ${CXX_OBJECTS}

.PHONY: clean all

all: program

program: $(OBJS)
	@echo Building $(OUTPUT)
	@$(CXX) $(LDFLAGS) -o $(OUTPUT) $(OBJS)

install:
	@echo Installing binaries to $(PREFIX)/bin
	@mkdir -p $(PREFIX)/bin
	@cp $(OUTPUT) $(PREFIX)/bin

deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(PREFIX)/bin/$(OUTPUT)


clean:
	@echo Cleaning
	@rm -f $(OBJS)
	@rm -f $(OUTPUT)


.c.o :
	@echo CC $<
	@$(CC) -c $(CFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L \
		-std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs -Wstrict-prototypes \
	    -Waggregate-return -Wno-implicit $<

.cc.o :
	@echo CXX $<
	@$(CXX) -c $(CXXFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L \
		-std=c++11 -Wall -Wundef -Wpointer-arith -Wcast-qual \
		-Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
		-Waggregate-return $<

