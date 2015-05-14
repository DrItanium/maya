########### MAKEFILE FOR MAYA ###########
include config.mk
LIBELECTRON_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libelectron/*.c))
LIBMAYA_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libmaya/*.c))
LIBUNICORNHAT_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libunicornhat/*.c))
MAYA_EXECUTABLE_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/cmd/repl/*.c))
OBJS = ${LIBELECTRON_OBJECTS} ${LIBMAYA_OBJECTS} $(LIBUNICORNHAT_OBJECTS) ${MAYA_EXECUTABLE_OBJECTS} 

.PHONY: clean all

all: program

program: $(OBJS) 
	@echo Building $(OUTPUT)
	@$(CC) $(LDFLAGS) -o $(OUTPUT) $(OBJS) -lm -lrt

install:
	@echo Installing binaries to $(PREFIX)/bin
	@mkdir -p $(PREFIX)/bin
	@cp $(OUTPUT) $(PREFIX)/bin
	@chmod +s $(PREFIX)/bin/${OUTPUT}

deinstall uninstall:
	@echo Uninstalling...
	@rm -f $(PREFIX)/bin/$(OUTPUT)


clean: 
	@echo Cleaning
	@rm -f $(OBJS) $(OUTPUT)


.c.o :
	@echo CC $<
	@$(CC) -c $(CFLAGS) -o $@ -D_POSIX_C_SOURCE=200112L -D_XOPEN_SOURCE \
		-std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs -Wstrict-prototypes \
	    -Waggregate-return -Wno-implicit -Iinclude/ $<

