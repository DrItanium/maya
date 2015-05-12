########### MAKEFILE FOR MAYA ###########
include config.mk
LIBELECTRON_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libelectron/*.c))
LIBMAYA_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libmaya/*.c))
LIBUNICORNHAT_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/libunicornhat/*.c))
MAYA_EXECUTABLE_OBJECTS = $(patsubst %.c,%.o, $(wildcard src/cmd/repl/*.c))
OBJS = ${LIBELECTRON_OBJECTS} ${LIBMAYA_OBJECTS} ${MAYA_EXECUTABLE_OBJECTS} ${LIBUNICORNHAT_OBJECTS}
CPU := $(shell cat /proc/cpuinfo | grep BCM | awk '{print $$3}' | tr -d ' ')

$(warning Detected CPU=$(CPU))

ifeq ($(CPU),BCM2709)
	PERI_BASE := 0x3F000000
	RPI2 := -DRPI2
else
	PERI_BASE := 0x20000000
endif
CFLAGS += -DPERI_BASE=$(PERI_BASE) $(RPI2)

.PHONY: clean all

all: program

program: $(OBJS) 
	@echo Building $(OUTPUT)
	@$(CC) $(LDFLAGS) -o $(OUTPUT) $(OBJS) -lm -lrt

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
	    -Waggregate-return -Wno-implicit -Iinclude/ $<

