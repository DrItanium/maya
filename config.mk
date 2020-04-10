ASMPREFIX_CCOMP := asminst -a x86_64_gcc --
ASMPREFIX_LD := asminst -a gnu_ld -- 
ASMPREFIX_AR := asminst -a gnu_ar -- 
CC := ${ASMPREFIX_CCOMP} gcc
OUTPUT := maya
PREFIX := /usr/local
CFLAGS := -Os -g3 -std=c99
LIBRARIES := -lm -lrt -lc
LDFLAGS :=
CXXEXTENSIONS ?= TRUE
ifeq ($(CXXEXTENSIONS), TRUE)
	CXX := ${ASMPREFIX_CCOMP} g++
	#LIBRARIES += /usr/lib/libboost_system.a /usr/lib/libboost_filesystem.a
	LIBRARIES += -lboost_system -lboost_filesystem -ltag
	CXXFLAGS := -Os -g3 -std=c++17 
	#LD := $(CXX)
else
	CFLAGS += -DBOOST_EXTENSIONS=0 -DFUNCTIONAL_EXTENSIONS=0 -DTAGLIB_EXTENSIONS=0
	#LD := $(CC)
endif
LD := ${ASMPREFIX_LD} ${LD}
AR := ${ASMPREFIX_AR} ${AR}
COMMAND_PROMPT := "maya> "
BANNER_STRING := "\"     maya (based off of CLIPS \" VERSION_STRING \" \" CREATION_DATE_STRING \". Built on \" __DATE__ \" at \" __TIME__ \")\n\""

