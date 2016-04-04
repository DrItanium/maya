CC := cc
OUTPUT := maya
PREFIX := /usr/local
CFLAGS := -Os -g3 -std=c99
LDFLAGS := -lm -lrt
CXXEXTENSIONS ?= TRUE
SDLEXTENSIONS ?= TRUE
ifeq ($(CXXEXTENSIONS), TRUE)
	CXX := c++
	LDFLAGS += -lboost_system -lboost_filesystem
	CXXFLAGS := -Os -g3 -std=c++11
	LD := $(CXX)
else
	CFLAGS += -DBOOST_EXTENSIONS=0 -DFUNCTIONAL_EXTENSIONS=0
	LD := $(CC)
endif

ifeq ($(SDLEXTENSIONS), TRUE) 
	CFLAGS += -I/usr/include/SDL2 -DSDL_EXTENSIONS=1
	CXXFLAGS += -I/usr/include/SDL2 -DSDL_EXTENSIONS=1
	LDFLAGS += -lSDL2 -lSDL2_gfx -lGL
else
	CFLAGS += -DSDL_EXTENSIONS=0
	CXXFLAGS += -DSDL_EXTENSIONS=0
endif
COMMAND_PROMPT := "maya> "
BANNER_STRING := "\"     maya (based off of CLIPS \" VERSION_STRING \" \" CREATION_DATE_STRING \". Built on \" __DATE__ \" at \" __TIME__ \")\n\""

