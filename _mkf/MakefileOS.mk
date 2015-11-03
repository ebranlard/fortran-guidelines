# This make file should maybe be updated according to: http://mad-scientist.net/make/multi-arch.html
#--------------------------------------------------------------------------------
# ---  Architecture, system name, objects
# --------------------------------------------------------------------------------
ifeq ($(OS),Windows_NT)
    OSNAME=windows
    #REG=$(shell reg query "HKLM\System\CurrentControlSet\Control\Session Manager\Environment" /v PROCESSOR_ARCHITECTURE)

    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        ARCHI = amd64
    endif
    
    ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
        ARCHI ?= amd64
    else
        ARCHI ?= ia32
    endif

    OSDEF=-DWINDOWS -D_WIN32
	# Forcing the usual preprocessor flags
    ifeq ($(ARCHI),amd64)
        OSDEF := $(OSDEF) -D_WIN64
    endif


    # File Extensions
    o=obj
    lib=lib
    dll=dll
    EXE=.exe

else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
         OSNAME=linux
    else ifeq ($(UNAME_S),Darwin)
        OSNAME=mac
    endif
    UNAME_P := $(shell uname -p)
    UNAME_M := $(shell uname -m)
    ifeq ($(UNAME_M),x86_64)
        ARCHI=amd64
    # STUFF BELOW NEED TO BE re-tested..
    else ifneq ($(filter %86,$(UNAME_P)),)
        ARCHI=ia32
    else ifneq ($(filter arm%,$(UNAME_P)),)
        ARCHI=arm
    else ifneq ($(filter unknown%,$(UNAME_P)),)
        ARCHI=ia32
    endif

    OSDEF=-D__linux__ -D__unix__ -D__LINUX__ -D__UNIX__
	# Forcing the usual preprocessor flags
    ifeq ($(ARCHI),amd64)
        OSDEF := $(OSDEF)
    endif
 
    # File Extensions
    o=o
    lib=a
    dll=so
    EXE=

endif

#--------------------------------------------------------------------------------
# ---  System Commands
# --------------------------------------------------------------------------------
ifeq ($(OS),Windows_NT)
    # System
    RM=del /q
    LN=copy /y
    CP=copy /y
    MKDIR=mkdir 
    SLASH=/
    SLASH := $(subst /,\,$(SLASH))
    TOUCH=echo.>
    MKDEPF=makedepf90.exe
    SHELL=cmd.exe
    LD=link.exe
    LD_OUT=/out:
    LD_DLL=/nologo /dll
    AR=Lib
    CAT=type
    ECHOSAFE=echo(
else
    # System
    RM=rm -rf
    LN=ln -sf
    CP=cp
    MKDIR=mkdir -p
    SLASH=/
    TOUCH=touch
    MKDEPF=makedepf90
    SHELL=/bin/bash
    LD=LD
    LD_OUT=-o
    LD_DLL=
    AR=ar
    CAT=cat
    ECHOSAFE=echo 
endif



HOSTNAME=$(shell hostname)
