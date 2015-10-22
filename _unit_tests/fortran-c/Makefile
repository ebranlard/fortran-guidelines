# --------------------------------------------------------------------------------
# --- Defining OS and Archi
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileOS.mk

# --------------------------------------------------------------------------------
# --- Directories
# --------------------------------------------------------------------------------
OBJ_DIR_BASE=../../_build
LIB_DIR_BASE=../../_lib

# --------------------------------------------------------------------------------
# --- Setup Compilers
# --------------------------------------------------------------------------------
ifeq ($(RELEASE),)
    RELEASE=1
endif
ifeq ($(FCOMPILER),)
    ifeq ($(OSNAME),windows)
        FCOMPILER=1
    else
        FCOMPILER=1
    endif
endif
ifeq ($(CCOMPILER),)
    ifeq ($(OSNAME),windows)
        CCOMPILER=2
    else
        CCOMPILER=1
    endif
endif

# --- Generic COMPILERS Makefiles
include ../../_mkf/MakefileC.mk
include ../../_mkf/MakefileFortran.mk

CFLAGS=
FFLAGS=$(FFNOLOGO)
DEFS=
INCS=
LIBS=

# --------------------------------------------------------------------------------
# --- Setup Support
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileSupport.mk




# --------------------------------------------------------------------------------
# ---  General Rules
# --------------------------------------------------------------------------------
TESTFILES_F   =$(wildcard *.$(f))
TESTFILES_C   =$(wildcard *.$(c))
TESTPROGS     =$(patsubst %.$(f),%, $(TESTFILES_F) ) $(patsubst %.$(c),%,$(TESTFILES_C) )
RULES_COMPILE =$(patsubst %,comp-%,  $(TESTPROGS) )
RULES_RUN     =$(patsubst %,run-%,   $(TESTPROGS) )
RULES_CLEAN   =$(patsubst %,clean-%, $(TESTPROGS) )

all:  compile run clean
	
compile: $(RULES_COMPILE)
   
run:   $(RULES_RUN) 
	
clean:$(RULES_CLEAN)

# C Compilation
comp-%: %.$(c)
	@$(CC) $(DEFS) $(INCS) $(CFLAGS) $(LDFLAGS) $*.$(c) $(LIBS) $(COUT)$*
# Fortran Compilation
comp-%: %.$(f)
	@$(FC) $(DEFS) $(INCS) $(FFLAGS) $(LDFLAGS) $*.$(f) $(LIBS) $(FOUT_EXE)$* 

# Run requires compilation
run-%: comp-%
	@echo "> Running test ($(SUPPORT))" $*
	@./$*

clean-%: 
	@$(RM) $*$(EXE) *.$(o)

echo:
	@echo "RULES_RUN: " $(RULES_RUN)
	@echo "TESTPROGS: " $(TESTPROGS)
	@echo "TESTFILES: " $(TESTFILES_F) $(TESTFILES_C)

# --------------------------------------------------------------------------------
# --- Setup some easy rules
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileSimpleRules.mk
