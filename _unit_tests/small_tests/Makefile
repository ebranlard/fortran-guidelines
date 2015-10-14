# --------------------------------------------------------------------------------
# --- Defining OS and Archi
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileOS.mk
# --------------------------------------------------------------------------------
# --- Makefile flags
# --------------------------------------------------------------------------------
ifeq ($(RELEASE),)
    RELEASE=1
endif
ifeq ($(FCOMPILER),)
    FCOMPILER=1
endif

LIB_NAME_BASE=
OBJ_DIR_BASE=../../_build
LIB_DIR_BASE=../../_lib

# C Preprocessor
FFLAGS_EXTRA+=$(FFFPP)

ifeq ($(FCOMPILER),1)
FFLAGS_EXTRA=-threads -fpp
endif
ifeq ($(FCOMPILER),0)
FFLAGS_EXTRA=-cpp  -Wno-attributes
endif
AFLAGS_EXTRA=

MAKE_STATIC=0
MAKE_DLL=1

# --------------------------------------------------------------------------------
# --- Setup Fortran Compiler
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileFortran.mk

# --------------------------------------------------------------------------------
# --- Setup Flags and support
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileSupport.mk

# --------------------------------------------------------------------------------
# ---  
# --------------------------------------------------------------------------------
# include $(OMNIVOR_MKF_DIR)MakefileInc.mk
# Params
# LIB_DIR := $(OMNIVOR_LIB_DIR)
# OBJ_DIR := $(OMNIVOR_OBJ_DIR)
# ALLOBJ   =$(wildcard $(OBJ_DIR)/*.o)

TESTFILES     =$(wildcard *.$(f))
TESTPROGS     =$(patsubst %.$(f),%,      $(TESTFILES) )
RULES_COMPILE =$(patsubst %.$(f),comp-%, $(TESTFILES) )
RULES_RUN     =$(patsubst %.$(f),run-%,  $(TESTFILES) )

# --------------------------------------------------------------------------------
# ---  General Rules
# --------------------------------------------------------------------------------

# all: clean run-test_hills_yaps
all: unittest

unittest: $(RULES_RUN) clean

# Compilation
comp-%: %.$(f)
	@$(FC) $(FFLAGS) $(LDFLAGS) -I$(OBJ_DIR) $*.$(f)  -o $* $(ALLOBJ)

# Run requires compilation
run-%: comp-%
	@echo $(SUPPORT) $*
ifeq ($(MPI),1)
	@$(MPIRUN) ./$* 
else
	@./$* 
endif
clean:
	@rm -rf $(TESTPROGS) *.vtk *.txt


echo:
	@echo "ALL OBJ  : " $(ALLOBJ)
	@echo "LIB_DIR  : " $(LIB_DIR)
	@echo "OBJ_DIR  : " $(LIB_DIR)
	@echo "RULES    : " $(RULES_COMPILE) $(RULES_RUN)
	@echo "TESTPROGS: " $(TESTPROGS)
	@echo "TESTFILES: " $(TESTFILES)

# --------------------------------------------------------------------------------
# --- Setup some easy rules
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileSimpleRules.mk


# --------------------------------------------------------------------------------
# --- Overriding rules 
# --------------------------------------------------------------------------------
