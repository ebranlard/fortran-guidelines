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

# --------------------------------------------------------------------------------
# ---  General Rules
# --------------------------------------------------------------------------------

# all: clean run-test_hills_yaps
all: test1 clean
#    	unittest

# Compilation
test1: test_preproc.$(f)
	@$(FC) $(FFLAGS) $(LDFLAGS) -I$(OBJ_DIR) test_preproc.$(f)  -o test_preproc $(ALLOBJ)
	@echo "$(SUPPORT) $^"
	@./test_preproc

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
comp-test_preproc:FFLAGS_EXTRA+=-D__MYMACRO__
comp-test_preproc:



# --------------------------------------------------------------------------------
# ---  
# --------------------------------------------------------------------------------
show_macros:FFLAGS_EXTRA+=-D__MYMACRO__
show_macros:
ifeq ($(FCOMPILER),0)
	gfortran -cpp -E -dM $(FFLAGS) empty.f90 > _macros-$(SUPPORT)
endif
ifeq ($(FCOMPILER),1)
	ifort -E -fpp $(FFLAGS) empty.f90 -dryrun 2> _macros-$(SUPPORT)
endif
