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
    ifeq ($(OSNAME),windows)
        FCOMPILER=2
    else
        FCOMPILER=1
    endif
endif
# --------------------------------------------------------------------------------
# --- Setup Fortran Compiler
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileFortran.mk

#FFLAGS= $(FFNOLOGO) $(FFFPP)
FFLAGS= $(FFNOLOGO)
LDFLAGS=
#DEFS= $(OSDEF) -D__MYMACRO__
#DEFS=-D__MYMACRO__
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
TESTPROGS     =$(patsubst %.$(f),%$(EXE),      $(TESTFILES) )

# --------------------------------------------------------------------------------
# ---  General Rules
# --------------------------------------------------------------------------------

# all: clean run-test_hills_yaps
all: test1 clean
#    	unittest

# Compilation
test1: test_preproc.$(f)
	@$(FC) $(DEFS) $(FFLAGS) $(LDFLAGS) test_preproc.$(f) $(FOUTEXE)test_preproc$(EXE)
	@echo "$(SUPPORT) $^"
	@./test_preproc

clean:
	@$(RM) $(TESTPROGS) *.vtk *.txt


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
