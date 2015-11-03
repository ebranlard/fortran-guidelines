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
# --- Generic COMPILERS Makefiles
include ../../_mkf/MakefileFortran.mk
FFLAGS= $(FFNOLOGO)
LDFLAGS=
DEFS= $(FCOMPILERDEF) $(OSDEF) 
INCS=
LIBS=
# --------------------------------------------------------------------------------
# --- Setup Support
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileSupport.mk

# --------------------------------------------------------------------------------
# ---  General Rules
# --------------------------------------------------------------------------------
TESTFILES_F   = \
                ..$(SLASH)..$(SLASH)_support$(SLASH)SupportISO_$(FCNAME).f90 \
                ..$(SLASH)..$(SLASH)_support$(SLASH)SupportCompiler_$(FCNAME).f90 \
            	..$(SLASH)..$(SLASH)_support$(SLASH)SupportPrecision.f90 \
                test_precision_iso.f90
TESTPROG =  iso

all:  compile run clean

compile: comp

# Fortran Compilation
comp: $(TESTFILES_F)
	$(FC) $(DEFS) $(INCS) $(FFLAGS) $(LDFLAGS) $^ $(LIBS) $(FOUT_EXE)$(TESTPROG)$(EXE)

# Run requires compilation
run: comp
	@echo "> Running test ($(SUPPORT))" $(TESTPROG)
	@./$(TESTPROG)

clean: 
	@$(RM) $(TESTPROG)$(EXE) *.$(o) *.mod

# --------------------------------------------------------------------------------
# --- Setup some easy rules
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileSimpleRules.mk

