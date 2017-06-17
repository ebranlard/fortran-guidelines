include _mkf/MakefileOS.mk

all: pdf test_batch

clean:
	@make --no-print-directory -C tex clean
	@make --no-print-directory -C _unit_tests clean
# --------------------------------------------------------------------------------
# --- Pdf
# --------------------------------------------------------------------------------
pdf:
	@make -C tex
	@cp tex/fortran-guidelines.pdf .


# --------------------------------------------------------------------------------
# ---  
# --------------------------------------------------------------------------------
test:
	@make -C _unit_tests

test_batch:
ifeq ($(OSNAME),windows)
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- GFORTRAN "
	@echo "# --------------------------------------------------------------------------------"
	@vcvarsall.bat x86& @make --no-print-directory -C _unit_tests FCOMPILER=0
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- INTEL 32 "
	@echo "# --------------------------------------------------------------------------------"
	@vcvarsall.bat x86 & ifortvars.bat ia32 vs2010  & make --no-print-directory -C _unit_tests FCOMPILER=1
ifeq ($(ARCHI),amd64)
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- INTEL 64 "
	@echo "# --------------------------------------------------------------------------------"
	@vcvarsall.bat amd64 & ifortvars.bat amd64 vs2010  & make --no-print-directory -C _unit_tests FCOMPILER=1
endif
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- COMPAQ "
	@echo "# --------------------------------------------------------------------------------"
	@vcvarsall.bat x86 & dfvars.bat & make --no-print-directory -C _unit_tests FCOMPILER=2
else
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- GFORTRAN "
	@echo "# --------------------------------------------------------------------------------"
	@make --no-print-directory -C _unit_tests FCOMPILER=0
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- INTEL "
	@echo "# --------------------------------------------------------------------------------"
	@make --no-print-directory -C _unit_tests FCOMPILER=1
endif

include _mkf/MakefileSimpleRules.mk
