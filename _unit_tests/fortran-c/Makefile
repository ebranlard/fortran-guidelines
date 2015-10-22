CCOMPILER=0

include ../../_mkf/MakefileOS.mk
include ../../_mkf/MakefileC.mk


TESTFILES     =$(wildcard *.$(f)) $(wildcard *.$(c))
TESTPROGS     =$(patsubst %.$(f),%,      $(TESTFILES) ) $(patsubst %.$(c),%,      $(TESTFILES) )
RULES_COMPILE =$(patsubst %.$(f),comp-%, $(TESTFILES) )
RULES_RUN     =$(patsubst %,run-%,  $(TESTFILES) )
RULES_CLEAN   =$(patsubst %,clean-%, $(TESTFILES) )


all:  compile run clean
	
compile: $(RULES_COMPILE)
   
run:   $(RULES_RUN) 
	
clean:$(RULES_CLEAN)

# all: compile tests clean
# 
# compile:
# 	@$(CC) test_c_precision.c -o test_c_precision
# 
# 
# tests:
# 	@./test_c_precision
# 
# clean:
# 	@$(RM) test_c_precision

# C Compilation
comp-%: %.$(f)
	@$(CC) $(DEFS) $(INCS) $(CFLAGS) $(LDFLAGS) $*.$(c) $(LIBS) -o $*
# Fortran Compilation
comp-%: %.$(f)
	@$(FC) $(DEFS) $(INCS) $(FFLAGS) $(LDFLAGS) -I$(OBJ_DIR) $*.$(f) $(LIBS) -o $* 

# Run requires compilation
run-%: comp-%
	@echo $(SUPPORT) $*
	./$*

clean-%: 
	@$(RM) $*

echo:
	@echo "RULES_RUN: " $(RULES_RUN)
	@echo "TESTPROGS: " $(TESTPROGS)
	@echo "TESTFILES: " $(TESTFILES)

include ../../_mkf/MakefileSimpleRules.mk
