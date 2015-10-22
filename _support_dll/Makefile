CCOMPILER=0

include ../_mkf/MakefileOS.mk
include ../_mkf/MakefileC.mk

all: compile tests clean

compile:
	@$(CC) include_me_in_C_programs.c -o include_me


tests:
	@./include_me

clean:
	@$(RM) include_me
