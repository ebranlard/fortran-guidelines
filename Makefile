all: pdf test_batch

clean:
	@make --no-print-directory -C tex clean
	@make --no-print-directory -C _unit_tests clean
# --------------------------------------------------------------------------------
# --- Pdf
# --------------------------------------------------------------------------------
pdf:tex/fortran-guidelines.pdf
	
tex/fortran-guidelines.pdf:
	@make -C tex


# --------------------------------------------------------------------------------
# ---  
# --------------------------------------------------------------------------------
test:
	@make -C _unit_tests

test_batch:
	@make --no-print-directory -C _unit_tests FCOMPILER=0
	@make --no-print-directory -C _unit_tests FCOMPILER=1
