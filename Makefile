all: test_batch
	#@make -C tex


test:
	@make -C _unit_tests

test_batch:
	@make --no-print-directory -C _unit_tests FCOMPILER=0
	@make --no-print-directory -C _unit_tests FCOMPILER=1
