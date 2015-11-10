PROG=test_char_array
S=$(PROG).f90

FCC=gfortran
# FCC=ifort

 
all:test


test:
	@$(FCC) -Wall $(S) -o $(PROG)
	@./$(PROG)

clean:
	rm -f *.dat $(PROG)  *.o
