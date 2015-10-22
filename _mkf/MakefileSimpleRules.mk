# --------------------------------------------------------------------------------
# --- Simple rules
# --------------------------------------------------------------------------------
# Convenient rule to print any variable
echo-%:
	@echo '$*=$($*)'

flags:
	@echo ""
	@echo "OS-Archi-Build: " $(OSNAME)-$(ARCHI)-$(BUILD)
	@echo ""
	@echo "SUPPORT:        " $(SUPPORT)
	@echo ""
	@echo "Compilers:      " $(FC) $(CC)
	@echo ""
	@echo "C FLAGS:        " $(CFLAGS)
	@echo ""
	@echo "Fortran FLAGS:  " $(FFLAGS)
	@echo ""
	@echo "Linker  FLAGS:  " $(LDFLAGS)
	@echo ""
	@echo "Archiver FLAGS: " $(AFLAGS)
	@echo ""
	@echo "INCLUDES:       " $(INCS)
	@echo ""
	@echo "DEFS:           " $(DEFS)
	@echo ""
	@echo "LIBS:           " $(LIBS)
	@echo ""
