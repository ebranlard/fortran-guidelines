# --------------------------------------------------------------------------------
# --- Simple rules
# --------------------------------------------------------------------------------
# Convenient rule to print any variable
echo-%:
	@echo '$*=$($*)'


flags:
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- Flags"
	@echo "# --------------------------------------------------------------------------------"
	@echo ""
	@echo "OS-Archi-Build: " $(OSNAME)-$(ARCHI)-$(BUILD)
	@echo ""
	@echo "Compiler:       " $(FC)
	@echo ""
	@echo "Fortran FLAGS:  " $(FFLAGS)
	@echo ""
	@echo "Linker  FLAGS:  " $(AFLAGS)
	@echo ""
