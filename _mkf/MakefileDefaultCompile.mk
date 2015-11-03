$(OBJ_DIR)/%.$(o): %.for
	@echo "($(SUPPORT)) :" $< 
	@$(FC) $(DEFS) $(INCS) $(FFLAGS) -c $<  $(LIBS) $(FOUT_OBJ)$(OBJ_DIR)$(SLASH)$*.$(o)

$(OBJ_DIR)/%.$(o): %.F90
	@echo "($(SUPPORT)) :" $< 
	@$(FC) $(DEFS) $(INCS) $(FFFREE) $(FFLAGS) -c $< $(LIBS) $(FOUT_OBJ)$(OBJ_DIR)$(SLASH)$*.$(o)

$(OBJ_DIR)/%.$(o): %.$(f)
	@echo "($(SUPPORT)) :" $< 
	@$(FC) $(DEFS) $(INCS) $(FFFREE) $(FFLAGS) -c $< $(LIBS) $(FOUT_OBJ)$(OBJ_DIR)$(SLASH)$*.$(o) 
