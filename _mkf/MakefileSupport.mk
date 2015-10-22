# --------------------------------------------------------------------------------
# --- Defining variables based on config
# --------------------------------------------------------------------------------
SUPPORT=$(strip $(OSNAME))-$(strip $(ARCHI))-$(strip $(FC))
ifeq ($(RELEASE),0)
    SUPPORT:=$(SUPPORT)-debug
endif
LIB_DIR=$(LIB_DIR_BASE)-$(SUPPORT)
OBJ_DIR=$(OBJ_DIR_BASE)-$(SUPPORT)
