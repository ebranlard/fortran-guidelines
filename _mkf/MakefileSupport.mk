# Note: This is a default makefile to compile a library
#
# --------------------------------------------------------------------------------
# --- Defining variables based on OS and fortran
# --------------------------------------------------------------------------------
SUPPORT=$(strip $(OSNAME))-$(strip $(ARCHI))-$(strip $(FC))
ifeq ($(RELEASE),0)
    SUPPORT:=$(SUPPORT)-debug
endif
# LIB_DIR=$(LIB_DIR_BASE)-$(OSNAME)-$(ARCHI)
LIB_DIR=$(LIB_DIR_BASE)-$(SUPPORT)
OBJ_DIR=$(OBJ_DIR_BASE)-$(SUPPORT)
LIB_NAME= $(LIB_NAME_BASE)
ifeq ($(OSNAME),linux)
    LIB_NAME=lib$(LIB_NAME_BASE)
endif

ifeq ($(MAKE_STATIC),1)
    RULES+= $(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib)
endif
ifeq ($(MAKE_DLL),1)
    RULES+= $(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll)
endif

# --------------------------------------------------------------------------------
# --- Compiler Flags 
# --------------------------------------------------------------------------------
FFLAGS    = $(FFNOLOGO) $(FFMODINC)$(OBJ_DIR)
FFLAGS   += $(FFDLL)
ifeq ($(RELEASE),0)
    FFLAGS   += $(FFDEBUGINFO) $(FFDEBUG) $(FFPE) $(FFWARN) $(FFWARNEXTRA) $(FFWARNERROR) $(FFOPT0)
    FFLAGS   += $(FFTRACE)
    BUILD=debug
else
    FFLAGS   += $(FFOPTO5)
    BUILD=release
endif
FFLAGS   += $(FFLAGS_EXTRA)
# FFLAGS   += $(OSDEF)
#
# --------------------------------------------------------------------------------
# ---  ARCHIVER flags
# --------------------------------------------------------------------------------
ifeq ($(OSNAME),windows)
    AFLAGS=$(FFNOLOGO)
else
	# v: verbose
	# r: insert with replacement
	# c: create
	# q: quickly append without checking for replacements
    #AFLAGS=-cq 
    AFLAGS=-cr
endif
AFLAGS+= $(AFLAGS_EXTRA)
