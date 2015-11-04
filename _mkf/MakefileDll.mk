# Note: This is a default makefile to compile a library
#
# --------------------------------------------------------------------------------
# --- Defining variables based on OS and fortran
# --------------------------------------------------------------------------------
include ../_mkf/MakefileSupport.mk

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
# --- INCLUDES 
# --------------------------------------------------------------------------------
INCS=-I$(LIB_DIR_BASE)-$(SUPPORT)
# --------------------------------------------------------------------------------
# --- DEFINITIONS
# --------------------------------------------------------------------------------
DEFS=$(OSDEF)
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

# --------------------------------------------------------------------------------
# --- Defining Objects based on SRC
# --------------------------------------------------------------------------------
# Setting up objects
OBJ:= $(patsubst %.f90,%.$(o),$(SRC)) 
OBJ:= $(patsubst %.F90,%.$(o),$(OBJ))
OBJ:= $(patsubst %.for,%.$(o),$(OBJ))
OBJ:= $(patsubst %,$(OBJ_DIR)/%,$(OBJ))


vpath %.f90 
vpath %.F90
vpath %.for

# --------------------------------------------------------------------------------
# --- Main rules  
# --------------------------------------------------------------------------------
.PHONY: lib all clean flags

all: $(RULES)

clean:
	@$(RM) $(OBJ_DIR)$(SLASH)*.$(o)  $(OBJ_DIR)$(SLASH)*.mod
	@$(RM) $(OBJ_DIR)
	@echo "- $(LIB_NAME_BASE) lib cleaned"

purge: clean
	@$(RM) $(LIB_DIR)$(SLASH)$(LIB_NAME)*
	@$(RM) $(OBJ_DIR_BASE)*
	@echo "- $(LIB_NAME_BASE) lib purged"


# --------------------------------------------------------------------------------
# ---  Static library
# --------------------------------------------------------------------------------
$(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib): $(LIB_DIR) $(OBJ_DIR) $(OBJ)
	@echo "- Compiling static library:  " $(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib)
	@$(ARCHIVER) $(AFLAGS) $(LIBS) -o $(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib) $(OBJ_DIR)$(SLASH)*.$(o)
	@$(TOUCH) $(OBJ_DIR)$(SLASH)dummy.mod
	@$(CP) $(OBJ_DIR)$(SLASH)*.mod $(LIB_DIR)
	@$(RM) $(OBJ_DIR)$(SLASH)dummy.mod
	@$(RM) $(LIB_DIR)$(SLASH)dummy.mod

# --------------------------------------------------------------------------------
# ---  DLL library
# --------------------------------------------------------------------------------
$(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll): $(LIB_DIR) $(OBJ_DIR) $(OBJ)
	@echo "- Compiling dynamic library: " $(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll)
ifeq ($(OSNAME),windows)
	@$(FC) $(DEFS) $(INCS) $(LDFLAGS) -shared -Wl,-soname,$(LIB_NAME).$(dll).1  $(OBJ_DIR)$(SLASH)*.$(o) $(LIBS) -o $(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll) 
else
	@$(FC) $(DEFS) $(INCS) $(LDFLAGS) -shared -Wl,-soname,$(LIB_NAME).$(dll).1  $(OBJ_DIR)$(SLASH)*.$(o) $(LIBS) -o $(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll) 
endif


# --------------------------------------------------------------------------------
# --- Low-level Compilation rules 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDefaultCompile.mk


# --------------------------------------------------------------------------------
# --- DEPENDENCIES 
# --------------------------------------------------------------------------------
# Creating build directory
$(OBJ_DIR):
	@echo "# --------------------------------------------------------------------------------"
	@echo "# --- Compilation of $(LIB_NAME) "
	@echo "# --------------------------------------------------------------------------------"
	@make --no-print-directory flags
	@$(MKDIR) $(OBJ_DIR)

$(LIB_DIR):
	@$(MKDIR) $(LIB_DIR)

# --------------------------------------------------------------------------------
# --- SIMPLE RULES 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileSimpleRules.mk
