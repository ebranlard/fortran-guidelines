# General makefile propagating to all subdirectories
# --------------------------------------------------------------------------------
# --- Defining subdirectories and rule names 
# --------------------------------------------------------------------------------
# Subdirectories with rule suffix
#SUBDIRS  = $(shell find . -maxdepth 1 -mindepth 1 -type d)
SUBDIRS = ${sort ${dir ${wildcard ./*/*/}}}
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
SUBALL   = $(addsuffix .all,$(SUBDIRS))
SUBTESTS  = $(addsuffix .tests,$(SUBDIRS))


# --------------------------------------------------------------------------------
# --- General rules 
# --------------------------------------------------------------------------------
.PHONY: clean all tests $(SUBCLEAN) $(SUBALL) $(SUBTESTS)

all: $(SUBALL)

clean: $(SUBCLEAN)

tests: $(SUBTESTS)

echo:
	@echo $(SUBDIRS)

# --------------------------------------------------------------------------------
# ---  Rules for sub directories
# --------------------------------------------------------------------------------
$(SUBALL): %.all:
	@$(MAKE) --no-print-directory -C $* all
$(SUBCLEAN):%.clean:
	@$(MAKE) --no-print-directory -C $* clean
$(SUBTESTS): %.tests:
	@$(MAKE) --no-print-directory -C $* tests
