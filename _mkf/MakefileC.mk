c=c
# 0: Gnu compiler GCC
# 1: intel C Compiler icc
# 2: visual studio compiler cl.exe

# INTEL C COMPILER
ifeq ($(CCOMPILER),1)
    CC         = icc
    COUT       = -o
    CFFREE     =
    CFOPT      = -O3
    CFOPTO5    = -O3
    CFACC      = #-offload-build #-no-offload
    CFOPENMP   = -openmp
    CFWARN     = -Wall
    CFDEBUGINFO= -g
    CFDEBUG    = 
    CFMODINC   =
    CFAUTOPAR  = -parallel -par-report1
    CFFPP      = -fpp
    CFC99      = -std=c99
    CFDLL      = -fPIC
    CFTRACE    = -traceback
ifeq ($(OSNAME),windows)
    CC         = icc
    CFMODINC   = -module=
    CFFREE     = /free
    CFOPENMP   = -Qopenmp
    CFWARN     = -warn:all
    CFWARNERROR= -warn:error
#     CFDEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -fpe0 -gen-interface -traceback
    CFDEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -gen-interface
    CFF95      = -assume:norealloc_lhs
    CFDLL      = /libs:dll 
endif
endif
# Gcc COMPILER
ifeq ($(CCOMPILER),0)
    CC         = gcc
    COUT       = -o
    CFFREE     =
    CFOPT      = -O3
    CFOPTO5    = -O5
    CFACC      = #-offload-build #-no-offload
    CFOPENMP   = -fopenmp
    CFWARN     = -Wall
    CFDEBUGINFO= -g
    CFDEBUG    = 
    CFMODINC   =
    CFAUTOPAR  = -parallel -par-report1
    CFFPP      = -fpp
    CFC99      = -std=c99
    CFDLL      = -fPIC
    CFTRACE    = -traceback
ifeq ($(OSNAME),windows)
    CC         = icc
    CFMODINC   = -module=
    CFFREE     = /free
    CFWARN     = -warn:all
    CFWARNERROR= -warn:error
#     CFDEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -fpe0 -gen-interface -traceback
    CFDEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -gen-interface
    CFDLL      = /libs:dll 
    CC         = gcc
    CFMODINC   = 
    CFFREE     = 
    CFWARN     = -Wall
    CFWARNERROR= 
    CFDEBUG    = 
    CFDLL      = -fPIC
endif
endif

# INTEL C COMPILER
ifeq ($(CCOMPILER),2)
    CC         = cl /nologo
    COUT       = /Fe
endif
