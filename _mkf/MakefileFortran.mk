f=f90

# INTEL FORTRAN COMPILER
ifeq ($(FCOMPILER),1)
    FC         = ifort
    FFNOLOGO   = -nologo
    FFFREE     = -free
    FFOPT0     = -O0
    FFOPT      = -O3
    FFOPTO3    = -O3
    FFOPTO5    = -O5
    FFACC      = #-offload-build #-no-offload
    FFOPENMP   = -openmp
    FFWARN     = -warn all
    FFWARNEXTRA= 
    FFDEBUGINFO= -g
    FFDEBUG    = -check bounds -check format -check output_conversion -check pointers -check uninit -debug full -gen-interface
    FFPE       = -fpe0 
    FFDEBUGARG = -check arg_temp_created
    FFMODINC   = -module 
    FFAUTOPAR  = -parallel -par-report1
    FFFPP      = -fpp
    FFF90      = -stand f90
    FFF95      = -stand f95
#      FFF95      = -assume norealloc_lhs
    FFF03      = -assume realloc_lhs -stand f03
    FFDLL      = -fPIC
    FFTRACE    = -traceback
    FFBYTERECL = -assume byterecl
    FFSAVE     = -save
ifeq ($(OSNAME),windows)
    FFOPT0     = -O0
    FFOPTO5    = -O3
    FFOPENMP   = -Qopenmp
    FFMODINC   = -module=
    FFWARN     = -warn:all
    FFWARNERROR= -warn:error
    FFDEBUGINFO= 
#     FFDEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -fpe0 -gen-interface -traceback
    FFDEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -gen-interface
    FFF95      = -assume:norealloc_lhs
    FFDLL      = /libs:dll 
    FFSAVE     = /Qsave
#      FFDLL      = /iface:stdcall 
endif
endif

# GFORTRAN COMPILER
ifeq ($(FCOMPILER),0)
# Wall contains: -Waliasing, -Wampersand, -Wconversion, -Wsurprising, -Wc-binding-type, -Wintrinsics-std, -Wno-tabs,  
# 	              -Wintrinsic-shadow, -Wline-truncation, -Wtarget-lifetime, -Wreal-q-constant -Wunused
# Other Flags:
#  -Warray-temporaries -Wcharacter-truncation:
#     FC		   = gfortran-4.8
    FC		   = gfortran
    FFNOLOGO   = 
    FFFREE     = -free 
    FFOPT      = -O3
    FFOPTO3    = -O3
    FFOPTO5    = -O5
    FFOPENMP   = -fopenmp
    FFWARN     = -Wall -Wno-intrinsic-shadow  -Wtabs -Wuninitialized -O -Wunused
#     -Wno-c-binding-type -Wno-unused-function 
    FFWARNEXTRA= -Wcharacter-truncation -Wextra -Wno-implicit-interface -Wno-implicit-procedure -Wunderflow -Wunused-dummy-argument -Wunused-parameter -Wmaybe-uninitialized 
    FFDEBUGINFO= -g
    FFDEBUG    = -fbounds-check -finit-real=nan 
    FFPE       = -ffpe-trap=invalid,zero,overflow 
    FFMODINC   = -J
    FFAUTOPAR  = 
    FFFPP      = -cpp
    FFF95      = -std=f95 -fno-realloc-lhs
    FFF03      = -ffree-line-length-none
    FFDLL      = -fPIC
    FFTRACE    = -fbacktrace -fdump-core 
    FFBYTERECL = 
ifeq ($(OSNAME),windows)
    FFDLL      = 
endif
endif


# SUN COMPILER
ifeq ($(FCOMPILER),2)
    FC		   = f95
    FFFREE     = -free
    FFNOLOGO   = 
    FFOPT      = -O3
    FFOPENMP   = -openmp
    FFWARN     =
    FFDEBUGINFO= -g
    FFDEBUG    = -C
    FFMODINC   = -M
    FFAUTOPAR  = -xloopinfo -xautopar
    FFFPP      = -xpp
#  FFLAGS    = -xopenmp=noopt
endif


# --------------------------------------------------------------------------------
# --- MKL LIBRARY 
# --------------------------------------------------------------------------------
## DEFAULT VARIABLES (May be overriden upstream)
ifeq ($(MKL_VERSION),)
    MKL_VERSION=12
endif
ifeq ($(MKL_DIR),)
    MKL_DIR=
endif


# !!!!!!!!!!!!!!!!!!!!!!!!!    IMPORTANT NOTE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Python and MKL have some conflicts (see e.g.https://github.com/GalSim-developers/GalSim/issues/261)
# 
# mkl_intel_lp64: resolves the lapack symbols at compilation but creates a bug when loading from python
# It seems the solution is to link with -lmkl_rt instead of -lmkl_sequential -lmkl_intel_lp64 -lmkl_core
# In newer version where mkl_rt is available, then it's fine. 
# For older versions, I had to add mkl_lapack AND mkl_mc3
# I added a switch depending on MKL_VERSION. 
#


## INFERFACE LAYER
# This layer provides matching between the compiled code of an application and the threading/computational
# components of the library
# CHOICES: lmkl_intel_ilp64 lmkl_intel_lp64 lmkl_gf_lp64 lmkl_gf_ilp64 lmkl_intel_sp2dp
#
MKL_INTERF=
ifeq ($(ARCHI),amd64)
    MKL_INTERF=-lmkl_intel_lp64
endif
ifeq ($(MKL_VERSION),12)
    # cf IMPORTANT NOTE above
    MKL_INTERF=-lmkl_rt 
endif

## COMPUTATIONAL LAYER
# This is the heart of MKL and has only one variant for any processor/operating system family, The
# computational layer accommodates multiple architectures through identification of the architecture or
# architectural feature and chooses the appropriate binary code at execution. Intel MKL may be thought of as
# the large computational layer that is unaffected by different computational environments. Then, as it has no
# RTL requirements, RTLs refer not to the computational layer but to one of the layers above it: the Interface
# layer or Threading layer. The most likely case is matching the threading layer with the RTL layer.
# CHOICES: lmkl_lapack lmkl_core lmkl_scalapack_lp64/ilp64
MKL_COMPUT=-lmkl_core
ifeq ($(ARCHI),ia32)
    MKL_COMPUT=-lmkl_intel -lmkl_core
endif
ifeq ($(MKL_VERSION),11)
    # cf IMPORTANT NOTE above
    MKL_COMPUT=-lmkl_lapack -lmkl_core -lmkl_mc3
endif

## THREADING LAYER
# This layer helps the threaded MKL to co-operate with compiler level threading. This also provides the
# sequential version lay
# CHOICES: lmkl_intel_thread lmkl_gnu_thread lmkl_pgi_thread lmkl_sequential
ifeq ($(OSNAME),linux) 
    ifeq ($(LIB_ACCELERATOR),0)
         MKL_THREAD=-lmkl_sequential
    else
        ifeq ($(OPENMP),0)
            MKL_THREAD=-lmkl_sequential
        else
             ifeq ($(FCOMPILER),0)
                  MKL_THREAD=-lmkl_gnu_thread
             else
                  MKL_THREAD=-lmkl_intel_thread
             endif
         endif
    endif
endif

ifeq ($(OSNAME),windows) 
    ifeq ($(LIB_ACCELERATOR),0)
         MKL_THREAD=-Qmkl:sequential
    else
        ifeq ($(OPENMP),0)
            MKL_THREAD=-Qmkl:sequential
        else
			# if threaded, IO might fail, but seem ok with dbglibs. Otherwise het rid of IO, or threads. 
            MKL_THREAD=-threads -dbglibs -Qmkl:sequential 
         endif
    endif
endif
# To force the use of specified number of threads: export MKL_DYNAMIC=FALSE (but a bad idea)

## RUN-TIME LIBRARY LAYER
# This layer has run-time library support functions. For example, libiomp and libguide are run-time libraries
# providing threading support for the OpenMP threading in Intel MKL. Note that when using the legacy
# libguide you should also link against the POSIX threads library by appending -lpthread.
# In addition to the libraries provided through the layered model you have the solver libraries, Fortran 90/95
# interfaces and cluster components. Each of them fits in the computational or RTL layer.
# CHOICES: Solver Libraries mkl_solver_ilp64_sequential mkl_solver_lp64_sequential lmkl_solver_lp64 lmkl_solver_ilp64
# CHOICES Fortran 90/95 Interfaces: lmkl_lapack95 lmkl_blas95
# CHOICES Cluster Components: lmkl_blacs_intelmpi_ilp64 libmkl_blacs_intelmpi_lp64 lmkl_blacs_openmpi_ilp64 libmkl_blacs_openmpi_lp64 lmkl_blacs_sgimpt_ilp64 lmkl_blacs_sgimpt_lp64 lmkl_cdft_core lmkl_scalapack_ilp64 lmkl_scalapack_lp64
# CHOICES FFT Interfaces: lfftw2x_cdft_DOUBLE/SINGLE lfftw2xc_intel/_sp lfftw2xf_intel/_sp lfftw3xc_intel/_sp lfftw3xf_intel/_sp 
MKL_RUNTIME=
ifeq ($(LIB_ACCELERATOR),1)
	#     MKL_RUNTIME=-liomp5 -lpthread   # Introduced some bugs
#     MKL_RUNTIME=-lgomp -lpthread 
#     MKL_RUNTIME=-lguide -lpthread 
    MKL_RUNTIME= -lpthread 
#     
endif


ifeq ($(OSNAME),linux) 
    ifeq ($(ARCHI),ia32)
        MKL_DIR := $(MKL_DIR)$(MKL_32)
    endif
    ifeq ($(ARCHI),amd64)
        MKL_DIR := $(MKL_DIR)$(MKL_64)
    endif
endif



## DUMP of WINDOWS DLL FLAGS
#### FROM MIN TEST
    # DEBUG
# FFLAGS=/nologo /debug:full /Od /warn:interfaces /module:".\\" /object:".\\" /Fd"vc100.pdb" /traceback /check:bounds /libs:dll /threads /dbglibs /Qmkl:sequential 
# RELEASE
# LDFLAGS_DLL=/NOLOGO /SUBSYSTEM:WINDOWS /DLL  /OUT:"test.dll" 
# LDFLAGS_DLL=/INCREMENTAL:NO /NOLOGO /MANIFEST /MANIFESTFILE:"raccoon.dll.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /DEBUG /PDB:"test.pdb" /SUBSYSTEM:WINDOWS /IMPLIB:"test.lib" /DLL  /OUT:"test.dll" 
# FFLAGS=/nologo /O3 /module:".\\" /object:".\\"  /traceback /libs:dll /threads /dbglibs /Qmkl:sequential 
# FFLAGS=/nologo /O3 /module:".\\" /object:".\\"  /traceback /libs:dll                   /Qmkl:sequential 

#### ADAPTED FOR OMNIVOR
# DEBUG
# LDFLAGS_DLL=/OUT:"_lib\windows-ia32\libraccoon.dll" /INCREMENTAL:NO /NOLOGO /LIBPATH:$(LIB_DIR) /MANIFEST /MANIFESTFILE:"_lib\windows-ia32\raccoon.dll.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /DEBUG /PDB:"_lib\windows-ia32\raccoon.pdb" /SUBSYSTEM:WINDOWS /IMPLIB:"_lib\windows-ia32\raccoon.lib" /DLL 
# RELEASE
# LDFLAGS=/OUT:"raccoon.dll" /NOLOGO /MANIFEST /MANIFESTFILE:"raccoon.dll.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /SUBSYSTEM:WINDOWS /IMPLIB:"raccoon.lib" /DLL
#  /OUT:"hawc2mb.exe" /INCREMENTAL:NO /NOLOGO /MANIFEST /MANIFESTFILE:"F:\Exchange\hawc2mb\hawc2mb\Release\hawc2mb.exe.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /SUBSYSTEM:CONSOLE /IMPLIB:"hawc2mb.lib" 
#### ORIGINALS
  # ORIGINALS
    #/I"_includes"
    # DEBUG
# FFLAGS=/nologo /debug:full /Od /warn:interfaces /module:"_build\windows-ia32\\" /object:"_build\windows-ia32\\" /Fd"_build\windows-ia32\vc100.pdb" /traceback /check:bounds /libs:dll /threads /dbglibs /Qmkl:sequential /c
    # RELEASE
# FFLAGS=/nologo /module:"Release\\" /object:"Release\\" /Fd"Release\vc100.pdb" /libs:dll    /threads /c
# FFLAGS=/nologo /module:"Release\\" /object:"Release\\" /Fd"Release\vc100.pdb" /libs:statuc /threads /c
    #/LIBPATH:"_includes"
    

ifeq ($(OSNAME),linux) 
    ifeq ($(LIB_ACCELERATOR),2)
        LDFLAGS_MKL = 
        LIBS_MKL    = -llapack
    else
        LDFLAGS_MKL = -Wl,-R/$(MKL_DIR)
        LIBS_MKL    = -L$(MKL_DIR) $(MKL_INTERF) $(MKL_THREAD) $(MKL_COMPUT) $(MKL_RUNTIME) 
    endif

    LDFLAGS_DLL= $(LDFLAGS_MKL)
endif
ifeq ($(OSNAME),windows) 
    ifeq ($(FCOMPILER),1)
	    # DEBUG:
        #LDFLAGS =/nologo /SUBSYSTEM:WINDOWS /INCREMENTAL:NO 
	    # RELEASE:
        LDFLAGS_MKL  =/nologo /SUBSYSTEM:CONSOLE -threads -dbglibs    
        LIBS_MKL     =/Qmkl:sequential 
        LDFLAGS_DLL  =/DLL /OUT:
	# if threaded, IO might fail, but seem ok with dbglibs. Otherwise het rid of IO, or threads. 
    #FFLAGS+=/threads /dbglibs
    else
        LDFLAGS = NOT_SET
        LDFLAGS_DLL = NOT_SET
    endif
endif




# MPI
MPIFC  =   mpif90
MPIRUN =   mpirun -n $(PPN)
RUN =   mpirun 
ifeq ($(strip $(HOSTNAME)),jess.dtu.dk)
    MPIFC  =   mpiifort
    MPIRUN =   mpirun -n $(PPN)
    RUN    =   mpirun 
endif
ifeq ($(strip $(PBS_O_HOST)),jess.dtu.dk)
    MPIFC  =   mpiifort
    MPIRUN =   mpirun -n $(PPN)
    RUN    =   mpirun 
endif
ifeq ($(strip $(HOSTNAME)),g-000.risoe.dk)
    MPIFC  =   mpiifort
    MPIRUN =   mpirun -n $(PPN)
    RUN    =   mpirun 
endif
ifeq ($(strip $(HOSTNAME)),work)
ifeq ($(FCOMPILER),0)
MPIFC  =   mpif90.openmpi
MPIRUN =   mpirun.openmpi -n $(PPN)
    RUN    =   mpirun.openmpi
endif
endif
ifeq ($(strip $(HOSTNAME)),olympe)
ifeq ($(FCOMPILER),0)
MPIFC  =   mpif90.openmpi
MPIRUN =   mpirun.openmpi -n $(PPN)
RUN    =   mpirun.openmpi
endif
endif

