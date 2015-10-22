# NVIDIA CUDA COMPILER
CUDA_PATH    = /usr/local/cuda
NVCC         = nvcc
ifeq ($(shell echo $$HOSTNAME),work)
    CUDA_ARCH     = sm_21
else
    CUDA_ARCH     = sm_35
endif
NVCFLAGS     = -m64 -I$(CUDA_PATH)/include -I$(CUDA_PATH)/samples/common/inc -arch $(CUDA_ARCH) --compiler-options "-fPIC -fopenmp"

NVCFFREE     =
NVCFOPT      = -O3
NVCFACC      =
NVCFOPENMP   =
NVCFWARN     =
NVCFDEBUGINFO= -g -Xptxas=-v
NVCFDEBUG    =
NVCFMODINC   =
NVCFAUTOPAR  =
NVCFFPP      =
NVCFC99      = -std=c99
NVCFDLL      = -fPIC
NVCFTRACE    = 
