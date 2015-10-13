module SupportPrecision
  ! Compiler interface to iso_c_binding
  use SupportCompiler, only: C_FLOAT, C_DOUBLE, C_CHAR, C_INT, C_BOOL
  ! Compiler interface to iso_fortran_env
  use SupportCompiler, only: REAL32, REAL64, INT32, INT64
  ! Compiler interface to int_ptr_kind
  use SupportCompiler, only: IPTRK
  !
  integer, parameter :: R4 = REAL32   ! 32 bits
  integer, parameter :: R8 = REAL64   ! 64 bits
  integer, parameter :: SP = kind(1e0)! "Single precision"
  integer, parameter :: DP = kind(1d0)! "Double precision"
  integer, parameter :: MK = C_DOUBLE ! MK stands for My Kind
end module
