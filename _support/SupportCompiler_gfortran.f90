module SupportCompiler
    use iso_c_binding, only: C_INTPTR_T
    implicit none
    !
    integer, parameter :: IPTRK=C_INTPTR_T !< for pointers
    integer, parameter :: RECORD_LENGTH=1  !< for direct access binaries
    !
    integer, parameter :: ISTR_LEN = 64 !< parameter for ease of comparison of parameter-strings
    character(len=ISTR_LEN), parameter :: FORTRAN_COMPILER='gfortran'



end module
