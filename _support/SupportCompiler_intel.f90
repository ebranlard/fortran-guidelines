module SupportCompiler
    implicit none
    !
    integer, parameter :: IPTRK=int_ptr_kind() !< for pointers
    integer, parameter :: RECORD_LENGTH=1      !< for direct access binaries
    !
    integer, parameter :: ISTR_LEN = 64 !< parameter for ease of comparison of parameter-strings
    character(len=ISTR_LEN), parameter :: FORTRAN_COMPILER='ifort'

end module
