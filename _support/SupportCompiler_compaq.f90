module SupportCompiler
    ! Builtin types kind
    integer, parameter :: C_INT    = 4 
    integer, parameter :: C_FLOAT  = 4
    integer, parameter :: C_DOUBLE = 8
    integer, parameter :: C_CHAR   = 1
    !integer, parameter :: C_BOOL   = 4  ! TODO
    integer, parameter :: INT32  = 4
    !integer, parameter :: INT64 = 8 ! TODO
    integer, parameter :: REAL32 = 4
    integer, parameter :: REAL64 = 8

    !
    integer, parameter :: IPTRK=int_ptr_kind() !< for pointers
    integer, parameter :: RECORD_LENGTH=1      !< for direct access binaries
    !
    integer, parameter :: ISTR_LEN = 64 !< parameter for ease of comparison of parameter-strings
    character(len=ISTR_LEN), parameter :: FORTRAN_COMPILER='compaq'



end module
