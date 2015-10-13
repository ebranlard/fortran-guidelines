module SupportCompiler
    ! Builtin types kind
    use iso_c_binding, only: C_INT, C_CHAR, C_BOOL, C_FLOAT, C_DOUBLE
    use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
    !
    integer, parameter :: IPTRK=int_ptr_kind() !< for pointers
    integer, parameter :: RECORD_LENGTH=1      !< for direct access binaries
    !
    integer, parameter :: ISTR_LEN = 64 !< parameter for ease of comparison of parameter-strings
    character(len=ISTR_LEN), parameter :: FORTRAN_COMPILER='ifort'

end module
