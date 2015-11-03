module SupportCompiler
    implicit none
    !
    integer, parameter :: IPTRK=int_ptr_kind() !< for pointers
    integer, parameter :: RECORD_LENGTH=1      !< for direct access binaries
    !
    integer, parameter :: ISTR_LEN = 64 !< parameter for ease of comparison of parameter-strings
    character(len=ISTR_LEN), parameter :: FORTRAN_COMPILER='compaq'


    contains
        !
       integer function command_argument_count() result(n)
           use DFLIB
           n=NARGS()
       end function

       subroutine get_command_argument(i,arg) 
           use DFLIB
           integer, intent(in) :: i
           character(len=*), intent(inout) :: arg
           call getarg(i,arg)
       end subroutine
end module
