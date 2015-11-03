module SupportISO
    use iso_c_binding
    use iso_fortran_env

    implicit none

contains

    subroutine print_iso()
        print*,'C_INTPTR_T', C_INTPTR_T
        print*,'C_INT', C_INT
    end subroutine

end module SupportISO
