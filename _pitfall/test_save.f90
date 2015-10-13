program test_save
    implicit none
    call test_init_bad()
    call test_init_bad()
    call test_init_good()
    call test_init_good()
contains
    !
    subroutine test_init_bad()
        integer :: var =0! bad
        print *, var
        var = 5
    end subroutine
    !
    subroutine test_init_good()
        integer :: var
        var = 0
        print *, var
        var = 5
    end subroutine
end program
