program test_save
    implicit none
    logical :: bGood
    bGood=.true.
    bGood=bGood.and.test_init_bad()==0
    bGood=bGood.and.test_init_bad()==5
    bGood=bGood.and.test_init_good()==0
    bGood=bGood.and.test_init_good()==0
    if (bGood) then 
        print'(A)','[ OK ]'
    else
        print'(A)','[FAIL]'
    endif
contains
    !
    integer function test_init_bad() result(i)
        integer :: var =0! bad
        !print *, var
        i=var
        var = 5
    end function
    !
    integer function test_init_good() result(i)
        integer :: var
        var = 0
        !print *, var
        i=var
        var = 5
    end function
end program
