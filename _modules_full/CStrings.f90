module CStrings
    implicit none
contains
    subroutine cstring2fortran(s_c,s)
        use SupportPrecision, only: C_CHAR
        character(kind=C_CHAR,len=1),dimension(*),intent(in) :: s_c
        character(len=*),intent(inout):: s
        integer :: i
        loop_string: do i=1,len(s)
            if ( s_c(i) == CHAR(0) ) then
                exit loop_string
            else
                s(i:i) = s_c(i)
            end if
        end do loop_string

        if(i==1) then
            s=''
        else
            s = s(1:(i-1))
            s = trim(s)
        endif
    end subroutine
    
    subroutine fortranstring2c(s_f,s_c,n)
        use SupportPrecision, only: C_CHAR
        character(len=*),intent(in):: s_f
        character(kind=C_CHAR,len=1),dimension(*),intent(inout) :: s_c
        integer, intent(out), optional :: n  
        integer :: i
        loop_string: do i=1,len(s_f)
            if ( s_f(i:i) == CHAR(0) ) then
                exit loop_string
            else
                s_c(i) = s_f(i:i)
            end if
        end do loop_string
        if(present(n))then
            n=i-1
        endif
    end subroutine
end module
