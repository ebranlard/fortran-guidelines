program test
    ! character(len=20), dimension(5) :: v_s
    character*20 :: v_s(5)
!     character(:), allocatable  :: v_s2(:)
!     character(len=:), dimension(:), pointer  :: v_s2
    character(len=20), dimension(:), allocatable :: strings2 ! fine 

    ! allocate(v_s2(size(v_s,1),size(v_s,2)))
    v_s(1)='a'
    v_s(2)='b'
    v_s(3)='c'


    print*,v_s(1)(1:20)
    print*,len(v_s(1)),size(v_s,1)
!     allocate(character(len=len(v_s(1))) :: v_s2(size(v_s,1)))
    allocate(character(len=len(v_s(1))) :: strings2(size(v_s,1)))
!     v_s2(1)='a'
    strings2(1)='a'

    call disp(v_s(1))

contains
    subroutine disp(msg)
        CHARACTER*(*)         msg
        print*,msg
    end subroutine

end program
