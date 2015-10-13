!> General Input Output module
module MainIO
    implicit none
contains
    !> Returns a free unit for an open statement
    integer function get_free_unit()
        integer :: i
        integer :: ios
        logical :: bopen

        get_free_unit = 0
        do i = 10, 99
            inquire ( unit = i, opened = bopen, iostat = ios )
            if ( ios == 0 ) then
                if ( .not. bopen ) then
                    get_free_unit = i
                    return
                end if
            end if
        end do
    end function


    !> Counts number of lines in a file
    integer function line_count(iunit)
        integer, intent(in) :: iunit
        character(len=512) :: line
        ! safety for infinite loop..
        integer :: i
        integer, parameter :: nline_max=100000000 ! 100 M
        line_count=0
        do i=1,nline_max 
            line=''
            read(iunit,'(A)',END=100)line
            line_count=line_count+1
            !print*,'l ',trim(line),i
        enddo
        if (line_count==nline_max) then
            print*,'Error: MainIO: maximum number of line exceeded'
            STOP
        endif
    100 if(len(trim(line))>0) then
            !print*,'le ',trim(line),i
            line_count=line_count+1
        endif
        rewind(iunit)
        return
    end function

!      subroutine filesize( filename, size )
!          ! This routine calls the routine Stat to obtain the file size
!          ! corresponding to a file name or returns -1 on error.
!          ! The standard version of the routine uses the file unit instead of file name.
!          ! Argument declarations:
!  #if defined __INTEL_COMPILER 
!          use                             ifport
!  #elif defined __GNUC__ 
!          ! intrinsic declarations:
!          integer(kind=1)              :: stat
!  #else
!  #endif
!  
!          integer, intent(out)         :: size
!          character(*), intent(in)     :: filename
!          ! local declarations:
!          integer                      :: statarray(13)
!          integer                      :: status
!  
!  #ifdef __INTEL_COMPILER 
!          integer                      :: ios
!          integer                      :: unit
!          !bjj: unit is not set before it is used!!!  should it also be an input parameter?
!          open( unit, file=trim( filename ), status='old', iostat=ios, action='read' )
!          if ( ios /= 0 )  then
!              size = -1
!          else
!              status = fstat( unit , statarray )
!          end if
!  #elif defined __GNUC__ 
!          status = stat( filename, statarray )
!  #else
!          call error(__file__,__line__,'compiler unknown');
!  #endif
!  
!          if ( status /= 0 ) then
!              size = -1
!          else
!              size = statarray(8)
!          end if
!          return
!      end subroutine filesize ! ( filename, size )
end module MainIO






