!> 
module PackFunctions
    use SupportPrecision, only: MK
    implicit none
contains

    !> Performs exactly like I_out=pack(I_in,bMask), but performs the allocation within the function. (kindof rhs_alloc..)
    ! But some compilers fail for large arrays.  
    subroutine pack_alloc(I_in,bMask,I_out)
        integer,dimension(:),intent(in)              :: I_in  !< 
        logical,dimension(:),intent(in)              :: bMask
        integer,dimension(:),allocatable,intent(out) :: I_out
        integer :: n_in,n_out,i,k
        integer :: ierr

        n_out=count(bMask)
        if(allocated(I_out)) then
            print*,'Warning: Pack_Alloc: I_out already allocated'
            deallocate(I_out)
        endif

        allocate(I_out(1:n_out),stat=ierr)
        if(ierr/=0) then
            print*,'Error: Pack_Alloc: allocation error',ierr,n_out
            STOP
        else
            k=0
            n_in=size(I_in)
            if(size(bMask)/=n_in) then
                print*,'Error: Pack_Alloc: Mask size different than input size'
                STOP
            endif
            do i=1,n_in
                if(bMask(i)) then
                    k=k+1
                    I_out(k)=I_in(i)
                endif
            enddo
        endif
    end subroutine

    !> Performs exactly like I_out=pack(I_in,bMask), wherer I_in=(/i,i=1,n/) and performs the allocation within the function. (kindof rhs_alloc..)
    subroutine packindex_alloc(bMask,I_out,n_out_opt)
        !
        logical,dimension(:),intent(in)              :: bMask
        integer,dimension(:),allocatable,intent(out) :: I_out
        integer, intent(out), optional               :: n_out_opt !< = size(I_out) = count(bMask)
        !
        integer :: n_in,n_out,i,k
        integer :: ierr

        n_out=count(bMask)
        if(present(n_out_opt)) then
            n_out_opt=n_out
        endif

        if(allocated(I_out)) then
            print*,'Warning: Packindex_Alloc: Warning, I_out already allocated'
            deallocate(I_out)
        endif

        allocate(I_out(1:n_out),stat=ierr)
        if(ierr/=0) then
            print*,'Error: Packindex_Alloc: allocation error',ierr,n_out
            STOP
        else
            k=0
            n_in=size(bMask)
            do i=1,n_in
                if(bMask(i)) then
                    k=k+1
                    I_out(k)=i
                endif
            enddo
        endif
    end subroutine

    !> Performs exactly like I_out=pack(I_in,bMask), wherer I_in=(/i,i=1,n/) 
    ! The size of I_out is already known, and I_out is allocated
    subroutine packindex(bMask,I_out,n_out)
        logical,dimension(:),intent(in)              :: bMask
        integer, intent(in) :: n_out
        integer,dimension(n_out),intent(out) :: I_out
        integer :: i,k,n_in

        if(n_out==0) then
            return
        endif
        if(n_out/=count(bMask)) then
            print*,'Error: Pack_index: Count(bMask) is different than n_in '
            STOP
        endif
        k=0
        n_in=size(bMask)
        do i=1,n_in
            if(bMask(i)) then
                k=k+1
                I_out(k)=i
            endif
        enddo
    end subroutine

            
end module PackFunctions
