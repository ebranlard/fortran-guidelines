module MemoryManager
    use SupportPrecision, only: MK
    implicit none


    !> Allocates and copy. (compiler dependent and fortran version dependent)
    ! In fortran 2003 allocate(A,source=B), or simply A=B
    ! gfortran supported
    ! ifortran use the flag -assume realloc_lhs
    !      interface allocate_copy
    !          module procedure allocate_copy1, allocate_copy2
    !      end interface

    interface resize_array
        module procedure rresize_array1,iresize_array1,lresize_array1, rresize_array2, iresize_array2
    end interface
    interface allocate_safe; module procedure &
            rallocate_safe1,iallocate_safe1,ballocate_safe1,iallocate_safe2,rallocate_safe2,rallocate_safe3,&
            rallocate_safe4
    end interface
    interface append_arrays
        module procedure rappend_arrays2
    end interface

    private
    public :: append_arrays
    public :: resize_array
    public :: allocate_safe
contains

    ! --------------------------------------------------------------------------------
    ! --- Append arrays 
    ! --------------------------------------------------------------------------------
    !> Real, dimension 2
    subroutine rappend_arrays2(array1,n1,array2,n2,margin_factor,default_val_in)
        real(MK),dimension(:,:), allocatable :: array1
        real(MK),dimension(:,:) :: array2
        integer , intent(inout) :: n1 !< SIDE EFFECTS
        integer , intent(in) :: n2
        real(MK), intent(in), optional :: margin_factor !< value >1 Used if reallocation is needed to allow for additional empty space
        real(MK), intent(in), optional :: default_val_in !< default value for empty space (in combination with margin_factor)
        ! Local variables
        integer  :: nNew, nVar
        real(MK) :: default_val
        ! --- Safety
        nVar=size(array1,1)
        if(size(array2,1)/=nVar) then
            print*,         'Append array: leading dimension mismatch'
            !call log_error('Append array: leading dimension mismatch')
            STOP
        endif


        nNew=n1+n2
        ! --- Making enough space if needed
        if(nNew>size(array1,2)) then
            if(.not.present(default_val_in)) then
                default_val=0.0_MK
            else
                default_val=default_val_in
            endif
            if(present(margin_factor)) then
                nNew=int(nNew*margin_factor)
            endif
            call resize_array(array1,nNew,default_val)
        endif

        ! --- Appending
        array1(1:nVar,(n1+1):(n1+n2))=array2(1:nVar,1:n2)

        ! updating n1
        n1=n1+n2;

    end subroutine 
    

    ! --------------------------------------------------------------------------------
    ! ---   Resize array
    ! --------------------------------------------------------------------------------
 

    !> Real, dimension 1
    subroutine rresize_array1(array,nNewSize,default_val)
        real(MK),dimension(:),allocatable,intent(inout) :: array
        integer , intent(in) :: nNewSize
        real(MK), intent(in) :: default_val
        ! Local variables
        real(MK),dimension(:),allocatable :: tmp !< backup of input
        integer :: nDimTmp
        integer :: AllocateStatus
        ! To save memory, if nNewSize is below second dim, we take the min
        nDimTmp= min(size(array,1),nNewSize)

        ! Making of copy of the input
        allocate(tmp(1:nDimTmp), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        tmp(1:nDimTmp)=array(1:nDimTmp)
        ! Reallocating the array 
        deallocate(array)
        allocate(array(1:nNewSize), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        ! We copy the original data into it
        array(1:nDimTmp)=tmp(1:nDimTmp)
        if(nDimTmp+1<=nNewSize) array(nDimTmp+1:nNewSize)=default_val
    end subroutine 

    !> Logical, dimension 1
    subroutine lresize_array1(array,nNewSize,default_val)
        logical,dimension(:),allocatable,intent(inout) :: array
        integer , intent(in) :: nNewSize
        logical, intent(in) :: default_val
        ! Local variables
        logical,dimension(:),allocatable :: tmp !< backup of input
        integer :: nDimTmp
        integer :: AllocateStatus
        ! To save memory, if nNewSize is below second dim, we take the min
        nDimTmp= min(size(array,1),nNewSize)

        ! Making of copy of the input
        allocate(tmp(1:nDimTmp), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        tmp(1:nDimTmp)=array(1:nDimTmp)
        ! Reallocating the array 
        deallocate(array)
        allocate(array(1:nNewSize), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        ! We copy the original data into it
        array(1:nDimTmp)=tmp(1:nDimTmp)
        if(nDimTmp+1<=nNewSize) array(nDimTmp+1:nNewSize)=default_val
    end subroutine 

    !> Integer, dimension 1
    subroutine iresize_array1(array,nNewSize,default_val)
        integer,dimension(:),allocatable,intent(inout) :: array
        integer , intent(in) :: nNewSize
        integer, intent(in) :: default_val
        ! Local variables
        integer,dimension(:),allocatable :: tmp !< backup of input
        integer :: nDimTmp
        integer :: AllocateStatus
        ! To save memory, if nNewSize is below second dim, we take the min
        nDimTmp= min(size(array,1),nNewSize)

        ! Making of copy of the input
        allocate(tmp(1:nDimTmp), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        tmp(1:nDimTmp)=array(1:nDimTmp)
        ! Reallocating the array 
        deallocate(array)
        allocate(array(1:nNewSize), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        ! We copy the original data into it
        array(1:nDimTmp)=tmp(1:nDimTmp)
        if(nDimTmp+1<=nNewSize) array(nDimTmp+1:nNewSize)=default_val
    end subroutine 

    !> Real, dimension 2
    subroutine rresize_array2(array,nNewSize,default_val)
        real(MK),dimension(:,:),allocatable,intent(inout) :: array
        integer , intent(in) :: nNewSize
        real(MK), intent(in) :: default_val
        ! Local variables
        real(MK),dimension(:,:),allocatable :: tmp !< backup of input
        integer :: nFirstDim
        integer :: nSecondDimTmp
        integer :: AllocateStatus
        nFirstDim= size(array,1)
        ! To save memory, if nNewSize is below second dim, we take the min
        nSecondDimTmp= min(size(array,2),nNewSize)

        ! Making of copy of the input
        allocate(tmp(1:nFirstDim, 1:nSecondDimTmp), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        tmp(1:nFirstDim,1:nSecondDimTmp)=array(1:nFirstDim,1:nSecondDimTmp)
        ! Reallocating the array 
        deallocate(array)
        allocate(array(1:nFirstDim,1:nNewSize), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        ! We copy the original data into it
        array(1:nFirstDim,1:nSecondDimTmp)=tmp(1:nFirstDim,1:nSecondDimTmp)
        if(nSecondDimTmp+1<=nNewSize) array(1:nFirstDim,nSecondDimTmp+1:nNewSize)=default_val

    end subroutine 


    !> Integer, dimension 2
    subroutine iresize_array2(array,nNewSize,default_val)
        integer,dimension(:,:),allocatable,intent(inout) :: array
        integer , intent(in) :: nNewSize
        integer, intent(in) :: default_val
        ! Local variables
        integer,dimension(:,:),allocatable :: tmp !< backup of input
        integer :: nFirstDim
        integer :: nSecondDimTmp
        integer :: AllocateStatus
        nFirstDim= size(array,1)
        ! To save memory, if nNewSize is below second dim, we take the min
        nSecondDimTmp= min(size(array,2),nNewSize)

        ! Making of copy of the input
        allocate(tmp(1:nFirstDim, 1:nSecondDimTmp), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        tmp(1:nFirstDim,1:nSecondDimTmp)=array(1:nFirstDim,1:nSecondDimTmp)
        ! integerlocating the array 
        deallocate(array)
        allocate(array(1:nFirstDim,1:nNewSize), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        ! We copy the original data into it
        array(1:nFirstDim,1:nSecondDimTmp)=tmp(1:nFirstDim,1:nSecondDimTmp)
        if(nSecondDimTmp+1<=nNewSize) array(1:nFirstDim,nSecondDimTmp+1:nNewSize)=default_val

    end subroutine 

    ! --------------------------------------------------------------------------------
    ! --- Allocate safe 
    ! --------------------------------------------------------------------------------

    subroutine iallocate_safe1(array_name,array,dim1,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        integer, dimension(:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        integer, intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                !call log_error('Array '//trim(array_name)//' is already allocated')
                STOP
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1)=default_val
            endif
        endif

    end subroutine
    
    subroutine ballocate_safe1(array_name,array,dim1,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        logical, dimension(:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        logical, intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                !call log_error('Array '//trim(array_name)//' is already allocated')
                STOP
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
                STOP
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1)=default_val
            endif
        endif

    end subroutine
    
    subroutine rallocate_safe1(array_name,array,dim1,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        real(MK), dimension(:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        real(MK), intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                !call log_error('Array '//trim(array_name)//' is already allocated')
                STOP
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1)=default_val
            endif
        endif

    end subroutine
    
    
    subroutine iallocate_safe2(array_name,array,dim1,dim2,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        integer,dimension(:,:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        integer , intent(in) :: dim2
        integer, intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                !call log_error('Array '//trim(array_name)//' is already allocated')
                STOP
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1,1:dim2),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1,1:dim2)=default_val
            endif
        endif

    end subroutine

    
    subroutine rallocate_safe2(array_name,array,dim1,dim2,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        real(MK),dimension(:,:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        integer , intent(in) :: dim2
        real(MK), intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                !call log_error('Array '//trim(array_name)//' is already allocated')
                STOP
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1,1:dim2),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1,1:dim2)=default_val
            endif
        endif

    end subroutine

    subroutine rallocate_safe3(array_name,array,dim1,dim2,dim3,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        real(MK),dimension(:,:,:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        integer , intent(in) :: dim2
        integer , intent(in) :: dim3
        real(MK), intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                !call log_error('Array '//trim(array_name)//' is already allocated')
                STOP
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1,1:dim2,1:dim3),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1,1:dim2,1:dim3)=default_val
            endif
        endif

    end subroutine

    subroutine rallocate_safe4(array_name,array,dim1,dim2,dim3,dim4,default_val,bDealloc)
        character(len=*), intent(in) :: array_name
        real(MK),dimension(:,:,:,:),allocatable,intent(inout) :: array
        integer , intent(in) :: dim1
        integer , intent(in) :: dim2
        integer , intent(in) :: dim3
        integer , intent(in) :: dim4
        real(MK), intent(in) :: default_val
        logical, intent(in) :: bDealloc
        ! 
        integer :: ierr

        if(allocated(array)) then
            if (.not.bDealloc) then
                print*,        'Array '//trim(array_name)//' is already allocated'
                STOP
                !call log_error('Array '//trim(array_name)//' is already allocated')
            endif
            deallocate(array,stat=ierr)
            if(ierr/=0) then
                print*,          'Deallocation of '//trim(array_name)//' failed'
                !call log_warning('Deallocation of '//trim(array_name)//' failed')
            endif
        endif
        if(.not.allocated(array)) then
            allocate(array(1:dim1,1:dim2,1:dim3,1:dim4),stat=ierr)
            if(ierr/=0) then
                print*,        'Cannot allocate array'//trim(array_name)//'. status:',ierr
                !call log_error('Cannot allocate array'//trim(array_name)//'.')
                STOP
            else
                array(1:dim1,1:dim2,1:dim3,1:dim4)=default_val
            endif
        endif

    end subroutine

end module MemoryManager
