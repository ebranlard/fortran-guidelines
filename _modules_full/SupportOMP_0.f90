!> 
module SupportOMP
    implicit none
    public :: omp_init
    public :: omp_term
    !public :: omp_set_num_threads
    !public :: omp_get_max_threads
    !public :: omp_get_num_procs
    public :: omp_set_proc_nthreads
    public :: omp_get_proc_init_nthreads
    public :: omp_get_proc_max_nthreads
    public :: omp_initialized
contains
    ! --------------------------------------------------------------------------------
    ! --- Mains
    ! --------------------------------------------------------------------------------
    subroutine omp_init()
        use BadgerData, only: Device ! Exception
        Device%bOMPInitialized = .true.
    end subroutine
    
    subroutine omp_term()
        use BadgerData, only: Device ! Exception
        Device%bOMPInitialized = .false.
    end subroutine
        
    ! --------------------------------------------------------------------------------
    ! --- Mutator
    ! --------------------------------------------------------------------------------
    subroutine omp_set_proc_nthreads(n)
        integer, intent(in) :: n 
        if(n==-1) then
        endif
    end subroutine

    ! --------------------------------------------------------------------------------
    ! --- Accessor 
    ! --------------------------------------------------------------------------------
    integer function omp_get_proc_init_nthreads() result(n)
        use BadgerData, only: Device ! Exception
        n=Device%nthreads_init
    end function
    
   integer function omp_get_proc_max_nthreads() result(n)
        n=1
    end function
    
    logical function omp_initialized() result(b)
        use BadgerData, only: Device ! Exception
        b=Device%bOMPInitialized
    end function
            
end module SupportOMP
