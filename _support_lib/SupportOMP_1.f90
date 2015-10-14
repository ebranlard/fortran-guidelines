!> 
module SupportOMP
    use OMP_LIB
    use ProcTypes, only:T_Proc
    implicit none

    private
    public :: omp_init
    public :: omp_term
    public :: omp_set_proc_nthreads
    public :: omp_get_proc_init_nthreads
    public :: omp_get_proc_max_nthreads
    public :: omp_initialized
contains
            
    ! --------------------------------------------------------------------------------
    ! --- Mains
    ! --------------------------------------------------------------------------------
    subroutine omp_init(Proc)
        type(T_Proc), intent(inout) :: Proc
        Proc%nthreads_init=omp_get_max_threads() !OTHER-COMPILER
        Proc%nthreads_max =omp_get_num_procs()!OTHER-COMPILER
        Proc%bOMPInitialized = .true.
        !call log_info('Number of procs: '//num2str(nprocs)//' - max number of threads: '//num2str(nmax_threads))
        !if(nprocs>nmax_threads) then
        !    call log_warning('Not using full openmp capacity: '//num2str(nmax_threads)//'/'//num2str(nprocs))
        !endif
    end subroutine
   
    subroutine omp_term(Proc)
        type(T_Proc), intent(inout) :: Proc
        Proc%nthreads_init =-1
        Proc%nthreads_max  =-1
        Proc%bOMPInitialized = .false.
    end subroutine
    ! --------------------------------------------------------------------------------
    ! --- Mutator
    ! --------------------------------------------------------------------------------
    subroutine omp_set_proc_nthreads(Proc,n)
        type(T_Proc), intent(inout) :: Proc
        integer, intent(in) :: n 
        call omp_set_num_threads(n)
        Proc%nthreads=n
    end subroutine

    ! --------------------------------------------------------------------------------
    ! --- Accessor 
    ! --------------------------------------------------------------------------------
    integer function omp_get_proc_init_nthreads(Proc) result(n)
        type(T_Proc), intent(in) :: Proc
        n=Proc%nthreads_init
    end function

    integer function omp_get_proc_max_nthreads(Proc) result(n)
        type(T_Proc), intent(in) :: Proc
        n=Proc%nthreads_max
    end function

    logical function omp_initialized(Proc) result(b)
        type(T_Proc), intent(in) :: Proc
        b=Proc%bOMPInitialized
    end function
end module SupportOMP
