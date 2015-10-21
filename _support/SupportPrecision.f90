module SupportPrecision
    ! Compiler interface to iso_c_binding
    use SupportCompiler, only: C_FLOAT, C_DOUBLE, C_CHAR, C_INT, C_BOOL
    ! Compiler interface to iso_fortran_env
    use SupportCompiler, only: REAL32, REAL64, INT32, INT64
    ! Compiler interface to int_ptr_kind
    use SupportCompiler, only: IPTRK
    !
    integer, parameter :: R4 = REAL32   ! 32 bits
    integer, parameter :: R8 = REAL64   ! 64 bits
    integer, parameter :: SP = kind(1e0)! "Single precision"
    integer, parameter :: DP = kind(1d0)! "Double precision"
    integer, parameter :: MK = C_DOUBLE ! MK stands for My Kind

contains
    ! --------------------------------------------------------------------------------
    ! ---  
    ! --------------------------------------------------------------------------------
    subroutine print_precision_kinds()
        print*,'C_INT    ',C_INT    
        print*,'C_FLOAT  ',C_FLOAT  
        print*,'C_DOUBLE ',C_DOUBLE 
        print*,'C_CHAR   ',C_CHAR   
        print*,'C_BOOL   ',C_BOOL   
        print*,'INT32    ',INT32    
        print*,'INT64    ',INT64    
        print*,'REAL32   ',REAL32   
        print*,'REAL64   ',REAL64   
        print*,'SP       ',kind(1e0)
        print*,'DP       ',kind(1d0)
    end subroutine



    ! Below we have functions for MK, DP, SP (no interface is used because redundancy is possible)
    ! --------------------------------------------------------------------------------
    ! --- MK, default 
    ! --------------------------------------------------------------------------------
    logical function precision_equal(x,y) result(b)
        real(MK), intent(in) :: x,y
        b=.not.precision_different(x,y)
    end function

    logical function precision_different(x,y) result(b)
        real(MK), intent(in) :: x,y
        b= abs(x -y) >0.0_MK 
    end function

    ! --------------------------------------------------------------------------------
    ! ---  Double precision
    ! --------------------------------------------------------------------------------
    logical function precision_equal_dp(x,y) result(b)
        real(DP), intent(in) :: x,y
        b=.not.precision_different_dp(x,y)
    end function

    logical function precision_different_dp(x,y) result(b)
        real(DP), intent(in) :: x,y
        b= abs(x -y) >0.0_MK 
    end function

    ! --------------------------------------------------------------------------------
    ! ---  Single precision
    ! --------------------------------------------------------------------------------
    logical function precision_equal_sp(x,y) result(b)
        real(SP), intent(in) :: x,y
        b=.not.precision_different_sp(x,y)
    end function

    logical function precision_different_sp(x,y) result(b)
        real(SP), intent(in) :: x,y
        b= abs(x -y) >0.0_MK 
    end function

end module
