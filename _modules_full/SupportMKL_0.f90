!> 
module SupportMKL
    implicit none

    TYPE, PUBLIC :: DFTI_DESCRIPTOR
        PRIVATE
        INTEGER :: dontuse
        ! Structure of this type is not used in Fortran code
        ! the pointer to this type is used only
    END TYPE DFTI_DESCRIPTOR

contains

    ! --------------------------------------------------------------------------------
    ! ---  
    ! --------------------------------------------------------------------------------
    SUBROUTINE D_INIT_HELMHOLTZ_3D(AX,BX,AY,BY,AZ,BZ,NX,NY,NZ,BCTYPE,Q,IPAR,DPAR,STAT)
        INTEGER NX, NY, NZ, STAT
        INTEGER IPAR(*)
        DOUBLE PRECISION AX,BX,AY,BY,AZ,BZ,Q
        CHARACTER(6) BCTYPE
        DOUBLE PRECISION DPAR(*)

        if(.false.) then
            Nx=0; Ny=0; Nz=0; Stat=0; iPar(1)=0;
            AX=0;AY=0;BX=0;BY=0;AZ=0;BZ=0;Q=0; DPar(1)=0;
            BCTYPE=''
        endif
        print*,'Fake Helmholtz'

    END SUBROUTINE

    !---------------------------------------------------------------------

    SUBROUTINE D_COMMIT_HELMHOLTZ_3D(F,BD_AX,BD_BX,BD_AY,BD_BY,BD_AZ,BD_BZ,XHANDLE,YHANDLE,IPAR,DPAR,STAT)

        INTEGER STAT
        INTEGER IPAR(*)
        DOUBLE PRECISION DPAR(*)
        DOUBLE PRECISION F(IPAR(11)+1,IPAR(12)+1,*)
        DOUBLE PRECISION BD_AX(IPAR(12)+1,*),BD_BX(IPAR(12)+1,*),BD_AY(IPAR(11)+1,*),BD_BY(IPAR(11)+1,*)
        DOUBLE PRECISION BD_AZ(IPAR(11)+1,*),BD_BZ(IPAR(11)+1,*)
        TYPE(DFTI_DESCRIPTOR), POINTER :: XHANDLE, YHANDLE
        if(.false.) then
            Stat=0; iPar(1)=0;
            F(1,1,1)=0
            BD_AX(1,1)=0;
            BD_AY(1,1)=0;
            BD_BX(1,1)=0;
            BD_BY(1,1)=0;
            BD_AZ(1,1)=0;
            BD_BZ(1,1)=0;
            DPar(1)=0;
            xhandle%dontuse=1
            yhandle%dontuse=1
        endif
        print*,'Fake Helmholtz'
    END SUBROUTINE

    !---------------------------------------------------------------------


    SUBROUTINE D_HELMHOLTZ_3D(F,BD_AX,BD_BX,BD_AY,BD_BY,BD_AZ,BD_BZ,XHANDLE,YHANDLE,IPAR,DPAR,STAT)

        INTEGER STAT
        INTEGER IPAR(*)
        DOUBLE PRECISION F(IPAR(11)+1,IPAR(12)+1,*)
        DOUBLE PRECISION BD_AX(IPAR(12)+1,*),BD_BX(IPAR(12)+1,*),BD_AY(IPAR(11)+1,*),BD_BY(IPAR(11)+1,*)
        DOUBLE PRECISION BD_AZ(IPAR(11)+1,*),BD_BZ(IPAR(11)+1,*)
        DOUBLE PRECISION DPAR(*)
        TYPE(DFTI_DESCRIPTOR), POINTER :: XHANDLE, YHANDLE
        if(.false.) then
            Stat=0; iPar(1)=0;
            F(1,1,1)=0
            BD_AX(1,1)=0;
            BD_AY(1,1)=0;
            BD_BX(1,1)=0;
            BD_BY(1,1)=0;
            BD_AZ(1,1)=0;
            BD_BZ(1,1)=0;
            DPar(1)=0;
            xhandle%dontuse=1
            yhandle%dontuse=1
        endif
    END SUBROUTINE

    !---------------------------------------------------------------------

    SUBROUTINE FREE_HELMHOLTZ_3D(XHANDLE,YHANDLE,IPAR,STAT)

        INTEGER STAT
        INTEGER IPAR(*)
        TYPE(DFTI_DESCRIPTOR), POINTER :: XHANDLE, YHANDLE
        if(.false.) then
            Stat=0; iPar(1)=0;
            xhandle%dontuse=1
            yhandle%dontuse=1
        endif
    END SUBROUTINE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!INTERFACES FOR 2D CASE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    SUBROUTINE D_INIT_HELMHOLTZ_2D(AX,BX,AY,BY,NX,NY,BCTYPE,Q,IPAR,DPAR,STAT)
        INTEGER NX, NY, STAT
        INTEGER IPAR(*)
        DOUBLE PRECISION AX,BX,AY,BY,Q
        CHARACTER(4) BCTYPE
        DOUBLE PRECISION DPAR(*)
        if(.false.) then
            Nx=0; Ny=0; Stat=0; iPar(1)=0;
            AX=0;AY=0;BX=0;BY=0;Q=0; DPar(1)=0;
            BCTYPE=''
        endif
    END SUBROUTINE

    !---------------------------------------------------------------------

    SUBROUTINE D_COMMIT_HELMHOLTZ_2D(F,BD_AX,BD_BX,BD_AY,BD_BY,HANDLE,IPAR,DPAR,STAT)
        INTEGER STAT
        INTEGER IPAR(*)
        DOUBLE PRECISION F(IPAR(11)+1,*)
        DOUBLE PRECISION BD_AX(*),BD_BX(*),BD_AY(*),BD_BY(*)
        DOUBLE PRECISION DPAR(*)
        TYPE(DFTI_DESCRIPTOR), POINTER :: HANDLE
        if(.false.) then
            Stat=0; iPar(1)=0;
            F(1,1)=0
            BD_AX(1)=0;
            BD_AY(1)=0;
            BD_BX(1)=0;
            BD_BY(1)=0;
            DPar(1)=0;
            handle%dontuse=1
        endif
    END SUBROUTINE

    !---------------------------------------------------------------------

    SUBROUTINE D_HELMHOLTZ_2D(F,BD_AX,BD_BX,BD_AY,BD_BY,HANDLE,IPAR,DPAR,STAT)
        INTEGER STAT
        INTEGER IPAR(*)
        DOUBLE PRECISION F(IPAR(11)+1,*)
        DOUBLE PRECISION BD_AX(*),BD_BX(*),BD_AY(*),BD_BY(*)
        DOUBLE PRECISION DPAR(*)
        TYPE(DFTI_DESCRIPTOR), POINTER :: HANDLE
        if(.false.) then
            Stat=0; iPar(1)=0;
            F(1,1)=0
            BD_AX(1)=0;
            BD_AY(1)=0;
            BD_BX(1)=0;
            BD_BY(1)=0;
            DPar(1)=0;
            handle%dontuse=1
        endif
    END SUBROUTINE

    !---------------------------------------------------------------------

    SUBROUTINE FREE_HELMHOLTZ_2D(HANDLE,IPAR,STAT)
        INTEGER STAT
        INTEGER IPAR(*)
        TYPE(DFTI_DESCRIPTOR), POINTER :: HANDLE
        if(.false.) then
            Stat=0; iPar(1)=0;
            handle%dontuse=1
        endif
    END SUBROUTINE
end module SupportMKL
