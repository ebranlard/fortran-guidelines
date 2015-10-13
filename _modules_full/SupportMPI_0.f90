!>
! This modules provides SIMPLE interfaces that replaces MPI calls if MPI is not supported.
! The modules present the following limitations:
! - Not all MPI routines are implemented.
! - The send and receive types are assumed to be the same
! - For now, most routines require that the send and receive buffers have the exact same dimensions and sizes 
! 
module SupportMPI
    implicit none



    interface MPI_reduce; module procedure &
        MPI_reduce_i0,MPI_reduce_r0,MPI_reduce_d0,MPI_reduce_b0,MPI_reduce_i1,&
        MPI_reduce_r1,MPI_reduce_d1,MPI_reduce_b1,MPI_reduce_i2,MPI_reduce_r2,&
        MPI_reduce_d2,MPI_reduce_b2,MPI_reduce_i3,MPI_reduce_r3,MPI_reduce_d3,&
        MPI_reduce_b3,MPI_reduce_i4,MPI_reduce_r4,MPI_reduce_d4,MPI_reduce_b4
    end interface
    interface MPI_Allreduce; module procedure &
        MPI_Allreduce_i0,MPI_Allreduce_r0,MPI_Allreduce_d0,MPI_Allreduce_b0,MPI_Allreduce_i1,&
        MPI_Allreduce_r1,MPI_Allreduce_d1,MPI_Allreduce_b1,MPI_Allreduce_i2,MPI_Allreduce_r2,&
        MPI_Allreduce_d2,MPI_Allreduce_b2,MPI_Allreduce_i3,MPI_Allreduce_r3,MPI_Allreduce_d3,&
        MPI_Allreduce_b3,MPI_Allreduce_i4,MPI_Allreduce_r4,MPI_Allreduce_d4,MPI_Allreduce_b4
    end interface
    interface MPI_Bcast; module procedure &
        MPI_Bcast_i0,MPI_Bcast_r0,MPI_Bcast_d0,MPI_Bcast_b0,MPI_Bcast_i1,&
        MPI_Bcast_r1,MPI_Bcast_d1,MPI_Bcast_b1,MPI_Bcast_i2,MPI_Bcast_r2,&
        MPI_Bcast_d2,MPI_Bcast_b2,MPI_Bcast_i3,MPI_Bcast_r3,MPI_Bcast_d3,&
        MPI_Bcast_b3,MPI_Bcast_i4,MPI_Bcast_r4,MPI_Bcast_d4,MPI_Bcast_b4
    end interface
    interface MPI_Send; module procedure &
        MPI_Send_i0,MPI_Send_r0,MPI_Send_d0,MPI_Send_b0,MPI_Send_i1,&
        MPI_Send_r1,MPI_Send_d1,MPI_Send_b1,MPI_Send_i2,MPI_Send_r2,&
        MPI_Send_d2,MPI_Send_b2,MPI_Send_i3,MPI_Send_r3,MPI_Send_d3,&
        MPI_Send_b3,MPI_Send_i4,MPI_Send_r4,MPI_Send_d4,MPI_Send_b4
    end interface
    interface MPI_Gather; module procedure &
        MPI_Gather_i0,MPI_Gather_r0,MPI_Gather_d0,MPI_Gather_b0,MPI_Gather_i1,&
        MPI_Gather_r1,MPI_Gather_d1,MPI_Gather_b1,MPI_Gather_i2,MPI_Gather_r2,&
        MPI_Gather_d2,MPI_Gather_b2,MPI_Gather_i3,MPI_Gather_r3,MPI_Gather_d3,&
        MPI_Gather_b3,MPI_Gather_i4,MPI_Gather_r4,MPI_Gather_d4,MPI_Gather_b4
    end interface
    interface MPI_Gatherv; module procedure &
        MPI_Gatherv_i0,MPI_Gatherv_r0,MPI_Gatherv_d0,MPI_Gatherv_b0,MPI_Gatherv_i1,&
        MPI_Gatherv_r1,MPI_Gatherv_d1,MPI_Gatherv_b1,MPI_Gatherv_i2,MPI_Gatherv_r2,&
        MPI_Gatherv_d2,MPI_Gatherv_b2,MPI_Gatherv_i3,MPI_Gatherv_r3,MPI_Gatherv_d3,&
        MPI_Gatherv_b3,MPI_Gatherv_i4,MPI_Gatherv_r4,MPI_Gatherv_d4,MPI_Gatherv_b4
    end interface
    interface MPI_Recv; module procedure &
        MPI_Recv_i0,MPI_Recv_r0,MPI_Recv_d0,MPI_Recv_b0,MPI_Recv_i1,&
        MPI_Recv_r1,MPI_Recv_d1,MPI_Recv_b1,MPI_Recv_i2,MPI_Recv_r2,&
        MPI_Recv_d2,MPI_Recv_b2,MPI_Recv_i3,MPI_Recv_r3,MPI_Recv_d3,&
        MPI_Recv_b3,MPI_Recv_i4,MPI_Recv_r4,MPI_Recv_d4,MPI_Recv_b4
    end interface
    interface MPI_SendRecv; module procedure &
        MPI_SendRecv_i0,MPI_SendRecv_r0,MPI_SendRecv_d0,MPI_SendRecv_b0,MPI_SendRecv_i1,&
        MPI_SendRecv_r1,MPI_SendRecv_d1,MPI_SendRecv_b1,MPI_SendRecv_i2,MPI_SendRecv_r2,&
        MPI_SendRecv_d2,MPI_SendRecv_b2,MPI_SendRecv_i3,MPI_SendRecv_r3,MPI_SendRecv_d3,&
        MPI_SendRecv_b3,MPI_SendRecv_i4,MPI_SendRecv_r4,MPI_SendRecv_d4,MPI_SendRecv_b4
    end interface

    ! 
    integer, parameter :: MPI_STATUS_SIZE=0
    integer, parameter :: MPI_ANY_TAG=0
    integer, parameter :: MPI_PROC_NULL=-2
    ! Types
    integer, parameter :: MPI_INTEGER=0
    integer, parameter :: MPI_REAL=0
    integer, parameter :: MPI_DOUBLE=0
    integer, parameter :: MPI_DOUBLE_PRECISION=0
    ! Operations
    integer, parameter :: MPI_SUM=0
    integer, parameter :: MPI_MAX=0
    integer, parameter :: MPI_MIN=0
    ! Groups
    integer, parameter :: MPI_COMM_WORLD=0


contains

    ! --------------------------------------------------------------------------------
    ! --- Standard subroutines  
    ! --------------------------------------------------------------------------------
    subroutine MPI_init(ierr)
        integer, intent(inout) :: ierr
        ierr=0
    end subroutine 
    
    subroutine MPI_finalize(ierr)
        integer, intent(inout) :: ierr
        ierr=0
    end subroutine 

    subroutine MPI_comm_rank(MPI_group,rank,ierr)
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: rank
        integer, intent(inout) :: ierr
        rank=0
        ierr=0*MPI_group
    end subroutine

    subroutine MPI_comm_size(MPI_group,nprocs,ierr)
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: nprocs
        integer, intent(inout) :: ierr
        nprocs=1
        ierr=0*MPI_group
    end subroutine
    
    subroutine MPI_barrier(MPI_group,ierr)
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        ierr=0*MPI_group
    end subroutine



    ! --------------------------------------------------------------------------------
    ! ---  
    ! --------------------------------------------------------------------------------


    subroutine MPI_reduce_i0( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        integer, intent(in)    :: Isent
        integer, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_i0( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        integer, intent(in)    :: Isent
        integer, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_i0( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        integer, intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_i0( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        integer, intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_i0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        integer, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, dimension(:), intent(out)    :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_i0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        integer, intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        integer, intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        RecvBuf=SendBuf

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_i0( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        integer, intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_i0( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        integer, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        integer, intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_r0( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        real, intent(in)    :: Isent
        real, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_r0( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        real, intent(in)    :: Isent
        real, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_r0( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        real, intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_r0( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        real, intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_r0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        real, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        real, dimension(:), intent(out)    :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_r0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        real, intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        real, intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        RecvBuf=SendBuf

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_r0( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        real, intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_r0( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        real, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        real, intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_d0( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        double precision, intent(in)    :: Isent
        double precision, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_d0( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        double precision, intent(in)    :: Isent
        double precision, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_d0( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        double precision, intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_d0( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        double precision, intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_d0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        double precision, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        double precision, dimension(:), intent(out)    :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_d0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        double precision, intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        double precision, intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        RecvBuf=SendBuf

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_d0( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        double precision, intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_d0( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        double precision, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        double precision, intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_b0( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        logical, intent(in)    :: Isent
        logical, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_b0( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        logical, intent(in)    :: Isent
        logical, intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_b0( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        logical, intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_b0( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        logical, intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_b0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        logical, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        logical, dimension(:), intent(out)    :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_b0( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        logical, intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        logical, intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        RecvBuf=SendBuf

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_b0( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        logical, intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_b0( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        logical, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        logical, intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_i1( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        integer, dimension(:), intent(in)    :: Isent
        integer, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_i1( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        integer, dimension(:), intent(in)    :: Isent
        integer, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_i1( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        integer, dimension(:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_i1( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        integer, dimension(:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_i1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        integer, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_i1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        integer, dimension(:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        integer, dimension(:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1))=SendBuf(:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_i1( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        integer, dimension(:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_i1( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        integer, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        integer, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_r1( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        real, dimension(:), intent(in)    :: Isent
        real, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_r1( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        real, dimension(:), intent(in)    :: Isent
        real, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_r1( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        real, dimension(:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_r1( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        real, dimension(:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_r1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        real, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        real, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_r1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        real, dimension(:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        real, dimension(:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1))=SendBuf(:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_r1( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        real, dimension(:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_r1( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        real, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        real, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_d1( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        double precision, dimension(:), intent(in)    :: Isent
        double precision, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_d1( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        double precision, dimension(:), intent(in)    :: Isent
        double precision, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_d1( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        double precision, dimension(:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_d1( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        double precision, dimension(:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_d1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        double precision, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_d1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        double precision, dimension(:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1))=SendBuf(:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_d1( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        double precision, dimension(:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_d1( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        double precision, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        double precision, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_b1( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        logical, dimension(:), intent(in)    :: Isent
        logical, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_b1( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        logical, dimension(:), intent(in)    :: Isent
        logical, dimension(:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_b1( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        logical, dimension(:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_b1( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        logical, dimension(:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_b1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        logical, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        logical, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_b1( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        logical, dimension(:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        logical, dimension(:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1))=SendBuf(:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_b1( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        logical, dimension(:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_b1( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        logical, dimension(:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        logical, dimension(:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_i2( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        integer, dimension(:,:), intent(in)    :: Isent
        integer, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_i2( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        integer, dimension(:,:), intent(in)    :: Isent
        integer, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_i2( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        integer, dimension(:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_i2( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        integer, dimension(:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_i2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        integer, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_i2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        integer, dimension(:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        integer, dimension(:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2))=SendBuf(:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_i2( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        integer, dimension(:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_i2( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        integer, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        integer, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_r2( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        real, dimension(:,:), intent(in)    :: Isent
        real, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_r2( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        real, dimension(:,:), intent(in)    :: Isent
        real, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_r2( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        real, dimension(:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_r2( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        real, dimension(:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_r2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        real, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        real, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_r2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        real, dimension(:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        real, dimension(:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2))=SendBuf(:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_r2( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        real, dimension(:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_r2( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        real, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        real, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_d2( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        double precision, dimension(:,:), intent(in)    :: Isent
        double precision, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_d2( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        double precision, dimension(:,:), intent(in)    :: Isent
        double precision, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_d2( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        double precision, dimension(:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_d2( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        double precision, dimension(:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_d2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        double precision, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_d2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        double precision, dimension(:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2))=SendBuf(:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_d2( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        double precision, dimension(:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_d2( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        double precision, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        double precision, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_b2( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        logical, dimension(:,:), intent(in)    :: Isent
        logical, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_b2( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        logical, dimension(:,:), intent(in)    :: Isent
        logical, dimension(:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_b2( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        logical, dimension(:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_b2( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        logical, dimension(:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_b2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        logical, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        logical, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_b2( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        logical, dimension(:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        logical, dimension(:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2))=SendBuf(:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_b2( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        logical, dimension(:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_b2( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        logical, dimension(:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        logical, dimension(:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_i3( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        integer, dimension(:,:,:), intent(in)    :: Isent
        integer, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_i3( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        integer, dimension(:,:,:), intent(in)    :: Isent
        integer, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_i3( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        integer, dimension(:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_i3( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        integer, dimension(:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_i3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        integer, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_i3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        integer, dimension(:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        integer, dimension(:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3))=SendBuf(:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_i3( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        integer, dimension(:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_i3( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        integer, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        integer, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_r3( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        real, dimension(:,:,:), intent(in)    :: Isent
        real, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_r3( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        real, dimension(:,:,:), intent(in)    :: Isent
        real, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_r3( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        real, dimension(:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_r3( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        real, dimension(:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_r3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        real, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        real, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_r3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        real, dimension(:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        real, dimension(:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3))=SendBuf(:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_r3( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        real, dimension(:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_r3( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        real, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        real, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_d3( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        double precision, dimension(:,:,:), intent(in)    :: Isent
        double precision, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_d3( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        double precision, dimension(:,:,:), intent(in)    :: Isent
        double precision, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_d3( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        double precision, dimension(:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_d3( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        double precision, dimension(:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_d3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        double precision, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_d3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        double precision, dimension(:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3))=SendBuf(:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_d3( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        double precision, dimension(:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_d3( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        double precision, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        double precision, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_b3( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        logical, dimension(:,:,:), intent(in)    :: Isent
        logical, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_b3( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        logical, dimension(:,:,:), intent(in)    :: Isent
        logical, dimension(:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_b3( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        logical, dimension(:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_b3( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        logical, dimension(:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_b3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        logical, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        logical, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_b3( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        logical, dimension(:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        logical, dimension(:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3))=SendBuf(:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_b3( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        logical, dimension(:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_b3( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        logical, dimension(:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        logical, dimension(:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_i4( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        integer, dimension(:,:,:,:), intent(in)    :: Isent
        integer, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_i4( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        integer, dimension(:,:,:,:), intent(in)    :: Isent
        integer, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_i4( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        integer, dimension(:,:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_i4( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        integer, dimension(:,:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_i4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        integer, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_i4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        integer, dimension(:,:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        integer, dimension(:,:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3),1:size(SendBuf,4))=SendBuf(:,:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_i4( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        integer, dimension(:,:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_i4( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        integer, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        integer, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_r4( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        real, dimension(:,:,:,:), intent(in)    :: Isent
        real, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_r4( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        real, dimension(:,:,:,:), intent(in)    :: Isent
        real, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_r4( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        real, dimension(:,:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_r4( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        real, dimension(:,:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_r4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        real, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        real, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_r4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        real, dimension(:,:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        real, dimension(:,:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3),1:size(SendBuf,4))=SendBuf(:,:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_r4( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        real, dimension(:,:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_r4( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        real, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        real, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_d4( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        double precision, dimension(:,:,:,:), intent(in)    :: Isent
        double precision, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_d4( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        double precision, dimension(:,:,:,:), intent(in)    :: Isent
        double precision, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_d4( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        double precision, dimension(:,:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_d4( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        double precision, dimension(:,:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_d4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        double precision, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_d4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        double precision, dimension(:,:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        double precision, dimension(:,:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3),1:size(SendBuf,4))=SendBuf(:,:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_d4( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        double precision, dimension(:,:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_d4( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        double precision, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        double precision, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_reduce_b4( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        logical, dimension(:,:,:,:), intent(in)    :: Isent
        logical, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(in) :: root
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,root,ierr,n1
        endif
    end subroutine

    subroutine MPI_Allreduce_b4( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        logical, dimension(:,:,:,:), intent(in)    :: Isent
        logical, dimension(:,:,:,:), intent(inout) :: Irecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: MPI_oper
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr

        Irecv=Isent
        if(.false.) then
            print*,MPI_type,MPI_oper,MPI_group,ierr,n1
        endif
    end subroutine

    subroutine MPI_Bcast_b4( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        logical, dimension(:,:,:,:), intent(inout) :: SendRecv
        integer, intent(in) :: n1
        integer, intent(in) :: MPI_type
        integer, intent(in) :: RankRecv
        integer, intent(in) :: MPI_group
        integer, intent(inout) :: ierr
        SendRecv=SendRecv

        if(.false.) then
            print*,MPI_type,MPI_group,ierr,n1,rankrecv
        endif
    end subroutine

    subroutine MPI_Send_b4( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        logical, dimension(:,:,:,:), intent(inout)   :: SendRecvBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: tag
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        if(.false.) then
            print*,SendRecvBuf,SendType,SendCount,dest,tag,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gather_b4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        logical, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        logical, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: root
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr
        endif
    end subroutine

    subroutine MPI_Gatherv_b4( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        logical, dimension(:,:,:,:), intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        logical, dimension(:,:,:,:), intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3),1:size(SendBuf,4))=SendBuf(:,:,:,:)

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_b4( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        logical, dimension(:,:,:,:), intent(inout)                 :: Buf
        integer, intent(in)                  :: SendCount
        integer, intent(in)                  :: SendType
        integer, intent(in)                  :: dest
        integer, intent(in)                  :: tag
        integer, intent(in)                  :: MPI_group
        integer, dimension(:), intent(inout) :: stat
        integer, intent(inout)               :: ierr
        if(.false.) then
            print*,Buf,SendCount,SendType,dest,tag ,MPI_group,ierr,stat
        endif
    end subroutine



    subroutine MPI_SendRecv_b4( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        logical, dimension(:,:,:,:), intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        logical, dimension(:,:,:,:), intent(out)     :: RecvBuf
        integer, intent(in)    :: RecvCount
        integer, intent(in)    :: RecvType
        integer, intent(in)    :: source
        integer, intent(in)    :: recvtag
        integer, dimension(:), intent(inout) :: stat
        integer, intent(in)    :: MPI_group
        integer, intent(inout) :: ierr
        RecvBuf=SendBuf
        if(.false.) then
            print*,SendCount,SendType,dest, sendtag,RecvCount,RecvType,source,recvtag,stat,MPI_group,ierr
        endif
    end subroutine
end module


