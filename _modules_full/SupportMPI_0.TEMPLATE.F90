#def list_me($var,$ndim)
#set $list='%s1'%$var
#for $dim in range($ndim-1)
#set $list=$list+',%s%s'%($var,$dim+2)
#end for
#return $list
#end def
#def list_dimensions($ndim)
#if $ndim==0:
#set $list=''
#else:
#set $list=', dimension(:'
#for $dim in range($ndim-1)
#set $list=$list+',:'
#end for
#set $list=$list+')'
#end if
#return $list
#end def
#def list_dimensions_n($var,$ndim)
#if $ndim==0:
#set $list=''
#else:
#set $list=', dimension('+$list_me($var,$ndim)+')'
#end if
#return $list
#end def
#set $dims=[0,1,2,3,4]
#set $vars      =['integer','real','double precision','logical']
#set $vars_short=['i','r','d','b']
#set $nvars=len($vars)
!>
! This modules provides SIMPLE interfaces that replaces MPI calls if MPI is not supported.
! The modules present the following limitations:
! - Not all MPI routines are implemented.
! - The send and receive types are assumed to be the same
! - For now, most routines require that the send and receive buffers have the exact same dimensions and sizes 
! 
module SupportMPI
    implicit none

#set $routines=['MPI_reduce','MPI_Allreduce','MPI_Bcast','MPI_Send','MPI_Gather','MPI_Gatherv','MPI_Recv','MPI_SendRecv']


#for $routine_base in $routines:
    interface ${routine_base}; module procedure &
#set $routines=''
#set $nroutines=len($vars)*len($dims)
#set $iroutines=0
#for $dim in dims
#for $ivar in range(len($vars))
#set $iroutines=$iroutines+1
#set $td='%s%s'%($vars_short[$ivar],$dim)
#set $routines=$routines+$routine_base+'_'+$td
#if  $iroutines<$nroutines:
#set $routines=$routines+','
#if  $iroutines%5==0:
#set $routines=$routines+'&\n        '
#end if
#end if
#end for
#end for
        $routines
    end interface
#end for

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

#for $dim in $dims
#for $ivar in range(len($vars))
#set $td='%s%s'%($vars_short[$ivar],$dim)
#set $TD='%s%s'%($vars[$ivar],$list_dimensions($dim))
#set $TD2='%s%s'%($vars[$ivar],$list_dimensions($dim+1))

    subroutine MPI_reduce_${td}( Isent, Irecv, n1, MPI_type,MPI_oper , root, MPI_group ,ierr )  
        ${TD}, intent(in)    :: Isent
        ${TD}, intent(inout) :: Irecv
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

    subroutine MPI_Allreduce_${td}( Isent, Irecv, n1, MPI_type,MPI_oper , MPI_group ,ierr )  
        ${TD}, intent(in)    :: Isent
        ${TD}, intent(inout) :: Irecv
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

    subroutine MPI_Bcast_${td}( SendRecv, n1, MPI_type, RankRecv, MPI_group ,ierr )  
        ${TD}, intent(inout) :: SendRecv
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

    subroutine MPI_Send_${td}( SendRecvBuf, SendCount, SendType, dest, tag, MPI_group ,ierr )  
        ${TD}, intent(inout)   :: SendRecvBuf
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

    subroutine MPI_Gather_${td}( SendBuf, SendCount, SendType, RecvBuf, RecvCount, RecvType, root, MPI_group, ierr)  
        ${TD}, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
#if dim==0:
        ${TD2}, intent(out)    :: RecvBuf
#else:
        ${TD}, intent(out)     :: RecvBuf
#end if
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

    subroutine MPI_Gatherv_${td}( SendBuf, SendCount, SendType, RecvBuf, RecvCount, Displs, RecvType, root, MPI_group, ierr)  
        ${TD}, intent(in)                 :: SendBuf
        integer, intent(in)               :: SendCount
        integer, intent(in)               :: SendType
        ${TD}, intent(out)                :: RecvBuf
        integer, dimension(:), intent(in) :: RecvCount
        integer, dimension(:), intent(in) :: Displs
        integer, intent(in)               :: RecvType
        integer, intent(in)               :: root
        integer, intent(in)               :: MPI_group
        integer, intent(inout)            :: ierr
#if dim>=1:
        if(size(SendBuf)/=SendCount) then
            print*,'Fake MPI_Gatherv: sendcount should match size(SendBuf)'
        endif
        if(size(RecvBuf)<SendCount) then
            print*,'Fake MPI_Gatherv: RecvBuf not large enough'
        endif
#end if 
#if dim==0:
        RecvBuf=SendBuf
#elif dim==1:
        RecvBuf(1:size(SendBuf,1))=SendBuf(:)
#elif dim==2:
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2))=SendBuf(:,:)
#elif dim==3:
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3))=SendBuf(:,:,:)
#elif dim==4:
        RecvBuf(1:size(SendBuf,1),1:size(SendBuf,2),1:size(SendBuf,3),1:size(SendBuf,4))=SendBuf(:,:,:,:)
#end if 

        if(.false.) then
            print*,SendCount,SendType,RecvCount,RecvType,root,MPI_group,ierr,Displs
        endif
    end subroutine
    
    subroutine MPI_Recv_${td}( Buf, SendCount, SendType, dest, tag, MPI_group, stat, ierr)  
        ${TD}, intent(inout)                 :: Buf
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



    subroutine MPI_SendRecv_${td}( SendBuf, SendCount, SendType, dest, sendtag, RecvBuf, RecvCount, RecvType, source, recvtag, &
            MPI_group, stat,ierr)  
        ${TD}, intent(in)      :: SendBuf
        integer, intent(in)    :: SendCount
        integer, intent(in)    :: SendType
        integer, intent(in)    :: dest
        integer, intent(in)    :: sendtag
        ${TD}, intent(out)     :: RecvBuf
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
#end for
#end for
end module


