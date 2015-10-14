!> 
module FileSystem
    implicit none
    character(len=1), parameter :: SP=' '

    interface string2filename
        module procedure string2filename, string2filename2
    end interface

contains


    subroutine system_rename(name1,name2)
        use SupportSystem, only:RENAME
        character(len=*), intent(in) :: name1,name2
!          integer stat
        call system(RENAME//trim(name1)//SP//trim(name2))
    end subroutine 


    subroutine system_mkdir(dirname)
        use SupportSystem, only:MKDIR
        character(len=*), intent(in) :: dirname
!          integer stat
!          print*,'Making directory',trim(dirname)
!          print*,'Command: ',MKDIR//trim(dirname)//' ;'
        call system(MKDIR//trim(dirname))
    end subroutine 

    !> Makes sure the string in input has a proper file format depending on OS
    subroutine string2filename(string) 
        use SupportSystem, only:BADSLASH, SLASH
        use MatlabFunctions, only: strrep
        character(len=*),intent(inout) :: string
        !
        string=trim(string)
!          print*,'before',trim(string)
        call strrep(string,BADSLASH,SLASH)
!          print*,'after ',trim(string)
    end subroutine

    !> Makes sure the string in input has a proper file format depending on OS, returns the filename and containing folder
    subroutine string2filename2(string,folder) 
        use SupportSystem, only:BADSLASH, SLASH
        use MatlabFunctions, only: strrep
        character(len=*),intent(inout) :: string   !< Gets an input string, returns the filename in it
        character(len=*),intent(out)   :: folder   !< returns containing folder of the file
        !
        integer :: islash
        !
        string=trim(string)
        !print*,'before',trim(string)
        ! Replacing slashes with system one 
        call strrep(string,BADSLASH,SLASH)

        ! Find the last slash if any 
        islash=index(string,SLASH,.true.) 
        ! Retrieve folder and filename from it
        folder=string(1:islash)

        !print*,'after:',trim(string),':folder:',trim(folder),':'

    end subroutine
    
    !> Joins a fodler and a filename, or two folders
    subroutine path_join(path1,path2,path_joined) 
        use MatlabFunctions, only: strrep
        character(len=*),intent(in) :: path1,path2
        character(len=*),intent(out) :: path_joined
        !
        character(len=255) :: path_tmp
        !
        path_joined=''
        ! --------------------------------------------------------------------------------
        ! ---  Special cases
        ! --------------------------------------------------------------------------------
        ! Empty path 1
        if(len_trim(path1)==0) then
            path_joined=trim(path2)
            return
        endif
        ! Empty path 2
        if(len_trim(path2)==0) then
            path_joined=trim(path1)
            return
        endif
        ! --------------------------------------------------------------------------------
        ! ---  
        ! --------------------------------------------------------------------------------
        ! - We put path1 in path_joined and we convert it to a folder
        path_joined=trim(path1)
        call string2folder(path_joined)
        ! - We make sure path2 has the proper format
        path_tmp=path2
        call string2filename(path_tmp)
        ! - We join the path
        path_joined=trim(path_joined)//trim(path_tmp)
    end subroutine

    
    !> Joins two folders 
    subroutine folder_join(path1,path2,path_joined) 
        use MatlabFunctions, only: strrep
        character(len=*),intent(in) :: path1,path2
        character(len=*),intent(out) :: path_joined
        !
        character(len=255) :: path_tmp
        !
        ! --------------------------------------------------------------------------------
        ! ---  Special cases
        ! --------------------------------------------------------------------------------
        ! Empty path 1
        if(len_trim(path1)==0) then
            path_joined=trim(path2)
            call string2folder(path_joined)
            return
        endif
        ! Empty path 2
        if(len_trim(path2)==0) then
            path_joined=trim(path1)
            call string2folder(path_joined)
            return
        endif
        if(path2=='./') then
            path_joined=trim(path1)
            call string2folder(path_joined)
            return
        endif
        ! --------------------------------------------------------------------------------
        ! --- None of them are empty 
        ! --------------------------------------------------------------------------------
        path_joined=trim(path1)
        call string2folder(path_joined)
        !
        path_tmp=trim(path2)
        call string2folder(path_tmp)
        path_joined=trim(path_joined)//trim(path_tmp)
    end subroutine

    


    !> Makes sure the string in input has proper folder format depending on OS
    subroutine string2folder(string) 
        use SupportSystem, only:SLASH
        character(len=*),intent(inout) :: string
        integer :: ipos 
        !
        ! replacing bad slashes in name
        call string2filename(string)

        ! empty string case
        if(string=='') then
            string='.'//SLASH
        else
            ! 
            ipos=scan(string,SLASH,.true.) ! back search
            if(ipos/=len_trim(string)) then
                string=trim(string)//SLASH
            endif
        endif
    end subroutine



    !>
    logical function file_exists(filename)
        character(len=*),intent(in) ::filename  ! 'input.txt' '/input.txt'
        INQUIRE(FILE=trim(filename), EXIST=file_exists)   ! file_exists will be TRUE if the file
    end function file_exists

    
    
    !> Didn't manage to do something that works..
    logical function isdir(dirname)
        character(len=*),intent(in) ::dirname  ! './dir/' 
        character(len=255)::dirn
        character(1) :: DOT = char(46) ! New Line character
        logical :: try1
        logical :: try2
        logical :: try3
        integer :: n
        dirn=dirname
        call string2folder(dirn) ! makes sure there are proper slashes, and ends up with a slash
        ! trying with directory with slash at the end
        inquire( file=trim(dirn), exist=try1)
!          print*,'Exist Directory:',trim(dirn),try1
        
        ! trying with directory with slash and dot at the end
        dirn=trim(dirn)//DOT
        inquire( file=trim(dirn), exist=try2)
!          print*,'Exist Directory:',trim(dirn),try2
        
        ! trying with directory without slash and dot at the end
        n=len_trim(dirn)
        dirn=trim(dirn(1:n-2))
        inquire(file=trim(dirn), exist=try3)
!          print*,'Exist Directory:',trim(dirn),try3
        isdir=try1.or.try2.or.try3
    end function 
            

    !> Performs necessary checks for opening file
    subroutine prepare_file_write(filename)
        !use FileSystem,    only: isdir, string2folder,string2filename,system_mkdir
        !use OmnivorData,   only: bDEBUG, prefix, sim_folder,suffix
        ! Arguments
        character(len=*), intent(inout) :: filename !< filename (may contain folder)
        !
        character(len=255) :: folder !< containing folders if any
        ! 
        if (filename=='') then
            print'(A)','Error: FileSystem: prepare file open called with an empty filename'
            STOP
        endif
        ! Safety check on the simulation folder structure, if provided
        !call string2folder(filename)
        ! Safety checks on files input from user (if any)
        call string2filename ( filename      , folder      ) 
        !print*,'filename:',trim(filename)
        !print*,'folder  :',trim(folder)
        ! Creating directory structure 
        if(folder/='') then
            call system_mkdir(folder)
        endif
    end subroutine 

end module FileSystem
