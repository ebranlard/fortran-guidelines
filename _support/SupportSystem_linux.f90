!> Contains Parameters/Data that are system/architecture specific, autogenerated by Makefile
module SupportSystem
    implicit none
    !LINUX
    character(len=10), parameter :: OSNAME = "linux"
    character(len=1),  parameter :: SLASH  = '/'  !< Path separator.
    character(len=1),  parameter :: BADSLASH  = '\'  !< Bad slash
    character(len=1),  parameter :: SWITCH = '-'  !< switch for command-line options.
    character(len=20), parameter :: COPY          = "cp "
    character(len=20), parameter :: RENAME        = "mv "
    character(len=20), parameter :: REMOVE        = "rm -f "
    character(len=10), parameter :: MKDIR         = "mkdir -p "
    character(len=20), parameter :: RMDIR         = "rmdir "
    character(len=20), parameter :: FILELIST      = "ls "     ! Only file names!
    character(len=20), parameter :: REDIRECT      = ">"
    character(len=20), parameter :: TEMPDIR       = "/tmp/"

    character(len=20), parameter :: BG_CMD_PREFIX = "nohup"
    character(len=20), parameter :: BG_CMD_SUFFIX = "&"
    character(len=20), parameter :: SUPPRESS_MSG  = "2>/dev/null"
end module
