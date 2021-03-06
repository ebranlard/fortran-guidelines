!> Contains Parameters/Data that are system/architecture specific, autogenerated by Makefile
module SystemParameters
    implicit none
    !WINDOWS
    character(len=10), parameter :: OSNAME = "windows"
    character(len=1),  parameter :: SLASH  = '\'  !< Path separator.
    character(len=1),  parameter :: BADSLASH  = '/'  !< Bad slash
    character(len=1),  parameter :: SWITCH = '/'  !< switch for command-line options.
    character(len=20), parameter :: COPY          = "copy /y "
    character(len=20), parameter :: RENAME        = "ren "
    character(len=20), parameter :: REMOVE        = "del /q "
    character(len=20), parameter :: MKDIR         = "md "
    character(len=20), parameter :: RMDIR         = "rd "
    character(len=20), parameter :: FILELIST      = "dir /b "   ! Only file names!
    character(len=20), parameter :: REDIRECT      = ">"
    character(len=20), parameter :: TEMPDIR       = "c:\tmp\"

    character(len=20), parameter :: BG_CMD_PREFIX = "start /nowait"
    character(len=20), parameter :: BG_CMD_SUFFIX = ""
    character(len=20), parameter :: SUPPRESS_MSG  = "2>nul"
end module
