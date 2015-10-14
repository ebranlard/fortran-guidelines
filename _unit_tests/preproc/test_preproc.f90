! --------------------------------------------------------------------------------
! ---  Conclusions from this test
! --------------------------------------------------------------------------------
! C  -Prepro  IS     case sensitive
! DEC-Preproc IS NOT case sensitive

program test_preproc

#if defined __LINUX__
print*,'C __LINUX__'
#endif
#if defined __linux__
print*,'C __linux__'
#endif

#if defined __UNIX__
print*,'C __UNIX__'
#endif
#if defined __unix__
print*,'C __unix__'
#endif
!DEC$ if defined(__LINUX__)
print*,'Intel __LINUX__'
!DEC$ endif
!dec$ if defined(__linux__)
print*,'Intel __linux__'
!DEC$ endif
#if defined __MYMACRO__
print*,'C __MYMACRO__'
#endif
#if defined __mymacro__
print*,'C __mymacro__'
#endif
!DEC$ if defined(__MYMACRO__)
print*,'Intel __MYMACRO__'
!DEC$ endif
!dec$ if defined(__mymacro__)
print*,'Intel __mymacro__'
!DEC$ endif
end program
