@echo OFF


:: --------------------------------------------
:: --- First make windows less annoying
:: --------------------------------------------
:: A better variable editor  http://eveditor.com/download/	:: A better command line  ConEmu
:: A better Command line ConEmu: http://www.fosshub.com/ConEmu.html
:: A Windows placement manager Wind Split revolution version 11



:: --------------------------------------------
:: --- Setting up ifortran 
:: --------------------------------------------
:: (you need to have the proper folder of Visual Studio in your environment variable PATH)
:: For Example: C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC
::call vcvarsall.bat x86
call vcvarsall.bat amd64

:: --------------------------------------------
:: --- Setting up ifortran 
:: --------------------------------------------
:: (you need to have the folder bin of intel in your environment variable PATH)
:: For example:  C:\Program Files (x86)\Intel\ComposerXE-2011\bin
::call ifortvars.bat ia32 vs2010
::call ifortvars.bat intel64 vs2010
call ifortvars.bat intel64 vs2010



:: --------------------------------------------
:: --- OMNIVOR ENVIRONMENT VARIABLES (if not set globally!!)
:: --------------------------------------------
::set OMNIVOR_MKF_DIR=./_includes/
set OMNIVOR_MKF_DIR=%CD%\_includes\
set OMNIVOR_LIB_DIR=%CD%\_lib\
set OMNIVOR_BIN_DIR=%CD%\_bin\windows-ia32\
set OMNIVOR_OBJ_DIR=%CD%\_build/windows-ia32\
set OMNIVOR_SRC_DIR=%CD%\

echo OMNIVOR_MKF_DIR: %OMNIVOR_MKF_DIR%


cmd

