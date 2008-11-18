# Microsoft Developer Studio Project File - Name="libimod" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libimod - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libimod.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libimod.mak" CFG="libimod - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libimod - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libimod - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "libimod - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\buildlib"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /GX /O2 /I "..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libimod - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\buildlib"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /Gm /GX /ZI /Od /I "..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "libimod - Win32 Release"
# Name "libimod - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\adoc_fwrap.c
# End Source File
# Begin Source File

SOURCE=.\amat_to_rotmagstr.c
# End Source File
# Begin Source File

SOURCE=.\amoeba.c
# End Source File
# Begin Source File

SOURCE=.\autodoc.c
# End Source File
# Begin Source File

SOURCE=.\b3dutil.c
# End Source File
# Begin Source File

SOURCE=.\circlefit.c
# End Source File
# Begin Source File

SOURCE=.\colormap.c
# End Source File
# Begin Source File

SOURCE=.\cubinterp.c
# End Source File
# Begin Source File

SOURCE=.\filtxcorr.c
# End Source File
# Begin Source File

SOURCE=.\histogram.c
# End Source File
# Begin Source File

SOURCE=.\ilist.c
# End Source File
# Begin Source File

SOURCE=.\insidecontour.c
# End Source File
# Begin Source File

SOURCE=.\islice.c
# End Source File
# Begin Source File

SOURCE=.\parse_params.c
# End Source File
# Begin Source File

SOURCE=.\parselist.c
# End Source File
# Begin Source File

SOURCE=.\pip_fwrap.c
# End Source File
# Begin Source File

SOURCE=.\reduce_by_binning.c
# End Source File
# Begin Source File

SOURCE=.\samplemeansd.c
# End Source File
# Begin Source File

SOURCE=.\scaledsobel.c
# End Source File
# Begin Source File

SOURCE=.\simplestat.c
# End Source File
# Begin Source File

SOURCE=.\taperatfill.c
# End Source File
# Begin Source File

SOURCE=.\taperpad.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
