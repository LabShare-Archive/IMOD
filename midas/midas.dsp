# Microsoft Developer Studio Project File - Name="midas" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=midas - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "midas.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "midas.mak" CFG="midas - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "midas - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "midas - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "midas - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "tmp"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "tmp"
# PROP Target_Dir ""
# ADD CPP /nologo /MD /W3 /O1 /I "..\include" /I "$(QTDIR)\include" /I "tmp\\" /I "$(QTDIR)\mkspecs\win32-msvc" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "UNICODE" /D "QT_DLL" /D "QT_THREAD_SUPPORT" /D "QT_NO_DEBUG" /FD -Zm200 /c
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# ADD LINK32 "qt-mt311.lib" "qtmain.lib" "opengl32.lib" "glu32.lib" "delayimp.lib" "..\buildlib\libimod.lib" "..\buildlib\libiimod.lib" "..\buildlib\libdiaqt.lib" "kernel32.lib" "user32.lib" "gdi32.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "imm32.lib" "winmm.lib" "wsock32.lib" "winspool.lib" delayimp.lib /nologo /subsystem:windows /machine:IX86 /nodefaultlib:"msvcrtd.lib" /nodefaultlib:"libcd.lib" /libpath:"$(QTDIR)\lib" /DELAYLOAD:opengl32.dll /DELAYLOAD:comdlg32.dll /DELAYLOAD:oleaut32.dll /DELAYLOAD:winmm.dll /DELAYLOAD:wsock32.dll /DELAYLOAD:winspool.dll

!ELSEIF  "$(CFG)" == "midas - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD CPP /nologo /MDd /W3 /Z7 /Od /I "..\include" /I "$(QTDIR)\include" /I "tmp\\" /I "$(QTDIR)\mkspecs\win32-msvc" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "UNICODE" /D "QT_DLL" /D "QT_THREAD_SUPPORT" /FD /GZ -Zm200 /c
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# ADD LINK32 "qt-mt311.lib" "qtmain.lib" "opengl32.lib" "glu32.lib" "delayimp.lib" "..\buildlib\libimod.lib" "..\buildlib\libiimod.lib" "..\buildlib\libdiaqt.lib" "kernel32.lib" "user32.lib" "gdi32.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "imm32.lib" "winmm.lib" "wsock32.lib" "winspool.lib" /nologo /subsystem:windows /debug /machine:IX86 /nodefaultlib:"msvcrt.lib" /nodefaultlib:"libcd.lib" /pdbtype:sept /libpath:"$(QTDIR)\lib" /DELAYLOAD:opengl32.dll

!ENDIF 

# Begin Target

# Name "midas - Win32 Release"
# Name "midas - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=amat_to_rotmagstr.cpp
# End Source File
# Begin Source File

SOURCE=file_io.cpp
# End Source File
# Begin Source File

SOURCE=gaussj.cpp
# End Source File
# Begin Source File

SOURCE=graphics.cpp
# End Source File
# Begin Source File

SOURCE=midas.cpp
# End Source File
# Begin Source File

SOURCE=slots.cpp
# End Source File
# Begin Source File

SOURCE=transforms.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=graphics.h

!IF  "$(CFG)" == "midas - Win32 Release"

USERDEP__GRAPH="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing graphics.h...
InputPath=graphics.h

"tmp\moc_graphics.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc graphics.h -o tmp\moc_graphics.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "midas - Win32 Debug"

USERDEP__GRAPH="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing graphics.h...
InputPath=graphics.h

"tmp\moc_graphics.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc graphics.h -o tmp\moc_graphics.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=midas.h

!IF  "$(CFG)" == "midas - Win32 Release"

USERDEP__MIDAS="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing midas.h...
InputPath=midas.h

"tmp\moc_midas.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc midas.h -o tmp\moc_midas.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "midas - Win32 Debug"

USERDEP__MIDAS="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing midas.h...
InputPath=midas.h

"tmp\moc_midas.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc midas.h -o tmp\moc_midas.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=slots.h

!IF  "$(CFG)" == "midas - Win32 Release"

USERDEP__SLOTS="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing slots.h...
InputPath=slots.h

"tmp\moc_slots.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc slots.h -o tmp\moc_slots.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "midas - Win32 Debug"

USERDEP__SLOTS="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing slots.h...
InputPath=slots.h

"tmp\moc_slots.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc slots.h -o tmp\moc_slots.cpp

# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Group "Generated"

# PROP Default_Filter ""
# Begin Source File

SOURCE=tmp\moc_graphics.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_midas.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_slots.cpp
# End Source File
# End Group
# End Target
# End Project
