# Microsoft Developer Studio Project File - Name="diaqt" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=diaqt - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libdiaqt.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libdiaqt.mak" CFG="diaqt - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "diaqt - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "diaqt - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "diaqt - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD CPP /nologo /MD /O1 /I "..\include" /I "$(QTDIR)\include" /I "$(QTDIR)\mkspecs\win32-msvc" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "UNICODE" /D "QT_DLL" /D "QT_THREAD_SUPPORT" /FD -Zm200 /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD LIB32 /nologo /out:"..\buildlib\libdiaqt.lib"

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD CPP /nologo /MDd /Z7 /Od /I "..\include" /I "$(QTDIR)\include" /I "$(QTDIR)\mkspecs\win32-msvc" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "UNICODE" /D "QT_DLL" /D "QT_THREAD_SUPPORT" /FD /GZ -Zm200 /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD LIB32 /nologo /out:"..\buildlib\libdiaqt.lib"

!ENDIF 

# Begin Target

# Name "diaqt - Win32 Release"
# Name "diaqt - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=arrowbutton.cpp
# End Source File
# Begin Source File

SOURCE=colorselector.cpp
# End Source File
# Begin Source File

SOURCE=dia_qtutils.cpp
# End Source File
# Begin Source File

SOURCE=dialog_frame.cpp
# End Source File
# Begin Source File

SOURCE=.\floatspinbox.cpp
# End Source File
# Begin Source File

SOURCE=multislider.cpp
# End Source File
# Begin Source File

SOURCE=tooledit.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\include\arrowbutton.h

!IF  "$(CFG)" == "diaqt - Win32 Release"

USERDEP__ARROW="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\arrowbutton.h...
InputPath=..\include\arrowbutton.h

"..\include\moc_arrowbutton.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\arrowbutton.h -o ..\include\moc_arrowbutton.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

USERDEP__ARROW="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\arrowbutton.h...
InputPath=..\include\arrowbutton.h

"..\include\moc_arrowbutton.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\arrowbutton.h -o ..\include\moc_arrowbutton.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\include\colorselector.h

!IF  "$(CFG)" == "diaqt - Win32 Release"

USERDEP__COLOR="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\colorselector.h...
InputPath=..\include\colorselector.h

"..\include\moc_colorselector.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\colorselector.h -o ..\include\moc_colorselector.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

USERDEP__COLOR="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\colorselector.h...
InputPath=..\include\colorselector.h

"..\include\moc_colorselector.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\colorselector.h -o ..\include\moc_colorselector.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\include\dia_qtutils.h
# End Source File
# Begin Source File

SOURCE=..\include\dialog_frame.h

!IF  "$(CFG)" == "diaqt - Win32 Release"

USERDEP__DIALO="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\dialog_frame.h...
InputPath=..\include\dialog_frame.h

"..\include\moc_dialog_frame.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\dialog_frame.h -o ..\include\moc_dialog_frame.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

USERDEP__DIALO="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\dialog_frame.h...
InputPath=..\include\dialog_frame.h

"..\include\moc_dialog_frame.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\dialog_frame.h -o ..\include\moc_dialog_frame.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\include\floatspinbox.h
# End Source File
# Begin Source File

SOURCE=..\include\floatspinbox.h

!IF  "$(CFG)" == "diaqt - Win32 Release"

USERDEP__FLOAT="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\floatspinbox.h...
InputPath=..\include\floatspinbox.h

"..\include\moc_floatspinbox.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\floatspinbox.h -o ..\include\moc_floatspinbox.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

USERDEP__FLOAT="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\floatspinbox.h...
InputPath=..\include\floatspinbox.h

"..\include\moc_floatspinbox.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\floatspinbox.h -o ..\include\moc_floatspinbox.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\include\multislider.h

!IF  "$(CFG)" == "diaqt - Win32 Release"

USERDEP__MULTI="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\multislider.h...
InputPath=..\include\multislider.h

"..\include\moc_multislider.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\multislider.h -o ..\include\moc_multislider.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

USERDEP__MULTI="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\multislider.h...
InputPath=..\include\multislider.h

"..\include\moc_multislider.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\multislider.h -o ..\include\moc_multislider.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\plax.h
# End Source File
# Begin Source File

SOURCE=..\include\tooledit.h

!IF  "$(CFG)" == "diaqt - Win32 Release"

USERDEP__TOOLE="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\tooledit.h...
InputPath=..\include\tooledit.h

"..\include\moc_tooledit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\tooledit.h -o ..\include\moc_tooledit.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

USERDEP__TOOLE="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing ..\include\tooledit.h...
InputPath=..\include\tooledit.h

"..\include\moc_tooledit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc ..\include\tooledit.h -o ..\include\moc_tooledit.cpp

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

SOURCE=..\include\moc_arrowbutton.cpp

!IF  "$(CFG)" == "diaqt - Win32 Release"

# PROP Intermediate_Dir "Release"

!ELSEIF  "$(CFG)" == "diaqt - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\include\moc_colorselector.cpp
# End Source File
# Begin Source File

SOURCE=..\include\moc_dialog_frame.cpp
# End Source File
# Begin Source File

SOURCE=..\include\moc_floatspinbox.cpp
# End Source File
# Begin Source File

SOURCE=..\include\moc_multislider.cpp
# End Source File
# Begin Source File

SOURCE=..\include\moc_tooledit.cpp
# End Source File
# End Group
# End Target
# End Project
