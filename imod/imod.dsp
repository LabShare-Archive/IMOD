# Microsoft Developer Studio Project File - Name="imod" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=imod - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "imod.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "imod.mak" CFG="imod - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "imod - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "imod - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "imod - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD CPP /nologo /MD /W3 /O1 /I "..\include" /I "$(QTDIR)\include" /I "tmp\\" /I "$(QTDIR)\mkspecs\win32-msvc" /I "." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "UNICODE" /D "QT_DLL" /D "QT_THREAD_SUPPORT" /D "QT_NO_DEBUG" /FD -Zm200 /c
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# ADD LINK32 "qt-mt311.lib" "qtmain.lib" "opengl32.lib" "glu32.lib" "delayimp.lib" "..\buildlib\libimod.lib" "..\buildlib\libiimod.lib" "..\buildlib\libdiaqt.lib" "kernel32.lib" "user32.lib" "gdi32.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "imm32.lib" "winmm.lib" "wsock32.lib" "winspool.lib" delayimp.lib /nologo /subsystem:windows /machine:IX86 /nodefaultlib:"msvcrtd.lib" /nodefaultlib:"libcd.lib" /nodefaultlib:"libc.lib" /libpath:"$(QTDIR)\lib" /DELAYLOAD:opengl32.dll /DELAYLOAD:comdlg32.dll /DELAYLOAD:oleaut32.dll /DELAYLOAD:winmm.dll /DELAYLOAD:wsock32.dll /DELAYLOAD:winspool.dll

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "tmp"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "tmp"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD CPP /nologo /MDd /w /W0 /Z7 /Od /I "." /I "..\include" /I "$(QTDIR)\include" /I "tmp\\" /I "$(QTDIR)\mkspecs\win32-msvc" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "UNICODE" /D "QT_DLL" /D "QT_THREAD_SUPPORT" /FD /GZ -Zm200 /c
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# ADD LINK32 "qt-mt311.lib" "qtmain.lib" "opengl32.lib" "glu32.lib" "delayimp.lib" "libimod.lib" "libiimod.lib" "libdiaqt.lib" "kernel32.lib" "user32.lib" "gdi32.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "imm32.lib" "winmm.lib" "wsock32.lib" "winspool.lib" /nologo /subsystem:windows /debug /machine:IX86 /nodefaultlib:"msvcrt.lib" /nodefaultlib:"libcd.lib" /pdbtype:sept /libpath:"$(QTDIR)\lib" /libpath:"..\buildlib\Debug" /DELAYLOAD:opengl32.dll
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "imod - Win32 Release"
# Name "imod - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=autox.cpp
# End Source File
# Begin Source File

SOURCE=b3dfile.c
# End Source File
# Begin Source File

SOURCE=b3dgfx.cpp
# End Source File
# Begin Source File

SOURCE=control.cpp
# End Source File
# Begin Source File

SOURCE=form_autox.ui.h
# End Source File
# Begin Source File

SOURCE=form_cont_edit.ui.h
# End Source File
# Begin Source File

SOURCE=form_info.ui.h
# End Source File
# Begin Source File

SOURCE=form_moviecon.ui.h
# End Source File
# Begin Source File

SOURCE=form_object_edit.ui.h
# End Source File
# Begin Source File

SOURCE=formv_control.ui.h
# End Source File
# Begin Source File

SOURCE=formv_depthcue.ui.h
# End Source File
# Begin Source File

SOURCE=formv_modeled.ui.h
# End Source File
# Begin Source File

SOURCE=formv_movie.ui.h
# End Source File
# Begin Source File

SOURCE=formv_objed.ui.h
# End Source File
# Begin Source File

SOURCE=formv_views.ui.h
# End Source File
# Begin Source File

SOURCE=imod.cpp
# End Source File
# Begin Source File

SOURCE=imod_cachefill.cpp
# End Source File
# Begin Source File

SOURCE=imod_client_message.cpp
# End Source File
# Begin Source File

SOURCE=imod_cont_copy.cpp
# End Source File
# Begin Source File

SOURCE=imod_cont_edit.cpp
# End Source File
# Begin Source File

SOURCE=imod_display.cpp
# End Source File
# Begin Source File

SOURCE=imod_edit.cpp
# End Source File
# Begin Source File

SOURCE=imod_info.cpp
# End Source File
# Begin Source File

SOURCE=imod_info_cb.cpp
# End Source File
# Begin Source File

SOURCE=imod_input.cpp
# End Source File
# Begin Source File

SOURCE=imod_io.cpp
# End Source File
# Begin Source File

SOURCE=imod_iscale.cpp
# End Source File
# Begin Source File

SOURCE=imod_menu.cpp
# End Source File
# Begin Source File

SOURCE=imod_model_draw.cpp
# End Source File
# Begin Source File

SOURCE=imod_model_edit.cpp
# End Source File
# Begin Source File

SOURCE=imod_moviecon.cpp
# End Source File
# Begin Source File

SOURCE=imod_object_edit.cpp
# End Source File
# Begin Source File

SOURCE=imod_workprocs.cpp
# End Source File
# Begin Source File

SOURCE=imodplug.cpp
# End Source File
# Begin Source File

SOURCE=imodv.cpp
# End Source File
# Begin Source File

SOURCE=imodv_control.cpp
# End Source File
# Begin Source File

SOURCE=imodv_depthcue.cpp
# End Source File
# Begin Source File

SOURCE=imodv_gfx.cpp
# End Source File
# Begin Source File

SOURCE=imodv_image.cpp
# End Source File
# Begin Source File

SOURCE=imodv_input.cpp
# End Source File
# Begin Source File

SOURCE=imodv_light.cpp
# End Source File
# Begin Source File

SOURCE=imodv_menu.cpp
# End Source File
# Begin Source File

SOURCE=imodv_modeled.cpp
# End Source File
# Begin Source File

SOURCE=imodv_movie.cpp
# End Source File
# Begin Source File

SOURCE=imodv_objed.cpp
# End Source File
# Begin Source File

SOURCE=imodv_ogl.cpp
# End Source File
# Begin Source File

SOURCE=imodv_stereo.cpp
# End Source File
# Begin Source File

SOURCE=imodv_views.cpp
# End Source File
# Begin Source File

SOURCE=imodv_window.cpp
# End Source File
# Begin Source File

SOURCE=imodview.cpp
# End Source File
# Begin Source File

SOURCE=iproc.cpp
# End Source File
# Begin Source File

SOURCE=pixelview.cpp
# End Source File
# Begin Source File

SOURCE=samplemeansd.c
# End Source File
# Begin Source File

SOURCE=sliceproc.c
# End Source File
# Begin Source File

SOURCE=slicer.cpp
# End Source File
# Begin Source File

SOURCE=slicer_classes.cpp
# End Source File
# Begin Source File

SOURCE=wprint.cpp
# End Source File
# Begin Source File

SOURCE=xcramp.cpp
# End Source File
# Begin Source File

SOURCE=xgraph.cpp
# End Source File
# Begin Source File

SOURCE=xtum.cpp
# End Source File
# Begin Source File

SOURCE=xyz.cpp
# End Source File
# Begin Source File

SOURCE=xzap.cpp
# End Source File
# Begin Source File

SOURCE=zap_classes.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=autox.h
# End Source File
# Begin Source File

SOURCE=b3dfile.h
# End Source File
# Begin Source File

SOURCE=b3dgfx.h
# End Source File
# Begin Source File

SOURCE=control.h
# End Source File
# Begin Source File

SOURCE=hotkey.h
# End Source File
# Begin Source File

SOURCE=hotslider.h
# End Source File
# Begin Source File

SOURCE=imod.h
# End Source File
# Begin Source File

SOURCE=imod_cachefill.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_cachefill.h...
InputPath=imod_cachefill.h

"tmp\moc_imod_cachefill.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_cachefill.h -o tmp\moc_imod_cachefill.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_cachefill.h...
InputPath=imod_cachefill.h

"tmp\moc_imod_cachefill.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_cachefill.h -o tmp\moc_imod_cachefill.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_client_message.h
# End Source File
# Begin Source File

SOURCE=imod_client_message.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_C="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_client_message.h...
InputPath=imod_client_message.h

"tmp\moc_imod_client_message.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_client_message.h -o tmp\moc_imod_client_message.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_C="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_client_message.h...
InputPath=imod_client_message.h

"tmp\moc_imod_client_message.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_client_message.h -o tmp\moc_imod_client_message.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_cont_copy.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_CO="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_cont_copy.h...
InputPath=imod_cont_copy.h

"tmp\moc_imod_cont_copy.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_cont_copy.h -o tmp\moc_imod_cont_copy.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_CO="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_cont_copy.h...
InputPath=imod_cont_copy.h

"tmp\moc_imod_cont_copy.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_cont_copy.h -o tmp\moc_imod_cont_copy.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_cont_edit.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_CON="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_cont_edit.h...
InputPath=imod_cont_edit.h

"tmp\moc_imod_cont_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_cont_edit.h -o tmp\moc_imod_cont_edit.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_CON="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_cont_edit.h...
InputPath=imod_cont_edit.h

"tmp\moc_imod_cont_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_cont_edit.h -o tmp\moc_imod_cont_edit.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_display.h
# End Source File
# Begin Source File

SOURCE=imod_edit.h
# End Source File
# Begin Source File

SOURCE=imod_info.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_I="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_info.h...
InputPath=imod_info.h

"tmp\moc_imod_info.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_info.h -o tmp\moc_imod_info.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_I="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_info.h...
InputPath=imod_info.h

"tmp\moc_imod_info.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_info.h -o tmp\moc_imod_info.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_info_cb.h
# End Source File
# Begin Source File

SOURCE=imod_input.h
# End Source File
# Begin Source File

SOURCE=imod_io.h
# End Source File
# Begin Source File

SOURCE=imod_iscale.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_IS="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_iscale.h...
InputPath=imod_iscale.h

"tmp\moc_imod_iscale.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_iscale.h -o tmp\moc_imod_iscale.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_IS="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_iscale.h...
InputPath=imod_iscale.h

"tmp\moc_imod_iscale.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_iscale.h -o tmp\moc_imod_iscale.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_model_edit.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_M="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_model_edit.h...
InputPath=imod_model_edit.h

"tmp\moc_imod_model_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_model_edit.h -o tmp\moc_imod_model_edit.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_M="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_model_edit.h...
InputPath=imod_model_edit.h

"tmp\moc_imod_model_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_model_edit.h -o tmp\moc_imod_model_edit.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_moviecon.h
# End Source File
# Begin Source File

SOURCE=imod_object_edit.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_O="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_object_edit.h...
InputPath=imod_object_edit.h

"tmp\moc_imod_object_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_object_edit.h -o tmp\moc_imod_object_edit.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_O="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_object_edit.h...
InputPath=imod_object_edit.h

"tmp\moc_imod_object_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_object_edit.h -o tmp\moc_imod_object_edit.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imod_workprocs.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMOD_W="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_workprocs.h...
InputPath=imod_workprocs.h

"tmp\moc_imod_workprocs.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_workprocs.h -o tmp\moc_imod_workprocs.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMOD_W="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imod_workprocs.h...
InputPath=imod_workprocs.h

"tmp\moc_imod_workprocs.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imod_workprocs.h -o tmp\moc_imod_workprocs.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imodP.h
# End Source File
# Begin Source File

SOURCE=imodplug.h
# End Source File
# Begin Source File

SOURCE=imodv.h
# End Source File
# Begin Source File

SOURCE=imodv_control.h
# End Source File
# Begin Source File

SOURCE=imodv_depthcue.h
# End Source File
# Begin Source File

SOURCE=imodv_gfx.h
# End Source File
# Begin Source File

SOURCE=imodv_image.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMODV="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_image.h...
InputPath=imodv_image.h

"tmp\moc_imodv_image.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_image.h -o tmp\moc_imodv_image.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMODV="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_image.h...
InputPath=imodv_image.h

"tmp\moc_imodv_image.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_image.h -o tmp\moc_imodv_image.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imodv_input.h
# End Source File
# Begin Source File

SOURCE=imodv_light.h
# End Source File
# Begin Source File

SOURCE=imodv_menu.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMODV_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_menu.h...
InputPath=imodv_menu.h

"tmp\moc_imodv_menu.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_menu.h -o tmp\moc_imodv_menu.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMODV_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_menu.h...
InputPath=imodv_menu.h

"tmp\moc_imodv_menu.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_menu.h -o tmp\moc_imodv_menu.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imodv_modeled.h
# End Source File
# Begin Source File

SOURCE=imodv_movie.h
# End Source File
# Begin Source File

SOURCE=imodv_objed.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMODV_O="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_objed.h...
InputPath=imodv_objed.h

"tmp\moc_imodv_objed.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_objed.h -o tmp\moc_imodv_objed.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMODV_O="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_objed.h...
InputPath=imodv_objed.h

"tmp\moc_imodv_objed.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_objed.h -o tmp\moc_imodv_objed.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imodv_ogl.h
# End Source File
# Begin Source File

SOURCE=imodv_stereo.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMODV_S="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_stereo.h...
InputPath=imodv_stereo.h

"tmp\moc_imodv_stereo.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_stereo.h -o tmp\moc_imodv_stereo.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMODV_S="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_stereo.h...
InputPath=imodv_stereo.h

"tmp\moc_imodv_stereo.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_stereo.h -o tmp\moc_imodv_stereo.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=imodv_views.h
# End Source File
# Begin Source File

SOURCE=imodv_window.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IMODV_W="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_window.h...
InputPath=imodv_window.h

"tmp\moc_imodv_window.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_window.h -o tmp\moc_imodv_window.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IMODV_W="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing imodv_window.h...
InputPath=imodv_window.h

"tmp\moc_imodv_window.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc imodv_window.h -o tmp\moc_imodv_window.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=iproc.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__IPROC="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing iproc.h...
InputPath=iproc.h

"tmp\moc_iproc.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc iproc.h -o tmp\moc_iproc.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__IPROC="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing iproc.h...
InputPath=iproc.h

"tmp\moc_iproc.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc iproc.h -o tmp\moc_iproc.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=lock.bits
# End Source File
# Begin Source File

SOURCE=menus.h
# End Source File
# Begin Source File

SOURCE=pixelview.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__PIXEL="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing pixelview.h...
InputPath=pixelview.h

"tmp\moc_pixelview.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc pixelview.h -o tmp\moc_pixelview.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__PIXEL="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing pixelview.h...
InputPath=pixelview.h

"tmp\moc_pixelview.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc pixelview.h -o tmp\moc_pixelview.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=qcursor.bits
# End Source File
# Begin Source File

SOURCE=qcursor_mask.bits
# End Source File
# Begin Source File

SOURCE=sliceproc.h
# End Source File
# Begin Source File

SOURCE=slicer_classes.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__SLICE="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing slicer_classes.h...
InputPath=slicer_classes.h

"tmp\moc_slicer_classes.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc slicer_classes.h -o tmp\moc_slicer_classes.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__SLICE="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing slicer_classes.h...
InputPath=slicer_classes.h

"tmp\moc_slicer_classes.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc slicer_classes.h -o tmp\moc_slicer_classes.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=sslice.h
# End Source File
# Begin Source File

SOURCE=time_lock.bits
# End Source File
# Begin Source File

SOURCE=unlock.bits
# End Source File
# Begin Source File

SOURCE=xcramp.h
# End Source File
# Begin Source File

SOURCE=xgraph.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__XGRAP="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing xgraph.h...
InputPath=xgraph.h

"tmp\moc_xgraph.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc xgraph.h -o tmp\moc_xgraph.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__XGRAP="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing xgraph.h...
InputPath=xgraph.h

"tmp\moc_xgraph.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc xgraph.h -o tmp\moc_xgraph.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=xtum.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__XTUM_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing xtum.h...
InputPath=xtum.h

"tmp\moc_xtum.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc xtum.h -o tmp\moc_xtum.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__XTUM_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing xtum.h...
InputPath=xtum.h

"tmp\moc_xtum.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc xtum.h -o tmp\moc_xtum.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=xxyz.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__XXYZ_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing xxyz.h...
InputPath=xxyz.h

"tmp\moc_xxyz.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc xxyz.h -o tmp\moc_xxyz.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__XXYZ_="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing xxyz.h...
InputPath=xxyz.h

"tmp\moc_xxyz.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc xxyz.h -o tmp\moc_xxyz.cpp

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=xzap.h
# End Source File
# Begin Source File

SOURCE=zap_classes.h

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__ZAP_C="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing zap_classes.h...
InputPath=zap_classes.h

"tmp\moc_zap_classes.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc zap_classes.h -o tmp\moc_zap_classes.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__ZAP_C="$(QTDIR)\bin\moc.exe"	
# Begin Custom Build - Moc'ing zap_classes.h...
InputPath=zap_classes.h

"tmp\moc_zap_classes.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\moc zap_classes.h -o tmp\moc_zap_classes.cpp

# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Group "Forms"

# PROP Default_Filter "ui"
# Begin Source File

SOURCE=form_autox.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORM_="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_autox.ui...
InputPath=form_autox.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_autox.ui -o tmp\form_autox.h \
	$(QTDIR)\bin\uic form_autox.ui -i form_autox.h -o tmp\form_autox.cpp \
	$(QTDIR)\bin\moc tmp\form_autox.h -o tmp\moc_form_autox.cpp \
	

"tmp\form_autox.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_autox.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_autox.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORM_="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_autox.ui...
InputPath=form_autox.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_autox.ui -o tmp\form_autox.h \
	$(QTDIR)\bin\uic form_autox.ui -i form_autox.h -o tmp\form_autox.cpp \
	$(QTDIR)\bin\moc tmp\form_autox.h -o tmp\moc_form_autox.cpp \
	

"tmp\form_autox.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_autox.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_autox.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=form_cont_edit.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORM_C="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_cont_edit.ui...
InputPath=form_cont_edit.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_cont_edit.ui -o tmp\form_cont_edit.h \
	$(QTDIR)\bin\uic form_cont_edit.ui -i form_cont_edit.h -o tmp\form_cont_edit.cpp \
	$(QTDIR)\bin\moc tmp\form_cont_edit.h -o tmp\moc_form_cont_edit.cpp \
	

"tmp\form_cont_edit.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_cont_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_cont_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORM_C="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_cont_edit.ui...
InputPath=form_cont_edit.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_cont_edit.ui -o tmp\form_cont_edit.h \
	$(QTDIR)\bin\uic form_cont_edit.ui -i form_cont_edit.h -o tmp\form_cont_edit.cpp \
	$(QTDIR)\bin\moc tmp\form_cont_edit.h -o tmp\moc_form_cont_edit.cpp \
	

"tmp\form_cont_edit.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_cont_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_cont_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=form_info.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORM_I="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_info.ui...
InputPath=form_info.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_info.ui -o tmp\form_info.h \
	$(QTDIR)\bin\uic form_info.ui -i form_info.h -o tmp\form_info.cpp \
	$(QTDIR)\bin\moc tmp\form_info.h -o tmp\moc_form_info.cpp \
	

"tmp\form_info.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_info.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_info.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORM_I="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_info.ui...
InputPath=form_info.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_info.ui -o tmp\form_info.h \
	$(QTDIR)\bin\uic form_info.ui -i form_info.h -o tmp\form_info.cpp \
	$(QTDIR)\bin\moc tmp\form_info.h -o tmp\moc_form_info.cpp \
	

"tmp\form_info.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_info.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_info.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=form_moviecon.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORM_M="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_moviecon.ui...
InputPath=form_moviecon.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_moviecon.ui -o tmp\form_moviecon.h \
	$(QTDIR)\bin\uic form_moviecon.ui -i form_moviecon.h -o tmp\form_moviecon.cpp \
	$(QTDIR)\bin\moc tmp\form_moviecon.h -o tmp\moc_form_moviecon.cpp \
	

"tmp\form_moviecon.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_moviecon.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_moviecon.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORM_M="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_moviecon.ui...
InputPath=form_moviecon.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_moviecon.ui -o tmp\form_moviecon.h \
	$(QTDIR)\bin\uic form_moviecon.ui -i form_moviecon.h -o tmp\form_moviecon.cpp \
	$(QTDIR)\bin\moc tmp\form_moviecon.h -o tmp\moc_form_moviecon.cpp \
	

"tmp\form_moviecon.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_moviecon.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_moviecon.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=form_object_edit.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORM_O="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_object_edit.ui...
InputPath=form_object_edit.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_object_edit.ui -o tmp\form_object_edit.h \
	$(QTDIR)\bin\uic form_object_edit.ui -i form_object_edit.h -o tmp\form_object_edit.cpp \
	$(QTDIR)\bin\moc tmp\form_object_edit.h -o tmp\moc_form_object_edit.cpp \
	

"tmp\form_object_edit.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_object_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_object_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORM_O="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing form_object_edit.ui...
InputPath=form_object_edit.ui

BuildCmds= \
	$(QTDIR)\bin\uic form_object_edit.ui -o tmp\form_object_edit.h \
	$(QTDIR)\bin\uic form_object_edit.ui -i form_object_edit.h -o tmp\form_object_edit.cpp \
	$(QTDIR)\bin\moc tmp\form_object_edit.h -o tmp\moc_form_object_edit.cpp \
	

"tmp\form_object_edit.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\form_object_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_form_object_edit.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=formv_control.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORMV="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_control.ui...
InputPath=formv_control.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_control.ui -o tmp\formv_control.h \
	$(QTDIR)\bin\uic formv_control.ui -i formv_control.h -o tmp\formv_control.cpp \
	$(QTDIR)\bin\moc tmp\formv_control.h -o tmp\moc_formv_control.cpp \
	

"tmp\formv_control.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_control.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_control.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORMV="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_control.ui...
InputPath=formv_control.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_control.ui -o tmp\formv_control.h \
	$(QTDIR)\bin\uic formv_control.ui -i formv_control.h -o tmp\formv_control.cpp \
	$(QTDIR)\bin\moc tmp\formv_control.h -o tmp\moc_formv_control.cpp \
	

"tmp\formv_control.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_control.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_control.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=formv_depthcue.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORMV_="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_depthcue.ui...
InputPath=formv_depthcue.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_depthcue.ui -o tmp\formv_depthcue.h \
	$(QTDIR)\bin\uic formv_depthcue.ui -i formv_depthcue.h -o tmp\formv_depthcue.cpp \
	$(QTDIR)\bin\moc tmp\formv_depthcue.h -o tmp\moc_formv_depthcue.cpp \
	

"tmp\formv_depthcue.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_depthcue.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_depthcue.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORMV_="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_depthcue.ui...
InputPath=formv_depthcue.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_depthcue.ui -o tmp\formv_depthcue.h \
	$(QTDIR)\bin\uic formv_depthcue.ui -i formv_depthcue.h -o tmp\formv_depthcue.cpp \
	$(QTDIR)\bin\moc tmp\formv_depthcue.h -o tmp\moc_formv_depthcue.cpp \
	

"tmp\formv_depthcue.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_depthcue.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_depthcue.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=formv_modeled.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORMV_M="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_modeled.ui...
InputPath=formv_modeled.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_modeled.ui -o tmp\formv_modeled.h \
	$(QTDIR)\bin\uic formv_modeled.ui -i formv_modeled.h -o tmp\formv_modeled.cpp \
	$(QTDIR)\bin\moc tmp\formv_modeled.h -o tmp\moc_formv_modeled.cpp \
	

"tmp\formv_modeled.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_modeled.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_modeled.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORMV_M="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_modeled.ui...
InputPath=formv_modeled.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_modeled.ui -o tmp\formv_modeled.h \
	$(QTDIR)\bin\uic formv_modeled.ui -i formv_modeled.h -o tmp\formv_modeled.cpp \
	$(QTDIR)\bin\moc tmp\formv_modeled.h -o tmp\moc_formv_modeled.cpp \
	

"tmp\formv_modeled.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_modeled.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_modeled.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=formv_movie.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORMV_MO="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_movie.ui...
InputPath=formv_movie.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_movie.ui -o tmp\formv_movie.h \
	$(QTDIR)\bin\uic formv_movie.ui -i formv_movie.h -o tmp\formv_movie.cpp \
	$(QTDIR)\bin\moc tmp\formv_movie.h -o tmp\moc_formv_movie.cpp \
	

"tmp\formv_movie.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_movie.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_movie.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORMV_MO="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_movie.ui...
InputPath=formv_movie.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_movie.ui -o tmp\formv_movie.h \
	$(QTDIR)\bin\uic formv_movie.ui -i formv_movie.h -o tmp\formv_movie.cpp \
	$(QTDIR)\bin\moc tmp\formv_movie.h -o tmp\moc_formv_movie.cpp \
	

"tmp\formv_movie.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_movie.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_movie.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=formv_objed.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORMV_O="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_objed.ui...
InputPath=formv_objed.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_objed.ui -o tmp\formv_objed.h \
	$(QTDIR)\bin\uic formv_objed.ui -i formv_objed.h -o tmp\formv_objed.cpp \
	$(QTDIR)\bin\moc tmp\formv_objed.h -o tmp\moc_formv_objed.cpp \
	

"tmp\formv_objed.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_objed.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_objed.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORMV_O="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_objed.ui...
InputPath=formv_objed.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_objed.ui -o tmp\formv_objed.h \
	$(QTDIR)\bin\uic formv_objed.ui -i formv_objed.h -o tmp\formv_objed.cpp \
	$(QTDIR)\bin\moc tmp\formv_objed.h -o tmp\moc_formv_objed.cpp \
	

"tmp\formv_objed.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_objed.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_objed.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=formv_views.ui

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__FORMV_V="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_views.ui...
InputPath=formv_views.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_views.ui -o tmp\formv_views.h \
	$(QTDIR)\bin\uic formv_views.ui -i formv_views.h -o tmp\formv_views.cpp \
	$(QTDIR)\bin\moc tmp\formv_views.h -o tmp\moc_formv_views.cpp \
	

"tmp\formv_views.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_views.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_views.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__FORMV_V="$(QTDIR)\bin\moc.exe"	"$(QTDIR)\bin\uic.exe"	
# Begin Custom Build - Uic'ing formv_views.ui...
InputPath=formv_views.ui

BuildCmds= \
	$(QTDIR)\bin\uic formv_views.ui -o tmp\formv_views.h \
	$(QTDIR)\bin\uic formv_views.ui -i formv_views.h -o tmp\formv_views.cpp \
	$(QTDIR)\bin\moc tmp\formv_views.h -o tmp\moc_formv_views.cpp \
	

"tmp\formv_views.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\formv_views.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"tmp\moc_formv_views.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Images"

# PROP Default_Filter "png jpeg bmp xpm"
# Begin Source File

SOURCE=downarrow.png
# End Source File
# Begin Source File

SOURCE=leftarrow.png
# End Source File
# Begin Source File

SOURCE=rightarrow.png
# End Source File
# Begin Source File

SOURCE=uparrow.png

!IF  "$(CFG)" == "imod - Win32 Release"

USERDEP__UPARR="uparrow.png"	"downarrow.png"	"rightarrow.png"	"leftarrow.png"	
# Begin Custom Build - Creating image collection...
InputPath=uparrow.png

"tmp\qmake_image_collection.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\uic -embed imod -f images.tmp -o tmp\qmake_image_collection.cpp

# End Custom Build

!ELSEIF  "$(CFG)" == "imod - Win32 Debug"

USERDEP__UPARR="uparrow.png"	"downarrow.png"	"rightarrow.png"	"leftarrow.png"	
# Begin Custom Build - Creating image collection...
InputPath=uparrow.png

"tmp\qmake_image_collection.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(QTDIR)\bin\uic -embed imod -f images.tmp -o tmp\qmake_image_collection.cpp

# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Generated"

# PROP Default_Filter ""
# Begin Source File

SOURCE=tmp\form_autox.cpp
# End Source File
# Begin Source File

SOURCE=tmp\form_autox.h
# End Source File
# Begin Source File

SOURCE=tmp\form_cont_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\form_cont_edit.h
# End Source File
# Begin Source File

SOURCE=tmp\form_info.cpp
# End Source File
# Begin Source File

SOURCE=tmp\form_info.h
# End Source File
# Begin Source File

SOURCE=tmp\form_moviecon.cpp
# End Source File
# Begin Source File

SOURCE=tmp\form_moviecon.h
# End Source File
# Begin Source File

SOURCE=tmp\form_object_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\form_object_edit.h
# End Source File
# Begin Source File

SOURCE=tmp\formv_control.cpp
# End Source File
# Begin Source File

SOURCE=tmp\formv_control.h
# End Source File
# Begin Source File

SOURCE=tmp\formv_depthcue.cpp
# End Source File
# Begin Source File

SOURCE=tmp\formv_depthcue.h
# End Source File
# Begin Source File

SOURCE=tmp\formv_modeled.cpp
# End Source File
# Begin Source File

SOURCE=tmp\formv_modeled.h
# End Source File
# Begin Source File

SOURCE=tmp\formv_movie.cpp
# End Source File
# Begin Source File

SOURCE=tmp\formv_movie.h
# End Source File
# Begin Source File

SOURCE=tmp\formv_objed.cpp
# End Source File
# Begin Source File

SOURCE=tmp\formv_objed.h
# End Source File
# Begin Source File

SOURCE=tmp\formv_views.cpp
# End Source File
# Begin Source File

SOURCE=tmp\formv_views.h
# End Source File
# Begin Source File

SOURCE=tmp\moc_form_autox.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_form_cont_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_form_info.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_form_moviecon.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_form_object_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_formv_control.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_formv_depthcue.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_formv_modeled.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_formv_movie.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_formv_objed.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_formv_views.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_cachefill.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_client_message.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_cont_copy.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_cont_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_info.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_iscale.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_model_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_object_edit.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imod_workprocs.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imodv_image.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imodv_menu.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imodv_objed.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imodv_stereo.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_imodv_window.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_iproc.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_pixelview.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_slicer_classes.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_xgraph.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_xtum.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_xxyz.cpp
# End Source File
# Begin Source File

SOURCE=tmp\moc_zap_classes.cpp
# End Source File
# Begin Source File

SOURCE=tmp\qmake_image_collection.cpp
# End Source File
# End Group
# End Target
# End Project
