TEMPLATE	= app
LANGUAGE	= C++

CONFIG	+= qt opengl assistant

HEADERS	+= autox.h \
	b3dgfx.h \
	b3dfile.h \
	imod_edit.h \
	imod_client_message.h \
	imod.h \
	imod_info.h \
	imod_info_cb.h \
	imod_input.h \
	imod_io.h \
	imod_moviecon.h \
	imod_cachefill.h \
	imod_model_edit.h \
	imod_iscale.h \
	imod_cont_edit.h \
	imod_cont_copy.h \
	imodP.h \
	control.h \
	imodv.h \
	imod_workprocs.h \
	imod_utilities.h \
	iproc.h \
	slicer_classes.h \
	sslice.h \
	xtum.h \
	xxyz.h \
	xzap.h \
	xgraph.h \
	imod_object_edit.h \
	pixelview.h \
	imodv_control.h \
	zap_classes.h \
	imod_display.h \
	imodv_window.h \
	imodv_gfx.h \
	imodv_ogl.h \
	imodv_input.h \
	imodv_menu.h \
	imodv_light.h \
	imodv_stereo.h \
	imodv_depthcue.h \
	imodv_views.h \
	imodv_modeled.h \
	imodv_objed.h \
	imodv_listobj.h \
	imodv_image.h \
	histwidget.h \
	imodv_isosurface.h \
	imodv_movie.h \
	qcursor.bits \
	qcursor_mask.bits \
	time_lock.bits \
	unlock.bits \
	lock.bits \
	hottoolbar.h \
	imodplug.h \
	xcramp.h \
	preferences.h \
	imodplugP.h \
	imodview.h \
	imodviewP.h \
	controlP.h \
	beadfix.h \
	special_module.h \
	xcorr.h \
	undoredo.h \
	locator.h \
	imod_assistant.h \
	finegrain.h \
	pegged.xpm \
	unpegged.xpm \
	b3dicon.xpm \
	iirawimage.h \
	scalebar.h \
	isothread.h \
	imodv_mcubes.h \
	imodv_mcubescpp.h \
	imodv_surfpieces.h \
	imodv_mappingtable.h \
	form_object_edit.h \
	formv_control.h \
	formv_movie.h \
	formv_modeled.h \
	formv_views.h \
	formv_depthcue.h \
	formv_objed.h \
	form_info.h \
	form_moviecon.h \
	form_autox.h \
	form_cont_edit.h \
	form_behavior.h \
	form_mouse.h \
	form_startup.h \
	form_rawimage.h \
	form_finegrain.h \
	form_slicerangle.h \
	form_scalebar.h \
	form_appearance.h


SOURCES	+= autox.cpp \
	b3dfile.c \
	b3dgfx.cpp \
	control.cpp \
	imod_cachefill.cpp \
	imod_client_message.cpp \
	imod_cont_copy.cpp \
	imod_cont_edit.cpp \
	imod_display.cpp \
	imod_edit.cpp \
	imod_info.cpp \
	imod_info_cb.cpp \
	imod_input.cpp \
	imod_io.cpp \
	imod_iscale.cpp \
	imod_menu.cpp \
	imod_model_draw.cpp \
	imod_model_edit.cpp \
	imod_moviecon.cpp \
	imodplug.cpp \
	imodv_depthcue.cpp \
	imodv_gfx.cpp \
	imodview.cpp \
	imodv_image.cpp \
	histwidget.cpp \
	isothread.cpp \
	imodv_isosurface.cpp \
	imodv_surfpieces.cpp \
	imodv_input.cpp \
	imodv_light.cpp \
	imodv_menu.cpp \
	imodv_modeled.cpp \
	imodv_movie.cpp \
	imodv_objed.cpp \
	imodv_listobj.cpp \
	imodv_ogl.cpp \
	imodv_stereo.cpp \
	imodv_views.cpp \
	imod_workprocs.cpp \
	imod_utilities.cpp \
	iproc.cpp \
	pixelview.cpp \
	slicer.cpp \
	slicer_classes.cpp \
	wprint.cpp \
	xgraph.cpp \
	xtum.cpp \
	xyz.cpp \
	xzap.cpp \
	imod.cpp \
	imod_object_edit.cpp \
	imodv_control.cpp \
	imodv.cpp \
	zap_classes.cpp \
	imodv_window.cpp \
	xcramp.cpp \
	preferences.cpp \
	beadfix.cpp \
	xcorr.cpp \
	undoredo.cpp \
	locator.cpp \
	imod_assistant.cpp \
	iiqimage.cpp \
	iirawimage.cpp \
	finegrain.cpp \
	scalebar.cpp \
	imodv_mappingtable.cpp \
	form_object_edit.cpp \
	formv_control.cpp \
	formv_movie.cpp \
	formv_modeled.cpp \
	formv_views.cpp \
	formv_depthcue.cpp \
	formv_objed.cpp \
	form_info.cpp \
	form_moviecon.cpp \
	form_autox.cpp \
	form_cont_edit.cpp \
	form_behavior.cpp \
	form_mouse.cpp \
	form_startup.cpp \
	form_rawimage.cpp \
	form_finegrain.cpp \
	form_slicerangle.cpp \
	form_scalebar.cpp \
	form_appearance.cpp


FORMS	= form_object_edit.ui \
	formv_control.ui \
	formv_movie.ui \
	formv_modeled.ui \
	formv_views.ui \
	formv_depthcue.ui \
	formv_objed.ui \
	form_info.ui \
	form_moviecon.ui \
	form_autox.ui \
	form_cont_edit.ui \
	form_behavior.ui \
	form_mouse.ui \
	form_startup.ui \
	form_rawimage.ui \
	form_finegrain.ui \
	form_slicerangle.ui \
	form_scalebar.ui \
	form_appearance.ui 

RESOURCES = 3dmod.qrc

TARGET = 3dmod
INSTALLS += target

include (qconfigure)

QT +=  opengl 

