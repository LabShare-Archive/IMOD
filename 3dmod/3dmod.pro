TEMPLATE	= app
LANGUAGE	= C++

CONFIG	+= qt opengl

HEADERS	+= autox.h \
	b3dgfx.h \
	b3dfile.h \
	imod_edit.h \
	client_message.h \
	imod.h \
	info_setup.h \
	info_cb.h \
	imod_input.h \
	imod_io.h \
	moviecon.h \
	cachefill.h \
	model_edit.h \
	rescale.h \
	cont_edit.h \
	cont_copy.h \
	imodP.h \
	control.h \
	imodv.h \
	workprocs.h \
	utilities.h \
	iproc.h \
	slicer_classes.h \
	sslice.h \
	xtum.h \
	xxyz.h \
	xzap.h \
	xgraph.h \
	object_edit.h \
	pixelview.h \
	mv_control.h \
	zap_classes.h \
	display.h \
	mv_window.h \
	mv_gfx.h \
	mv_ogl.h \
	mv_input.h \
	mv_menu.h \
	mv_light.h \
	mv_stereo.h \
	mv_depthcue.h \
	mv_views.h \
	mv_modeled.h \
	mv_objed.h \
	mv_listobj.h \
	mv_image.h \
	histwidget.h \
	isosurface.h \
	mv_movie.h \
	qcursor.bits \
	qcursor_mask.bits \
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
	mcubes.h \
	mcubescpp.h \
	surfpieces.h \
	mappingtable.h \
        vertexbuffer.h \
        pyramidcache.h \
        resizetool.h \
        rotationtool.h \
	form_object_edit.h \
	formv_control.h \
	formv_movie.h \
	formv_modeled.h \
	formv_views.h \
	formv_depthcue.h \
	formv_objed.h \
	formv_sequence.h \
	form_info.h \
	form_moviecon.h \
	form_autox.h \
	form_cont_edit.h \
	form_behavior.h \
	form_snapshot.h \
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
	cachefill.cpp \
	client_message.cpp \
	cont_copy.cpp \
	cont_edit.cpp \
	display.cpp \
	imod_edit.cpp \
	info_setup.cpp \
	info_cb.cpp \
	imod_input.cpp \
	imod_io.cpp \
	rescale.cpp \
	info_menu.cpp \
	model_draw.cpp \
	model_edit.cpp \
	moviecon.cpp \
	imodplug.cpp \
	mv_depthcue.cpp \
	mv_gfx.cpp \
	imodview.cpp \
	mv_image.cpp \
	histwidget.cpp \
	isothread.cpp \
	isosurface.cpp \
	surfpieces.cpp \
	mv_input.cpp \
	mv_light.cpp \
	mv_menu.cpp \
	mv_modeled.cpp \
	mv_movie.cpp \
	mv_objed.cpp \
	mv_listobj.cpp \
	mv_ogl.cpp \
	mv_stereo.cpp \
	mv_views.cpp \
	workprocs.cpp \
	utilities.cpp \
	iproc.cpp \
	pixelview.cpp \
	slicer.cpp \
	slicer_classes.cpp \
	slicerthreads.cpp \
	wprint.cpp \
	xgraph.cpp \
	xtum.cpp \
	xyz.cpp \
	xzap.cpp \
	imod.cpp \
	object_edit.cpp \
	mv_control.cpp \
	imodv.cpp \
	zap_classes.cpp \
	mv_window.cpp \
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
	mappingtable.cpp \
        vertexbuffer.cpp \
        pyramidcache.cpp \
        rotationtool.cpp \
        resizetool.cpp \
	form_object_edit.cpp \
	formv_control.cpp \
	formv_movie.cpp \
	formv_modeled.cpp \
	formv_views.cpp \
	formv_depthcue.cpp \
	formv_objed.cpp \
	formv_sequence.cpp \
	form_info.cpp \
	form_moviecon.cpp \
	form_autox.cpp \
	form_cont_edit.cpp \
	form_behavior.cpp \
	form_snapshot.cpp \
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
	formv_sequence.ui \
	form_info.ui \
	form_moviecon.ui \
	form_autox.ui \
	form_cont_edit.ui \
	form_snapshot.ui \
	form_mouse.ui \
	form_behavior.ui \
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

QT +=  opengl sql

