/* imod info.h */
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/
#ifndef IMOD_INFO_H
#define IMOD_INFO_H

#ifdef __cplusplus
extern "C" {
#endif

extern XtAppContext Imod_info_context;
extern int Imod_info_quit;
extern int ImodForbidLevel;

/* Widgets */
extern Widget Imod_info_top;
extern Widget Imod_widget_image;
extern Widget Imod_widget_model;
extern Widget Imod_widget_blacklevel;
extern Widget Imod_widget_whitelevel;
extern Widget Imod_widget_object;
extern Widget Imod_widget_contour;
extern Widget Imod_widget_point;
extern Widget Imod_widget_info;
extern Widget Imod_widget_status;
extern Widget Imod_widget_blackval;
extern Widget Imod_widget_whiteval;
extern Widget Imod_widget_x;
extern Widget Imod_widget_y;
extern Widget Imod_widget_z;
extern Widget Imod_widget_model;
extern Widget Imod_widget_movie;
extern Widget Imod_widget_recallbyz;
extern XColor Imod_object_color;


void contSurfShow(void);

void imod_nextobj_cb(Widget w, XtPointer client, XtPointer call);
void imod_prevobj_cb(Widget w, XtPointer client, XtPointer call);
void imod_nextcont_cb(Widget w, XtPointer client, XtPointer call);
void imod_prevcont_cb(Widget w, XtPointer client, XtPointer call);
void imod_nextpoint_cb(Widget w, XtPointer client, XtPointer call);
void imod_prevpoint_cb(Widget w, XtPointer client, XtPointer call);
void imod_nextx_cb(Widget w, XtPointer client, XtPointer call);
void imod_prevx_cb(Widget w, XtPointer client, XtPointer call);
void imod_nexty_cb(Widget w, XtPointer client, XtPointer call);
void imod_prevy_cb(Widget w, XtPointer client, XtPointer call);
void imod_nextz_cb(Widget w, XtPointer client, XtPointer call);
void imod_prevz_cb(Widget w, XtPointer client, XtPointer call);
void imod_obj_select_cb(Widget w, XtPointer client, XtPointer call);
void imod_blacklevel_cb(Widget w, XtPointer client, XtPointer call);
void imod_whitelevel_cb(Widget w, XtPointer client, XtPointer call);
void imod_float_cb(Widget w, XtPointer client, XtPointer call);
void MaintainModelName(Imod *mod);



/*  Call back functions. */
void imod_file_cb(Widget w, XtPointer client, XtPointer call);
void imod_file_model_cb(Widget w, XtPointer client, XtPointer call);
void imod_file_write_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_model_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_object_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_surface_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_contour_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_point_cb(Widget w, XtPointer client, XtPointer call);
void imod_edit_image_cb(Widget w, XtPointer client, XtPointer call);
void imod_win_cb(Widget w, XtPointer client, XtPointer call);
void imod_help_cb(Widget w, XtPointer client, XtPointer call);

void imod_mmode_cb(Widget w, XtPointer client, XtPointer call);

#ifdef __cplusplus
}
#endif

#endif    /* IMOD_INFO_H */
