#include <X11/Intrinsic.h>

/* STATUS RETURNS */
#	define SG_VISUAL_SUCCESS	1
#	define SG_VISUAL_DEFAULT	(SG_VISUAL_SUCCESS + 1)

#	define SG_NO_VISUAL		(-1)
#	define SG_BAD_DISPLAY		(SG_NO_VISUAL-1)
#	define SG_NO_TYPE_AND_CLASS	(SG_BAD_DISPLAY-1)
#	define SG_NO_SUCH_VISUAL	(SG_NO_TYPE_AND_CLASS-1)


/* Visual types */
#	define SG_DEFAULT_PLANES	0	/* matches resource default */
#	define SG_UNDERLAY_PLANES	1
#	define SG_NORMAL_PLANES		2
#	define SG_OVERLAY_PLANES	3
#	define SG_POPUP_PLANES		4
#	define SG_MAX_TYPES		(SG_POPUP_PLANES + 1)


/* External declarations */

#ifdef SG_IS_STATIC
#define SG_STORAGE static
#else
#define SG_STORAGE extern
#endif /* SG_IS_STATIC */

#ifdef _NO_PROTO
SG_STORAGE int		SG_defaultDepthAndTypeResources ();
SG_STORAGE Colormap	SG_getDefaultColorMap();
SG_STORAGE int		SG_getDefaultDepth ();
SG_STORAGE XVisualInfo *SG_getMatchingVisual ();
SG_STORAGE int		SG_getMaxDepth ();
SG_STORAGE int		SG_getNormalArgs();
SG_STORAGE int		SG_getOverlayArgs();
SG_STORAGE int		SG_getOverlay2Args();
SG_STORAGE int		SG_getOverlay4Args();
SG_STORAGE int		SG_getPopupArgs();
SG_STORAGE int		SG_getUnderlayArgs();
SG_STORAGE int		SG_getVisualArgs();
#else /* _NO_PROTO */
SG_STORAGE int	    SG_defaultDepthAndTypeResources
	( Display *display, int screen, int *requestedClass,
	  char *requestedType, int *requestedTypeV, int *requestedDepth,
	  Visual **requestedVisual, Colormap *requestedColormap,
	  Drawable *requestedDrawable);
SG_STORAGE Colormap SG_getDefaultColormap
	(Display *dpy, int scr, Visual *vsl);
SG_STORAGE int      SG_getDefaultDepth
	(Display *dpy, int scr, int *class, int type);
SG_STORAGE XVisualInfo *SG_getMatchingVisual
	(Display *dpy, int scr, VisualID vsl, int *class, int type, int depth);
SG_STORAGE int      SG_getMaxDepth
	(Display *dpy, int scr, int *class, int type);
SG_STORAGE int      SG_getNormalArgs
	(Display *dpy, int scr, ArgList args, int *n);
SG_STORAGE int     SG_getOverlayArgs
	(Display *dpy, int scr, ArgList args, int *n);
SG_STORAGE int     SG_getOverlay2Args
	(Display *dpy, int scr, ArgList args, int *n);
SG_STORAGE int     SG_getOverlay4Args
	(Display *dpy, int scr, ArgList args, int *n);
SG_STORAGE int      SG_getPopupArgs
	(Display *dpy, int scr, ArgList args, int *n);
SG_STORAGE int      SG_getUnderlayArgs
	(Display *dpy, int scr, ArgList args, int *n);
SG_STORAGE int      SG_getVisualArgs
	(Display *dpy, int scr, int dpth, int *class, int type, ArgList args, int *n);
#endif /* _NO_PROTO */
#undef SG_STORAGE
