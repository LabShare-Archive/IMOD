/* options.h */


/*****************************************************************************/
/* keep program in foreground, writes garbage to screen. */
/* #define IMOD_DEBUG */


/*****************************************************************************/
/* select color requester for Edit->Object->Color menu item                  */
/* #define FORMS_COLOR */       /* Use forms color requester instead of xreq */
                                /* Must have forms libarary.                 */
#define XREQ_COLOR              /* Use the xreq color requester.             */


/*****************************************************************************/
/* select the type of tumble selected by tumble menu.                        */

/* hardware tumble, machine MUST support blendfunction(); */
#define IMOD_TUMBLE_OPEN  imod_tumble_open(XYZ_vi) 

/* or select software tumble, better but has bugs.  */
/* #define IMOD_TUMBLE_OPEN  imod_stum_open(XYZ_vi) */


