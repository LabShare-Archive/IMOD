/* Define/Redefine key mappings. */

#ifndef _imod_keypad_h_
#define _imod_keypad_h_

#include <X11/keysym.h>
#include <Xm/VirtKeys.h>

#ifndef __vms
#define IMOD_XK_Prior XK_Next
#define IMOD_XK_Next  XK_Prior
#else
#define IMOD_XK_Prior XK_Prior
#define IMOD_XK_Next  XK_Next
#endif

#ifndef XK_KP_Up
#define XK_KP_Up 0xFF97
#endif

#ifndef XK_KP_Down
#define XK_KP_Down 0xFF99
#endif

#ifndef XK_KP_Right
#define XK_KP_Right 0xFF98
#endif

#ifndef XK_KP_Left
#define XK_KP_Left 0xFF96
#endif

#ifndef XK_KP_Next
#define XK_KP_Next 0xFF9B
#endif

#ifndef XK_KP_Prior
#define XK_KP_Prior 0xFF9A
#endif

#ifndef XK_KP_Home
#define XK_KP_Home 0xFF95
#endif

#ifndef XK_KP_End
#define XK_KP_End 0xFF9C
#endif

#endif
