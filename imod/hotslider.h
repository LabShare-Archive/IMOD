#ifndef HOTSLIDER_H
#define HOTSLIDER_H

#ifndef IMOD_PREFERENCES_H
// Defines for the states: these are replicated in colorselector.cpp for now
#define HOT_SLIDER_KEYUP 0
#define HOT_SLIDER_KEYDOWN 1
#define NO_HOT_SLIDER 2
int hotSliderFlag();
int hotSliderKey();
#endif
#endif
