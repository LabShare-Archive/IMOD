#ifndef HOTSLIDER_H
#define HOTSLIDER_H

// Defines for the states: these are replicated in colorselector.cpp for now
#define NO_HOT_SLIDER 0
#define HOT_SLIDER_KEYUP 1
#define HOT_SLIDER_KEYDOWN -1
int hotSliderFlag();
int hotSliderKey();
#endif
