/*
 * ctfutils.h - declarations for utility functions used by ctfplotter and
 *                 ctfphaseflip
 *
 *  $Id$
 */
#ifndef CTFUTILS_H
#define CTFUTILS_H

typedef struct ilist_struct Ilist;

typedef struct defocus_struct {
  int startingSlice;
  int endingSlice;
  double lAngle;
  double hAngle;
  double defocus;
} SavedDefocus;

float *readTiltAngles(const char *angleFile, int mNzz, float mAngleSign,
                      float &mMinAngle, float &mMaxAngle);
void addItemToDefocusList(Ilist *mSaved, SavedDefocus toSave);
Ilist *readDefocusFile(const char *mFnDefocus, int &defVersion);
int checkAndFixDefocusList(Ilist *list, float *angles, int nz, int defVersion);

#endif
