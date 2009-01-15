/* slots.h - include file for slots class
 *
 *  $Id$
 *  Log at end of file
 */
#ifndef MIDASSLOTS_H
#define MIDASSLOTS_H
#include "midas.h"
//Added by qt3to4:
#include <QLabel>
#include <QKeyEvent>

class ImodAssistant;

class MidasSlots : public QObject
{
  Q_OBJECT
    
    public:
  MidasSlots();
  ~MidasSlots();

  void manage_xory(struct Midas_view *vw);
  void update_parameters(void);
  void update_sections(void);
  int getParamDecimals(int param);
  int getIncDecimals(int param);
  int getParamDigits(int param);
  int getIncDigits(int param);
  float getIncrement(int index, int type);
  void sprintf_decimals(char *string, int decimals, int digits, float val);
  void backup_current_mat(void);
  void sectionInc(int ds);
  void mouse_shift_image(void);
  void mouse_translate(void);
  void mouse_rotate(void);
  void mouse_stretch(unsigned int maskr);
  void midas_keyinput(QKeyEvent *event);
  void synchronizeChunk(int sec);
  int showHelpPage(const char *page);

  public slots:
    void slotFilemenu(int item);
  void slotEditmenu(int item);
  void slotHelpmenu(int item);
  void slotParameter(int item);
  void slotIncrement(int item);
  void slotAngle(int value);
  void slotCurValue(int sec);
  void slotRefValue(int sec);
  void slotChunkValue(int sec);
  void slotEdgeValue(int sec);
  void slotEdge(int upDown);
  void slotXory(int which);
  void slotLeave_out();
  void slotTop_error(int item);
  void slotZoom(int upDown);
  void slotBlock(int upDown);
  void slotInterpolate(bool state);
  void slotBlacklevel(int value);
  void slotWhitelevel(int value);
  void slotBlackPressed();
  void slotBlackReleased();
  void slotWhitePressed();
  void slotWhiteReleased();
  void slotApplyone(bool state);
  void slotKeepdiff(bool state);
  void slotAlign_arm();
  void slotAlign_disarm();
  void slotReverse(bool state);
  void slotOverlay(bool state);
  void slotGlobRot(double value);
  void slotTiltOff(double value);
  void slotConstrainMouse(bool state);
  void slotCosStretch(bool state);
  void slotAutoContrast();
  void slotMidas_quit();

 private:
  int index_to_edgeno(int index, int *xory);
  void retransform_slice(void);
  void update_overlay(void);
  void getChangeLimits (int *ist, int *ind);
  int save_transforms(void);
  int get_bw_index(void);
  int setbwlevels(int black, int white, int draw);
  void show_ref(void);
  void show_cur(void);
  void show_overlay(void);
  void control_help();
  void hotkey_help();
  void mouse_help();
  void scale(float step);
  void translate(float xstep, float ystep);
  void rotate(float step);
  void stretch(float step, float angle);
  void try_montage_section(int sec, int direction);
  void try_section_change(int ds, int dsref);
  void try_montage_edge(int sec, int direction);
  void display_bwslider_value(QLabel *w, int white);
  void convertNumLock(int &keysym, int &keypad);

  bool mBlackPressed;    // Flag that slider is being dragged
  bool mWhitePressed;
  int mBlackDisplayed;   // Value last displayed during drag
  int mWhiteDisplayed;
  ImodAssistant *mImodHelp;
};

#endif
/*
  
$Log$
Revision 3.7  2008/10/13 04:36:23  mast
Added cosine stretching

Revision 3.6  2007/10/03 21:36:10  mast
Added ImodAssistant help object

*/
