/* slots.h - include file for slots class
 *
 *  $Id$
 */
#ifndef MIDASSLOTS_H
#define MIDASSLOTS_H
#include "midas.h"
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
  void updateWarpEdit(void);
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
  void translate(float xstep, float ystep);

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
  void slotLowerXvalue(int sec);
  void slotLowerYvalue(int sec);
  void slotLeave_out();
  void slotTop_error(int item);
  void slotZoom(int upDown);
  void slotInterpolate(bool state);
  void slotBlacklevel(int value);
  void slotWhitelevel(int value);
  void slotBlackPressed();
  void slotBlackReleased();
  void slotWhitePressed();
  void slotWhiteReleased();
  void slotApplyone(bool state);
  void slotKeepdiff(bool state);
  void slotEditWarp(bool state);
  void slotAlign_arm();
  void slotAlign_disarm();
  void slotReverse(bool state);
  void slotOverlay(bool state);
  void slotGlobRot(double value);
  void slotTiltOff(double value);
  void slotConstrainMouse(bool state);
  void slotCosStretch(bool state);
  void slotSkipError(bool state);
  void slotSkipExcluded(bool state);
  void slotExcludeEdge(bool state);
  void slotRobustFit(bool state);
  void slotRobustCrit(double value);
  void slotAutoContrast();
  void slotCorrelate();
  void slotCorrBoxSize(int value);
  void slotCorrShiftLimit(int value);
  void slotSelectWarpPointBySize(int direction);
  void slotDrawVectors(bool state);
  void slotMidas_quit();

 private:
  int index_to_edgeno(int index, int &xory);
  int lower_piece_to_edgeno(int pcx, int pcy, int xory);
  void edgeno_to_lower_piece(int edge, int xory, int &pcx, int &pcy);
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
  void rotate(float step);
  void stretch(float step, float angle);
  void try_montage_section(int sec, int direction);
  void try_section_change(int ds, int dsref);
  void try_montage_edge(int sec, int direction);
  void try_lower_piece(int pcx, int pcy, int xory, int direction);
  void stepLowerXorY(int xory, int direction);
  void finishNewEdge();
  void display_bwslider_value(QLabel *w, int white);
  void convertNumLock(int &keysym, int &keypad);

  bool mBlackPressed;    // Flag that slider is being dragged
  bool mWhitePressed;
  int mBlackDisplayed;   // Value last displayed during drag
  int mWhiteDisplayed;
  ImodAssistant *mImodHelp;
};

#endif
