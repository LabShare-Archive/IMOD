/*   formv_sequence.h  -  declarations for formv_sequence.cpp
 *
 *  $Id$
 */
#ifndef SEQUENCEFORM_H
#define SEQUENCEFORM_H

// This is based on autodoc buffer size of 10240 and two characters per object
#define MAX_OBJ_ONOFF 5000

#include <qvariant.h>

#include "ui_formv_sequence.h"
#include "mv_movie.h"

class MovieSequenceForm : public QWidget, public Ui::MovieSequenceForm
{
  Q_OBJECT

    public:
  MovieSequenceForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~MovieSequenceForm();

  void updateEnables(bool movieEnabled, bool makingMovie);

  public slots:
  virtual void init();
  virtual void changeEvent(QEvent *e);
  virtual void addAfterClicked();
  virtual void addBeforeClicked();
  virtual void replaceClicked();
  virtual void deleteClicked();
  virtual void setMovieClicked();
  virtual void setStartClicked();
  virtual void setEndClicked();
  virtual void runAllClicked();
  virtual void saveClicked();
  virtual void loadClicked();
  virtual void helpClicked();
  virtual void closeEvent( QCloseEvent * e );
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );
  virtual void entryChanged(int row, int column);
  

  protected slots:
  virtual void languageChange();

 private:
  int saveSequence();
  void setFontDependentWidths();
  void addAtIndex(int index);
  void setStartOrEnd(int startEnd);
  void loadRow(MovieSegment *
segment, int row, bool block);
  void loadTable();
  std::vector<MovieSegment> *mSegments;
  bool mMakingMovie;
  bool mMovieEnabled;
  bool mModified;
};

#endif // SEQUENCEFORM_H
