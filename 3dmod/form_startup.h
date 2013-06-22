/*   form_startup.h  -  declarations for form_startup.cpp
 *
 *  $Id$
 */
#ifndef STARTUPFORM_H
#define STARTUPFORM_H

#include <qvariant.h>

typedef struct ViewInfo ImodView;

#include "ui_form_startup.h"
class QButtonGroup;

class StartupForm : public QDialog, public Ui::StartupForm
{
  Q_OBJECT

    public:
  StartupForm(QWidget* parent = 0, bool modal = false, Qt::WindowFlags fl = 0);
  ~StartupForm();

  public slots:
    virtual void init();
  virtual void manageForModView();
  virtual void manageMontage();
  virtual void manageModvSize();
  virtual void modvSizeClicked( int id );
  virtual void cacheTypeClicked( int id );
  virtual void showMontageToggled( bool state );
  virtual void startAsClicked( int id );
  virtual void imageChanged( const QString & images );
  virtual void modelChanged( const QString & model );
  virtual void pfileChanged( const QString & pfile );
  virtual void angleFileChanged( const QString & afile );
  virtual void imageSelectClicked();
  virtual void modelSelectClicked();
  virtual void pieceSelectClicked();
  virtual void angleSelectClicked();
  virtual void enableButtons( bool enable );
  virtual void addArg( const char * arg );
  virtual char ** getArguments( int & argc );
  virtual void addImageFiles();
  virtual void setValues(ImodView *vi, char **argv, int firstfile, int argc, 
                            int doImodv, QStringList &plFileNames, char *anglefname,
                            int useMdoc, int xyzwinopen, int sliceropen, int zapOpen,
                            int modelViewOpen, int fillCache, int ImodTrans, int mirror,
                            int frames, int nframex, int nframey, int overx, int overy,
                          int overEntered);
  virtual void helpClicked();

 protected:
  bool mShowMontage;
  int mCacheOption;
  int mModvSizeOption;
  bool mModvMode;
  QString mImageFiles;
  QString mPieceFiles;
  QStringList mImageFileList;
  QStringList mPieceFileList;
  QString mModelFile;
  bool mJoinedWithSpace;
  bool mPieceJoinedwSpace;
  bool mFilesChanged;
  bool mPiecesChanged;
  char **mArgv;
  int mArgc;
  QString mStr;
  QString mAngleFile;
  QButtonGroup *startAsGroup;
  QButtonGroup *cacheGroup;
  QButtonGroup *windowSizeGroup;

  protected slots:
    virtual void languageChange();

 private:
    void loadFileList(QStringList &fileList, QString &files, QLineEdit *fileEdit, 
                      bool &joinedWithSpace);
};

#endif // STARTUPFORM_H
