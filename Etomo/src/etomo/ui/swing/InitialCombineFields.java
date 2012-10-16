package etomo.ui.swing;

import etomo.type.FiducialMatch;
import etomo.type.MatchMode;
import etomo.ui.FieldValidationFailedException;

/**
* <p>Description: </p>
*
* <p>Copyright: Copyright 2004</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$
* <p> $Revision 1.1  2010/11/13 16:07:34  sueh
* <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p> $
* <p> $Revision 1.6  2006/09/13 23:45:38  sueh
* <p> $bug# 921 Added get and setMatchMode().
* <p> $
* <p> $Revision 1.5  2006/05/16 21:35:41  sueh
* <p> $bug# 856 Added useCorrespondingPoints and useList.
* <p> $
* <p> $Revision 1.4  2006/03/16 01:55:45  sueh
* <p> $bug# 828 Added isEnabled().
* <p> $
* <p> $Revision 1.3  2004/06/14 23:39:53  rickg
* <p> $Bug #383 Transitioned to using solvematch
* <p> $
* <p> $Revision 1.2  2004/05/11 21:46:48  sueh
* <p> $bug# 302 making InitialCombineFields interface local to the
* <p> $ui package
* <p> $
* <p> $Revision 1.1  2004/05/11 20:50:39  sueh
* <p> $bug #302 interface to initial combine screen fields.  Used by
* <p> $setup panel and initial panel.
* <p> $$ </p>
*/
interface InitialCombineFields {
  public static final String rcsid = "$$Id$$";

  public void setSurfacesOrModels(FiducialMatch useMatchingModels);

  public FiducialMatch getSurfacesOrModels();

  public void setBinBy2(boolean binBy2);

  public boolean isBinBy2();

  public void setFiducialMatchListA(String fiducialMatchListA);

  public String getFiducialMatchListA(boolean doValidation)
      throws FieldValidationFailedException;

  public String getFiducialMatchListA();

  public void setFiducialMatchListB(String fiducialMatchListB);

  public String getFiducialMatchListB(boolean doValidation)
      throws FieldValidationFailedException;

  public String getFiducialMatchListB();

  public boolean isEnabled();

  public boolean isUseCorrespondingPoints();

  public void setUseCorrespondingPoints(boolean use);

  public void setUseList(String useList);

  public String getUseList(final boolean doValidation)
      throws FieldValidationFailedException;

  public String getUseList();

  public MatchMode getMatchMode();

  public void setMatchMode(MatchMode matchMode);
}
