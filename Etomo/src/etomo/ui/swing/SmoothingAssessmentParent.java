package etomo.ui.swing;

import etomo.ui.FieldValidationFailedException;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2009/12/19 01:20:03  sueh
 * <p> bug# 1294 Interface implemented by the parent of SmoothingAssessmentPanel.
 * <p> </p>
 */
interface SmoothingAssessmentParent {
  public static final String rcsid = "$Id$";

  public boolean isOneSurface();

  public String getWarpSpacingX(boolean doValidation)
      throws FieldValidationFailedException;

  public String getWarpSpacingY(boolean doValidation)
      throws FieldValidationFailedException;
}
