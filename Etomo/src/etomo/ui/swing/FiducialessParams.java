/**
 * <p>Description: Specifies the fiducialess UI parameters</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 18:08:55  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.4  2008/10/16 21:20:16  sueh
 * <p> bug# 1141 Removed functions from this interface which are not being used generically.
 * <p>
 * <p> Revision 1.3  2006/07/28 19:45:44  sueh
 * <p> bug# 868 Changed isFiduciallessAlignment to isFiducialess
 * <p>
 * <p> Revision 1.2  2006/05/19 19:46:55  sueh
 * <p> bug# 866 Changed set/getTiltAxisAngle to set/getImageRotation.
 * <p>
 * <p> Revision 1.1  2004/05/25 23:24:53  rickg
 * <p> Bug #391 initial revision
 * <p> </p>
 */
package etomo.ui.swing;

import etomo.ui.FieldValidationFailedException;

public interface FiducialessParams {
  public static final String rcsid = "$Id$";

  public boolean isFiducialess();

  public String getImageRotation(boolean doValidation)
      throws FieldValidationFailedException;
}
