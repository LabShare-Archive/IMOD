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
 * <p> Revision 1.2  2006/05/19 19:46:55  sueh
 * <p> bug# 866 Changed set/getTiltAxisAngle to set/getImageRotation.
 * <p>
 * <p> Revision 1.1  2004/05/25 23:24:53  rickg
 * <p> Bug #391 initial revision
 * <p> </p>
 */
package etomo.ui;


public interface FiducialessParams {
  public static final String rcsid = "$Id$";
  public void setFiducialessAlignment(boolean state);
  public boolean isFiducialess();
  public void setImageRotation(float imageRotation);
  public float getImageRotation() throws NumberFormatException;
}
