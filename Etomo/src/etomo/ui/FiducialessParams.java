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
 * <p> $Log$ </p>
 */
package etomo.ui;


public interface FiducialessParams {
  public static final String rcsid = "$Id$";
  public void setFiducialessAlignment(boolean state);
  public boolean isFiducialessAlignment();
  public void setTiltAxisAngle(float tiltAxisAngle);
  public float getTiltAxisAngle() throws NumberFormatException;
}
