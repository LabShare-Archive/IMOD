package etomo.ui;

/**
 * <p>Description: Parent of FilterFullVolumePanel.</p>
 * 
 * <p>Copyright: Copyright 2010</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
interface FilterFullVolumeParent {
  public static final String rcsid = "$Id$";

  public void cleanUp();

  public String getVolume();

  public boolean initSubdir();

  public boolean isLoadWithFlipping();
}
