package etomo.ui.swing;

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
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.1  2010/01/21 21:31:33  sueh
 * <p> bug# 1305 Factored filter full volume panel out of AnisotropicDiffusionDialog.
 * <p> </p>
 */
interface FilterFullVolumeParent extends ProcessInterface {
  public static final String rcsid = "$Id$";

  public void cleanUp();

  public String getVolume();

  public boolean initSubdir();

  public boolean isLoadWithFlipping();
}
