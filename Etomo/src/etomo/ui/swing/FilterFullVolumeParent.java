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
 * <p> Revision 1.1  2010/01/21 21:31:33  sueh
 * <p> bug# 1305 Factored filter full volume panel out of AnisotropicDiffusionDialog.
 * <p> </p>
 */
interface FilterFullVolumeParent {
  public static final String rcsid = "$Id$";

  public void cleanUp();

  public String getVolume();

  public boolean initSubdir();

  public boolean isLoadWithFlipping();
}
