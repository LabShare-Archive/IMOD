package etomo.ui.swing;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/12/01 00:25:25  sueh
 * <p> bug# 1285 Factored MissingWedgeCompensation out of PeetDialog.
 * <p> </p>
 */
interface MissingWedgeCompensationParent {
  public static final String rcsid = "$Id$";

  public boolean isVolumeTableEmpty();

  public boolean isReferenceParticleSelected();

  public void updateDisplay();
}
