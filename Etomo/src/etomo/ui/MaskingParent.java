package etomo.ui;

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
 * <p> $Log$ </p>
 */
interface MaskingParent {
  public static final String rcsid = "$Id$";

  public boolean isReferenceFileSelected();

  public int getVolumeTableSize();

  public boolean fixIncorrectPath(FileTextField fileTextField,
      boolean choosePath);

  public void updateDisplay();
}
