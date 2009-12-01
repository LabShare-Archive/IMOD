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
 * <p> $Log$
 * <p> Revision 1.1  2009/11/20 17:32:43  sueh
 * <p> bug# 1282 Parent of ReferencePanel.
 * <p> </p>
 */
interface ReferenceParent {
  public static final String rcsid = "$Id$";

  public boolean fixIncorrectPath(FileTextField fileTextField,
      boolean choosePath);

  public int getVolumeTableSize();

  public void updateDisplay();
}
