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
 * <p> Revision 1.1  2009/12/01 23:41:38  sueh
 * <p> bug# 1290 Parent of iteration table.
 * <p> </p>
 */
interface IterationParent {
  public static final String rcsid = "$Id$";

  public void updateDisplay();
  public boolean isSampleSphere();
}
