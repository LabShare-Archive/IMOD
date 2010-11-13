package etomo.ui.swing;

/**
 * <p>Description: Parent of an instance of a UseExistingProjectPanel.</p>
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
 * <p> Revision 1.1  2009/10/15 23:39:47  sueh
 * <p> bug# 1274 Implemented by PeetDialog.
 * <p> </p>
 */
interface UseExistingProjectParent {
  public static final String rcsid = "$Id$";

  public FileTextField getDirectory();
}
