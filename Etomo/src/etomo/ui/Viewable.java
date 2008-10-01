package etomo.ui;

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
 * <p> Revision 1.1  2008/09/30 22:53:28  sueh
 * <p> bug# 1113 An interface for tables that can be used with a Viewport.
 * <p> </p>
 */
interface Viewable {
  public static final String rcsid = "$Id$";

  public void msgViewportMoved();

  public int size();
}
