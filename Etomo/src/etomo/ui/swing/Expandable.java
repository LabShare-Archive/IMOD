package etomo.ui.swing;

/**
 * <p>Description: UI components which contain ExpandButtons must implement this
 * interface.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.3  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2004/11/19 23:52:09  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/17 21:35:29  sueh
 * <p> bug# 520 Interface to allow the expand button to be encapsulated.
 * <p> </p>
 */
public interface Expandable {
  public static final String rcsid = "$Id$";

  /**
   * Match the expandButton parameter and perform the expand/contract operation.
   * @param expandButton
   */
  public void expand(ExpandButton button);

  /**
   * An expand call from ProcessDialog.btnAdvanced.
   * @param button
   */
  public void expand(GlobalExpandButton button);
}
