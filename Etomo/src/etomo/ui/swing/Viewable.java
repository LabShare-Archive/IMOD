package etomo.ui.swing;

/**
 * <p>Description: Interface for a table using Viewport.</p>
 * 
 * <p>To use, add a Viewport member to the table.  Call viewport.getPagingPanel
 * to get a panel containing paging buttons to display.  To handle paging, just
 * implement msgViewportPaged to remove and redisplay all the rows.  The rows
 * can be set up to decide whether they should display or that decision can be
 * made in the table, but it seems simplest to let the rows decide.</p>
 * 
 * <p>If hotkeys are desired, pass up to three panels to be focused on for
 * hotkey paging.  The panels need to be focusable (see SpacedPane.
 * FocusablePanel).  The BaseManager function getFocusComponent must be
 * overridden to pass the current focusable panel, which will request focus
 * after each pack or repaint call.</p>
 * 
 * <p>The Viewport uses a private function called resetViewport to keep its
 * start and end values correct.  The function is called by paging functions and
 * functions used by the table and rows to query, inform, and change the
 * viewport.  ResetViewport is also responsible for managing the state of the
 * paging buttons</p>
 * 
 * <p>Rows can pass their index to viewport.inViewport to decide if they should
 * be displayed.  This will always be the real table index that starts from
 * zero.  In the case of the processor table, there are really two table
 * indexes:  one for the whole table and one for the contracted table.  The row
 * must use the index which matches the current state of the display.</p>
 * 
 * <p>The table can use viewport.adjustViewport to put a specific row inside the
 * viewport.  After doing this, the table should remove and redisplay all rows.
 * The table can also tell the viewport that it has changed by using viewport.
 * msgViewableChanged.  Since inViewport also calls resetViewport, it is not
 * necessary to use msgViewableChanged unless there are no rows to display.
 * When there are rows, table can go ahead and remove and redisplay all rows and
 * the viewport will be reset.  Packing and repainting should be done after the
 * all the changes to the display are finished so that paging appears smooth to
 * the user.</p>
 *
 * <p>Viewport.resetViewport is being called a lot but this does not seem to be
 * causing any problems.  Being more selective about when to call resetViewport
 * caused reliability problems because it wasn't always being called when it was
 * needed.</p>
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
 * <p> Revision 1.3  2008/10/07 16:44:20  sueh
 * <p> bug# 1113 Improved names:  changed Viewport.msgViewportMoved to
 * <p> msgViewportPaged.  Added to instructions to use Viewport.
 * <p>
 * <p> Revision 1.2  2008/10/01 22:53:45  sueh
 * <p> bug# 1113 Renamed repositionViewer to msgViewportMoved.
 * <p>
 * <p> Revision 1.1  2008/09/30 22:53:28  sueh
 * <p> bug# 1113 An interface for tables that can be used with a Viewport.
 * <p> </p>
 */
interface Viewable {
  public static final String rcsid = "$Id$";

  /**
   * Only called when the viewport is paged.  Should remove and redisplay the
   * rows in the table.
   */
  public void msgViewportPaged();

  /**
   * Should return the total number of rows in the table.
   * @return
   */
  public int size();
}
