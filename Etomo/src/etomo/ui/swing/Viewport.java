package etomo.ui.swing;

import java.awt.Component;

import javax.swing.JComponent;

/**
 * <p>Description: Calculates a viewport that displays a portion of a table.
 * Receives paging commands, can tell a table when to redisplay the viewport.
 * Can tell a row whether it is in the viewport.</p>
 * 
 * <p>This class calls resetViewport constantly.  This is the simplest way to
 * handle the great variety of potential situations that can come up when trying
 * the handle all the different requirements of the four (so far) tables.</p>
 * 
 * <p>See Viewable for a complete description of how to use Viewport with a
 * table.</p>
 * 
 * <p>Future changes.  It would be easy to add more focusableParents if
 * necessary.
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.4  2008/10/07 16:44:41  sueh
 * <p> bug# 1113 Improved names:  changed Viewport.msgViewportMoved to
 * <p> msgViewportPaged.
 * <p>
 * <p> Revision 1.3  2008/10/06 22:48:50  sueh
 * <p> bug# 1113 Made the viewport size changeable.  Added a minimum size.
 * <p> Simplified the decisions to call resetViewport:  always call it.  This cleaned up bugs.
 * <p>
 * <p> Revision 1.2  2008/10/01 22:54:28  sueh
 * <p> bug# 1113 Renamed Viewer.repositionViewer to msgViewportMoved.
 * <p>
 * <p> Revision 1.1  2008/09/30 22:55:16  sueh
 * <p> bug# 1113.  Class that keeps track of which rows should be displayed in
 * <p> a table.  Does not contain a container, but does have a paging panel that
 * <p> it can pass to the table that is being viewed.
 * <p> </p>
 */
final class Viewport {
  public static final String rcsid = "$Id$";

  private static final int MINUMUM_SIZE = 5;

  private final PagingPanel pagingPanel;
  private final Viewable viewable;
  private final int size;

  private int start = 0;
  private int end = -1;

  Viewport(final Viewable viewable, final int size, final JComponent focusableParent1,
      final JComponent focusableParent2, final JComponent focusableParent3,
      final String uniqueKey) {
    this.viewable = viewable;
    if (size < MINUMUM_SIZE) {
      this.size = MINUMUM_SIZE;
    }
    else {
      this.size = size;
    }
    pagingPanel = PagingPanel.getInstance(this, focusableParent1, focusableParent2,
        focusableParent3, uniqueKey);
    pagingPanel.setVisible(false);
  }

  void homeButtonAction() {
    pageViewport(0);
  }

  void pageUpButtonAction() {
    pageViewport(start - size);
  }

  void upButtonAction() {
    pageViewport(start - 1);
  }

  void downButtonAction() {
    pageViewport(start + 1);
  }

  void pageDownButtonAction() {
    pageViewport(start + size);
  }

  void endButtonAction() {
    pageViewport(viewable.size() - size);
  }

  /**
   * Sets a new start value.  Notifies viewable if there has been a change in
   * the viewport.  Used by button action functions.  Calls resetViewport with
   * newStart.
   * @param newStart - new start value
   */
  private void pageViewport(final int newStart) {
    //save old start and end values
    int origStart = start;
    int origEnd = end;
    //Reset viewport and decide whether viewable must update its display.
    if (resetViewport(newStart) && start != origStart || end != origEnd) {
      viewable.msgViewportPaged();
    }
  }

  /**
   * Takes newStart index, fixes it based on the size and viewport size,
   * and sets it to the start member variable.  The new start index may be the
   * value of the start member variable or it may be a new value.  Makes
   * sure that the viewport is always equal to the viewport size, unless the
   * viewable size is less then the viewport size.  Sets the end member variable
   * from the start member variable and from the viewport size.  Fixes the end
   * location.
   * Also hides and shows the paging panel and enables/disabled the paging
   * buttons.  Combining all of this functionality in one function and always
   * running resetViewport is most reliable.
   * This function needs to handle every situation that the viewport can get
   * into.
   * @param newStart - start with be set to newStart and then adjusted
   * @return false if the viewable size is too small to page
   */
  private boolean resetViewport(final int newStart) {
    int viewableSize = viewable.size();
    //No paging if the viewport is set to the entire viewable area and the
    //viewable area is smaller or equals to the viewport.
    if (viewableSize <= size) {
      //Hide the paging panel when it is unnecesary.
      pagingPanel.setVisible(false);
    }
    else {
      pagingPanel.setVisible(true);
    }
    boolean changed;
    //No paging if the viewport is set to the entire viewable area and the
    //viewable area is smaller or equals to the viewport.
    if (viewableSize <= size && start == 0 && end == viewableSize - 1) {
      changed = false;
    }
    else {
      changed = true;
      //set start
      start = newStart;
      //fix start
      //Prevent the viewport from going out of the viewable area or displaying
      //less then the viewport size.
      if (start > viewableSize - size) {
        start = viewableSize - size;
      }
      //Handles the case where the viewable area is smaller then the viewport
      //size.
      if (start < 0) {
        start = 0;
      }
      //set end
      end = start + size - 1;
      //fix end
      //Handles the case where the viewable area is smaller then the viewport
      //size.
      if (end >= viewableSize) {
        end = viewableSize - 1;
      }
    }
    pagingPanel.setUpEnabled(start > 0);
    pagingPanel.setDownEnabled(end < viewable.size() - 1);
    return changed;
  }

  /**
   * @return The Component containing paging buttons.
   */
  Component getPagingPanel() {
    return pagingPanel.getContainer();
  }

  /**
   * Runs resetViewport with the existing start value.  Useful for 0 length
   * tables, especially when multiple rows where removed at the same time - the
   * Processor table with nothing checked for instance.
   */
  void msgViewableChanged() {
    resetViewport(start);
  }

  /**
   * Force newIndex to appear in the viewport.  Useful for deleting, adding, and
   * moving up or down.  Calls resetViewport.  Moves the viewport as little as
   * possible.
   * @param newIndex
   * @return true if the viewport may have changed
   */
  boolean adjustViewport(final int newIndex) {
    //If newIndex is below the viewport, find a start value which puts the index
    //in the viewport.
    if (newIndex > end) {
      return resetViewport(newIndex - size + 1);
    }
    return resetViewport(newIndex);
  }

  /**
   * Returns true if an index is current in the viewport.  Useful for displaying
   * individual rows.  Calls resetViewport.
   * @param index
   * @return True if an index is currently in the viewport.
   */
  boolean inViewport(final int index) {
    resetViewport(start);
    return index >= start && index <= end;
  }
}
