package etomo.ui;

import java.awt.Component;

import javax.swing.JComponent;

/**
 * <p>Description: Calculates a viewport that displays a portion of a table.
 * Receives paging commands, can tell a table when to redisplay the viewport.
 * Can tell a row whether it is in the viewport.</p>
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
 * <p> Revision 1.1  2008/09/30 22:55:16  sueh
 * <p> bug# 1113.  Class that keeps track of which rows should be displayed in
 * <p> a table.  Does not contain a container, but does have a paging panel that
 * <p> it can pass to the table that is being viewed.
 * <p> </p>
 */
final class Viewport {
  public static final String rcsid = "$Id$";

  private static final int VIEWPORT_SIZE = 10;

  private final PagingPanel pagingPanel;
  private final Viewable viewable;

  private int start = 0;
  private int end = -1;

  Viewport(final Viewable viewable, final JComponent parent1,
      final JComponent parent2, final JComponent parent3, final String uniqueKey) {
    this.viewable = viewable;
    pagingPanel = PagingPanel.getInstance(this, parent1, parent2, parent3,
        uniqueKey);
    pagingPanel.setVisible(false);
  }

  void homeButtonAction() {
    moveViewport(0);
  }

  void pageUpButtonAction() {
    moveViewport(start - VIEWPORT_SIZE);
  }

  void upButtonAction() {
    moveViewport(start - 1);
  }

  void downButtonAction() {
    moveViewport(start + 1);
  }

  void pageDownButtonAction() {
    moveViewport(start + VIEWPORT_SIZE);
  }

  void endButtonAction() {
    moveViewport(viewable.size() - VIEWPORT_SIZE);
  }

  /**
   * Notifies viewable if there has been a change in the viewport.
   * @param start
   */
  private void moveViewport(final int newStart) {
    updateDisplay();
    //save old start and end values
    int origStart = start;
    int origEnd = end;
    if (!resetViewport(newStart)) {
      return;
    }
    //decide whether viewable must update its display.
    if (start != origStart || end != origEnd) {
      viewable.msgViewportMoved();
    }
  }

  /**
   * Sets the visibility of the paging panel based on whether
   * the viewable size is large enough to page.
   */
  private void updateDisplay() {
    //No paging if the viewport is set to the entire viewable area and the
    //viewable area is smaller or equals to the viewport.
    if (viewable.size() <= VIEWPORT_SIZE) {
      //Hide the paging panel when it is unnecesary.
      pagingPanel.setVisible(false);
    }
    else {
      pagingPanel.setVisible(true);
    }
  }

  /**
   * Takes a new start index, fixes it based on the size and viewport size,
   * and sets it to the start member variable.  The new start index may be the
   * value of the start member variable or it may be a different number.  Makes
   * sure that the viewport is always equal to the viewport size, unless the
   * viewable size is less then the viewport size.  Sets the end member variable
   * from the start member variable and from the viewport size.  Fixes the end
   * location.
   * @param newStart
   * @return false if the viewable size is too small to page
   */
  private boolean resetViewport(final int newStart) {
    int viewableSize = viewable.size();
    //No paging if the viewport is set to the entire viewable area and the
    //viewable area is smaller or equals to the viewport.
    if (viewableSize <= VIEWPORT_SIZE && start == 0 && end == viewableSize - 1) {
      return false;
    }
    start = newStart;
    //fix start
    //Prevent the viewport from going out of the viewable area or displaying
    //less then the viewport size.
    if (start > viewableSize - VIEWPORT_SIZE) {
      start = viewableSize - VIEWPORT_SIZE;
    }
    //Handles the case where the viewable area is smaller then the viewport
    //size.
    if (start < 0) {
      start = 0;
    }
    //set end
    end = start + VIEWPORT_SIZE - 1;
    //fix end
    //Handles the case where the viewable area is smaller then the viewport
    //size.
    if (end >= viewableSize) {
      end = viewableSize - 1;
    }
    return true;
  }

  /**
   * @return The Component containing paging buttons.
   */
  Component getPagingPanel() {
    return pagingPanel.getContainer();
  }

  /**
   * Force an index to appear in the viewport.  Move the viewport as little as
   * possible.
   * @param index
   * @return true if the viewport may have changed
   */
  boolean includeRowInViewport(final int index) {
    updateDisplay();
    //If index is below the viewport, find a start value which puts the index in
    //the viewport.
    if (index > end) {
      return resetViewport(index - VIEWPORT_SIZE + 1);
    }
    else {
      return resetViewport(index);
    }
  }

  /**
   * @param index
   * @return True if an index is current in the viewport.
   */
  boolean inViewport(final int index) {
    updateDisplay();
    if (start < 0 || start > viewable.size() - VIEWPORT_SIZE
        || end < VIEWPORT_SIZE - 1 || end >= viewable.size()) {
      resetViewport(start);
    }
    return index >= start && index <= end;
  }
}
