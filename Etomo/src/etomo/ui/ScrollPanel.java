package etomo.ui;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JPanel;
import javax.swing.Scrollable;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 */
public class ScrollPanel extends JPanel implements Scrollable {
  public static final String rcsid = "$Id$";

  /**
   * @see javax.swing.Scrollable#getPreferredScrollableViewportSize()
   */
  public Dimension getPreferredScrollableViewportSize() {
    return getPreferredSize();
  }

  /**
   * @see javax.swing.Scrollable#getScrollableUnitIncrement(Rectangle, int, int)
   */
  public int getScrollableUnitIncrement(
    Rectangle visibleRect,
    int orientation,
    int direction) {
    return 0;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableBlockIncrement(Rectangle, int, int)
   */
  public int getScrollableBlockIncrement(
    Rectangle visibleRect,
    int orientation,
    int direction) {
    return 0;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportWidth()
   */
  public boolean getScrollableTracksViewportWidth() {
    return false;
  }

  /**
   * @see javax.swing.Scrollable#getScrollableTracksViewportHeight()
   */
  public boolean getScrollableTracksViewportHeight() {
    return false;
  }

}
