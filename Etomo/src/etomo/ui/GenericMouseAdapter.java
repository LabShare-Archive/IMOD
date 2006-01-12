package etomo.ui;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.SwingUtilities;

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
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/10/15 16:38:22  rickg
 * <p> Responds to pressed instead clicked right mouse button events now
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
class GenericMouseAdapter implements MouseListener {
  public static final String rcsid =
    "$Id$";

  ContextMenu adaptee;

  GenericMouseAdapter(ContextMenu adaptee) {
    this.adaptee = adaptee;
  }
  public void mouseClicked(MouseEvent e) {
  }
  public void mousePressed(MouseEvent e) {
		if (SwingUtilities.isRightMouseButton(e))
			adaptee.popUpContextMenu(e);
  }

  public void mouseReleased(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }
}
