package etomo.ui;

import java.awt.event.MouseEvent;

/**
 * <p>Description: Defines the popUpContextMenu interface for right mouse
 *  button events.</p>
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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

interface ContextMenu {
  public static final String rcsid =
    "$Id$";

  void popUpContextMenu(MouseEvent mouseEvent);
}
