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
 * <p> $Log$ </p>
 */

interface ContextMenu {
  public static final String rcsid = "$Id$";

  void popUpContextMenu(MouseEvent mouseEvent);
}
