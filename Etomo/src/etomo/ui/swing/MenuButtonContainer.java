package etomo.ui.swing;

import etomo.type.ActionElement;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.2  2007/09/13 18:38:25  sueh
 * <p> bug# 847 Added action().
 * <p>
 * <p> Revision 1.1  2007/08/10 17:34:29  sueh
 * <p> bug# 847 Interface for a class that responds to options in MenuButton's right
 * <p> click menu.
 * <p> </p>
 */
interface MenuButtonContainer {
  public static final String rcsid = "$Id$";

  public void action(String command, ActionElement actionElement);
}
