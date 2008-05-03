package etomo.ui;

import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
interface Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public void action(Run3dmodButton button, Run3dmodMenuOptions menuOptions);
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2005/08/11 23:58:17  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.
 * <p>
 * <p> Revision 1.1  2005/08/09 20:34:10  sueh
 * <p> bug# 711  Interface for panel or dialog containing instance(s) of
 * <p> Run3dmodButton.
 * <p> </p>
 */
