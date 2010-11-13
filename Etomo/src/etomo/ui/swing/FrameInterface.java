package etomo.ui.swing;

import java.awt.event.ActionEvent;

import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.1  2010/02/17 04:59:46  sueh
 * <p> bug# 1301 frame interface for UIHarness.
 * <p> </p>
 */
interface FrameInterface {
  public static final String rcsid = "$Id$";

  public void menuFileAction(ActionEvent actionEvent);

  public void menuToolsAction(ActionEvent actionEvent);

  public void menuViewAction(ActionEvent actionEvent);

  public void menuOptionsAction(ActionEvent actionEvent);

  public void menuHelpAction(ActionEvent actionEvent);

  public void repaint();

  public void pack(boolean force);

  public void repaint(AxisID axisID);

  public void pack(AxisID axisID);
}
