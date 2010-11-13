package etomo.ui.swing;

import java.awt.Component;

import javax.swing.JPanel;

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
* <p> $Log$ </p>
*/
public final class SirtPanelView {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();

  private SirtPanelView() {
  }

  public static SirtPanelView getInstance() {
    SirtPanelView instance = new SirtPanelView();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
  }

  public Component getRoot() {
    return pnlRoot;
  }
}
