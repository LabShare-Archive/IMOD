package etomo.ui.swing;


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
final class SirtPanel {
  public static final String rcsid = "$Id$";

  private SirtPanelView view = SirtPanelView.getInstance();

  SirtPanel() {
  }
  
  SirtPanelView getView() {
    return view;
  }
}
