package etomo.ui;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.ui.swing.FrontPageDialog;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class FrontPageUIHarness {
  public static final String rcsid = "$Id:$";

  private final BaseManager manager;
  private final AxisID axisID;

  private FrontPageDialog dialog = null;

  public FrontPageUIHarness(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  public void openDialog() {
    if (dialog == null && !EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      dialog = FrontPageDialog.getInstance(manager, axisID);
      dialog.show();
    }
  }

  public void reconActionForAutomation() {
    EtomoDirector.INSTANCE.openTomogramAndDoAutomation(true, axisID);
  }
}
