package etomo.ui;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.ui.swing.SetupDialogExpert;

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
public final class SetupReconUIHarness {
  public static final String rcsid = "$Id:$";

  private SetupDialogExpert expert = null;

  public void doAutomation() {
    if (expert != null && !EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      expert.doAutomation();
    }
  }

  public SetupDialogExpert getSetupDialogExpert(final ApplicationManager manager,
      final boolean calibrationAvailable) {
    if (expert == null) {
      expert = SetupDialogExpert.getInstance(manager, calibrationAvailable);
    }
    return expert;
  }

  public void freeDialog() {
    expert = null;
  }
}
