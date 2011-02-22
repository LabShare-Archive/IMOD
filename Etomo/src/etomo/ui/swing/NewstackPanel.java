package etomo.ui.swing;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ProcessName;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.6  2009/09/21 17:57:50  sueh
 * <p> bug# 1267 ProcessManager.newst handles newst and newst_3dfind so the
 * <p> process name must be passed.
 * <p>
 * <p> Revision 1.5  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class NewstackPanel extends NewstackOrBlendmontPanel {
  public static final String rcsid = "$Id$";

  private NewstackPanel(ApplicationManager manager, AxisID axisID, DialogType dialogType,
      GlobalExpandButton globalAdvancedButton) {
    super(manager, axisID, dialogType, globalAdvancedButton);
  }

  static NewstackPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    NewstackPanel instance = new NewstackPanel(manager, axisID, dialogType,
        globalAdvancedButton);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  String getHeaderTitle() {
    return "Newstack";
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  void action(final String command, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(getRunProcessButtonActionCommand())) {
      manager.newst(getRunProcessResultDisplay(), null, deferred3dmodButton, axisID,
          run3dmodMenuOptions, dialogType, getFiducialessParams(), this,
          ProcessName.NEWST);
    }
    else if (command.equals(get3dmodFullButtonActionCommand())) {
      manager.imodFineAlign(axisID, run3dmodMenuOptions);
    }
  }
}
