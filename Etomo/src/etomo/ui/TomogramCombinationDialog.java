package etomo.ui;

import java.awt.GridLayout;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
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
 * <p> Revision 1.6.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.6  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.5  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.4  2002/10/17 22:40:16  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.3  2002/10/08 23:55:39  rickg
 * <p> createCombineScript call now include a reference to this
 * <p> added NoumberFormatException
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TomogramCombinationDialog extends ProcessDialog {
  public static final String rcsid =
    "$Id$";
  SetupCombinePanel panelSetupCombine = new SetupCombinePanel();

  private JPanel panelButton = new JPanel();
  private JToggleButton buttonImodVolumeA = new JToggleButton("Imod volume A");
  private JToggleButton buttonImodVolumeB = new JToggleButton("Imod volume B");
  private JButton buttonCreate = new JButton("Create combine script");

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.FIRST);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    panelButton.setLayout(new GridLayout(1, 2, 30, 10));
    panelButton.add(buttonImodVolumeA);
    panelButton.add(buttonImodVolumeB);
    panelButton.add(buttonCreate);

    //  Construct the main panel for this dialog panel
    rootPanel.add(panelSetupCombine.getContainer());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelButton);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the buttons to the action listener
    TomogramCombinationActionAdapter actionListener =
      new TomogramCombinationActionAdapter(this);
    buttonImodVolumeA.addActionListener(actionListener);
    buttonImodVolumeB.addActionListener(actionListener);
    buttonCreate.addActionListener(actionListener);

    // Set the default advanced dialog state
    updateAdvanced();
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    applicationManager.packMainWindow();
  }

  public void setCombineParams(ConstCombineParams combineParams) {
    panelSetupCombine.setParameters(combineParams);
  }

  public void getCombineParams(CombineParams combineParams)
    throws NumberFormatException {
    panelSetupCombine.getParameters(combineParams);
  }

  //  Action functions for setup panel buttons
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(buttonImodVolumeA.getActionCommand())) {
      applicationManager.imodTomogram(AxisID.FIRST);
    }
    if (command.equals(buttonImodVolumeA.getActionCommand())) {
      applicationManager.imodTomogram(AxisID.SECOND);
    }
    if (command.equals(buttonCreate.getActionCommand())) {
      applicationManager.createCombineScripts();
    }
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramCombinationDialog();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramCombinationDialog();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramCombinationDialog();
  }

} //  Action adapter
class TomogramCombinationActionAdapter implements ActionListener {

  TomogramCombinationDialog adaptee;
  public TomogramCombinationActionAdapter(TomogramCombinationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
