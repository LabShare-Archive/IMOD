package etomo.ui;

import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.UserConfiguration;

/**
 * @author rickg
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class SettingsDialog extends JDialog {
  public static final String rcsid = "$Id:";

  ApplicationManager applicationManager;
  JPanel panelSettings = new JPanel();
  LabeledTextField ltfTooltipsInitialDelay =
    new LabeledTextField("Tooltips initial delay");
  LabeledTextField ltfTooltipsDismissDelay =
    new LabeledTextField("Tooltips dismiss delay");
  JCheckBox chkNativeLAF = new JCheckBox("Native look & feel");
  JCheckBox chkAdvancedDialogs = new JCheckBox("Always use advanced dialogs");

  JPanel panelButtons = new JPanel();
  JButton buttonCancel = new JButton("Cancel");
  JButton buttonApply = new JButton("Apply");
  JButton buttonDone = new JButton("Done");

  public SettingsDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;

    panelSettings = (JPanel) getContentPane();
    panelSettings.setLayout(new BoxLayout(panelSettings, BoxLayout.Y_AXIS));

    setTitle("eTomo Settings");

    panelSettings.add(ltfTooltipsInitialDelay.getContainer());
    panelSettings.add(ltfTooltipsDismissDelay.getContainer());
    panelSettings.add(chkNativeLAF);
    panelSettings.add(chkAdvancedDialogs);
    panelSettings.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the buttons and lay them out
    buttonCancel.addActionListener(new buttonSettingsCancelListener(this));
    buttonApply.addActionListener(new buttonSettingsApplyListener(this));
    buttonDone.addActionListener(new buttonSettingsDoneListener(this));
    panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.X_AXIS));
    panelButtons.add(buttonCancel);
    panelButtons.add(buttonApply);
    panelButtons.add(buttonDone);

    panelSettings.add(panelButtons);
    panelSettings.add(Box.createRigidArea(FixedDim.x0_y10));

    pack();
  }

  public void setParameters(UserConfiguration userConfig) {
    //  Convert the tooltips times to seconds
    ltfTooltipsInitialDelay.setText(
      userConfig.getToolTipsInitialDelay() / 1000);
    ltfTooltipsDismissDelay.setText(
      userConfig.getToolTipsDismissDelay() / 1000);
    chkNativeLAF.setSelected(userConfig.getNativeLookAndFeel());
    chkAdvancedDialogs.setSelected(userConfig.getAdvancedDialogs());
  }

  public void getParameters(UserConfiguration userConfig) {
    //  Convert the tooltips times to milliseconds
    float delay = Float.parseFloat(ltfTooltipsInitialDelay.getText());
    userConfig.setToolTipsInitialDelay((int) (delay * 1000));

    delay = Float.parseFloat(ltfTooltipsDismissDelay.getText());
    userConfig.setToolTipsDismissDelay((int) (delay * 1000));
    userConfig.setNativeLookAndFeel(chkNativeLAF.isSelected());
    userConfig.setAdvancedDialogs(chkAdvancedDialogs.isSelected());
  }

  void buttonCancelAction() {
    applicationManager.closeSettingsDialog();
  }

  void buttonApplyAction() {
    applicationManager.getSettingsParameters();
  }

  void buttonDoneAction() {
    applicationManager.getSettingsParameters();    
    applicationManager.closeSettingsDialog();
  }
}

/**
 *  Listeners to bind the buttons their action functions
 */
class buttonSettingsCancelListener implements ActionListener {

  SettingsDialog adaptee;

  buttonSettingsCancelListener(SettingsDialog adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonCancelAction();
  }
}

class buttonSettingsApplyListener implements ActionListener {

  SettingsDialog adaptee;

  buttonSettingsApplyListener(SettingsDialog adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonApplyAction();
  }
}

class buttonSettingsDoneListener implements ActionListener {

  SettingsDialog adaptee;

  buttonSettingsDoneListener(SettingsDialog adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonDoneAction();
  }
}
