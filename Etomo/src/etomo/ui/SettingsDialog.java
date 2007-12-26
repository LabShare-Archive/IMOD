package etomo.ui;

import java.awt.GraphicsEnvironment;
import java.awt.event.*;
import javax.swing.*;

import etomo.EtomoDirector;
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
  public static final String rcsid = "$Id$";

  // Font selection panel
  JPanel panelFontSelect = new JPanel();
  String[] fontFamilies = GraphicsEnvironment.getLocalGraphicsEnvironment()
      .getAvailableFontFamilyNames();
  JList listFontFamily = new JList(fontFamilies);
  JScrollPane scrollFontFamily = new JScrollPane(listFontFamily);
  LabeledTextField ltfFontSize = new LabeledTextField("Size:");
  JPanel panelSettings = new JPanel();
  LabeledTextField ltfTooltipsInitialDelay = new LabeledTextField(
      "Tooltips initial delay");
  LabeledTextField ltfTooltipsDismissDelay = new LabeledTextField(
      "Tooltips dismiss delay");
  CheckBox cbNativeLAF = new CheckBox("Native look & feel");
  CheckBox cbAdvancedDialogs = new CheckBox("Always use advanced dialogs");
  CheckBox cbAutoFit = new CheckBox("Auto-fit");
  CheckBox cbCompactDisplay = new CheckBox("Compact Display");

  JPanel panelButtons = new JPanel();
  JButton buttonCancel = new JButton("Cancel");
  JButton buttonApply = new JButton("Apply");
  JButton buttonDone = new JButton("Done");
  CheckBox cbSingleAxis = new CheckBox(SetupDialog.AXIS_TYPE_LABEL + ":  "
      + SetupDialog.SINGLE_AXIS_LABEL);
  CheckBox cbNoParallelProcessing = new CheckBox("Start with "
      + ParallelPanel.FIELD_LABEL + " off");
  CheckBox cbTiltAnglesRawtltFile = new CheckBox("Angle Source:  "
      + TiltAnglePanel.EXISTING_RAWTILT_FILE);
  CheckBox cbSwapYAndZ = new CheckBox(TrimvolPanel.REORIENTATION_GROUP_LABEL
      + "  " + TrimvolPanel.SWAP_YZ_LABEL);

  public SettingsDialog() {
    panelSettings = (JPanel) getContentPane();
    panelSettings.setLayout(new BoxLayout(panelSettings, BoxLayout.Y_AXIS));

    setTitle("eTomo Settings");

    //  Layout the font panel
    panelFontSelect.add(new JLabel("Font family:"));
    listFontFamily.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listFontFamily.setVisibleRowCount(3);
    ltfFontSize.setColumns(3);
    panelFontSelect.add(scrollFontFamily);
    panelFontSelect.add(ltfFontSize.getContainer());
    panelSettings.add(panelFontSelect);
    panelSettings.add(ltfTooltipsInitialDelay.getContainer());
    panelSettings.add(ltfTooltipsDismissDelay.getContainer());
    panelSettings.add(cbAutoFit);
    //TEMP bug# 614
    cbAutoFit.setEnabled(false);
    //TEMP
    panelSettings.add(cbNativeLAF);
    panelSettings.add(cbAdvancedDialogs);
    panelSettings.add(cbCompactDisplay);
    panelSettings.add(Box.createRigidArea(FixedDim.x0_y10));
    JPanel panelDefaults = new JPanel();
    panelDefaults.setLayout(new BoxLayout(panelDefaults, BoxLayout.Y_AXIS));
    panelDefaults.setBorder(new EtchedBorder("Defaults").getBorder());
    panelDefaults.add(cbSingleAxis);
    panelDefaults.add(cbNoParallelProcessing);
    panelDefaults.add(cbTiltAnglesRawtltFile);
    panelDefaults.add(cbSwapYAndZ);
    panelSettings.add(panelDefaults);
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
    ltfTooltipsInitialDelay
        .setText(userConfig.getToolTipsInitialDelay() / 1000);
    ltfTooltipsDismissDelay
        .setText(userConfig.getToolTipsDismissDelay() / 1000);
    cbAutoFit.setSelected(userConfig.isAutoFit());
    cbNativeLAF.setSelected(userConfig.getNativeLookAndFeel());
    cbAdvancedDialogs.setSelected(userConfig.getAdvancedDialogs());
    cbCompactDisplay.setSelected(userConfig.getCompactDisplay());
    cbSingleAxis.setSelected(userConfig.getSingleAxis());
    cbNoParallelProcessing.setSelected(userConfig.getNoParallelProcessing());
    cbTiltAnglesRawtltFile.setSelected(userConfig.getTiltAnglesRawtltFile());
    cbSwapYAndZ.setSelected(userConfig.getSwapYAndZ());

    // Get the current font parameters to set the UI
    // Since they may not be all the same make the assumption that the first
    // object contains the font 
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    String currentFontFamily = "";
    int currentFontSize = 0;
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      if (value instanceof javax.swing.plaf.FontUIResource) {
        currentFontFamily = UIManager.getFont(key).getFamily();
        currentFontSize = UIManager.getFont(key).getSize();
        break;
      }
    }

    // Find the font family index from the fontFamilies
    for (int i = 0; i < fontFamilies.length; i++) {
      if (fontFamilies[i].compareToIgnoreCase(currentFontFamily) == 0) {
        listFontFamily.setSelectedIndex(i);
      }
    }
    ltfFontSize.setText(currentFontSize);
  }

  public void getParameters(UserConfiguration userConfig) {
    //  Convert the tooltips times to milliseconds
    float delay = Float.parseFloat(ltfTooltipsInitialDelay.getText());
    userConfig.setToolTipsInitialDelay((int) (delay * 1000));

    delay = Float.parseFloat(ltfTooltipsDismissDelay.getText());
    userConfig.setToolTipsDismissDelay((int) (delay * 1000));
    userConfig.setAutoFit(cbAutoFit.isSelected());
    userConfig.setNativeLookAndFeel(cbNativeLAF.isSelected());
    userConfig.setAdvancedDialogs(cbAdvancedDialogs.isSelected());
    userConfig.setCompactDisplay(cbCompactDisplay.isSelected());
    userConfig.setFontSize(Integer.parseInt(ltfFontSize.getText()));
    userConfig.setFontFamily(fontFamilies[listFontFamily.getSelectedIndex()]);
    userConfig.setSingleAxis(cbSingleAxis.isSelected());
    userConfig.setNoParallelProcessing(cbNoParallelProcessing.isSelected());
    userConfig.setTiltAnglesRawtltFile(cbTiltAnglesRawtltFile.isSelected());
    userConfig.setSwapYAndZ(cbSwapYAndZ.isSelected());
  }

  public boolean isAppearanceSettingChanged(UserConfiguration userConfig) {
    if (userConfig.getNativeLookAndFeel() != cbNativeLAF.isSelected()
        || userConfig.getCompactDisplay() != cbCompactDisplay.isSelected()
        || userConfig.getSingleAxis() != cbSingleAxis.isSelected()
        || userConfig.getNoParallelProcessing() != cbNoParallelProcessing
            .isSelected()
        || userConfig.getTiltAnglesRawtltFile() != cbTiltAnglesRawtltFile
            .isSelected()
        || userConfig.getSwapYAndZ() != cbSwapYAndZ.isSelected()
        || userConfig.getFontSize() != Integer.parseInt(ltfFontSize.getText())
        || !userConfig.getFontFamily().equals(
            fontFamilies[listFontFamily.getSelectedIndex()])) {
      return true;
    }
    return false;
  }

  void buttonCancelAction() {
    EtomoDirector.INSTANCE.closeSettingsDialog();
  }

  void buttonApplyAction() {
    EtomoDirector.INSTANCE.getSettingsParameters();
  }

  void buttonDoneAction() {
    EtomoDirector.INSTANCE.getSettingsParameters();
    EtomoDirector.INSTANCE.saveSettingsDialog();
    EtomoDirector.INSTANCE.closeSettingsDialog();
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
