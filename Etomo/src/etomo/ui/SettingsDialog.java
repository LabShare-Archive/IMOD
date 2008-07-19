package etomo.ui;

import java.awt.GraphicsEnvironment;
import java.awt.event.*;
import javax.swing.*;

import etomo.EtomoDirector;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.UserConfiguration;

/**
 * @author rickg
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public final class SettingsDialog extends JDialog {
  public static final String rcsid = "$Id$";

  // Font selection panel
  private final String[] fontFamilies = GraphicsEnvironment
      .getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
  private final JList listFontFamily = new JList(fontFamilies);
  private final LabeledTextField ltfFontSize = new LabeledTextField("Size: ");
  private final LabeledTextField ltfTooltipsInitialDelay = new LabeledTextField(
      "Tooltips initial delay: ");
  private final LabeledTextField ltfTooltipsDismissDelay = new LabeledTextField(
      "Tooltips dismiss delay: ");
  private final CheckBox cbNativeLAF = new CheckBox("Native look & feel");
  private final CheckBox cbAdvancedDialogs = new CheckBox(
      "Always use advanced dialogs");
  private final CheckBox cbAutoFit = new CheckBox("Auto-fit");
  private final CheckBox cbCompactDisplay = new CheckBox("Compact Display");
  private final JButton buttonCancel = new JButton("Cancel");
  private final JButton buttonApply = new JButton("Apply");
  private final JButton buttonDone = new JButton("Done");
  private final CheckBox cbParallelProcessing = new CheckBox("Enable "
      + ParallelPanel.FIELD_LABEL);
  private final LabeledTextField ltfCpus = new LabeledTextField(
      ProcessorTable.NUMBER_CPUS_HEADER + ": ");
  private final CheckBox cbSingleAxis = new CheckBox(
      SetupDialog.AXIS_TYPE_LABEL + ":  " + SetupDialog.SINGLE_AXIS_LABEL);
  private final CheckBox cbNoParallelProcessing = new CheckBox("Start with "
      + ParallelPanel.FIELD_LABEL + " off");
  private final CheckBox cbTiltAnglesRawtltFile = new CheckBox(
      "Angle Source:  " + TiltAnglePanel.EXISTING_RAWTILT_FILE);
  private final CheckBox cbSwapYAndZ = new CheckBox(
      TrimvolPanel.REORIENTATION_GROUP_LABEL + "  "
          + TrimvolPanel.SWAP_YZ_LABEL);

  private SettingsDialog() {
  }

  public static SettingsDialog getInstance(String propertyUserDir) {
    SettingsDialog instance = new SettingsDialog();
    instance.buildDialog();
    instance.loadData(propertyUserDir);
    instance.addListeners();
    return instance;
  }

  private void buildDialog() {
    setTitle("eTomo Settings");
    SpacedPanel pnlMain = new SpacedPanel();
    pnlMain.setBoxLayout(BoxLayout.Y_AXIS);
    pnlMain.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    ((JPanel) getContentPane()).add(pnlMain.getComponent());
    //  Layout the font panel
    SpacedPanel panelFontSelect = new SpacedPanel();
    panelFontSelect.setBoxLayout(BoxLayout.X_AXIS);
    panelFontSelect.add(new JLabel("Font family:"));
    listFontFamily.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listFontFamily.setVisibleRowCount(3);
    ltfFontSize.setColumns(3);
    JScrollPane scrollFontFamily = new JScrollPane(listFontFamily);
    panelFontSelect.add(scrollFontFamily);
    panelFontSelect.add(ltfFontSize.getContainer());
    pnlMain.add(panelFontSelect);
    pnlMain.add(ltfTooltipsInitialDelay.getContainer());
    pnlMain.add(ltfTooltipsDismissDelay.getContainer());
    //Settings
    SpacedPanel pnlSettings = new SpacedPanel();
    pnlSettings.setBoxLayout(BoxLayout.X_AXIS);
    pnlSettings.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    pnlMain.add(pnlSettings);
    JPanel pnlGeneralSettings = new JPanel();
    pnlGeneralSettings.setLayout(new BoxLayout(pnlGeneralSettings,
        BoxLayout.Y_AXIS));
    pnlSettings.add(pnlGeneralSettings);
    pnlGeneralSettings.add(cbAutoFit);
    //TEMP bug# 614
    cbAutoFit.setEnabled(false);
    //TEMP
    pnlGeneralSettings.add(cbNativeLAF);
    pnlGeneralSettings.add(cbAdvancedDialogs);
    pnlGeneralSettings.add(cbCompactDisplay);
    //parallel processing settings
    JPanel pnlParallelProcessing = new JPanel();
    pnlParallelProcessing.setLayout(new BoxLayout(pnlParallelProcessing,
        BoxLayout.Y_AXIS));
    pnlParallelProcessing.setBorder(new EtchedBorder(
        "User Level Parallel Processing").getBorder());
    pnlSettings.add(pnlParallelProcessing);
    JPanel pnlCheckBoxParallelProcessing = new JPanel();
    pnlCheckBoxParallelProcessing.setLayout(new BoxLayout(
        pnlCheckBoxParallelProcessing, BoxLayout.X_AXIS));
    pnlCheckBoxParallelProcessing.add(Box.createHorizontalGlue());
    pnlCheckBoxParallelProcessing.add(cbParallelProcessing);
    pnlParallelProcessing.add(pnlCheckBoxParallelProcessing);
    pnlParallelProcessing.add(ltfCpus.getContainer());
    //defaults settings
    SpacedPanel panelDefaults = new SpacedPanel();
    panelDefaults.setBoxLayout(BoxLayout.Y_AXIS);
    panelDefaults.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    panelDefaults.setBorder(new EtchedBorder("Defaults").getBorder());
    panelDefaults.add(cbSingleAxis);
    panelDefaults.add(cbNoParallelProcessing);
    panelDefaults.add(cbTiltAnglesRawtltFile);
    panelDefaults.add(cbSwapYAndZ);
    pnlMain.add(panelDefaults);
    //buttons
    SpacedPanel panelButtons = new SpacedPanel();
    panelButtons.setBoxLayout(BoxLayout.X_AXIS);
    panelButtons.setAlignmentX(Box.CENTER_ALIGNMENT);
    panelButtons.add(buttonCancel);
    panelButtons.add(buttonApply);
    panelButtons.add(buttonDone);
    pnlMain.add(panelButtons);
    pack();
  }

  /**
   * Get data from CpuAdoc.
   * @param propertyUserDir
   */
  private void loadData(String propertyUserDir) {
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(AxisID.FIRST, propertyUserDir);
    //Disable parallel processing if it was enabled by a way that takes
    //precidence over this one (cpu.adoc or IMOD_PROCESSORS).
    cbParallelProcessing.setEnabled(!cpuAdoc.isFile() && !cpuAdoc.isEnvVar());
  }

  private void updateDisplay() {
    ltfCpus.setEnabled(cbParallelProcessing.isEnabled()
        && cbParallelProcessing.isSelected());
  }

  private void addListeners() {
    SettingsDialogListener listener = new SettingsDialogListener(this);
    buttonCancel.addActionListener(listener);
    buttonApply.addActionListener(listener);
    buttonDone.addActionListener(listener);
    cbParallelProcessing.addActionListener(listener);
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
    cbParallelProcessing.setSelected(userConfig.getParallelProcessing());
    ltfCpus.setText(userConfig.getCpus());

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
    updateDisplay();
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
    userConfig.setParallelProcessing(cbParallelProcessing.isSelected());
    userConfig.setCpus(ltfCpus.getText());
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
            fontFamilies[listFontFamily.getSelectedIndex()])
        || userConfig.getParallelProcessing() != cbParallelProcessing
            .isSelected()
        || !userConfig.getCpus().toString().equals(ltfCpus.getText())) {
      return true;
    }
    return false;
  }

  void action(final String command) {
    if (command.equals(buttonCancel.getActionCommand())) {
      EtomoDirector.INSTANCE.closeSettingsDialog();
    }
    else if (command.equals(buttonApply.getActionCommand())) {
      EtomoDirector.INSTANCE.getSettingsParameters();
    }
    else if (command.equals(buttonDone.getActionCommand())) {
      EtomoDirector.INSTANCE.getSettingsParameters();
      EtomoDirector.INSTANCE.saveSettingsDialog();
      EtomoDirector.INSTANCE.closeSettingsDialog();
    }
    else if (command.equals(cbParallelProcessing.getActionCommand())) {
      updateDisplay();
    }
  }

  private final class SettingsDialogListener implements ActionListener {
    private final SettingsDialog adaptee;

    private SettingsDialogListener(final SettingsDialog settingsDialog) {
      adaptee = settingsDialog;
    }

    public void actionPerformed(final ActionEvent actionEvent) {
      adaptee.action(actionEvent.getActionCommand());
    }
  }
}