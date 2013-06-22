package etomo.ui.swing;

import java.awt.GraphicsEnvironment;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;

import javax.swing.*;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.logic.ConfigTool;
import etomo.storage.Network;
import etomo.type.AxisID;
import etomo.type.UserConfiguration;
import etomo.ui.FieldType;

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
  private final FontFamilies fontFamilies = new FontFamilies();
  private final JList listFontFamily = new JList(fontFamilies.getFontFamilies());
  private final LabeledTextField ltfFontSize = new LabeledTextField(FieldType.INTEGER,
      "Size: ");
  private final LabeledTextField ltfTooltipsInitialDelay = new LabeledTextField(
      FieldType.FLOATING_POINT, "Tooltips initial delay: ");
  private final LabeledTextField ltfTooltipsDismissDelay = new LabeledTextField(
      FieldType.FLOATING_POINT, "Tooltips dismiss delay: ");
  private final CheckBox cbNativeLAF = new CheckBox("Native look & feel");
  private final CheckBox cbAdvancedDialogs = new CheckBox("Always use advanced dialogs");
  private final CheckBox cbAutoFit = new CheckBox("Auto-fit");
  private final CheckBox cbCompactDisplay = new CheckBox("Compact Display");
  private final JButton buttonCancel = new JButton("Cancel");
  private final JButton buttonApply = new JButton("Apply");
  private final JButton buttonDone = new JButton("Done");
  private final CheckBox cbParallelProcessing = new CheckBox("Enable "
      + ParallelPanel.FIELD_LABEL);
  private final CheckBox cbGpuProcessing = new CheckBox("Enable graphics processing");
  private final LabeledTextField ltfCpus = new LabeledTextField(FieldType.INTEGER,
      "# CPUs: ");
  private final CheckBox cbSingleAxis = new CheckBox(SetupDialog.AXIS_TYPE_LABEL + ":  "
      + SetupDialog.SINGLE_AXIS_LABEL);
  private final CheckBox cbMontage = new CheckBox(SetupDialog.FRAME_TYPE_LABEL + ":  "
      + SetupDialog.MONTAGE_LABEL);
  private final CheckBox cbNoParallelProcessing = new CheckBox("Start with "
      + ParallelPanel.FIELD_LABEL + " off");
  private final CheckBox cbGpuProcessingDefault = new CheckBox(
      "Start with graphics card processing on");
  private final CheckBox cbTiltAnglesRawtltFile = new CheckBox("Angle Source:  "
      + TiltAnglePanel.EXISTING_RAWTILT_FILE);
  private final CheckBox cbSwapYAndZ = new CheckBox(
      TrimvolPanel.REORIENTATION_GROUP_LABEL + "  " + TrimvolPanel.SWAP_YZ_LABEL);
  private final LabeledTextField ltfParallelTableSize = new LabeledTextField(
      FieldType.INTEGER, "Parallel table size: ");
  private final LabeledTextField ltfJoinTableSize = new LabeledTextField(
      FieldType.INTEGER, "Join tables size: ");
  private final LabeledTextField ltfPeetTableSize = new LabeledTextField(
      FieldType.INTEGER, "PEET table size: ");
  private final CheckBox cbSetFEIPixelSize = new CheckBox(
      "Set pixel size in files from FEI");
  private final FileTextField2 ftfUserTemplateDir = FileTextField2.getInstance(null,
      "User templates directory: ");
  private final SettingsDialogListener listener = new SettingsDialogListener(this);

  private final TemplatePanel templatePanel;
  private final String propertyUserDir;

  private SettingsDialog(final BaseManager manager, final String propertyUserDir) {
    this.propertyUserDir = propertyUserDir;
    templatePanel = TemplatePanel.getInstance(manager, AxisID.ONLY, listener,
        "Default Templates", this);

  }

  public static SettingsDialog getInstance(final BaseManager manager,
      final String propertyUserDir) {
    SettingsDialog instance = new SettingsDialog(manager, propertyUserDir);
    instance.buildDialog();
    instance.loadData(manager, propertyUserDir);
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void buildDialog() {
    // init
    ftfUserTemplateDir.setAbsolutePath(true);
    ftfUserTemplateDir.setUseTextAsOriginDir(true);
    ftfUserTemplateDir.setFileSelectionMode(FileChooser.DIRECTORIES_ONLY);
    ftfUserTemplateDir.setFile(ConfigTool.getDefaultUserTemplateDir());
    ftfUserTemplateDir.setTurnOffFileHiding(true);
    setTitle("eTomo Settings");
    SpacedPanel pnlMain = SpacedPanel.getInstance();
    pnlMain.setBoxLayout(BoxLayout.Y_AXIS);
    ((JPanel) getContentPane()).add(pnlMain.getContainer());
    // Layout the font panel
    SpacedPanel panelFontSelect = SpacedPanel.getInstance();
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
    // Settings
    SpacedPanel pnlSettings = SpacedPanel.getInstance();
    pnlSettings.setBoxLayout(BoxLayout.X_AXIS);
    pnlSettings.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    pnlMain.add(pnlSettings);
    JPanel pnlGeneralSettings = new JPanel();
    pnlGeneralSettings.setLayout(new BoxLayout(pnlGeneralSettings, BoxLayout.Y_AXIS));
    pnlSettings.add(pnlGeneralSettings);
    pnlGeneralSettings.add(cbAutoFit);
    // TEMP bug# 614
    cbAutoFit.setEnabled(false);
    // TEMP
    pnlGeneralSettings.add(cbNativeLAF);
    pnlGeneralSettings.add(cbAdvancedDialogs);
    pnlGeneralSettings.add(cbCompactDisplay);
    // enhanced processing settings
    EtomoPanel pnlEnhancedProcessing = new EtomoPanel();
    pnlEnhancedProcessing
        .setLayout(new BoxLayout(pnlEnhancedProcessing, BoxLayout.Y_AXIS));
    pnlEnhancedProcessing.setBorder(new EtchedBorder("User Level Enhanced Processing")
        .getBorder());
    pnlSettings.add(pnlEnhancedProcessing);
    JPanel pnlCheckBoxParallelProcessing = new JPanel();
    pnlCheckBoxParallelProcessing.setLayout(new BoxLayout(pnlCheckBoxParallelProcessing,
        BoxLayout.X_AXIS));
    pnlCheckBoxParallelProcessing.add(Box.createHorizontalGlue());
    pnlCheckBoxParallelProcessing.add(cbParallelProcessing);
    pnlEnhancedProcessing.add(pnlCheckBoxParallelProcessing);
    pnlEnhancedProcessing.add(ltfCpus.getContainer());
    JPanel pnlGpuProcessing = new JPanel();
    pnlGpuProcessing.setLayout(new BoxLayout(pnlGpuProcessing, BoxLayout.X_AXIS));
    pnlGpuProcessing.add(cbGpuProcessing);
    pnlGpuProcessing.add(Box.createHorizontalGlue());
    pnlEnhancedProcessing.add(pnlGpuProcessing);
    // default settings
    SpacedPanel panelDefaults = SpacedPanel.getInstance();
    panelDefaults.setBoxLayout(BoxLayout.Y_AXIS);
    panelDefaults.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    panelDefaults.setBorder(new EtchedBorder("Defaults").getBorder());
    panelDefaults.add(cbSingleAxis);
    panelDefaults.add(cbMontage);
    panelDefaults.add(cbNoParallelProcessing);
    panelDefaults.add(cbGpuProcessingDefault);
    panelDefaults.add(cbTiltAnglesRawtltFile);
    panelDefaults.add(cbSwapYAndZ);
    panelDefaults.add(cbSetFEIPixelSize);
    pnlMain.add(panelDefaults.getContainer());
    // table settings
    EtomoPanel pnlTableSize = new EtomoPanel();
    pnlTableSize.setLayout(new BoxLayout(pnlTableSize, BoxLayout.Y_AXIS));
    pnlTableSize.setBorder(new EtchedBorder("Table Sizes").getBorder());
    pnlTableSize.add(ltfParallelTableSize.getContainer());
    pnlTableSize.add(ltfJoinTableSize.getContainer());
    pnlTableSize.add(ltfPeetTableSize.getContainer());
    pnlMain.add(pnlTableSize);
    pnlMain.add(ftfUserTemplateDir.getRootPanel());
    pnlMain.add(templatePanel.getComponent());
    // buttons
    SpacedPanel panelButtons = SpacedPanel.getInstance();
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
  private void loadData(final BaseManager manager, final String propertyUserDir) {
    // Disable parallel processing checkbox if it was enabled by a method that
    // takes precidence over this one (cpu.adoc or IMOD_PROCESSORS).
    cbParallelProcessing.setEnabled(!Network.isParallelProcessingSetExternally(manager,
        AxisID.ONLY, propertyUserDir));
    // Disable GPU processing checkbox if it was enabled by a method that takes
    // precidence over this one (cpu.adoc).
    cbGpuProcessing.setEnabled(!Network.isGpuProcessingSetExternally(manager,
        AxisID.ONLY, propertyUserDir));
  }

  private void updateDisplay() {
    ltfCpus.setEnabled(cbParallelProcessing.isEnabled()
        && cbParallelProcessing.isSelected());
  }

  private void addListeners() {
    buttonCancel.addActionListener(listener);
    buttonApply.addActionListener(listener);
    buttonDone.addActionListener(listener);
    cbParallelProcessing.addActionListener(listener);
  }

  public void setParameters(final UserConfiguration userConfig) {
    // Convert the tooltips times to seconds
    ltfTooltipsInitialDelay.setText(userConfig.getToolTipsInitialDelay() / 1000);
    ltfTooltipsDismissDelay.setText(userConfig.getToolTipsDismissDelay() / 1000);
    cbAutoFit.setSelected(userConfig.isAutoFit());
    cbNativeLAF.setSelected(userConfig.getNativeLookAndFeel());
    cbAdvancedDialogs.setSelected(userConfig.getAdvancedDialogs());
    cbCompactDisplay.setSelected(userConfig.getCompactDisplay());
    cbSingleAxis.setSelected(userConfig.getSingleAxis());
    cbMontage.setSelected(userConfig.getMontage());
    cbNoParallelProcessing.setSelected(userConfig.getNoParallelProcessing());
    cbGpuProcessingDefault.setSelected(userConfig.getGpuProcessingDefault());
    cbTiltAnglesRawtltFile.setSelected(userConfig.getTiltAnglesRawtltFile());
    cbSwapYAndZ.setSelected(userConfig.getSwapYAndZ());
    cbSetFEIPixelSize.setSelected(userConfig.isSetFEIPixelSize());
    cbParallelProcessing.setSelected(userConfig.isParallelProcessing());
    cbGpuProcessing.setSelected(userConfig.isGpuProcessing());
    ltfCpus.setText(userConfig.getCpus());
    ltfParallelTableSize.setText(userConfig.getParallelTableSize());
    ltfJoinTableSize.setText(userConfig.getJoinTableSize());
    ltfPeetTableSize.setText(userConfig.getPeetTableSize());
    String dir = userConfig.getUserTemplateDir();
    if (dir != null && !dir.matches("\\s*")) {
      ftfUserTemplateDir.setText(userConfig.getUserTemplateDir());
    }
    templatePanel.setParameters(userConfig);

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

    listFontFamily.setSelectedIndex(fontFamilies.getIndex(currentFontFamily));
    ltfFontSize.setText(currentFontSize);
    updateDisplay();
  }

  boolean equalsUserTemplateDir(final File input) {
    File userTemplateDir = ftfUserTemplateDir.getFile();
    if (userTemplateDir == null && input == null) {
      return true;
    }
    if (userTemplateDir == null) {
      return false;
    }
    return userTemplateDir.equals(input);
  }

  File getUserTemplateDir() {
    return ftfUserTemplateDir.getFile();
  }

  public void getParameters(final UserConfiguration userConfig) {
    // Convert the tooltips times to milliseconds
    double delay = Double.parseDouble(ltfTooltipsInitialDelay.getText());
    userConfig.setToolTipsInitialDelay((int) (delay * 1000));

    delay = Double.parseDouble(ltfTooltipsDismissDelay.getText());
    userConfig.setToolTipsDismissDelay((int) (delay * 1000));
    userConfig.setAutoFit(cbAutoFit.isSelected());
    userConfig.setNativeLookAndFeel(cbNativeLAF.isSelected());
    userConfig.setAdvancedDialogs(cbAdvancedDialogs.isSelected());
    userConfig.setCompactDisplay(cbCompactDisplay.isSelected());
    userConfig.setFontSize(Integer.parseInt(ltfFontSize.getText()));
    userConfig.setFontFamily(fontFamilies.getName(listFontFamily.getSelectedIndex()));
    userConfig.setSingleAxis(cbSingleAxis.isSelected());
    userConfig.setMontage(cbMontage.isSelected());
    userConfig.setNoParallelProcessing(cbNoParallelProcessing.isSelected());
    userConfig.setGpuProcessingDefault(cbGpuProcessingDefault.isSelected());
    userConfig.setTiltAnglesRawtltFile(cbTiltAnglesRawtltFile.isSelected());
    userConfig.setSwapYAndZ(cbSwapYAndZ.isSelected());
    userConfig.setSetFEIPixelSize(cbSetFEIPixelSize.isSelected());
    userConfig.setParallelProcessing(cbParallelProcessing.isSelected());
    userConfig.setGpuProcessing(cbGpuProcessing.isSelected());
    userConfig.setCpus(ltfCpus.getText());
    userConfig.setParallelTableSize(ltfParallelTableSize.getText());
    userConfig.setJoinTableSize(ltfJoinTableSize.getText());
    userConfig.setPeetTableSize(ltfPeetTableSize.getText());
    userConfig.setUserTemplateDir(ftfUserTemplateDir.getFile());
    templatePanel.getParameters(userConfig);
  }

  public boolean isAppearanceSettingChanged(final UserConfiguration userConfig) {
    if (templatePanel.isAppearanceSettingChanged(userConfig)) {
      return true;
    }
    if (userConfig.getNativeLookAndFeel() != cbNativeLAF.isSelected()
        || userConfig.getCompactDisplay() != cbCompactDisplay.isSelected()
        || userConfig.getSingleAxis() != cbSingleAxis.isSelected()
        || userConfig.getMontage() != cbMontage.isSelected()
        || userConfig.getNoParallelProcessing() != cbNoParallelProcessing.isSelected()
        || userConfig.getGpuProcessingDefault() != cbGpuProcessingDefault.isSelected()
        || userConfig.getTiltAnglesRawtltFile() != cbTiltAnglesRawtltFile.isSelected()
        || userConfig.getSwapYAndZ() != cbSwapYAndZ.isSelected()
        || userConfig.isSetFEIPixelSize() != cbSetFEIPixelSize.isSelected()
        || userConfig.getFontSize() != Integer.parseInt(ltfFontSize.getText())
        || !userConfig.getFontFamily().equals(
            fontFamilies.getName(listFontFamily.getSelectedIndex()))
        || userConfig.isParallelProcessing() != cbParallelProcessing.isSelected()
        || userConfig.isGpuProcessing() != cbGpuProcessing.isSelected()
        || !userConfig.getCpus().toString().equals(ltfCpus.getText())
        || !userConfig.getParallelTableSize().toString()
            .equals(ltfParallelTableSize.getText())
        || !userConfig.getJoinTableSize().toString().equals(ltfJoinTableSize.getText())
        || !userConfig.getPeetTableSize().toString().equals(ltfPeetTableSize.getText())) {
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

  void setTooltips() {
    cbSetFEIPixelSize.setToolTipText(TooltipFormatter.INSTANCE
        .format("During tomogram setup, transfer pixel size from extended header to "
            + "pixel."));
  }

  private static final class SettingsDialogListener implements TemplateActionListener {
    private final SettingsDialog adaptee;

    private SettingsDialogListener(final SettingsDialog settingsDialog) {
      adaptee = settingsDialog;
    }

    public void actionPerformed(final ActionEvent actionEvent) {
      adaptee.action(actionEvent.getActionCommand());
    }
  }

  /**
   * Gets the available font family name list, which may contain "'" (a
   * character that caused a nasty Java look and feel failure).  Put the usable
   * font families (ones that don't contain "'") into the usable font families
   * list.
   * @author sueh
   */
  private static final class FontFamilies {
    private final Vector usable = new Vector();

    private int defaultIndex = -1;

    /**
     * Creates the usable font family list from the available font family list.
     * Does not use fonts with "'" in their name.  Sets the default font family
     * to "Dialog".
     */
    private FontFamilies() {
      String[] available = GraphicsEnvironment.getLocalGraphicsEnvironment()
          .getAvailableFontFamilyNames();
      for (int i = 0; i < available.length; i++) {
        if (available[i].indexOf("'") == -1) {
          usable.add(available[i]);
          if (available[i].compareToIgnoreCase("dialog") == 0) {
            defaultIndex = i;
          }
        }
        else {
          System.err.println("Removing unusable font family:" + available[i]);
        }
      }
    }

    /**
     * Get the usable font families.
     * @return
     */
    private Vector getFontFamilies() {
      return usable;
    }

    /**
     * Gets an index to fontFamilyName in the usable font family list.  If not
     * found, returns the default font index.
     * @return
     */
    private int getIndex(final String fontFamilyName) {
      // Find the font family index from the available fontFamilies
      for (int i = 0; i < usable.size(); i++) {
        if (((String) usable.get(i)).compareToIgnoreCase(fontFamilyName) == 0) {
          return i;
        }
      }
      return defaultIndex;
    }

    private String getName(final int i) {
      return (String) usable.get(i);
    }
  }
}