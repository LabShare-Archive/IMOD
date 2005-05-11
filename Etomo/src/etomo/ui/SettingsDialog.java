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
  public static final String rcsid =
    "$Id$";

  // Font selection panel
  JPanel panelFontSelect = new JPanel();
  String[] fontFamilies =
    GraphicsEnvironment
      .getLocalGraphicsEnvironment()
      .getAvailableFontFamilyNames();
  JList listFontFamily = new JList(fontFamilies);
  JScrollPane scrollFontFamily = new JScrollPane(listFontFamily);
  LabeledTextField ltfFontSize = new LabeledTextField("Size:");
  /*  String[] fontStyles = { "Plain", "Bold", "Italic" };
    JList listFontStyle = new JList(fontStyles);
    JScrollPane scrollFontStyle = new JScrollPane(listFontStyle);
  */
  JPanel panelSettings = new JPanel();
  LabeledTextField ltfTooltipsInitialDelay =
    new LabeledTextField("Tooltips initial delay");
  LabeledTextField ltfTooltipsDismissDelay =
    new LabeledTextField("Tooltips dismiss delay");
  JCheckBox cbNativeLAF = new JCheckBox("Native look & feel");
  JCheckBox cbAdvancedDialogs = new JCheckBox("Always use advanced dialogs");
  JCheckBox cbAutoFit = new JCheckBox("Auto-fit");

  JPanel panelButtons = new JPanel();
  JButton buttonCancel = new JButton("Cancel");
  JButton buttonApply = new JButton("Apply");
  JButton buttonDone = new JButton("Done");

  public SettingsDialog() {
    panelSettings = (JPanel) getContentPane();
    panelSettings.setLayout(new BoxLayout(panelSettings, BoxLayout.Y_AXIS));

    setTitle("eTomo Settings");

    //  Layout the font panel
    //panelFontSelect.setLayout(new )
    panelFontSelect.add(new JLabel("Font family:"));
    listFontFamily.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listFontFamily.setVisibleRowCount(3);
    ltfFontSize.setColumns(3);
    panelFontSelect.add(scrollFontFamily);
    panelFontSelect.add(ltfFontSize.getContainer());
    /*    panelFontSelect.add(new JLabel("Style:"));
        listFontStyle.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        listFontStyle.setVisibleRowCount(3);
        panelFontSelect.add(scrollFontStyle);
    */
    panelSettings.add(panelFontSelect);
    panelSettings.add(ltfTooltipsInitialDelay.getContainer());
    panelSettings.add(ltfTooltipsDismissDelay.getContainer());
    panelSettings.add(cbAutoFit);
    //TEMP bug# 614
    cbAutoFit.setEnabled(false);
    //TEMP
    panelSettings.add(cbNativeLAF);
    panelSettings.add(cbAdvancedDialogs);
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
    //TEMP bug# 614
    userConfig.setAutoFit(true);
    //TEMP
    cbAutoFit.setSelected(userConfig.isAutoFit());
    cbNativeLAF.setSelected(userConfig.getNativeLookAndFeel());
    cbAdvancedDialogs.setSelected(userConfig.getAdvancedDialogs());

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
    userConfig.setFontSize(Integer.parseInt(ltfFontSize.getText()));
    userConfig.setFontFamily(fontFamilies[listFontFamily.getSelectedIndex()]);
  }

  void buttonCancelAction() {
    EtomoDirector.getInstance().closeSettingsDialog();
  }

  void buttonApplyAction() {
    EtomoDirector.getInstance().getSettingsParameters();
  }

  void buttonDoneAction() {
    EtomoDirector.getInstance().getSettingsParameters();
    EtomoDirector.getInstance().closeSettingsDialog();
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
