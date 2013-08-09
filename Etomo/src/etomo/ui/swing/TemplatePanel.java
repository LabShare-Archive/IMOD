package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.logic.ConfigTool;
import etomo.storage.DirectiveFileCollection;
import etomo.type.AxisID;
import etomo.type.DirectiveFileType;
import etomo.type.UserConfiguration;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
final class TemplatePanel {
  public static final String rcsid = "$Id:$";

  private static String EMPTY_OPTION = "None available";
  private static String SELECT_OPTION1 = "No selection (";
  private static final String SELECT_OPTION2 = " available)";

  private final JPanel pnlRoot = new JPanel();
  private final ComboBox cmbScopeTemplate = ComboBox
      .getInstance("Scope template:", false);
  private final ComboBox cmbSystemTemplate = ComboBox.getInstance("System template:",
      false);
  private final ComboBox cmbUserTemplate = ComboBox.getInstance("User template:", false);

  private final TemplateActionListener listener;
  private final BaseManager manager;
  private final AxisID axisID;
  final SettingsDialog settings;
  private final DirectiveFileCollection directiveFileCollection;

  private File[] scopeTemplateFileList = null;
  private File[] systemTemplateFileList = null;
  private File[] userTemplateFileList = null;
  private File newUserTemplateDir = null;

  private TemplatePanel(final BaseManager manager, final AxisID axisID,
      final TemplateActionListener listener, final SettingsDialog settings) {
    this.listener = listener;
    this.manager = manager;
    this.axisID = axisID;
    this.settings = settings;
    directiveFileCollection = new DirectiveFileCollection(manager, axisID);
  }

  static TemplatePanel getInstance(final BaseManager manager, final AxisID axisID,
      final TemplateActionListener listener, final String title,
      final SettingsDialog settings) {
    TemplatePanel instance = new TemplatePanel(manager, axisID, listener, settings);
    instance.createPanel(title);
    instance.addListeners();
    return instance;
  }

  private void createPanel(final String title) {
    // init
    scopeTemplateFileList = ConfigTool.getScopeTemplateFiles();
    if (scopeTemplateFileList != null && scopeTemplateFileList.length > 0) {
      cmbScopeTemplate.addItem(SELECT_OPTION1 + scopeTemplateFileList.length
          + SELECT_OPTION2);
      for (int i = 0; i < scopeTemplateFileList.length; i++) {
        cmbScopeTemplate.addItem(scopeTemplateFileList[i].getName());
      }
    }
    else {
      cmbScopeTemplate.addItem(EMPTY_OPTION);
      cmbScopeTemplate.setComboBoxEnabled(false);
    }
    cmbScopeTemplate.setSelectedIndex(0);
    systemTemplateFileList = ConfigTool.getSystemTemplateFiles();
    if (systemTemplateFileList != null && systemTemplateFileList.length > 0) {
      cmbSystemTemplate.addItem(SELECT_OPTION1 + systemTemplateFileList.length
          + SELECT_OPTION2);
      for (int i = 0; i < systemTemplateFileList.length; i++) {
        cmbSystemTemplate.addItem(systemTemplateFileList[i].getName());
      }
    }
    else {
      cmbSystemTemplate.addItem(EMPTY_OPTION);
      cmbSystemTemplate.setComboBoxEnabled(false);
    }
    cmbSystemTemplate.setSelectedIndex(0);
    loadUserTemplate();
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    if (title != null) {
      pnlRoot.setBorder(new EtchedBorder(title).getBorder());
    }
    else {
      pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    }
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y2));
    pnlRoot.add(cmbScopeTemplate.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlRoot.add(cmbSystemTemplate.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlRoot.add(cmbUserTemplate.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y2));
  }

  private void addListeners() {
    cmbScopeTemplate.addActionListener(listener);
    cmbSystemTemplate.addActionListener(listener);
    cmbUserTemplate.addActionListener(listener);
    cmbUserTemplate.addFocusListener(new TemplateFocusListener(this));
  }

  Component getComponent() {
    return pnlRoot;
  }

  private void loadUserTemplate() {
    userTemplateFileList = null;
    cmbUserTemplate.removeAllItems();
    // If the user template directory is in a different directory from the location of the
    // default user template, the user template will not be loaded.
    userTemplateFileList = ConfigTool.getUserTemplateFiles(newUserTemplateDir);
    if (userTemplateFileList != null && userTemplateFileList.length > 0) {
      cmbUserTemplate.addItem(SELECT_OPTION1 + userTemplateFileList.length
          + SELECT_OPTION2);
      cmbUserTemplate.setComboBoxEnabled(true);
      for (int i = 0; i < userTemplateFileList.length; i++) {
        cmbUserTemplate.addItem(userTemplateFileList[i].getName());
      }
    }
    else {
      cmbUserTemplate.addItem(EMPTY_OPTION);
      cmbUserTemplate.setComboBoxEnabled(false);
    }
    cmbUserTemplate.setSelectedIndex(0);
  }

  private File getTemplateFile(final ComboBox cmbTemplate, final File[] templateFileList) {
    if (cmbTemplate.isEnabled()) {
      int i = cmbTemplate.getSelectedIndex() - 1;
      if (i != -1 && templateFileList != null) {
        return templateFileList[i];
      }
    }
    return null;
  }

  private void setTemplate(final String templateAbsPath, final File[] templateFileList,
      final ComboBox cmbTemplate) {
    if (templateFileList == null) {
      return;
    }
    // If templateAbsPath doesn't match something in templateFileList, nothing will be
    // selected in the combobox.
    for (int i = 0; i < templateFileList.length; i++) {
      if (templateFileList[i].getAbsolutePath().equals(templateAbsPath)) {
        cmbTemplate.setSelectedIndex(i);
        break;
      }
    }
  }

  boolean equalsActionCommand(final String actionCommand) {
    return actionCommand.equals(cmbScopeTemplate.getActionCommand())
        || actionCommand.equals(cmbSystemTemplate.getActionCommand())
        || actionCommand.equals(cmbUserTemplate.getActionCommand());
  }

  void getParameters(final UserConfiguration userConfig) {
    userConfig.setScopeTemplate(getScopeTemplateFile());
    userConfig.setSystemTemplate(getSystemTemplateFile());
    userConfig.setUserTemplate(getUserTemplateFile());
  }

  /**
   * Refresh the directive file collection and return it.
   * @return
   */
  DirectiveFileCollection getDirectiveFileCollection() {
    directiveFileCollection.setDirectiveFile(getScopeTemplateFile(),
        DirectiveFileType.SCOPE);
    directiveFileCollection.setDirectiveFile(getSystemTemplateFile(),
        DirectiveFileType.SYSTEM);
    directiveFileCollection.setDirectiveFile(getUserTemplateFile(),
        DirectiveFileType.USER);
    return directiveFileCollection;
  }

  private File getScopeTemplateFile() {
    return getTemplateFile(cmbScopeTemplate, scopeTemplateFileList);
  }

  private File getSystemTemplateFile() {
    return getTemplateFile(cmbSystemTemplate, systemTemplateFileList);
  }

  private File getUserTemplateFile() {
    return getTemplateFile(cmbUserTemplate, userTemplateFileList);
  }

  private void focusGained() {
    // Only listening to user template combobox
    if (settings != null && !settings.equalsUserTemplateDir(newUserTemplateDir)) {
      // If a new user template directory has been entered, reload the user template combo
      // box.
      newUserTemplateDir = settings.getUserTemplateDir();
      loadUserTemplate();
    }
  }

  boolean isAppearanceSettingChanged(final UserConfiguration userConfig) {
    if (!userConfig.equalsScopeTemplate(getScopeTemplateFile())
        || !userConfig.equalsSystemTemplate(getSystemTemplateFile())
        || !userConfig.equalsUserTemplate(getUserTemplateFile())) {
      return true;
    }
    return false;
  }

  void setParameters(final UserConfiguration userConfig) {
    if (userConfig.isScopeTemplateSet()) {
      setTemplate(userConfig.getScopeTemplate(), scopeTemplateFileList, cmbScopeTemplate);
    }
    if (userConfig.isSystemTemplateSet()) {
      setTemplate(userConfig.getSystemTemplate(), systemTemplateFileList,
          cmbSystemTemplate);
    }
    if (userConfig.isUserTemplateSet()) {
      setTemplate(userConfig.getUserTemplate(), userTemplateFileList, cmbUserTemplate);
    }
  }

  private static final class TemplateFocusListener implements FocusListener {
    private final TemplatePanel template;

    TemplateFocusListener(final TemplatePanel template) {
      this.template = template;
    }

    public void focusGained(final FocusEvent e) {
      template.focusGained();
    }

    public void focusLost(final FocusEvent e) {
    }
  }
}
