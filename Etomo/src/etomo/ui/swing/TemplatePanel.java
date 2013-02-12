package etomo.ui.swing;

import java.awt.Component;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.logic.ConfigTool;
import etomo.storage.DirectiveFile;
import etomo.type.AxisID;
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

  private final JPanel pnlRoot = new JPanel();
  private final ComboBox cmbScopeTemplate = ComboBox.getInstance("Scope Template:");
  private final ComboBox cmbSystemTemplate = ComboBox.getInstance("System Template:");
  private final ComboBox cmbUserTemplate = ComboBox.getInstance("User Template:");

  private File[] scopeTemplateFileList = null;
  private File[] systemTemplateFileList = null;
  private File[] userTemplateFileList = null;

  private final TemplateActionListener listener;
  private final BaseManager manager;
  private final AxisID axisID;

  private TemplatePanel(final BaseManager manager, final AxisID axisID,
      final TemplateActionListener listener) {
    this.listener = listener;
    this.manager = manager;
    this.axisID = axisID;
  }

  static TemplatePanel getInstance(final BaseManager manager, final AxisID axisID,
      final TemplateActionListener listener) {
    TemplatePanel instance = new TemplatePanel(manager, axisID, listener);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    scopeTemplateFileList = ConfigTool.getScopeTemplateFiles();
    if (scopeTemplateFileList != null) {
      for (int i = 0; i < scopeTemplateFileList.length; i++) {
        cmbScopeTemplate.addItem(scopeTemplateFileList[i].getName());
      }
    }
    systemTemplateFileList = ConfigTool.getSystemTemplateFiles();
    if (systemTemplateFileList != null) {
      for (int i = 0; i < systemTemplateFileList.length; i++) {
        cmbSystemTemplate.addItem(systemTemplateFileList[i].getName());
      }
    }
    userTemplateFileList = ConfigTool.getUserTemplateFiles();
    if (userTemplateFileList != null) {
      for (int i = 0; i < userTemplateFileList.length; i++) {
        cmbUserTemplate.addItem(userTemplateFileList[i].getName());
      }
    }
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
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
  }

  Component getComponent() {
    return pnlRoot;
  }

  private File getTemplateFile(final ComboBox cmbTemplate, final File[] templateFileList) {
    int i = cmbTemplate.getSelectedIndex();
    if (i != -1 && templateFileList != null) {
      return templateFileList[i];
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

  DirectiveFile getScopeTemplate() {
    File templateFile = getScopeTemplateFile();
    if (templateFile != null) {
      return DirectiveFile.getInstance(manager, axisID, templateFile);
    }
    return null;
  }

 private File getScopeTemplateFile() {
    return getTemplateFile(cmbScopeTemplate, scopeTemplateFileList);
  }

  DirectiveFile getSystemTemplate() {
    File templateFile = getSystemTemplateFile();
    if (templateFile != null) {
      return DirectiveFile.getInstance(manager, axisID, templateFile);
    }
    return null;
  }

 private File getSystemTemplateFile() {
    return getTemplateFile(cmbSystemTemplate, systemTemplateFileList);
  }

  DirectiveFile getUserTemplate() {
    File templateFile = getUserTemplateFile();
    if (templateFile != null) {
      return DirectiveFile.getInstance(manager, axisID, templateFile);
    }
    return null;
  }

 private File getUserTemplateFile() {
    return getTemplateFile(cmbUserTemplate, userTemplateFileList);
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
}
