package etomo.ui.swing;

import java.awt.Component;
import java.io.File;

import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.logic.ConfigTool;
import etomo.storage.DirectiveFile;
import etomo.type.AxisID;

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
final class TemplatePanel {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();
  private final ComboBox cmbScopeTemplate = ComboBox.getInstance("Scope Template");
  private final ComboBox cmbSystemTemplate = ComboBox.getInstance("System Template");
  private final ComboBox cmbUserTemplate = ComboBox.getInstance("User Template");

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
    pnlRoot.add(cmbScopeTemplate.getComponent());
    pnlRoot.add(cmbSystemTemplate.getComponent());
    pnlRoot.add(cmbUserTemplate.getComponent());
  }

  private void addListeners() {
    cmbScopeTemplate.addActionListener(listener);
    cmbSystemTemplate.addActionListener(listener);
    cmbUserTemplate.addActionListener(listener);
  }

  Component getComponent() {
    return pnlRoot;
  }

  File getScopeTemplateFile() {
    int i = cmbScopeTemplate.getSelectedIndex();
    if (i != -1) {
      return scopeTemplateFileList[i];
    }
    return null;
  }

  File getSystemTemplateFile() {
    int i = cmbSystemTemplate.getSelectedIndex();
    if (i != -1) {
      return systemTemplateFileList[i];
    }
    return null;
  }

  File getUserTemplateFile() {
    int i = cmbUserTemplate.getSelectedIndex();
    if (i != -1) {
      return userTemplateFileList[i];
    }
    return null;
  }

  DirectiveFile getScopeTemplate() {
    int i = cmbScopeTemplate.getSelectedIndex();
    if (i != -1) {
      return DirectiveFile.getInstance(manager, axisID, scopeTemplateFileList[i]);
    }
    return null;
  }
  
  DirectiveFile getSystemTemplate() {
    int i = cmbSystemTemplate.getSelectedIndex();
    if (i != -1) {
      return DirectiveFile.getInstance(manager, axisID, systemTemplateFileList[i]);
    }
    return null;
  }
  DirectiveFile getUserTemplate() {
    int i = cmbUserTemplate.getSelectedIndex();
    if (i != -1) {
      return DirectiveFile.getInstance(manager, axisID, userTemplateFileList[i]);
    }
    return null;
  }
  
  boolean equalsActionCommand(final String actionCommand) {
    return actionCommand.equals(cmbScopeTemplate.getActionCommand())
        || actionCommand.equals(cmbSystemTemplate.getActionCommand())
        || actionCommand.equals(cmbUserTemplate.getActionCommand());
  }
}
