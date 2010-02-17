package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JFileChooser;

import etomo.PeetManager;
import etomo.storage.PeetAndMatlabParamFileFilter;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: Panel that creates a PEET dataset using an existing project.
 * It currently has no saved state so it is completely reusable.</p>
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
 * <p> Revision 1.4  2009/12/23 02:27:48  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.3  2009/11/20 17:40:39  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.
 * <p>
 * <p> Revision 1.2  2009/10/29 12:14:37  sueh
 * <p> bug# 1245 Removed btnDuplicateProject.  Removed
 * <p> duplicateExistingProject.  Changed importMatlabParam to importParam.
 * <p> Call PeetManager.loadParam() from importParam and copyParameters.
 * <p>
 * <p> Revision 1.1  2009/10/15 23:39:17  sueh
 * <p> bug# 1274 Factored UseExistingProjectPanel out of PeetDialog.
 * <p> </p>
 */
final class UseExistingProjectPanel {
  public static final String rcsid = "$Id$";

  private final MultiLineButton btnImportMatlabParamFile = new MultiLineButton(
      "Import an Existing Project");
  private final MultiLineButton btnCopyParameters = new MultiLineButton(
      "Copy Parameters");
  private final EtomoPanel pnlRoot = new EtomoPanel();

  private final PeetManager manager;
  private final UseExistingProjectParent parent;

  private UseExistingProjectPanel(final PeetManager manager,
      final UseExistingProjectParent parent) {
    this.manager = manager;
    this.parent = parent;
  }

  /**
   * Create instance and panel.  Add tooltips and listeners.
   * @param manager
   * @param parent
   * @return
   */
  static UseExistingProjectPanel getInstance(final PeetManager manager,
      final UseExistingProjectParent parent) {
    UseExistingProjectPanel instance = new UseExistingProjectPanel(manager,
        parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  /**
   * Create the panel.
   */
  private void createPanel() {
    //use existing project
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.setBorder(new EtchedBorder("Use Existing Project").getBorder());
    btnImportMatlabParamFile.setSize();
    pnlRoot.add(btnImportMatlabParamFile.getComponent());
    btnCopyParameters.setSize();
    pnlRoot.add(btnCopyParameters.getComponent());
  }

  /**
   * Add listeners.
   */
  private void addListeners() {
    UseExistingProjectActionListener actionListener = new UseExistingProjectActionListener(
        this);
    btnImportMatlabParamFile.addActionListener(actionListener);
    btnCopyParameters.addActionListener(actionListener);
  }

  /**
   * Get the root panel.
   * @return
   */
  Component getComponent() {
    return pnlRoot;
  }

  /**
   * Toggles between a setup-like mode where the location and root name being
   * chosen, and a regular mode.
   * @param paramFileSet
   */
  public void updateDisplay(final boolean paramFileSet) {
    btnImportMatlabParamFile.setEnabled(!paramFileSet);
    btnCopyParameters.setEnabled(!paramFileSet);
  }

  /**
   * Create a project out of a matlab param file or an .epe file.
   */
  private void importParam() {
    String path = parent.getDirectory().getText();
    if (path == null || path.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Please set the " + PeetDialog.DIRECTORY_LABEL
              + " field before importing a .prm file.", "Entry Error");
      return;
    }
    File dir = new File(parent.getDirectory().getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Please create "
          + dir.getAbsolutePath() + " before importing a file.", "Entry Error");
      return;
    }
    File file = null;
    JFileChooser chooser = new FileChooser(dir);
    chooser.setFileFilter(new PeetAndMatlabParamFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(pnlRoot);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    file = chooser.getSelectedFile();
    manager.loadParam(file, false);
  }

  /**
   * Create a project out of a peet file or a .prm file from another directory.
   * Copy fields not related to specific files.
   */
  private void copyParameters() {
    String path = parent.getDirectory().getText();
    if (path == null || path.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Please set the "
          + PeetDialog.DIRECTORY_LABEL + " field before copying parameters.",
          "Entry Error");
      return;
    }
    File dir = new File(parent.getDirectory().getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Please create "
          + dir.getAbsolutePath() + " before copy parameters.", "Entry Error");
      return;
    }
    JFileChooser chooser = new FileChooser(dir);
    chooser.setFileFilter(new PeetAndMatlabParamFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(pnlRoot);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File file = chooser.getSelectedFile();
    manager.loadParam(file, true);
  }

  /**
   * Responds to button presses.
   * @param actionCommand
   * @param run3dmodMenuOptions
   */
  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(btnImportMatlabParamFile.getActionCommand())) {
      importParam();
    }
    else if (actionCommand.equals(btnCopyParameters.getActionCommand())) {
      copyParameters();
    }
  }

  /**
   * Set tooltips.
   */
  private void setTooltips() {
    btnImportMatlabParamFile
        .setToolTipText("Create a new PEET project from an existing parameter or "
            + "project file, duplicating all parameters except root name and "
            + "location.");
    btnCopyParameters
        .setToolTipText("Create a new PEET project, copying all settings except "
            + "the Volume Table from an existing parameter or project file.");
  }

  /**
   * Action listener for UseExistingProjectPanel.
   * @author sueh
   */
  private static final class UseExistingProjectActionListener implements
      ActionListener {
    private final UseExistingProjectPanel panel;

    private UseExistingProjectActionListener(final UseExistingProjectPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event.getActionCommand(), null);
    }
  }
}
