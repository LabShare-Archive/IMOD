package etomo.ui.swing;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import etomo.PeetManager;
import etomo.logic.DatasetTool;
import etomo.logic.PeetStartupData;
import etomo.storage.PeetAndMatlabParamFileFilter;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.DataFileType;
import etomo.type.UITestFieldType;
import etomo.ui.FieldType;
import etomo.ui.UIComponent;
import etomo.util.Utilities;

/**
* <p>Description: Modal dialog for collecting the values needed to create the PEET dialog.</p>
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
public final class PeetStartupDialog implements UIComponent, SwingComponent {
  public static final String rcsid = "$Id:$";

  private static final String COPY_FROM_LABEL = "Copy project from ";
  private static final String NAME = "Starting PEET";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbCopyFrom = new CheckBox(COPY_FROM_LABEL);
  private final LabeledTextField ltfBaseName = new LabeledTextField(FieldType.STRING,
      "Base name: ");
  private final MultiLineButton btnOk = new MultiLineButton("OK");
  private final MultiLineButton btnCancel = new MultiLineButton("Cancel");

  private final FileTextField2 ftfDirectory;
  private final FileTextField2 ftfCopyFrom;
  private final JDialog dialog;
  private final AxisID axisID;
  private final PeetManager manager;

  private PeetStartupDialog(final PeetManager manager, final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    ftfDirectory = FileTextField2.getPeetInstance(manager, "Directory: ");
    ftfCopyFrom = FileTextField2.getUnlabeledPeetInstance(manager, COPY_FROM_LABEL);
    dialog = new JDialog(UIHarness.INSTANCE.getFrame(manager), NAME, true);
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    String name = Utilities.convertLabelToName(NAME);
    pnlRoot.setName(UITestFieldType.PANEL.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
  }

  public static PeetStartupDialog getInstance(final PeetManager manager,
      final AxisID axisID) {
    PeetStartupDialog instance = new PeetStartupDialog(manager, axisID);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    JPanel pnlLabel = new JPanel();
    JPanel pnlLabelX = new JPanel();
    JPanel pnlData = new JPanel();
    JPanel pnlDataX = new JPanel();
    JPanel pnlDirectory = new JPanel();
    JPanel pnlCopyFrom = new JPanel();
    JPanel pnlButtons = new JPanel();
    ltfBaseName.setPreferredWidth(125);
    ftfDirectory.setAdjustedFieldWidth(175);
    ftfDirectory.setFileSelectionMode(FileChooser.DIRECTORIES_ONLY);
    ftfDirectory.setAbsolutePath(true);
    ftfDirectory.setOriginEtomoRunDir(true);
    ftfDirectory.setFile(new File(""));
    ftfCopyFrom.setAdjustedFieldWidth(225);
    ftfCopyFrom.setFileFilter(new PeetAndMatlabParamFileFilter());
    ftfCopyFrom.setAbsolutePath(true);
    ftfCopyFrom.setOriginEtomoRunDir(true);
    updateDisplay();
    // dialog
    dialog.getContentPane().add(pnlRoot);
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y20));
    pnlRoot.add(pnlLabelX);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y20));
    pnlRoot.add(pnlDataX);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y40));
    pnlRoot.add(pnlButtons);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y40));
    // X direction label panel
    pnlLabelX.setLayout(new BoxLayout(pnlLabelX, BoxLayout.X_AXIS));
    pnlLabelX.add(Box.createRigidArea(FixedDim.x15_y0));
    pnlLabelX.add(pnlLabel);
    // label panel
    pnlLabel.setLayout(new GridLayout(2, 1));
    pnlLabel.add(new JLabel("Each PEET project must reside in its own directory."));
    pnlLabel
        .add(new JLabel("Please choose a directory and a base name for output files."));
    // X direction data panel
    pnlDataX.setLayout(new BoxLayout(pnlDataX, BoxLayout.X_AXIS));
    pnlDataX.add(Box.createRigidArea(FixedDim.x15_y0));
    pnlDataX.add(pnlData);
    // data panel
    pnlData.setLayout(new GridLayout(3, 1, 0, 15));
    pnlData.add(pnlDirectory);
    pnlData.add(pnlCopyFrom);
    pnlData.add(ltfBaseName.getContainer());
    // directory panel
    pnlDirectory.setLayout(new BoxLayout(pnlDirectory, BoxLayout.X_AXIS));
    pnlDirectory.add(ftfDirectory.getRootPanel());
    pnlDirectory.add(Box.createRigidArea(FixedDim.x142_y0));
    // copy from panel
    pnlCopyFrom.setLayout(new BoxLayout(pnlCopyFrom, BoxLayout.X_AXIS));
    pnlCopyFrom.add(cbCopyFrom);
    pnlCopyFrom.add(ftfCopyFrom.getRootPanel());
    pnlCopyFrom.add(Box.createRigidArea(FixedDim.x15_y0));
    // button panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(Box.createRigidArea(FixedDim.x200_y0));
    pnlButtons.add(btnOk.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x15_y0));
    pnlButtons.add(btnCancel.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x40_y0));
  }

  private void addListeners() {
    dialog.addWindowListener(new PeetStartupWindowListener(this));
    ftfCopyFrom.addResultListener(new PeetStartupResultListener(this));
    ActionListener listener = new PeetStartupActionListener(this);
    cbCopyFrom.addActionListener(listener);
    btnOk.addActionListener(listener);
    btnCancel.addActionListener(listener);
  }

  public void display() {
    dialog.pack();
    dialog.setVisible(true);
  }

  private void dispose() {
    dialog.setVisible(false);
    dialog.dispose();
  }

  public SwingComponent getUIComponent() {
    return this;
  }

  public Component getComponent() {
    return dialog;
  }

  /**
   * Returns filled peet startup data instance.  Does not do validation.
   * @return
   */
  private PeetStartupData getStartupData() {
    PeetStartupData startupData = new PeetStartupData();
    startupData.setDirectory(ftfDirectory.getText());
    if (cbCopyFrom.isSelected()) {
      startupData.setCopyFrom(ftfCopyFrom.getText());
    }
    startupData.setBaseName(ltfBaseName.getText());
    return startupData;
  }

  /**
   * Dialog validation.  Pops up an error message if validation fails.
   * @return false if validation fails.
   */
  private boolean validate() {
    String errorMessage = null;
    // Directory
    if (ftfDirectory.isEmpty()) {
      errorMessage = ftfDirectory.getQuotedLabel() + " is required.";
    }
    else {
      File file = ftfDirectory.getFile();
      if (!file.exists()) {
        errorMessage = ftfDirectory.getQuotedLabel()
            + " must contain a directory which exists.";
      }
      else if (!file.isDirectory()) {
        errorMessage = ftfDirectory.getQuotedLabel() + " must contain a directory.";
      }
      else if (!file.canRead()) {
        errorMessage = ftfDirectory.getQuotedLabel()
            + " must contain a readable directory.";
      }
      else if (!file.canWrite()) {
        errorMessage = ftfDirectory.getQuotedLabel()
            + " must contain a writable directory.";
      }
      // CopyFrom
      else if (cbCopyFrom.isSelected()) {
        if (ftfCopyFrom.isEmpty()) {
          errorMessage = ftfCopyFrom.getQuotedLabel() + " is required.";
        }
        else {
          file = ftfCopyFrom.getFile();
          PeetAndMatlabParamFileFilter filter = new PeetAndMatlabParamFileFilter();
          if (!file.exists()) {
            errorMessage = ftfCopyFrom.getQuotedLabel()
                + " must contain a file which exists: " + filter.getDescription() + ".";
          }
          else if (!file.isFile()) {
            errorMessage = ftfCopyFrom.getQuotedLabel() + " must contain a file: "
                + filter.getDescription() + ".";
          }
          else if (!filter.accept(file)) {
            errorMessage = ftfCopyFrom.getQuotedLabel()
                + " must contain the correct file type: " + filter.getDescription() + ".";
          }
          else if (!file.canRead()) {
            errorMessage = ftfCopyFrom.getQuotedLabel()
                + " must contain a readable file.";
          }
          // BaseName
          else if (ltfBaseName.isEmpty()) {
            errorMessage = ltfBaseName.getQuotedLabel() + " is required.";
          }
        }
      }
    }
    if (errorMessage == null) {
      if (!DatasetTool.validateDatasetName(manager, this, axisID, ftfDirectory.getFile(),
          ltfBaseName.getText(), DataFileType.PEET, null, true)) {
        return false;
      }
      return true;
    }
    UIHarness.INSTANCE.openMessageDialog(manager, this, errorMessage, "Entry Error",
        axisID);
    return false;
  }

  private void updateDisplay() {
    ftfCopyFrom.setEnabled(cbCopyFrom.isSelected());
  }

  private void action(final String command) {
    if (cbCopyFrom.getActionCommand().equals(command)) {
      updateDisplay();
    }
    else if (btnOk.getActionCommand().equals(command)) {
      if (!validate()) {
        return;
      }
      PeetStartupData startupData = getStartupData();
      String errorMessage = startupData.validate();
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, this, errorMessage, "Entry Error",
            axisID);
        return;
      }
      dispose();
      manager.setStartupData(startupData);
    }
    else if (btnCancel.getActionCommand().equals(command)) {
      dispose();
      manager.cancelStartup();
    }
  }

  private void windowClosing() {
    dispose();
    manager.cancelStartup();
  }

  private void processResult(final Object resultOrigin) {
    if (ftfCopyFrom == resultOrigin) {
      if (!ltfBaseName.isEmpty()) {
        return;
      }
      File fromFile = ftfCopyFrom.getFile();
      ltfBaseName.setText(Utilities.getStrippedFileName(fromFile));
    }
  }

  private void setTooltips() {
    ftfDirectory.setToolTipText("The directory which will contain the parameter and "
        + "project files, logs, intermediate files, and results. Data files "
        + "can also be located in this directory, but are not required to be.");
    ltfBaseName.setToolTipText("The base name of the output files for the average "
        + "volumes, the reference volumes, and the transformation parameters.");
    String tooltip = "Check and fill in an .epe or .prm file to create a new PEET project "
        + "from an existing parameter or project file, duplicating all parameters except "
        + "root name and location.";
    cbCopyFrom.setToolTipText(tooltip);
    ftfCopyFrom.setToolTipText(tooltip);
  }

  private static final class PeetStartupActionListener implements ActionListener {
    private final PeetStartupDialog dialog;

    private PeetStartupActionListener(final PeetStartupDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.action(event.getActionCommand());
    }
  }

  private static final class PeetStartupResultListener implements ResultListener {
    private final PeetStartupDialog dialog;

    private PeetStartupResultListener(final PeetStartupDialog dialog) {
      this.dialog = dialog;
    }

    public void processResult(final Object resultOrigin) {
      dialog.processResult(resultOrigin);
    }
  }

  private static final class PeetStartupWindowListener implements WindowListener {
    private final PeetStartupDialog dialog;

    private PeetStartupWindowListener(final PeetStartupDialog dialog) {
      this.dialog = dialog;
    }

    public void windowActivated(final WindowEvent event) {
    }

    public void windowClosed(final WindowEvent event) {
    }

    public void windowClosing(final WindowEvent event) {
      dialog.windowClosing();
    }

    public void windowDeactivated(final WindowEvent event) {
    }

    public void windowDeiconified(final WindowEvent event) {
    }

    public void windowIconified(final WindowEvent event) {
    }

    public void windowOpened(final WindowEvent event) {
    }
  }
}
