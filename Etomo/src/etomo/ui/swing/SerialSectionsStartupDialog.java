package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.ButtonGroup;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import javax.swing.WindowConstants;

import etomo.EtomoDirector;
import etomo.SerialSectionsManager;
import etomo.logic.DatasetDirectory;
import etomo.logic.SerialSectionsStartupData;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplayProxy;
import etomo.type.UITestFieldType;
import etomo.type.ViewType;
import etomo.ui.SimpleProcessResultDisplay;
import etomo.util.SharedConstants;
import etomo.util.Utilities;

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
public class SerialSectionsStartupDialog implements ContextMenu,
    SimpleProcessResultDisplay {
  public static final String rcsid = "$Id:$";

  private static final String NAME = "Starting Serial Sections";
  private static final String VIEW_TYPE_LABEL = "Frame Type";

  private final JPanel pnlRoot = new JPanel();
  private final MultiLineButton btnOk = new MultiLineButton("OK");
  private final MultiLineButton btnCancel = new MultiLineButton("Cancel");
  private final ButtonGroup bgViewType = new ButtonGroup();
  private final RadioButton rbViewTypeSingle = new RadioButton("Single frame",
      ViewType.SINGLE_VIEW, bgViewType);
  private final RadioButton rbViewTypeMontage = new RadioButton("Montage",
      ViewType.MONTAGE, bgViewType);
  private final LabeledSpinner spBinning = new LabeledSpinner("Binning: ",
      new SpinnerNumberModel(1, 1, 50, 1), 1);
  private final ProcessResultDisplayProxy processResultDisplayProxy = new ProcessResultDisplayProxy(
      this);

  private final FileTextField2 ftfStack;
  private final FileTextField2 ftfDistortionFile;
  private final JDialog dialog;
  private final AxisID axisID;
  private final SerialSectionsManager manager;

  private SerialSectionsStartupData startupData = null;

  private SerialSectionsStartupDialog(final SerialSectionsManager manager,
      final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    ftfStack = FileTextField2.getInstance(manager, "Stack: ");
    ftfDistortionFile = FileTextField2.getInstance(manager,
        "Image distortion field file: ");
    dialog = new JDialog(UIHarness.INSTANCE.getFrame(manager), NAME, true);
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    pnlRoot.setName(UITestFieldType.PANEL.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + Utilities.convertLabelToName(NAME));
  }

  public static SerialSectionsStartupDialog getInstance(
      final SerialSectionsManager manager, final AxisID axisID) {
    SerialSectionsStartupDialog instance = new SerialSectionsStartupDialog(manager,
        axisID);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    ftfStack.setAbsolutePath(true);
    ftfStack.setOriginEtomoRunDir(true);
    ftfDistortionFile.setAbsolutePath(true);
    ftfDistortionFile.setOrigin(EtomoDirector.INSTANCE.getIMODCalibDirectory());
    // dialog
    dialog.getContentPane().add(pnlRoot);
    // root
    pnlRoot.add(ftfStack.getRootPanel());
    pnlRoot.add(new JLabel(VIEW_TYPE_LABEL));
    pnlRoot.add(rbViewTypeSingle.getComponent());
    pnlRoot.add(rbViewTypeMontage.getComponent());
    pnlRoot.add(ftfDistortionFile.getRootPanel());
    pnlRoot.add(spBinning.getContainer());
    pnlRoot.add(btnOk.getComponent());
    pnlRoot.add(btnCancel.getComponent());
  }

  private void addListeners() {
    pnlRoot.addMouseListener(new GenericMouseAdapter(this));
    dialog.addWindowListener(new SerialSectionsStartupWindowListener(this));
    ActionListener listener = new SerialSectionsStartupActionListener(this);
    btnOk.addActionListener(listener);
    btnCancel.addActionListener(listener);
    rbViewTypeSingle.addActionListener(listener);
    rbViewTypeMontage.addActionListener(listener);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Processchunks", "3dmod" };
    String[] manPage = { "processchunks.html", "3dmod.html" };
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent, manPagelabel,
        manPage, true, manager, axisID);
  }

  public void display() {
    dialog.pack();
    dialog.setVisible(true);
  }

  private boolean validate() {
    return DatasetDirectory.validateDatasetName(manager, axisID, ftfStack.getFile(),
        DataFileType.SERIAL_SECTIONS, AxisType.SINGLE_AXIS);
  }

  /**
   * Called when extractpieces is done.
   */
  public void setProcessDone(final boolean succeeded) {
    if (succeeded) {
      dispose();
      manager.setStartupData(startupData);
    }
  }

  private void action(final ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnOk.getActionCommand())) {
      if (DatasetDirectory.validateDatasetName(manager, axisID, ftfStack.getFile(),
          DataFileType.SERIAL_SECTIONS, null)) {
        return;
      }
      startupData = getStartupData();
      String errorMessage = startupData.validate();
      if (errorMessage != null) {
        UIHarness.INSTANCE
            .openMessageDialog(manager, errorMessage, "Entry Error", axisID);
        return;
      }
      manager.extractpieces(axisID, processResultDisplayProxy, null,
          DialogType.SERIAL_SECTIONS_STARTUP, startupData.getViewType());
    }
    else if (command.equals(btnCancel.getActionCommand())) {
      dispose();
      manager.cancelStartup();
    }
    else if (command.equals(rbViewTypeSingle.getActionCommand())) {
      updateDisplay();
    }
    else if (command.equals(rbViewTypeMontage.getActionCommand())) {
      updateDisplay();
    }
  }

  private void updateDisplay() {
    ftfDistortionFile.setEnabled(rbViewTypeMontage.isSelected());
    spBinning.setEnabled(rbViewTypeMontage.isSelected());
  }

  /**
   * Returns filleed serial sections startup data instance.  Does not do validation.
   * @return
   */
  private SerialSectionsStartupData getStartupData() {
    SerialSectionsStartupData startupData = new SerialSectionsStartupData();
    startupData.setStack(ftfStack.getFile(), ftfStack.getQuotedLabel());
    startupData.setViewType(
        ((RadioButton.RadioButtonModel) bgViewType.getSelection()).getEnumeratedType(),
        "'" + VIEW_TYPE_LABEL + "'");
    if (ftfDistortionFile.isEnabled()) {
      startupData.setDistortionFile(ftfDistortionFile.getFile());
    }
    if (spBinning.isEnabled()) {
      startupData.setBinning(spBinning.getValue());
    }
    return startupData;
  }

  private void dispose() {
    dialog.setVisible(false);
    dialog.dispose();
  }

  private void windowClosing() {
    dispose();
    manager.cancelStartup();
  }

  private void setTooltips() {
    ftfStack
        .setToolTipText("Stack to be processed.  Location will be used as the dataset "
            + "directory.");
    ftfDistortionFile.setToolTipText(SharedConstants.DISTORTION_FILE_TOOLTIP);
    spBinning.setToolTipText(SharedConstants.BINNING_TOOLTIP);
  }

  private static final class SerialSectionsStartupWindowListener implements
      WindowListener {
    private final SerialSectionsStartupDialog dialog;

    private SerialSectionsStartupWindowListener(final SerialSectionsStartupDialog dialog) {
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

  private static final class SerialSectionsStartupActionListener implements
      ActionListener {
    private final SerialSectionsStartupDialog dialog;

    private SerialSectionsStartupActionListener(final SerialSectionsStartupDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.action(event);
    }
  }
}
