package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import etomo.SerialSectionsManager;
import etomo.logic.ConfigTool;
import etomo.logic.DatasetTool;
import etomo.logic.SerialSectionsStartupData;
import etomo.storage.DistortionFileFilter;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.UITestFieldType;
import etomo.type.ViewType;
import etomo.ui.UIComponent;
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
public class SerialSectionsStartupDialog implements ContextMenu, UIComponent,
    SwingComponent {
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
  private final LabeledSpinner spImagesAreBinned = LabeledSpinner.getInstance(
      "Binning: ", 1, 1, 50, 1);
  private final DialogType dialogType = DialogType.SERIAL_SECTIONS_STARTUP;

  private final FileTextField2 ftfStack;
  private final FileTextField2 ftfDistortionField;
  private final JDialog dialog;
  private final AxisID axisID;
  private final SerialSectionsManager manager;

  /**
   * Contains the saved state of the dialog.
   */
  private SerialSectionsStartupData startupData = null;
  private String distortionFieldNewstackTooltip = null;
  private String distortionFieldBlendmontTooltip = null;
  private String imagesAreBinnedNewstackTooltip = null;
  private String imagesAreBinnedBlendmontTooltip = null;

  private SerialSectionsStartupDialog(final SerialSectionsManager manager,
      final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    ftfStack = FileTextField2.getInstance(manager, "Stack: ");
    ftfDistortionField = FileTextField2.getInstance(manager,
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
    ftfDistortionField.setAdjustedFieldWidth(175);
    ftfDistortionField.setAbsolutePath(true);
    ftfDistortionField.setOrigin(ConfigTool.getDistortionDir(manager, null));
    ftfDistortionField.setFileFilter(new DistortionFileFilter());
    // panels
    JPanel pnlStack = new JPanel();
    JPanel pnlViewType = new JPanel();
    JPanel pnlViewTypeX = new JPanel();
    JPanel pnlImage = new JPanel();
    JPanel pnlButtons = new JPanel();
    // dialog
    dialog.getContentPane().add(pnlRoot);
    // root
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlStack);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlViewTypeX);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlImage);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y20));
    pnlRoot.add(pnlButtons);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    // stack
    pnlStack.setLayout(new BoxLayout(pnlStack, BoxLayout.X_AXIS));
    pnlStack.add(ftfStack.getRootPanel());
    pnlStack.add(Box.createRigidArea(FixedDim.x150_y0));
    // view type - x direction
    pnlViewTypeX.setLayout(new BoxLayout(pnlViewTypeX, BoxLayout.X_AXIS));
    pnlViewTypeX.add(pnlViewType);
    pnlViewTypeX.add(Box.createRigidArea(FixedDim.x272_y0));
    // view type
    pnlViewType.setLayout(new BoxLayout(pnlViewType, BoxLayout.Y_AXIS));
    pnlViewType.setBorder(new EtchedBorder(VIEW_TYPE_LABEL).getBorder());
    pnlViewType.add(rbViewTypeSingle.getComponent());
    pnlViewType.add(rbViewTypeMontage.getComponent());
    // image
    pnlImage.setLayout(new BoxLayout(pnlImage, BoxLayout.X_AXIS));
    pnlImage.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlImage.add(ftfDistortionField.getRootPanel());
    pnlImage.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlImage.add(spImagesAreBinned.getContainer());
    pnlImage.add(Box.createRigidArea(FixedDim.x5_y0));
    // buttons
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(Box.createRigidArea(FixedDim.x264_y0));
    pnlButtons.add(btnOk.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x15_y0));
    pnlButtons.add(btnCancel.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x40_y0));
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
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent, manager, axisID,
        true);
  }

  public void display() {
    dialog.pack();
    dialog.setVisible(true);
  }

  private boolean validate() {
    if (!DatasetTool.validateDatasetName(manager, this, axisID, ftfStack.getFile(),
        DataFileType.SERIAL_SECTIONS, AxisType.SINGLE_AXIS)) {
      return false;
    }
    File stack = getStack();
    return DatasetTool.validateViewType(getViewType(), stack.getParent(),
        stack.getName(), manager, this, axisID);
  }

  /**
   * Called when the OK button functionality completes successfully.
   */
  public void done() {
    dispose();
    manager.setStartupData(startupData);
  }

  /**
   * Throws away the saved state of the dialog.  Called when the OK button functionality
   * fails.
   */
  public void resetSavedState() {
    startupData = null;
  }

  private void action(final ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnOk.getActionCommand())) {
      if (!validate()) {
        return;
      }
      if (!saveState()) {
        return;
      }
      manager.completeStartup(this, axisID);
    }
    else if (command.equals(btnCancel.getActionCommand())) {
      resetSavedState();
      dispose();
      manager.cancelStartup();
    }
  }

  public SerialSectionsStartupData getStartupData() {
    return startupData;
  }

  public SwingComponent getUIComponent() {
    return this;
  }

  public Component getComponent() {
    return dialog;
  }

  /**
   * @return distortion field file or null if unavailable.  It the state has been saved,
   * use the saved value.
   */
  public File getDistortionField() {
    if (startupData != null) {
      return startupData.getDistortionField();
    }
    if (!ftfDistortionField.isEmpty()) {
      return ftfDistortionField.getFile();
    }
    return null;
  }

  public String getPropertyUserDir() {
    if (startupData != null) {
      return startupData.getStack().getParent();
    }
    if (!ftfStack.isEmpty()) {
      return ftfStack.getFile().getParent();
    }
    return null;
  }

  /**
   * @return stack file or null if unavailable.  It the state has been saved, use the
   * saved value.
   */
  public File getStack() {
    if (startupData != null) {
      return startupData.getStack();
    }
    if (!ftfStack.isEmpty()) {
      return ftfStack.getFile();
    }
    return null;
  }

  /**
   * @return stack file or null if unavailable.  It the state has been saved, use the
   * saved value.
   */
  public ViewType getViewType() {
    if (startupData != null) {
      return startupData.getViewType();
    }
    return ViewType
        .getInstance(((RadioButton.RadioButtonModel) bgViewType.getSelection())
            .getEnumeratedType());
  }

  /**
   * @return root name or null if unavailable.  It the state has been saved, use the saved
   * value.
   */
  public String getRootName() {
    if (startupData != null) {
      return startupData.getRootName();
    }
    if (!ftfStack.isEmpty()) {
      SerialSectionsStartupData tempStartupData = new SerialSectionsStartupData(null,
          null);
      tempStartupData.setStack(ftfStack.getFile());
      return tempStartupData.getRootName();
    }
    return null;
  }

  /**
   * Instanciates, loads, and validates serial sections startup data member variable.
   * Sets startup data to null if state is invalid.
   * @return true if valid state
   */
  private boolean saveState() {
    startupData = new SerialSectionsStartupData(ftfStack.getQuotedLabel(), "'"
        + VIEW_TYPE_LABEL + "'");
    if (!ftfStack.isEmpty()) {
      startupData.setStack(ftfStack.getFile());
    }
    startupData.setViewType(((RadioButton.RadioButtonModel) bgViewType.getSelection())
        .getEnumeratedType());
    if (!ftfDistortionField.isEmpty()) {
      startupData.setDistortionFile(ftfDistortionField.getFile());
    }
    startupData.setImagesAreBinned(spImagesAreBinned.getValue());
    String errorMessage = startupData.validate();
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(manager, this, errorMessage, "Entry Error",
          axisID);
      resetSavedState();
      return false;
    }
    return true;
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
    ftfStack.setToolTipText("Stack to be processed.  The stack location will be used as "
        + "the dataset directory.");
    ftfDistortionField.setToolTipText(SharedConstants.DISTORTION_FIELD_TOOLTIP);
    spImagesAreBinned.setToolTipText(SharedConstants.IMAGES_ARE_BINNED_TOOLTIP);
    rbViewTypeMontage.setToolTipText(SharedConstants.VIEW_TYPE_TOOLTIP);
    rbViewTypeSingle.setToolTipText(SharedConstants.VIEW_TYPE_TOOLTIP);
    btnOk.setToolTipText("Creates a dataset in the directory containing the serial "
        + "sections stack.");
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
