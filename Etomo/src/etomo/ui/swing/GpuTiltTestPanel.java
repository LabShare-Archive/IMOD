package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ToolsManager;
import etomo.comscript.GpuTiltTestParam;
import etomo.logic.DatasetTool;
import etomo.type.AxisID;
import etomo.type.DataFileType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
public class GpuTiltTestPanel implements ToolPanel, ContextMenu {
  public static final String rcsid = "$Id:$";

  private static final String DATASET_ROOT = "gputest";

  private final JPanel pnlRoot = new JPanel();
  private final LabeledTextField ltfNMinutes = new LabeledTextField(
      FieldType.FLOATING_POINT, "# of minutes: ");
  private final LabeledSpinner spGpuNumber = LabeledSpinner.getInstance("GPU #: ", 0, 0,
      8, 1);
  private final MultiLineButton btnRunTest = new MultiLineButton("Run GPU Test");

  private final ToolsManager manager;
  private final AxisID axisID;

  private GpuTiltTestPanel(final ToolsManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  public static GpuTiltTestPanel getInstance(final ToolsManager manager,
      final AxisID axisID) {
    GpuTiltTestPanel instance = new GpuTiltTestPanel(manager, axisID);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    ltfNMinutes.setText("1");
    ltfNMinutes.setPreferredWidth(80);
    btnRunTest.setSize();
    btnRunTest.setAlignmentX(Box.CENTER_ALIGNMENT);
    // panels
    JPanel pnlFields = new JPanel();
    // Root
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlFields);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y23));
    pnlRoot.add(btnRunTest.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y15));
    // Fields
    pnlFields.setLayout(new BoxLayout(pnlFields, BoxLayout.X_AXIS));
    pnlFields.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlFields.add(ltfNMinutes.getContainer());
    pnlFields.add(Box.createRigidArea(FixedDim.x15_y0));
    pnlFields.add(spGpuNumber.getContainer());
    pnlFields.add(Box.createRigidArea(FixedDim.x10_y0));
  }

  private void addListeners() {
    pnlRoot.addMouseListener(new GenericMouseAdapter(this));
    btnRunTest.addActionListener(new GpuTiltTestActionListener(this));
  }

  public boolean getParameters(final GpuTiltTestParam param, final boolean doValidation) {
    try {
      param.setNMinutes(ltfNMinutes.getText(doValidation));
      param.setGpuNumber(spGpuNumber.getValue());
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  private void action() {
    if (!DatasetTool.validateDatasetName(manager, axisID,
        new File(manager.getPropertyUserDir()), DATASET_ROOT, DataFileType.TOOLS, null,
        true)) {
      return;
    }
    manager.gpuTiltTest(axisID);
  }

  public Component getComponent() {
    return pnlRoot;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "gputilttest" };
    String[] manPage = { "gputilttest.html" };
    String[] logFileLabel = { "GPU test" };
    String[] logFile = new String[1];
    logFile[0] = DATASET_ROOT + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent, "GPU Test",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }

  private void setToolTipText() {
    ltfNMinutes.setToolTipText("The number of minutes to run the test");
    spGpuNumber
        .setToolTipText("The GPU number, numbered from 1.  When 0 is selected, the fastest "
            + "GPU will be used.");
    btnRunTest
        .setToolTipText("Test the reliability of the GPU by using gputilttest to run the "
            + "Tilt program repeatedly.");
  }

  private final class GpuTiltTestActionListener implements ActionListener {
    private final GpuTiltTestPanel adaptee;

    private GpuTiltTestActionListener(final GpuTiltTestPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action();
    }
  }
}
