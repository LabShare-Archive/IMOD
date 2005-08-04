package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import etomo.BaseManager;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.SplittiltParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class ParallelPanel implements ParallelProgressDisplay {
  public static final String rcsid = "$Id$";

  private JPanel rootPanel;
  private ProcessorTable processorTable;
  private MultiLineButton btnResume = new MultiLineButton("Resume");
  private MultiLineButton btnPause = new MultiLineButton("Pause");
  private MultiLineButton btnSaveDefaults = new MultiLineButton("Save As Defaults");
  private LabeledTextField ltfCpusSelected = new LabeledTextField(
      "CPUs selected: ");
  private LabeledTextField ltfChunksFinished = new LabeledTextField(
      "Chunks finished: ");
  private LabeledSpinner nice;
  private ArrayList randomRestarts = new ArrayList();
  private ArrayList randomSuccesses = new ArrayList();
  private Random random = new Random(new Date().getTime());

  private AxisID axisID = null;
  private ParallelDialog parent = null;
  private DialogType dialogType = null;
  private ParallelPanelActionListener actionListener = new ParallelPanelActionListener(
      this);
  private PanelHeader header;
  private final BaseManager manager;
  private boolean visible = false;

  ParallelPanel(BaseManager manager, ParallelDialog parent, AxisID axisID) {
    this.manager = manager;
    this.parent = parent;
    this.axisID = axisID;
    processorTable = new ProcessorTable(this, axisID);
    //set listeners
    btnResume.addActionListener(actionListener);
    //panels
    rootPanel = new JPanel();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    SpacedPanel bodyPanel = new SpacedPanel();
    bodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    JPanel tablePanel = new JPanel();
    tablePanel.setLayout(new BoxLayout(tablePanel, BoxLayout.X_AXIS));
    SpacedPanel southPanel = new SpacedPanel();
    southPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    header = new PanelHeader(manager, axisID, "Parallel Processing", bodyPanel);
    //southPanel;
    southPanel.add(ltfCpusSelected);
    SpinnerModel model = new SpinnerNumberModel(
        ProcesschunksParam.NICE_DEFAULT, ProcesschunksParam.NICE_FLOOR,
        ProcesschunksParam.NICE_CEILING, 1);
    nice = new LabeledSpinner("Nice: ", model);
    southPanel.add(nice);
    southPanel.add(btnPause);
    southPanel.add(btnResume);
    southPanel.add(btnSaveDefaults);
    //tablePanel
    tablePanel.add(Box.createHorizontalGlue());
    tablePanel.add(processorTable.getContainer());
    tablePanel.add(Box.createHorizontalGlue());
    //bodyPanel
    bodyPanel.addRigidArea();
    bodyPanel.add(tablePanel);
    bodyPanel.add(southPanel);
    //rootPanel
    rootPanel.add(header.getContainer());
    rootPanel.add(bodyPanel.getContainer());
    //configure fields
    header.setOpen(true);
    ltfChunksFinished.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfChunksFinished.setEditable(false);
    ltfCpusSelected.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfCpusSelected.setEditable(false);
    processorTable.signalCpusSelectedChanged();
    btnResume.setEnabled(false);
    btnPause.setEnabled(false);
  }

  public final void signalCpusSelectedChanged(int cpusSelected) {
    ltfCpusSelected.setText(cpusSelected);
  }

  final void setDialogType(DialogType dialogType) {
    this.dialogType = dialogType;
  }

  final Container getRootPanel() {
    return rootPanel;
  }

  private final void performAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnResume.getText()) {
      parent.resume();
    }
  }

  final void setVisible(boolean visible) {
    this.visible = visible;
    rootPanel.setVisible(visible);
  }

  public final int getCpusSelected() {
    return processorTable.getCpusSelected();
  }
  
  /**
   * pass the pause button to the axis process panel, to be managed like the
   * kill process button
   */
  public final void setupParallelProgressDisplay() {
    manager.getMainPanel().setPauseButton(btnPause, axisID);
  }
  
  /**
   * remove the pause button from the axis process panel
   */
  public final void teardownParallelProgressDisplay() {
    manager.getMainPanel().deletePauseButton(btnPause, axisID);
  }

  public final void addRestart(String computer) {
    processorTable.addRestart(computer);
  }
  
  public final void drop(String computer) {
    processorTable.drop(computer);
  }

  public final void addSuccess(String computer) {
    processorTable.addSuccess(computer);
  }
  
  final void resetResults() {
    processorTable.resetResults();
  }
  
  public final void setEnabledResume(boolean enabled) {
    btnResume.setEnabled(enabled);
  }
  
  final boolean getParameters(SplittiltParam param) {
    ConstEtomoNumber numMachines = param.setNumMachines(ltfCpusSelected
        .getText());
    if (!numMachines.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(ltfCpusSelected.getLabel() + " "
          + numMachines.getInvalidReason() + "\n"
          + processorTable.getHelpMessage(), "Table Error", axisID);
      return false;
    }
    return true;
  }
  
  final void getParameters(ProcesschunksParam param) {
     param.setNice(nice.getValue());
     processorTable.getParameters(param);
  }
  
  final void pack() {
    if (!visible) {
      return;
    }
    processorTable.pack();
  }

  private final class ParallelPanelActionListener implements ActionListener {
    ParallelPanel adaptee;

    ParallelPanelActionListener(ParallelPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.performAction(event);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.7  2005/08/01 18:11:31  sueh
 * <p> bug# 532 Changed ProcessorTableRow.signalRestart() to addRestart.
 * <p> Added nice spinner.  Added getParameters(ProcesschunksParam).
 * <p>
 * <p> Revision 1.6  2005/07/29 00:54:29  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.5  2005/07/21 22:21:07  sueh
 * <p> bug# 532 Implementing setup and teardownParallelProgressDisplay() for
 * <p> ParallelProgressDisplay so that the pause button can function like a kill
 * <p> button.
 * <p>
 * <p> Revision 1.4  2005/07/11 23:12:20  sueh
 * <p> bug# 619 Removed the split, start, and kill buttons.  Split and start are
 * <p> now handled by the parent dialog.  Kill is handled by progress panel.
 * <p> Added randomization to be used by the demo monitor.  Added functions:
 * <p> buildRandomizerLists, getCpusSelected, resetResults,
 * <p> setEnabledResume, setVisible, signalRandomRestart,
 * <p> signalRandomSuccess, signalStartProgress.  Removed totalResults().
 * <p>
 * <p> Revision 1.3  2005/07/06 23:45:38  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 1.2  2005/07/01 23:04:26  sueh
 * <p> bug# 619 removed parent dialog from constructor
 * <p>
 * <p> Revision 1.1  2005/07/01 21:21:23  sueh
 * <p> bug# 619 Panel containing parallel processing
 * <p> </p>
 */