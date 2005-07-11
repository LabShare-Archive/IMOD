package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
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
  private ProcessorTable processorTable = new ProcessorTable(this);
  private MultiLineButton btnResume = new MultiLineButton("Resume");
  private MultiLineButton btnSaveDefaults = new MultiLineButton("Save As Defaults");
  private LabeledTextField ltfCpusSelected = new LabeledTextField(
      "CPUs selected: ");
  private LabeledTextField ltfChunksFinished = new LabeledTextField(
      "Chunks finished: ");
  private ArrayList randomRestarts = new ArrayList();
  private ArrayList randomSuccesses = new ArrayList();
  private Random random = new Random(new Date().getTime());

  private AxisID axisID = null;
  private ParallelDialog parent = null;
  private DialogType dialogType = null;
  private ParallelPanelActionListener actionListener = new ParallelPanelActionListener(
      this);
  private PanelHeader header;

  ParallelPanel(ParallelDialog parent, AxisID axisID) {
    this.parent = parent;
    this.axisID = axisID;
    //set listeners
    btnResume.addActionListener(actionListener);
    //panels
    rootPanel = new JPanel();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    SpacedPanel bodyPanel = new SpacedPanel();
    bodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel southPanel = new SpacedPanel();
    southPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    header = new PanelHeader(axisID, "Parallel Processing", bodyPanel);
    //southPanel;
    southPanel.add(ltfCpusSelected);
    southPanel.add(btnResume);
    southPanel.add(btnSaveDefaults);
    //bodyPanel
    bodyPanel.addRigidArea();
    bodyPanel.add(processorTable.getContainer());
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
    ApplicationManager mgr = EtomoDirector.getInstance()
        .getCurrentReconManager();
    if (command == btnResume.getText()) {
      parent.resume();
    }
  }

  final void setVisible(boolean visible) {
    rootPanel.setVisible(visible);
    processorTable.setSize();
  }

  public final int getCpusSelected() {
    return processorTable.getCpusSelected();
  }
  
  public final void signalStartProgress() {
    buildRandomizerLists();
  }

  private final void buildRandomizerLists() {
    int index = processorTable.getFirstSelectedIndex();
    while (index != -1) {
      for (int i = 0; i < processorTable.getRestartFactor(index); i++) {
        randomRestarts.add(new Integer(index));
      }
      for (int i = 0; i < processorTable.getSuccessFactor(index); i++) {
        randomSuccesses.add(new Integer(index));
      }
      index = processorTable.getNextSelectedIndex(index);
    }
  }

  public final void signalRandomRestart() {
    if (randomRestarts == null) {
      return;
    }
    int randomRestartsSize = randomRestarts.size();
    if (randomRestartsSize == 0) {
      return;
    }
    int randomIndex = random.nextInt(randomRestarts.size());
    processorTable.signalRestart(((Integer) randomRestarts.get(randomIndex)).intValue());
  }

  public final void signalRandomSuccess() {
    if (randomSuccesses == null) {
      return;
    }
    int randomSuccessesSize = randomSuccesses.size();
    if (randomSuccessesSize == 0) {
      return;
    }
    int randomIndex = random.nextInt(randomSuccesses.size());
    processorTable.signalSuccess(((Integer) randomSuccesses.get(randomIndex)).intValue());
  }
  
  final void resetResults() {
    processorTable.resetResults();
  }
  
  public final void setEnabledResume(boolean enabled) {
    btnResume.setEnabled(enabled);
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