package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JScrollPane;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.DialogType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class ParallelPanel {
  public static final String rcsid = "$Id$";

  private SpacedPanel rootPanel = new SpacedPanel();
  private ProcessorTable processorTable = new ProcessorTable(this);
  private MultiLineButton btnSplit = new MultiLineButton(
      "Save & Split Command Files");
  private MultiLineButton btnStart = new MultiLineButton("Start");
  private MultiLineButton btnKill = new MultiLineButton("Kill / Pause");
  private MultiLineButton btnResume = new MultiLineButton("Resume");
  private MultiLineButton btnSaveDefaults = new MultiLineButton("Save Defaults");
  private LabeledTextField ltfCpusSelected = new LabeledTextField("CPUs selected: ");
  private LabeledTextField ltfChunksFinished = new LabeledTextField("Chunks finished: ");
  
  private AxisID axisID = null;
  private DialogType dialogType = null;
  private ParallelPanelActionListener actionListener = new ParallelPanelActionListener(
      this);

  ParallelPanel(AxisID axisID) {
    this.axisID = axisID;
    //set listeners
    btnSplit.addActionListener(actionListener);
    btnStart.addActionListener(actionListener);
    btnKill.addActionListener(actionListener);
    btnResume.addActionListener(actionListener);
    //panels and panes
    rootPanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel westPanel = new SpacedPanel();
    westPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel eastPanel = new SpacedPanel();
    eastPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel centerWestPanel = new SpacedPanel();
    centerWestPanel.setBoxLayout(BoxLayout.Y_AXIS);
    JScrollPane scrollPane = new JScrollPane(centerWestPanel.getContainer());
    SpacedPanel southWestPanel = new SpacedPanel();
    southWestPanel.setBoxLayout(BoxLayout.X_AXIS);
    //southWestPanel
    southWestPanel.add(btnSplit);
    southWestPanel.add(btnStart);
    southWestPanel.add(btnResume);
    southWestPanel.add(btnSaveDefaults);
    //centerWestPanel
    centerWestPanel.add(processorTable.getRootPanel());
    //eastPanel
    eastPanel.add(ltfChunksFinished);
    eastPanel.add(ltfCpusSelected);
    eastPanel.add(btnKill);
    //westPanel
    JLabel label = new JLabel("Processor Table");
    label.setAlignmentX(Component.CENTER_ALIGNMENT);
    westPanel.add(label);
    westPanel.add(scrollPane);
    westPanel.add(southWestPanel);
    //rootPanel
    rootPanel.add(westPanel);
    rootPanel.add(eastPanel);
    //configure fields
    btnStart.setEnabled(false);
    btnResume.setEnabled(false);
    ltfChunksFinished.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfChunksFinished.setEditable(false);
    ltfCpusSelected.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfCpusSelected.setEditable(false);
    processorTable.signalCpusSelectedChanged();
    btnKill.setEnabled(false);
  }

  private void totalResults() {
    ltfChunksFinished.setText(processorTable.getTotalSuccesses());
  }
  
  void signalCpusSelectedChanged(int cpusSelected) {
    ltfCpusSelected.setText(cpusSelected);
  }

  void setDialogType(DialogType dialogType) {
    this.dialogType = dialogType;
  }

  Container getRootPanel() {
    return rootPanel.getContainer();
  }

  private void performAction(ActionEvent event) {
    String command = event.getActionCommand();
    ApplicationManager mgr = EtomoDirector.getInstance()
        .getCurrentReconManager();
    if (command == btnSplit.getText() && mgr.dummySplitParallelProcess(axisID)) {
      btnSplit.setEnabled(true);
      btnStart.setEnabled(true);
      btnKill.setEnabled(false);
      btnResume.setEnabled(false);
      processorTable.resetResults();
      totalResults();
    }
    else if (command == btnStart.getText() && mgr.dummyParallelProcess(axisID)) {
      btnSplit.setEnabled(false);
      btnStart.setEnabled(false);
      btnKill.setEnabled(true);
      btnResume.setEnabled(false);
      processorTable.resetResults();
      processorTable.showResults();
      totalResults();
    }
    else if (command == btnKill.getText()) {
      btnSplit.setEnabled(true);
      btnStart.setEnabled(true);
      btnKill.setEnabled(false);
      btnResume.setEnabled(true);
    }
    else if (command == btnResume.getText() && mgr.dummyParallelProcess(axisID)) {
      btnSplit.setEnabled(false);
      btnStart.setEnabled(false);
      btnKill.setEnabled(true);
      btnResume.setEnabled(false);
      processorTable.showResults();
      totalResults();
    }
  }

  private class ParallelPanelActionListener implements ActionListener {
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
 * <p> Revision 1.2  2005/07/01 23:04:26  sueh
 * <p> bug# 619 removed parent dialog from constructor
 * <p>
 * <p> Revision 1.1  2005/07/01 21:21:23  sueh
 * <p> bug# 619 Panel containing parallel processing
 * <p> </p>
 */