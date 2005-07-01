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

  private SpacedPanel rootPanel = new SpacedPanel(FixedDim.x10_y0, true);
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
    rootPanel.setLayout(new BoxLayout(rootPanel.getContainer(), BoxLayout.X_AXIS));
    SpacedPanel eastPanel = new SpacedPanel(FixedDim.x0_y10, true);
    eastPanel.setLayout(new BoxLayout(eastPanel.getContainer(), BoxLayout.Y_AXIS));
    SpacedPanel westPanel = new SpacedPanel(FixedDim.x0_y10);
    westPanel.setLayout(new BoxLayout(westPanel.getContainer(), BoxLayout.Y_AXIS));
    SpacedPanel centerEastPanel = new SpacedPanel(FixedDim.x0_y10);
    centerEastPanel.setLayout(new BoxLayout(centerEastPanel.getContainer(),
        BoxLayout.Y_AXIS));
    JScrollPane scrollPane = new JScrollPane(centerEastPanel.getContainer());
    SpacedPanel southEastPanel = new SpacedPanel(FixedDim.x10_y0);
    southEastPanel.setLayout(new BoxLayout(southEastPanel.getContainer(), BoxLayout.X_AXIS));
    //southEastPanel
    southEastPanel.addMultiLineButton(btnSplit);
    southEastPanel.addMultiLineButton(btnStart);
    southEastPanel.addMultiLineButton(btnResume);
    southEastPanel.addMultiLineButton(btnSaveDefaults);
    //centerEastPanel
    centerEastPanel.add(processorTable.getRootPanel());
    //westPanel
    westPanel.add(ltfChunksFinished);
    westPanel.add(ltfCpusSelected);
    westPanel.addMultiLineButton(btnKill);
    //eastPanel
    JLabel label = new JLabel("Processor Table");
    label.setAlignmentX(Component.CENTER_ALIGNMENT);
    eastPanel.add(label);
    eastPanel.add(scrollPane);
    eastPanel.add(southEastPanel);
    //rootPanel
    rootPanel.add(eastPanel);
    rootPanel.add(westPanel);
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
    return rootPanel.getRootPanel();
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
 * <p> Revision 1.1  2005/07/01 21:21:23  sueh
 * <p> bug# 619 Panel containing parallel processing
 * <p> </p>
 */