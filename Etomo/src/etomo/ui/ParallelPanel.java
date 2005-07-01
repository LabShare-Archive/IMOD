package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
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
  public static  final String  rcsid =  "$Id$";
  
  private SpacedPanel rootPanel = new SpacedPanel(FixedDim.x0_y10, true);
  private ProcessorTable processorTable = new ProcessorTable();
  private MultiLineButton btnSplit = new MultiLineButton("Save & Split Command Files");
  private MultiLineButton btnStart = new MultiLineButton("Start");
  private MultiLineButton btnKill = new MultiLineButton("Kill / Pause");
  private MultiLineButton btnResume = new MultiLineButton("Resume");
  private JDialog dialogParent = null;
  
  private AxisID axisID = null;
  private DialogType dialogType = null;
  private ParallelPanelActionListener actionListener = new ParallelPanelActionListener(this);
  
  ParallelPanel(JDialog dialogParent, AxisID axisID) {
    this.dialogParent = dialogParent;
    this.axisID = axisID;
    //set listeners
    btnSplit.addActionListener(actionListener);
    btnStart.addActionListener(actionListener);
    btnKill.addActionListener(actionListener);
    btnResume.addActionListener(actionListener);
    //root
    rootPanel.setLayout(new BoxLayout(rootPanel.getContainer(), BoxLayout.Y_AXIS));
    //table
    SpacedPanel panel1 = new SpacedPanel(FixedDim.x10_y0, true);
    panel1.setLayout(new BoxLayout(panel1.getContainer(), BoxLayout.X_AXIS));
    SpacedPanel tablePanel = new SpacedPanel(FixedDim.x0_y10);
    tablePanel.setLayout(new BoxLayout(tablePanel.getContainer(), BoxLayout.Y_AXIS));
    tablePanel.add(processorTable.getRootPanel());
    JScrollPane tableScrollPane = new JScrollPane(tablePanel.getContainer());
    panel1.add(tableScrollPane);
    //side buttons
    SpacedPanel panel2 = new SpacedPanel(FixedDim.x0_y10);
    panel2.setLayout(new BoxLayout(panel2.getContainer(), BoxLayout.Y_AXIS));
    btnKill.setEnabled(false);
    panel2.addMultiLineButton(btnKill);
    btnResume.setEnabled(false);
    panel2.addMultiLineButton(btnResume);
    panel1.add(panel2);
    rootPanel.add(panel1);
    //bottom buttons
    SpacedPanel panel3 = new SpacedPanel(FixedDim.x10_y0);
    panel3.setLayout(new BoxLayout(panel3.getContainer(), BoxLayout.X_AXIS));
    panel3.addMultiLineButton(btnSplit);
    btnStart.setEnabled(false);
    panel3.addMultiLineButton(btnStart);
    rootPanel.add(panel3);
  }
  
  void setDialogType(DialogType dialogType) {
    this.dialogType = dialogType;
  }
  
  Container getRootPanel() {
    return rootPanel.getRootPanel();
  }
  
  private void performAction(ActionEvent event) {
    String command = event.getActionCommand();
    ApplicationManager mgr = EtomoDirector.getInstance().getCurrentReconManager();
    if (command == btnSplit.getText() && mgr.dummySplitParallelProcess(axisID)) {
      btnSplit.setEnabled(true);
      btnStart.setEnabled(true);
      btnKill.setEnabled(false);
      btnResume.setEnabled(false);
      processorTable.resetResults();
    }
    else if (command == btnStart.getText() && mgr.dummyParallelProcess(axisID)) {
      btnSplit.setEnabled(false);
      btnStart.setEnabled(false);
      btnKill.setEnabled(true);
      btnResume.setEnabled(false);
      processorTable.resetResults();
      processorTable.showResults();
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
* <p> $Log$ </p>
*/