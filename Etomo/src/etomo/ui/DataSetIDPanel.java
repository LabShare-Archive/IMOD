package etomo.ui;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;

import javax.swing.*;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$ </p>
 */
public class DataSetIDPanel {
  public static final String rcsid = "$Id$";

  JPanel panelDataSetID = new JPanel();

  LabeledTextField ltfWorkingDirectory  =
    new LabeledTextField("Working directory:");

  LabeledTextField ltfDataSet  = new LabeledTextField("Data Set:");

  LabeledTextField ltfBackupDir  = new LabeledTextField("Backup Directory:");

  public DataSetIDPanel() {
    setToolTipText();
    //
    //  Set the data set label sizes and state
    //
    ltfWorkingDirectory.setTextPreferredSize(new Dimension(80, 19));
    ltfWorkingDirectory.setEditable(false);

    //
    //  Set the data set label sizes and state
    //
    ltfDataSet.setTextPreferredSize(new Dimension(80, 19));
    ltfDataSet.setEditable(false);

    //
    //  Set the backup directory label sizes and state
    //
    ltfBackupDir.setTextPreferredSize(new Dimension(80, 19));
    ltfBackupDir.setEditable(false);

    panelDataSetID.setLayout(new BoxLayout(panelDataSetID, BoxLayout.X_AXIS));
    panelDataSetID.setMaximumSize(new Dimension(2000, 100));
	panelDataSetID.add(Box.createRigidArea(FixedDim.x5_y0));
    panelDataSetID.add(ltfWorkingDirectory.getContainer());
	  panelDataSetID.add(Box.createRigidArea(FixedDim.x10_y0));
    panelDataSetID.add(ltfDataSet.getContainer());
	panelDataSetID.add(Box.createRigidArea(FixedDim.x10_y0));
    panelDataSetID.add(ltfBackupDir.getContainer());
	  panelDataSetID.add(Box.createRigidArea(FixedDim.x5_y0));

  }

  public void setWorkingDirectory(String workingDirectory) {
    //textFieldWorkingDirectory.setText(workingDirectory);
    ltfWorkingDirectory.setText(workingDirectory);
  }

  public void setDataSetName(String dataSetName) {
    ltfDataSet.setText(dataSetName);
  }

  public void setBackupDirectory(String backupDirectory) {
    ltfBackupDir.setText(backupDirectory);
  }

  public JPanel getPanel() {
    return panelDataSetID;
  }

  public void addMouseListener(MouseListener listener) {
    panelDataSetID.addMouseListener(listener);
    ltfWorkingDirectory.addMouseListener(listener);
    ltfDataSet.addMouseListener(listener);
    ltfBackupDir.addMouseListener(listener);
  }

  private void setToolTipText(){
	ltfWorkingDirectory.setToolTipText(
      "<html>This specifies the working directory where the input<br>data is stored and the command files and results are<br>placed.<br><br>To set this parameter run the setup process below<br>to start a new tomogram.");

    ltfDataSet.setToolTipText(
      "<html>This specifies the data set to use within the working<br>directory.  For single axis data sets ommit the .st<br>from the filename.  For dual axis data sets ommit the<br>a.st from the file name.<br><br>To set this parameter run the setup process below<br>to start a new tomogram.");

    ltfBackupDir.setToolTipText(
       "<html>This specifies where the command files and small result<br>files are backed up.<br><br>To set this parameter run the setup process below<br>to start a new tomogram.");
  }
}
