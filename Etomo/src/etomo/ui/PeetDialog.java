package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.PeetManager;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.MatlabParamFile;
import etomo.type.AxisID;
import etomo.type.ConstPeetMetaData;
import etomo.type.ConstPeetScreenState;
import etomo.type.DialogType;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.7  2007/03/15 21:48:09  sueh
 * <p> bug# 964 Added setParameters(MatlabParamFile).
 * <p>
 * <p> Revision 1.6  2007/03/01 01:41:46  sueh
 * <p> bug# 964 Added initialize() to sets metadata fields that are only set once.
 * <p>
 * <p> Revision 1.5  2007/02/22 20:38:40  sueh
 * <p> bug# 964 Added a button to the Directory field.
 * <p>
 * <p> Revision 1.4  2007/02/21 22:30:22  sueh
 * <p> bug# 964 Fixing null pointer exception which occurred when loading the .epe file.
 * <p>
 * <p> Revision 1.3  2007/02/21 04:24:32  sueh
 * <p> bug# 964 Setting Output and Directory when Save As is called.  Disabling edit
 * <p> for Output and Directory when the paramFile is set.
 * <p>
 * <p> Revision 1.2  2007/02/20 20:36:46  sueh
 * <p> bug# 964 Started the setup panel.
 * <p>
 * <p> Revision 1.1  2007/02/19 22:03:19  sueh
 * <p> bug# 964 Dialog for PEET interface.
 * <p> </p>
 */

public final class PeetDialog implements AbstractParallelDialog, Expandable {
  public static final String rcsid = "$Id$";

  static final String DIRECTORY_LABEL = "Directory";
  static final String OUTPUT_LABEL = "Output";

  private static final DialogType DIALOG_TYPE = DialogType.PEET;
  private final JPanel rootPanel= new JPanel();
  private final VolumeTable volumeTable;
  private final PeetManager manager;
  private final AxisID axisID;

  private final PanelHeader setupHeader;
  private final FileTextField ftfDirectory= new FileTextField(
      DIRECTORY_LABEL + ": ");
  private final LabeledTextField ltfOutput= new LabeledTextField(OUTPUT_LABEL
      + ": ");
  private final SpacedPanel pnlSetupBody= new SpacedPanel();
  private final RadioButton rbInitMotlZero;
  private final RadioButton rbInitMotlZAxis;
  private final RadioButton rbInitMotlXAndZAxis;
  private final RadioButton rbInitMotlFiles;
  
  private PeetDialog(final PeetManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    //construnction
    volumeTable= VolumeTable.getInstance(manager,this);
    setupHeader = PanelHeader.getInstance("Setup", this, DIALOG_TYPE);
    //Init MOTL
    JPanel pnlInitMotl = new JPanel();
    pnlInitMotl.setLayout(new BoxLayout(pnlInitMotl,BoxLayout.Y_AXIS));
    pnlInitMotl.setBorder(new EtchedBorder("Initial Motive List").getBorder());
    pnlInitMotl.setAlignmentX(Component.CENTER_ALIGNMENT);
    ButtonGroup group = new ButtonGroup();
    rbInitMotlZero = new RadioButton("Set all rotational values to zero",0,group);
    rbInitMotlZAxis = new RadioButton("Initialize Z axis",1,group);
    rbInitMotlXAndZAxis  = new RadioButton("Initialize X and Z axis",2,group);
    rbInitMotlFiles = new RadioButton("Use files",group);
    rbInitMotlZero.setSelected(true);
    pnlInitMotl.add(rbInitMotlZero.getComponent());
    pnlInitMotl.add(rbInitMotlZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlXAndZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlFiles.getComponent());
    //setup
    pnlSetupBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetupBody.add(ftfDirectory.getContainer());
    pnlSetupBody.add(ltfOutput.getContainer());
    pnlSetupBody.add(volumeTable.getContainer());
    //setup header
    SpacedPanel pnlSetup= new SpacedPanel();
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetup.setBorder(BorderFactory.createEtchedBorder());
    pnlSetup.add(setupHeader.getContainer());
    pnlSetup.add(pnlSetupBody);
    //root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new EtchedBorder("PEET").getBorder());
    rootPanel.add(pnlSetup.getContainer());
    rootPanel.add(pnlInitMotl);
  }
  
  public static PeetDialog getInstance(final PeetManager manager, final AxisID axisID) {
    PeetDialog instance = new PeetDialog(manager,axisID);
    instance.addListeners();
    return instance;
  }

  public void updateDisplay(final boolean paramFileSet) {
    ftfDirectory.setEnabled(!paramFileSet);
    ltfOutput.setEditable(!paramFileSet);
  }

  public Container getContainer() {
    return rootPanel;
  }

  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }
  
  public void getParameters(final ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
    processchunksParam.setRootName(ltfOutput.getText());
  }
  
  public void getParameters(final PeetScreenState screenState) {
    setupHeader.getState(screenState.getPeetSetupHeaderState());
  }
  
  public void setParameters(final ConstPeetScreenState screenState) {
    setupHeader.setState(screenState.getPeetSetupHeaderState());
  }
  
  public void setParameters(final ConstPeetMetaData metaData) {
    ltfOutput.setText(metaData.getName());
  }
  
  public void setParameters(final MatlabParamFile matlabParamFile) {
    volumeTable.setParameters(matlabParamFile);
    MatlabParamFile.InitMotlCode initMotlCode = matlabParamFile.getInitMotlCode();
    if (initMotlCode==null) {
      rbInitMotlFiles.setSelected(true);
    }
    else if (initMotlCode==MatlabParamFile.InitMotlCode.ZERO) {
      rbInitMotlZero.setSelected(true);
    }
    else if (initMotlCode==MatlabParamFile.InitMotlCode.Z_AXIS) {
      rbInitMotlZAxis.setSelected(true);
    }
    else if (initMotlCode==MatlabParamFile.InitMotlCode.X_AND_Z_AXIS) {
      rbInitMotlXAndZAxis.setSelected(true);
    }
  }
  
  public void initialize(final PeetMetaData metaData) {
    metaData.setName(ltfOutput.getText());
  }

  public boolean usingParallelProcessing() {
    return true;
  }

  public void expand(final ExpandButton button) {
    if (setupHeader != null) {
      if (setupHeader.equalsOpenClose(button)) {
        pnlSetupBody.setVisible(button.isExpanded());
      }
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }
  
  public String getDirectory() {
    return ftfDirectory.getText();
  }
  
  public void setDirectory(final String directory) {
    ftfDirectory.setText(directory);
  }
  
  public void setOutput(final String output) {
    ltfOutput.setText(output);
  }
  
  void setUsingInitMotlFile() {
    rbInitMotlFiles.setSelected(true);
  }
  
  private void action(ActionEvent action) {
    String actionCommand = action.getActionCommand();
    if (actionCommand.equals(rbInitMotlZero.getActionCommand())) {
      volumeTable.setUsingInitMotlFile(false);
    }
    else if (actionCommand.equals(rbInitMotlZAxis.getActionCommand())) {
      volumeTable.setUsingInitMotlFile(false);
    }
    else if (actionCommand.equals(rbInitMotlXAndZAxis.getActionCommand())) {
      volumeTable.setUsingInitMotlFile(false);
    }
    else if (actionCommand.equals(rbInitMotlFiles.getActionCommand())) {
      volumeTable.setUsingInitMotlFile(true);
    }
  }
  
  private void directoryAction() {
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      ftfDirectory.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }
  
  private void addListeners() {
    ftfDirectory.addActionListener(new DirectoryActionListener(this));
    PDActionListener actionListener = new PDActionListener(this);
    rbInitMotlZero.addActionListener(actionListener);
    rbInitMotlZAxis.addActionListener(actionListener);
    rbInitMotlXAndZAxis.addActionListener(actionListener);
    rbInitMotlFiles.addActionListener(actionListener);
  }
  
  private class DirectoryActionListener implements ActionListener {
    private PeetDialog peetDialog;

    private DirectoryActionListener(final PeetDialog peetDialog) {
      this.peetDialog = peetDialog;
    }

    public void actionPerformed(final ActionEvent event) {
      peetDialog.directoryAction();
    }
  }
  
  private class PDActionListener implements ActionListener{
    private final PeetDialog peetDialog;
    
    private PDActionListener(final PeetDialog peetDialog) {
      this.peetDialog=peetDialog;
    }
    
    public void actionPerformed(final ActionEvent event) {
      peetDialog.action(event);
    }
  }
}
