package etomo.ui;

import java.awt.Container;

import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.DialogType;

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
* <p> $Log$ </p>
*/

public final class PeetDialog implements AbstractParallelDialog {
  public static  final String  rcsid =  "$Id$";
  
  private static final DialogType DIALOG_TYPE = DialogType.PEET;
  private final LabeledTextField ltfOutput=new LabeledTextField("Output: ");
  private JPanel rootPanel = null;
  
  public PeetDialog(BaseManager manager, AxisID axisID) {
  }
  
  public void setParameters(BaseMetaData metaData) {
  }
  
  public void updateDisplay() {
  }
  
  public Container getContainer() {
    createDialog();
    return rootPanel;
  }
  
  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }
  
  public void getParameters(ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
    processchunksParam.setRootName(ltfOutput.getText());
  }
  
  public final boolean usingParallelProcessing() {
    return true;
  }
  
  private void createDialog() {
    if (rootPanel!=null) {
      return;
    }
    rootPanel=new JPanel();
    rootPanel.add(ltfOutput.getContainer());
  }
}
