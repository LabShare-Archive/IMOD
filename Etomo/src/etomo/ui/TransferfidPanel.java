package etomo.ui;

import java.awt.Container;
import javax.swing.*;
import javax.swing.JCheckBox;

import etomo.comscript.TransferfidParam;

/**
 * @author rickg
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class TransferfidPanel {
  public static final String rcsid = "$Id$";


/*  String inputImageFile = "";
  String outputImageFile = "";
  String inputModelFile = "";
  String outputModelFile = "";
*/
  JPanel  panelTransferfid = new JPanel();
  
  JCheckBox chkRunMidas = new JCheckBox("Run midas");

  String logSuffix;  

  public TransferfidPanel(String suffix) {
    logSuffix = suffix;
    
    panelTransferfid.setLayout(new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.add(chkRunMidas);

  }
  
  public void getParameters(TransferfidParam params) {
    chkRunMidas.setSelected(params.isRunMidas());
  }
  
  public void setParameter(TransferfidParam params) {
    params.setRunMidas(chkRunMidas.isSelected());
  }
  
  public Container getContainer() {
    return panelTransferfid;
  }
  
    
}
