package etomo.ui;

import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.type.AxisID;

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
 * <p> $Log$
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TomogramCombinationDialog extends ProcessDialog {
  public static final String rcsid =
    "$Id$";

  ApplicationManager applicationManager;
  JPanel contentPane;
  SetupCombinePanel panelSetupCombine = new SetupCombinePanel();

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;
    contentPane = (JPanel) getContentPane();
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    setTitle("eTomo Tomogram Combination");

    contentPane.add(panelSetupCombine.getContainer());
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //
    // Calcute the necessary window size
    //
    pack();

    //  FIXME need a better way to get the panels to be the same size in
    //  setupcombine pane
    // panelSetupCombine.sizePanels();
    // pack();

    //  Bind the SetupCombinePanel buttons to the appropriate actions
    panelSetupCombine.addImodAActionListener(
      new CombineDialogImodAAdapter(this));

    panelSetupCombine.addImodBActionListener(
      new CombineDialogImodBAdapter(this));

    panelSetupCombine.addCreateActionListener(
      new CombineDialogCreateActionAdapter(this));
  }

  public void setCombineParams(ConstCombineParams combineParams) {
    panelSetupCombine.setParameters(combineParams);
  }

  public void getCombineParams(CombineParams combineParams)
    throws NumberFormatException {
    panelSetupCombine.getParameters(combineParams);
  }

  //  Action functions for setup panel buttons
  void buttonImodAAction(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.imodTomogram(AxisID.FIRST);
    }
    else {
      applicationManager.imodTomogram(AxisID.ONLY);
    }
  }

  void buttonImodBAction(ActionEvent event) {
    applicationManager.imodTomogram(AxisID.SECOND);
  }

  void buttonCreateAction(ActionEvent event) {
    applicationManager.createCombineScripts(this);
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramCombinationDialog(this);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramCombinationDialog(this);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramCombinationDialog(this);
  }

}

//  Action adapter classes
class CombineDialogImodAAdapter implements ActionListener {

  TomogramCombinationDialog adaptee;

  public CombineDialogImodAAdapter(TomogramCombinationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodAAction(event);
  }
}
class CombineDialogImodBAdapter implements ActionListener {

  TomogramCombinationDialog adaptee;

  public CombineDialogImodBAdapter(TomogramCombinationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodBAction(event);
  }
}

class CombineDialogCreateActionAdapter implements ActionListener {

  TomogramCombinationDialog adaptee;

  public CombineDialogCreateActionAdapter(TomogramCombinationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCreateAction(event);
  }
}
