package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;

import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.TiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;

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
public class CoarseAlignDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager;

  private JPanel contentPane;
  private JPanel panelCoarseAlign = new JPanel();

  private JPanel panelCoarseAlignA = new JPanel();
  private BeveledBorder borderA = new BeveledBorder("Axis: A");
  private CrossCorrelationPanel panelCrossCorrelationA;
  private JToggleButton buttonCoarseAlignA =
    new JToggleButton("<html><b>Generate coarse<br>aligned stack</b>");
  private JToggleButton buttonImodA =
    new JToggleButton("<html><b>View aligned<br>stack in imod</b>");
  private JToggleButton buttonMidasA =
     new JToggleButton("<html><b>Fix alignment<br>with Midas</b>");

  private JPanel panelCoarseAlignB = new JPanel();
  private BeveledBorder borderB = new BeveledBorder("Axis: B");
  private CrossCorrelationPanel panelCrossCorrelationB;
  private JToggleButton buttonCoarseAlignB =
    new JToggleButton("<html><b>Generate coarse<br>aligned stack</b>");
  private JToggleButton buttonImodB =
     new JToggleButton("<html><b>View aligned<br>stack in imod</b>");
  private JToggleButton buttonMidasB =
     new JToggleButton("<html><b>Fix alignment<br>with Midas</b>");


  public CoarseAlignDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;

    contentPane = (JPanel) getContentPane();
    setTitle("eTomo Coarse Alignment");


    if(applicationManager.isDualAxis()) {
      panelCrossCorrelationA = new CrossCorrelationPanel("a");
    }
    else {
      panelCoarseAlignB.setVisible(false);
      panelCrossCorrelationA = new CrossCorrelationPanel("");
    }
    panelCrossCorrelationB = new CrossCorrelationPanel("b");

    buttonExecute.setText("Done");

    buttonCoarseAlignA.setAlignmentX(0.5F);
    buttonCoarseAlignA.setPreferredSize(FixedDim.button2Line);
    buttonCoarseAlignA.setMaximumSize(FixedDim.button2Line);

    buttonCoarseAlignB.setAlignmentX(0.5F);
    buttonCoarseAlignB.setPreferredSize(FixedDim.button2Line);
    buttonCoarseAlignB.setMaximumSize(FixedDim.button2Line);

    buttonImodA.setAlignmentX(0.5F);
    buttonImodA.setPreferredSize(FixedDim.button2Line);
    buttonImodA.setMaximumSize(FixedDim.button2Line);

    buttonImodB.setAlignmentX(0.5F);
    buttonImodB.setPreferredSize(FixedDim.button2Line);
    buttonImodB.setMaximumSize(FixedDim.button2Line);

    buttonMidasA.setAlignmentX(0.5F);
    buttonMidasA.setPreferredSize(FixedDim.button2Line);
    buttonMidasA.setMaximumSize(FixedDim.button2Line);

    buttonMidasB.setAlignmentX(0.5F);
    buttonMidasB.setPreferredSize(FixedDim.button2Line);
    buttonMidasB.setMaximumSize(FixedDim.button2Line);

    panelCoarseAlignA.setLayout(
      new BoxLayout(panelCoarseAlignA, BoxLayout.Y_AXIS));
    panelCoarseAlignA.setBorder(borderA.getBorder());
    panelCoarseAlignA.add(panelCrossCorrelationA.getPanel());
    panelCoarseAlignA.add(Box.createRigidArea(FixedDim.x0_y5));
    panelCoarseAlignA.add(buttonCoarseAlignA);
    panelCoarseAlignA.add(Box.createRigidArea(FixedDim.x0_y5));
    panelCoarseAlignA.add(buttonImodA);
    panelCoarseAlignA.add(Box.createRigidArea(FixedDim.x0_y5));
    panelCoarseAlignA.add(buttonMidasA);

    panelCoarseAlignB.setLayout(
      new BoxLayout(panelCoarseAlignB, BoxLayout.Y_AXIS));
    panelCoarseAlignB.setBorder(borderB.getBorder());
    panelCoarseAlignB.add(panelCrossCorrelationB.getPanel());
    panelCoarseAlignB.add(Box.createRigidArea(FixedDim.x0_y5));
    panelCoarseAlignB.add(buttonCoarseAlignB);
    panelCoarseAlignB.add(Box.createRigidArea(FixedDim.x0_y5));
    panelCoarseAlignB.add(buttonImodB);
    panelCoarseAlignB.add(Box.createRigidArea(FixedDim.x0_y5));
    panelCoarseAlignB.add(buttonMidasB);

    panelCoarseAlign.setLayout(new BoxLayout(panelCoarseAlign, BoxLayout.X_AXIS));
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    contentPane.add(Box.createHorizontalGlue());
    panelCoarseAlign.add(panelCoarseAlignA);
    contentPane.add(Box.createHorizontalGlue());
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    panelCoarseAlign.add(panelCoarseAlignB);
    contentPane.add(Box.createHorizontalGlue());
    panelCoarseAlign.add(Box.createRigidArea(FixedDim.x10_y0));

    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    contentPane.add(panelCoarseAlign);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //
    //  Action listener assignments for the buttons
    //
    panelCrossCorrelationA.setButtonActionListener(new
      CoarseAlignDialogCrossCorrelateA(this));

    buttonCoarseAlignA.addActionListener(new
      CoarseAlignDialogCoarseAlignA(this));

    buttonImodA.addActionListener(new
      CoarseAlignDialogImodA(this));

    buttonMidasA.addActionListener(new
      CoarseAlignDialogMidasA(this));

    panelCrossCorrelationB.setButtonActionListener(new
      CoarseAlignDialogCrossCorrelateB(this));

    buttonCoarseAlignB.addActionListener(new
      CoarseAlignDialogCoarseAlignB(this));

    buttonImodB.addActionListener(new
      CoarseAlignDialogImodB(this));

    buttonMidasB.addActionListener(new
      CoarseAlignDialogMidasB(this));


    //  Set the default advanced state for the window
    //  FIXME: this needs to be defined by the options and
    //  the last state it was opened in
    panelCrossCorrelationA.setAdvanced(isAdvanced);
    panelCrossCorrelationB.setAdvanced(isAdvanced);

    //
    //  Mouse adapter for context menu
    //
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelCoarseAlign.addMouseListener(mouseAdapter);

    pack();
  }


  /**
   * Set the parameters for the specified cross correlation panel
   */
  public void setCrossCorrelationParams(ConstTiltxcorrParam tiltXcorrParams,
      AxisID axisID){
    if(axisID == AxisID.SECOND) {
      panelCrossCorrelationB.setParameters(tiltXcorrParams);
    }
    else {
      panelCrossCorrelationA.setParameters(tiltXcorrParams);
    }
  }

  /**
   * Get the parameters from the specified cross correlation panel
   */
  public void getCrossCorrelationParams(TiltxcorrParam tiltXcorrParams,
     AxisID axisID) throws FortranInputSyntaxException {
    if(axisID == AxisID.SECOND) {
      try {
	panelCrossCorrelationB.getParameters(tiltXcorrParams);
      }
      catch(FortranInputSyntaxException except) {
	String message = "Axis B: " + except.getMessage();
	throw new FortranInputSyntaxException(message);
      }

    }
    else {
      try {
	panelCrossCorrelationA.getParameters(tiltXcorrParams);
      }
      // FIXME specify exception
      catch(FortranInputSyntaxException except) {
	String message = "Axis A: " + except.getMessage();
	throw new FortranInputSyntaxException(message);
      }

    }
  }


  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    if(isAdvanced) {
      panelCrossCorrelationA.setAdvanced(true);
      panelCrossCorrelationB.setAdvanced(true);
    }
    else {
      panelCrossCorrelationA.setAdvanced(false);
      panelCrossCorrelationB.setAdvanced(false);
    }
    pack();
  }

  public void setCrossCorrelationStateA(boolean state) {
    panelCrossCorrelationA.setButtonState(state);
  }


  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel =
      {"xftoxg", "newst", "newstack", "imod", "midas"};
    String[] manPage =
      {"xftoxg.html", "newst.html", "newstack.html", "imod.html", "midas.html"};

    String[] logFileLabel;
    String[] logFile;
    if(applicationManager.isDualAxis()) {
      logFileLabel = new String[2];
      logFileLabel[0] = "prenewsta";
      logFileLabel[1] = "prenewstb";
      logFile = new String[2];
      logFile[0] = "prenewsta.log";
      logFile[1] = "prenewstb.log";
    }
    else {
      logFileLabel = new String[1];
      logFileLabel[0] = "prenewst";
      logFile = new String[1];
      logFile[0] = "prenewst.log";
    }

    ContextPopup contextPopup =
      new ContextPopup(panelCoarseAlign, mouseEvent,
	manPagelabel, manPage, logFileLabel, logFile);
  }

  //
  //  Action function for stack buttons
  //
  void buttonCrossCorrelateA(ActionEvent event) {
    panelCrossCorrelationA.setButtonState(true);
    if(applicationManager.isDualAxis()) {
      applicationManager.crossCorrelate(AxisID.FIRST, this);
    }
    else {
      applicationManager.crossCorrelate(AxisID.ONLY, this);
    }
  }

  void buttonCoarseAlignA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.coarseAlign(AxisID.FIRST);
    }
    else {
      applicationManager.coarseAlign(AxisID.ONLY);
    }
  }

  void buttonImodA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.imodAlign(AxisID.FIRST);
    }
    else {
      applicationManager.imodAlign(AxisID.ONLY);
    }
  }

  void buttonMidasA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.midasRawStack(AxisID.FIRST);
    }
    else {
      applicationManager.midasRawStack(AxisID.ONLY);
    }
  }

  void buttonCrossCorrelateB(ActionEvent event) {
    applicationManager.crossCorrelate(AxisID.SECOND, this);
  }

  void buttonCoarseAlignB(ActionEvent event) {
      applicationManager.coarseAlign(AxisID.SECOND);
  }

  void buttonImodB(ActionEvent event) {
    applicationManager.imodAlign(AxisID.SECOND);
  }

  void buttonMidasB(ActionEvent event) {
    applicationManager.midasRawStack(AxisID.SECOND);
  }

  //
  //  Action function overides for buttons
  //
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneCoarseAlignDialog(this);
  }


  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneCoarseAlignDialog(this);
  }


  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneCoarseAlignDialog(this);
  }


  public void setEnabledB(boolean state){
    buttonCoarseAlignB.setEnabled(state);
    buttonImodB.setEnabled(state);
    buttonMidasB.setEnabled(state);
  }
}

class CoarseAlignDialogCrossCorrelateA
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogCrossCorrelateA(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCrossCorrelateA(event);
  }
}

class CoarseAlignDialogCoarseAlignA
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogCoarseAlignA(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCoarseAlignA(event);
  }
}

class CoarseAlignDialogImodA
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogImodA(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodA(event);
  }
}

class CoarseAlignDialogMidasA
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogMidasA(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonMidasA(event);
  }
}

class CoarseAlignDialogCrossCorrelateB
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogCrossCorrelateB(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCrossCorrelateB(event);
  }
}

class CoarseAlignDialogCoarseAlignB
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogCoarseAlignB(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCoarseAlignB(event);
  }
}

class CoarseAlignDialogImodB
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogImodB(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodB(event);
  }
}

class CoarseAlignDialogMidasB
  implements ActionListener {

  CoarseAlignDialog adaptee;

  CoarseAlignDialogMidasB(CoarseAlignDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonMidasB(event);
  }
}
