package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.ConstTilt;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.TiltalignParam;

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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TomogramPositioningDialog
  extends ProcessDialog implements ContextMenu {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager;

  private JPanel contentPane;
  private JPanel panelPosition = new JPanel();
  private JPanel panelPositionA = new JPanel();
  private BeveledBorder borderA = new BeveledBorder("Axis: A");


  private LabeledTextField ltfSampleTomoThicknessA =
    new LabeledTextField("Sample tomogram thickness: ");
  private JToggleButton buttonSampleA =
    new JToggleButton("<html><b>Create sample tomograms</b>");

  private JToggleButton buttonCreateBoundaryA =
    new JToggleButton("<html><b>Create boundary models</b>");

  private JToggleButton buttonTomopitchA =
    new JToggleButton("<html><b>Execute tomopitch</b>");

  private LabeledTextField ltfTiltAngleOffsetA =
    new LabeledTextField("Tilt angle offset: ");
  private LabeledTextField ltfTiltAxisZShiftA =
    new LabeledTextField("Tilt axis z shift: ");
  private LabeledTextField ltfTiltAxisXShiftA =
    new LabeledTextField("Tilt axis x shift: ");
  private JToggleButton buttonAlignA =
    new JToggleButton("<html><b>Create final alignment</b>");

  private JPanel panelPositionB = new JPanel();
  private BeveledBorder borderB = new BeveledBorder("Axis: B");

  private LabeledTextField ltfSampleTomoThicknessB =
    new LabeledTextField("Sample tomogram thickness: ");
  private JToggleButton buttonSampleB =
    new JToggleButton("<html><b>Create sample tomograms</b>");

  private JToggleButton buttonCreateBoundaryB =
    new JToggleButton("<html><b>Create boundary models</b>");

  private JToggleButton buttonTomopitchB =
    new JToggleButton("<html><b>Execute tomopitch</b>");

  private LabeledTextField ltfTiltAngleOffsetB =
    new LabeledTextField("Tilt angle offset: ");
  private LabeledTextField ltfTiltAxisZShiftB =
    new LabeledTextField("Tilt axis z shift: ");
  private LabeledTextField ltfTiltAxisXShiftB =
    new LabeledTextField("Tilt axis x shift: ");
  private JToggleButton buttonAlignB =
    new JToggleButton("<html><b>Create final alignment</b>");


  public TomogramPositioningDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;

    contentPane = (JPanel) this.getContentPane();
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    setTitle("eTomo Tomogram Position");
    buttonExecute.setText("Done");

    ltfSampleTomoThicknessA.setTextPreferredSize(new Dimension(50,20));
    buttonSampleA.setAlignmentX(0.5F);
    buttonCreateBoundaryA.setAlignmentX(0.5F);
    buttonTomopitchA.setAlignmentX(0.5F);
    buttonAlignA.setAlignmentX(0.5F);

    buttonSampleA.addActionListener(
      new PositioningDialogSampleA(this));
    buttonCreateBoundaryA.addActionListener(
      new PositioningDialogCreateBoundaryA(this));
    buttonTomopitchA.addActionListener(
      new PositioningDialogTomopitchA(this));
    buttonAlignA.addActionListener(
      new PositioningDialogFinalAlignA(this));

    ltfSampleTomoThicknessB.setTextPreferredSize(new Dimension(50,20));
    buttonSampleB.setAlignmentX(0.5F);
    buttonCreateBoundaryB.setAlignmentX(0.5F);
    buttonTomopitchB.setAlignmentX(0.5F);
    buttonAlignB.setAlignmentX(0.5F);

    buttonSampleB.addActionListener(
      new PositioningDialogSampleB(this));
    buttonCreateBoundaryB.addActionListener(
      new PositioningDialogCreateBoundaryB(this));
    buttonTomopitchB.addActionListener(
      new PositioningDialogTomopitchB(this));
    buttonAlignB.addActionListener(
      new PositioningDialogFinalAlignB(this));

    //  Create the primary panels
    panelPositionA.setBorder(borderA.getBorder());
    panelPositionA.setLayout(new BoxLayout(panelPositionA, BoxLayout.Y_AXIS));

    panelPositionA.add(ltfSampleTomoThicknessA.getContainer());
    panelPositionA.add(Box.createVerticalGlue());
    panelPositionA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionA.add(buttonSampleA);
    panelPositionA.add(Box.createVerticalGlue());
    panelPositionA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionA.add(buttonCreateBoundaryA);
    panelPositionA.add(Box.createVerticalGlue());
    panelPositionA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionA.add(buttonTomopitchA);
    panelPositionA.add(Box.createVerticalGlue());
    panelPositionA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionA.add(ltfTiltAngleOffsetA.getContainer());
    panelPositionA.add(ltfTiltAxisZShiftA.getContainer());
    panelPositionA.add(ltfTiltAxisXShiftA.getContainer());
    panelPositionA.add(buttonAlignA);

    panelPositionB.setBorder(borderB.getBorder());
    panelPositionB.setLayout(new BoxLayout(panelPositionB, BoxLayout.Y_AXIS));

    panelPositionB.add(ltfSampleTomoThicknessB.getContainer());
    panelPositionB.add(Box.createVerticalGlue());
    panelPositionB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionB.add(buttonSampleB);
    panelPositionB.add(Box.createVerticalGlue());
    panelPositionB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionB.add(buttonCreateBoundaryB);
    panelPositionB.add(Box.createVerticalGlue());
    panelPositionB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionB.add(buttonTomopitchB);
    panelPositionA.add(Box.createVerticalGlue());
    panelPositionB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPositionB.add(ltfTiltAngleOffsetB.getContainer());
    panelPositionB.add(ltfTiltAxisZShiftB.getContainer());
    panelPositionB.add(ltfTiltAxisXShiftB.getContainer());
    panelPositionB.add(buttonAlignB);

    //  Create dialog content pane
    panelPosition.setLayout(new BoxLayout(panelPosition, BoxLayout.X_AXIS));
    panelPosition.add(panelPositionA);
    panelPosition.add(panelPositionB);
    contentPane.add(panelPosition);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Set the default advanced state for the window
    //  FIXME: this needs to be defined by the options and
    //  the last state it was opened in
    setAdvanced(isAdvanced);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    contentPane.addMouseListener(mouseAdapter);

    //
    // Calcute the necessary window size
    //
    pack();

  }


  public void setTiltParams(ConstTilt tiltParam, AxisID axisID) {
    if(axisID == AxisID.SECOND) {
      ltfSampleTomoThicknessB.setText(tiltParam.getThickness());
    }
    else {
      ltfSampleTomoThicknessA.setText(tiltParam.getThickness());
    }
  }


  public void getTiltParams(TiltParam tiltParam, AxisID axisID)
  throws NumberFormatException {
    if(axisID == AxisID.SECOND) {
      try {
	tiltParam.setThickness(
	  Integer.parseInt(ltfSampleTomoThicknessB.getText()));
      }
      catch(NumberFormatException except) {
	String message = "Axis B: " + except.getMessage();
	throw new NumberFormatException(message);
      }

    }
    else {
      try {
	tiltParam.setThickness(
	  Integer.parseInt(ltfSampleTomoThicknessA.getText()));

      }
      catch(NumberFormatException except) {
	String message = "Axis A: " + except.getMessage();
	throw new NumberFormatException(message);
      }
    }
  }


  public void setAlignParams(ConstTiltalignParam tiltalignParam,
    AxisID axisID) {
    if(axisID == AxisID.SECOND) {
      ltfTiltAngleOffsetB.setText(tiltalignParam.getTiltAngleOffset());
      ltfTiltAxisZShiftB.setText(tiltalignParam.getTiltAxisZShift());
      ltfTiltAxisXShiftB.setText(tiltalignParam.getTiltAxisXShift());
    }
    else {
      ltfTiltAngleOffsetA.setText(tiltalignParam.getTiltAngleOffset());
      ltfTiltAxisZShiftA.setText(tiltalignParam.getTiltAxisZShift());
      ltfTiltAxisXShiftA.setText(tiltalignParam.getTiltAxisXShift());
    }
  }


  public void getAlignParams(TiltalignParam tiltalignParam, AxisID axisID)
  throws NumberFormatException {
    if(axisID == AxisID.SECOND) {
      try {
	tiltalignParam.setTiltAngleOffset(ltfTiltAngleOffsetB.getText());
	tiltalignParam.setTiltAxisZShift(ltfTiltAxisZShiftB.getText());
	tiltalignParam.setTiltAxisXShift(ltfTiltAxisXShiftB.getText());
      }
      catch(NumberFormatException except) {
	String message = "Axis B: " + except.getMessage();
	throw new NumberFormatException(message);
      }

    }
    else {
      try {
	tiltalignParam.setTiltAngleOffset(ltfTiltAngleOffsetA.getText());
	tiltalignParam.setTiltAxisZShift(ltfTiltAxisZShiftA.getText());
	tiltalignParam.setTiltAxisXShift(ltfTiltAxisXShiftA.getText());
      }
      catch(NumberFormatException except) {
	String message = "Axis A: " + except.getMessage();
	throw new NumberFormatException(message);
      }
    }
  }


  public void setEnabledB(boolean state){
    panelPositionB.setVisible(state);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel =
      {"newst", "imod", "tomopitch", "tilt"};
    String[] manPage =
      {"newst.html", "imod.html", "tomopitch.html", "tilt.html"};

    String[] logFileLabel;
    String[] logFile;
    if(applicationManager.isDualAxis()) {
      logFileLabel = new String[4];
      logFileLabel[0] = "samplea";
      logFileLabel[1] = "sampleb";
      logFileLabel[2] = "tomopitcha";
      logFileLabel[3] = "tomopitchb";
      logFile = new String[4];
      logFile[0] = "samplea.log";
      logFile[1] = "sampleb.log";
      logFile[2] = "tomopitcha.log";
      logFile[3] = "tomopitchb.log";
    }
    else {
      logFileLabel = new String[2];
      logFileLabel[0] = "sample";
      logFileLabel[1] = "tomopitch";
      logFile = new String[2];
      logFile[0] = "sample.log";
      logFile[1] = "tomopitch.log";
    }

    ContextPopup contextPopup =
      new ContextPopup(contentPane, mouseEvent,
	manPagelabel, manPage, logFileLabel, logFile);
  }

  //  Button action handler methods
  void buttonSampleA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.createSample(AxisID.FIRST, this);
    }
    else {
      applicationManager.createSample(AxisID.ONLY, this);
    }
  }

  void buttonCreateBoundaryA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.imodSample(AxisID.FIRST);
    }
    else {
      applicationManager.imodSample(AxisID.ONLY);
    }
  }

  void buttonTomopitchA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.tomopitch(AxisID.FIRST);
    }
    else {
      applicationManager.tomopitch(AxisID.ONLY);
    }
  }

  void buttonFinalAlignA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.finalAlign(this, AxisID.FIRST);
    }
    else {
      applicationManager.finalAlign(this, AxisID.ONLY);
    }
  }

  void buttonSampleB(ActionEvent event) {
    applicationManager.createSample(AxisID.SECOND, this);
  }

  void buttonCreateBoundaryB(ActionEvent event) {
    applicationManager.imodSample(AxisID.SECOND);
  }

  void buttonTomopitchB(ActionEvent event) {
    applicationManager.tomopitch(AxisID.SECOND);
  }

  void buttonFinalAlignB(ActionEvent event) {
    applicationManager.finalAlign(this, AxisID.SECOND);
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramPositioningDialog(this);
  }


  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramPositioningDialog(this);
  }


  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramPositioningDialog(this);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    setAdvanced(isAdvanced);
  }

  private void setAdvanced(boolean state){
    ltfTiltAxisXShiftA.setVisible(state);
    ltfTiltAxisXShiftB.setVisible(state);
    pack();

  }
}


//
//  Action listener adapters
//
class PositioningDialogSampleA
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogSampleA(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonSampleA(event);
  }
}


class PositioningDialogCreateBoundaryA
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogCreateBoundaryA(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCreateBoundaryA(event);
  }
}


class PositioningDialogTomopitchA
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogTomopitchA(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTomopitchA(event);
  }
}


class PositioningDialogFinalAlignA
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogFinalAlignA(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonFinalAlignA(event);
  }
}


class PositioningDialogSampleB
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogSampleB(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonSampleB(event);
  }
}


class PositioningDialogCreateBoundaryB
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogCreateBoundaryB(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCreateBoundaryB(event);
  }
}


class PositioningDialogTomopitchB
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogTomopitchB(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTomopitchB(event);
  }
}


class PositioningDialogFinalAlignB
  implements ActionListener {

  TomogramPositioningDialog adaptee;

  PositioningDialogFinalAlignB(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonFinalAlignB(event);
  }
}
