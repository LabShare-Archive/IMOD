package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.ConstTilt;
import etomo.comscript.TiltParam;

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
public class TomogramGenerationDialog extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid = "$Id$";

  ApplicationManager applicationManager;
  JPanel contentPane;

  JPanel panelTilt = new JPanel();

  //  Axis A panel
  JPanel panelTiltA = new JPanel();
  BeveledBorder borderA = new BeveledBorder("Axis: A");

  JToggleButton buttonNewstA =
    new JToggleButton("<html><b>Create full<br>aligned stack</b>");

  LabeledTextField ltfTomoThicknessA =
    new LabeledTextField("Tomogram thickness: ");

  LabeledTextField ltfXAxisTiltA =
    new LabeledTextField("X Axis Tilt: ");

  JCheckBox chkBoxUseLocalAlignmentA = new JCheckBox("Use local alignments");

  JToggleButton buttonTiltA =
    new JToggleButton("<html><b>Generate<br>tomogram</b>");
  JToggleButton buttonImodA =
    new JToggleButton("<html><b>View tomogram<br>in imod</b>");

  //  Axis B panel
  JPanel panelTiltB = new JPanel();
  BeveledBorder borderB = new BeveledBorder("Axis: B");

  JToggleButton buttonNewstB =
    new JToggleButton("<html><b>Create full<br>aligned stack</b>");

  LabeledTextField ltfTomoThicknessB =
    new LabeledTextField("Tomogram thickness: ");

  LabeledTextField ltfXAxisTiltB =
    new LabeledTextField("X Axis Tilt: ");

  JCheckBox chkBoxUseLocalAlignmentB = new JCheckBox("Use local alignments");

  JToggleButton buttonTiltB =
    new JToggleButton("<html><b>Generate<br>tomogram</b>");
  JToggleButton buttonImodB =
    new JToggleButton("<html><b>View tomogram<br>in imod</b>");

  public TomogramGenerationDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;

    contentPane = (JPanel) getContentPane();
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    setTitle("eTomo Tomogram Generation");
    buttonExecute.setText("Done");


    buttonNewstA.setAlignmentX(0.5F);
    buttonNewstA.addActionListener(new GenerationDialogNewstA(this));
    buttonTiltA.setAlignmentX(0.5F);
    buttonTiltA.addActionListener(new GenerationDialogTiltA(this));
    buttonImodA.setAlignmentX(0.5F);
    buttonImodA.addActionListener(new GenerationDialogImodA(this));

    buttonNewstB.setAlignmentX(0.5F);
    buttonNewstB.addActionListener(new GenerationDialogNewstB(this));
    buttonTiltB.setAlignmentX(0.5F);
    buttonTiltB.addActionListener(new GenerationDialogTiltB(this));
    buttonImodB.setAlignmentX(0.5F);
    buttonImodB.addActionListener(new GenerationDialogImodB(this));

    panelTiltA.setBorder(borderA.getBorder());
    panelTiltA.setLayout(new BoxLayout(panelTiltA, BoxLayout.Y_AXIS));
    panelTiltA.add(buttonNewstA);
    panelTiltA.add(Box.createVerticalGlue());
    panelTiltA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelTiltA.add(ltfTomoThicknessA.getContainer());
    panelTiltA.add(ltfXAxisTiltA.getContainer());
    panelTiltA.add(chkBoxUseLocalAlignmentA);
    panelTiltA.add(buttonTiltA);
    panelTiltA.add(Box.createVerticalGlue());
    panelTiltA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelTiltA.add(buttonImodA);
    panelTiltA.add(Box.createVerticalGlue());
    panelTiltA.add(Box.createRigidArea(FixedDim.x0_y10));

    panelTiltB.setBorder(borderB.getBorder());
    panelTiltB.setLayout(new BoxLayout(panelTiltB, BoxLayout.Y_AXIS));
    panelTiltB.add(buttonNewstB);
    panelTiltB.add(Box.createVerticalGlue());
    panelTiltB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelTiltB.add(ltfTomoThicknessB.getContainer());
    panelTiltB.add(ltfXAxisTiltB.getContainer());
    panelTiltB.add(chkBoxUseLocalAlignmentB);
    panelTiltB.add(buttonTiltB);
    panelTiltB.add(Box.createVerticalGlue());
    panelTiltB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelTiltB.add(buttonImodB);
    panelTiltB.add(Box.createVerticalGlue());
    panelTiltB.add(Box.createRigidArea(FixedDim.x0_y10));

    panelTilt.setLayout(new BoxLayout(panelTilt, BoxLayout.X_AXIS));
    panelTilt.add(panelTiltA);
    panelTilt.add(panelTiltB);

    contentPane.add(panelTilt);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    contentPane.addMouseListener(mouseAdapter);

    //
    // Calcute the necessary window size
    //
    pack();

  }


  /**
   * Populate the dialog box with the tilt paramaters
   */
  public void setTiltParams(ConstTilt tiltParam, AxisID axisID) {
    if(axisID == AxisID.SECOND) {

      ltfTomoThicknessB.setText(tiltParam.getThickness());
      ltfXAxisTiltB.setText(tiltParam.getXAxisTilt());
      chkBoxUseLocalAlignmentB.setSelected(tiltParam.getUseLocalAlignFile());
    }
    else {
      ltfTomoThicknessA.setText(tiltParam.getThickness());
      ltfXAxisTiltA.setText(tiltParam.getXAxisTilt());
      chkBoxUseLocalAlignmentA.setSelected(tiltParam.getUseLocalAlignFile());
    }
  }


  /**
   * Get the tilt parameters from the requested axis panel
   */
  public void getTiltParams(TiltParam tiltParam, AxisID axisID)
  throws NumberFormatException {

    String currentAxisID = "unknown";
    try {
      if(axisID == AxisID.SECOND) {
	currentAxisID = "Axis B:";
	tiltParam.setThickness(
	  Integer.parseInt(ltfTomoThicknessB.getText()));
	tiltParam.setXAxisTilt(Double.parseDouble(ltfXAxisTiltB.getText()));

	if(chkBoxUseLocalAlignmentB.isSelected()) {
	  tiltParam.setLocalAlignFile(
	    applicationManager.getFilesetName() + "blocal.xf");
	}
	else {
	  tiltParam.setLocalAlignFile("");
	}
      }

      else {
	currentAxisID = "Axis A:";
	tiltParam.setThickness(
	  Integer.parseInt(ltfTomoThicknessA.getText()));
	tiltParam.setXAxisTilt(Double.parseDouble(ltfXAxisTiltA.getText()));


	if(chkBoxUseLocalAlignmentA.isSelected()) {
	  if(axisID == AxisID.ONLY) {
	    tiltParam.setLocalAlignFile(
	      applicationManager.getFilesetName() + "local.xf");
	  }
	  else {
	    tiltParam.setLocalAlignFile(
	      applicationManager.getFilesetName() + "alocal.xf");

	  }
	}
	else {
	  tiltParam.setLocalAlignFile("");
	}
      }
    }
    catch(NumberFormatException except) {
      String message = currentAxisID + except.getMessage();
      throw new NumberFormatException(message);
    }

  }

  public void setEnabledB(boolean state){
    panelTiltB.setVisible(state);
    pack();
    repaint();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel =
      {"newst", "tilt", "imod"};
    String[] manPage =
      {"newst.html", "tilt.html", "imod.html"};

    String[] logFileLabel;
    String[] logFile;
    if(applicationManager.isDualAxis()) {
      logFileLabel = new String[4];
      logFileLabel[0] = "newsta";
      logFileLabel[1] = "newstb";
      logFileLabel[2] = "tilta";
      logFileLabel[3] = "tiltb";
      logFile = new String[4];
      logFile[0] = "newsta.log";
      logFile[1] = "newstb.log";
      logFile[2] = "tilta.log";
      logFile[3] = "tiltb.log";
    }
    else {
      logFileLabel = new String[2];
      logFileLabel[0] = "newst";
      logFileLabel[1] = "tilt";
      logFile = new String[2];
      logFile[0] = "newst.log";
      logFile[1] = "tilt.log";
    }

    ContextPopup contextPopup =
      new ContextPopup(contentPane, mouseEvent,
	manPagelabel, manPage, logFileLabel, logFile);
  }

  //  Button action handler methods
  void buttonNewstA(ActionEvent event) {

    if(applicationManager.isDualAxis()) {
      applicationManager.newst(AxisID.FIRST);
    }
    else {
      applicationManager.newst(AxisID.ONLY);
    }
  }

  void buttonTiltA(ActionEvent event) {

    if(applicationManager.isDualAxis()) {
      applicationManager.tilt(AxisID.FIRST, this);
    }
    else {
      applicationManager.tilt(AxisID.ONLY, this);
    }
  }

  void buttonImodA(ActionEvent event) {

    if(applicationManager.isDualAxis()) {
      applicationManager.imodTomogram(AxisID.FIRST);
    }
    else {
      applicationManager.imodTomogram(AxisID.ONLY);
    }
  }

  void buttonNewstB(ActionEvent event) {
    applicationManager.newst(AxisID.SECOND);
  }

  void buttonTiltB(ActionEvent event) {
    applicationManager.tilt(AxisID.SECOND, this);
  }

  void buttonImodB(ActionEvent event) {
     applicationManager.imodTomogram(AxisID.SECOND);
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramGenerationDialog(this);
  }


  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramGenerationDialog(this);
  }


  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramGenerationDialog(this);
  }
}


class GenerationDialogNewstA
  implements ActionListener {

  TomogramGenerationDialog adaptee;

  GenerationDialogNewstA(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonNewstA(event);
  }
}


class GenerationDialogTiltA
  implements ActionListener {

  TomogramGenerationDialog adaptee;

  GenerationDialogTiltA(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTiltA(event);
  }
}


class GenerationDialogImodA
  implements ActionListener {

  TomogramGenerationDialog adaptee;

  GenerationDialogImodA(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodA(event);
  }
}

class GenerationDialogNewstB
  implements ActionListener {

  TomogramGenerationDialog adaptee;

  GenerationDialogNewstB(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonNewstB(event);
  }
}


class GenerationDialogTiltB
  implements ActionListener {

  TomogramGenerationDialog adaptee;

  GenerationDialogTiltB(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTiltB(event);
  }
}


class GenerationDialogImodB
  implements ActionListener {

  TomogramGenerationDialog adaptee;

  GenerationDialogImodB(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodB(event);
  }
}
