package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.ConstBeadtrackParam;
import etomo.comscript.FortranInputSyntaxException;

/**
 * <p>Description: The dialog box for creating the fiducial model(s).</p>
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
public class FiducialModelDialog extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager;

  JPanel contentPane;
  JPanel panelFiducialModel = new JPanel();

  JPanel panelFiducialModelA = new JPanel();
  BeveledBorder borderA = new BeveledBorder("Axis: A");
  JToggleButton buttonSeedA =
    new JToggleButton("<html><b>Seed fiducial<br>model using imod</b>");
  BeadtrackPanel panelBeadtrackA;
  JButton buttonFixA =
     new JButton("<html><b>Fix fiducial model<br>using bead fixer</b>");


  JPanel panelFiducialModelB = new JPanel();
  BeveledBorder borderB = new BeveledBorder("Axis: B");
  JToggleButton buttonSeedB =
    new JToggleButton("<html><b>Seed fiducial<br>model using imod</b>");
  BeadtrackPanel panelBeadtrackB;
  JButton buttonFixB =
     new JButton("<html><b>Fix fiducial model<br>using bead fixer</b>");


  public FiducialModelDialog(ApplicationManager appMgr) {
    contentPane = (JPanel) getContentPane();
    applicationManager = appMgr;

    if(applicationManager.isDualAxis()) {
      panelBeadtrackA = new BeadtrackPanel("a");
    }
    else {
      panelBeadtrackA = new BeadtrackPanel("");
    }
    panelBeadtrackB = new BeadtrackPanel("b");


    setTitle("eTomo Fiducial Model Generation");
    buttonExecute.setText("Done");

    buttonSeedA.setAlignmentX(0.5F);
    buttonSeedA.setPreferredSize(FixedDim.button2Line);
    buttonSeedA.setMaximumSize(FixedDim.button2Line);

    buttonSeedB.setAlignmentX(0.5F);
    buttonSeedB.setPreferredSize(FixedDim.button2Line);
    buttonSeedB.setMaximumSize(FixedDim.button2Line);


    buttonFixA.setAlignmentX(0.5F);
    buttonFixA.setPreferredSize(FixedDim.button2Line);
    buttonFixA.setMaximumSize(FixedDim.button2Line);

    buttonFixB.setAlignmentX(0.5F);
    buttonFixB.setPreferredSize(FixedDim.button2Line);
    buttonFixB.setMaximumSize(FixedDim.button2Line);

    panelFiducialModelA.setLayout(
      new BoxLayout(panelFiducialModelA, BoxLayout.Y_AXIS));
    panelFiducialModelA.setBorder(borderA.getBorder());
    panelFiducialModelA.add(buttonSeedA);
    panelFiducialModelA.add(Box.createRigidArea(FixedDim.x0_y5));

    panelFiducialModelA.add(panelBeadtrackA.getContainer());
    panelFiducialModelA.add(Box.createRigidArea(FixedDim.x0_y5));
    panelFiducialModelA.add(buttonFixA);

    panelFiducialModelB.setLayout(
      new BoxLayout(panelFiducialModelB, BoxLayout.Y_AXIS));
    panelFiducialModelB.setBorder(borderB.getBorder());
    panelFiducialModelB.add(Box.createRigidArea(FixedDim.x0_y5));
    panelFiducialModelB.add(buttonSeedB);
    panelFiducialModelB.add(Box.createRigidArea(FixedDim.x0_y5));
    panelFiducialModelB.add(panelBeadtrackB.getContainer());
    panelFiducialModelB.add(Box.createRigidArea(FixedDim.x0_y5));
    panelFiducialModelB.add(buttonFixB);

    panelFiducialModel.setLayout(
      new BoxLayout(panelFiducialModel, BoxLayout.X_AXIS));
    panelFiducialModel.add(Box.createRigidArea(FixedDim.x10_y0));
    contentPane.add(Box.createHorizontalGlue());
    panelFiducialModel.add(panelFiducialModelA);
    contentPane.add(Box.createHorizontalGlue());
    panelFiducialModel.add(Box.createRigidArea(FixedDim.x10_y0));
    panelFiducialModel.add(panelFiducialModelB);
    contentPane.add(Box.createHorizontalGlue());
    panelFiducialModel.add(Box.createRigidArea(FixedDim.x10_y0));

    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    contentPane.add(panelFiducialModel);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //
    //  Action listener assignments for the buttons
    //
    buttonSeedA.addActionListener(new FiducialModelDialogSeedA(this));
    buttonFixA.addActionListener(new FiducialModelDialogFixA(this));
    panelBeadtrackA.setButtonTrackActionListener(
      new FiducialModelDialogTrackA(this));
    buttonSeedB.addActionListener(new FiducialModelDialogSeedB(this));
    buttonFixB.addActionListener(new FiducialModelDialogFixB(this));
    panelBeadtrackB.setButtonTrackActionListener(
      new FiducialModelDialogTrackB(this));

    //  Disable the second CCDeraser panel if this is a single axis
    if(! applicationManager.isDualAxis()) {
      panelFiducialModelB.setVisible(false);
    }

    //  Set the window to the default state in case the calling function doesn't
    setAdvanced(isAdvanced);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelFiducialModel.addMouseListener(mouseAdapter);

    pack();
  }


  /**
   * Set the advanced state for the dialog box
   */
  public void setAdvanced(boolean state) {
    //isAdvanced = state;
    panelBeadtrackA.setAdvanced(state);
    panelBeadtrackB.setAdvanced(state);
    pack();
  }

  /**
   * Set the advanced state for the dialog box
   */
  public boolean getAdvanced() {
    return isAdvanced;
  }

  /**
   * Set the parameters for the specified beadtrack panel
   */
  public void setBeadtrackParams(ConstBeadtrackParam beadtrackParams,
      AxisID axisID){
    if(axisID == AxisID.SECOND) {
      panelBeadtrackB.setParameters(beadtrackParams);
    }
    else {
      panelBeadtrackA.setParameters(beadtrackParams);
    }
  }

  /**
   * Get the parameters for the specified beadtrack command
   */
  public void getBeadtrackParams(BeadtrackParam beadtrackParams,
     AxisID axisID) throws FortranInputSyntaxException {
    if(axisID == AxisID.SECOND) {
      try {
	panelBeadtrackB.getParameters(beadtrackParams);
      }
      catch(FortranInputSyntaxException except) {
	String message = "Axis B: " + except.getMessage();
	throw new FortranInputSyntaxException(message);
      }

    }
    else {
      try {
	panelBeadtrackA.getParameters(beadtrackParams);
      }
      catch(FortranInputSyntaxException except) {
	String message = "Axis A: " + except.getMessage();
	throw new FortranInputSyntaxException(message);
      }
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] label = {"imod"};
    String[] manPage = {"imod.html"};
    ContextPopup contextPopup = new ContextPopup(panelFiducialModel,
						 mouseEvent,
						 label,
						 manPage);
  }


  //
  //  Action function for stack buttons
  //
  void buttonSeedA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.imodSeedFiducials(AxisID.FIRST);
    }
    else {
      applicationManager.imodSeedFiducials(AxisID.ONLY);
    }
  }

  void buttonTrackA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.fiducialModelTrack(AxisID.FIRST, this);
    }
    else {
      applicationManager.fiducialModelTrack(AxisID.ONLY, this);
    }
  }

  void buttonFixA(ActionEvent event) {
    if(applicationManager.isDualAxis()) {
      applicationManager.imodFixFiducials(AxisID.FIRST);
    }
    else {
      applicationManager.imodFixFiducials(AxisID.ONLY);
    }
  }

  void buttonSeedB(ActionEvent event) {
    applicationManager.imodSeedFiducials(AxisID.SECOND);
  }

  void buttonTrackB(ActionEvent event) {
    applicationManager.fiducialModelTrack(AxisID.SECOND, this);
  }

  void buttonFixB(ActionEvent event) {
    applicationManager.imodFixFiducials(AxisID.SECOND);
  }


  public void setEnabledB(boolean state){
    buttonSeedB.setEnabled(state);
    buttonFixB.setEnabled(state);
  }

  //
  //  Action function overides for buttons
  //
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneFiducialModelDialog(this);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneFiducialModelDialog(this);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneFiducialModelDialog(this);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    setAdvanced(isAdvanced);
  }
}

//
//  Action listener adapters
//
class FiducialModelDialogSeedA
  implements ActionListener {

  FiducialModelDialog adaptee;

  FiducialModelDialogSeedA(FiducialModelDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonSeedA(event);
  }
}

class FiducialModelDialogTrackA
  implements ActionListener {

  FiducialModelDialog adaptee;

  FiducialModelDialogTrackA(FiducialModelDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTrackA(event);
  }
}

class FiducialModelDialogFixA
  implements ActionListener {

  FiducialModelDialog adaptee;

  FiducialModelDialogFixA(FiducialModelDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonFixA(event);
  }
}

class FiducialModelDialogSeedB
  implements ActionListener {

  FiducialModelDialog adaptee;

  FiducialModelDialogSeedB(FiducialModelDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonSeedB(event);
  }
}


class FiducialModelDialogTrackB
  implements ActionListener {

  FiducialModelDialog adaptee;

  FiducialModelDialogTrackB(FiducialModelDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTrackB(event);
  }
}


class FiducialModelDialogFixB
  implements ActionListener {

  FiducialModelDialog adaptee;

  FiducialModelDialogFixB(FiducialModelDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonFixB(event);
  }
}
