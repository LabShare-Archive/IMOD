package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.TiltalignParam;
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
 * <p> $Log$
 * <p> Revision 1.2  2002/10/07 22:30:28  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class AlignmentEstimationDialog
  extends ProcessDialog
  implements ContextMenu {

  public static final String rcsid =
    "$Id$";
  private ApplicationManager applicationManager;

  private JPanel contentPane;
  private JPanel panelAlignEst = new JPanel();

  private JPanel panelAlignEstA = new JPanel();
  private BeveledBorder borderA = new BeveledBorder("Axis: A");

  private TiltalignPanel panelTiltalignA;

  private JPanel panelButtonA = new JPanel();

  private JToggleButton buttonTiltDefaultsA =
    new JToggleButton("<html><b>Set tilt angle and<br>magnification defaults</b>");

  private JToggleButton buttonTiltEstA =
    new JToggleButton("<html><b>Estimate tilt and<br>magnification</b>");

  private JToggleButton buttonDistortionDefaultsA =
    new JToggleButton("<html><b>Set distortion<br>defaults</b>");

  private JToggleButton buttonDistortionEstA =
    new JToggleButton("<html><b>Estimate<br>distortion</b>");

  private JToggleButton buttonImodA =
    new JToggleButton("<html><b>View/Fix model<br>in imod</b>");

  private JPanel panelAlignEstB = new JPanel();
  private BeveledBorder borderB = new BeveledBorder("Axis: B");

  private TiltalignPanel panelTiltalignB;

  private JPanel panelButtonB = new JPanel();

  private JToggleButton buttonTiltDefaultsB =
    new JToggleButton("<html><b>Set tilt angle and<br>magnification defaults</b>");

  private JToggleButton buttonTiltEstB =
    new JToggleButton("<html><b>Estimate tilt and<br>magnification</b>");

  private JToggleButton buttonDistortionDefaultsB =
    new JToggleButton("<html><b>Set distortion<br>defaults</b>");

  private JToggleButton buttonDistortionEstB =
    new JToggleButton("<html><b>Estimate<br>distortion</b>");

  private JToggleButton buttonImodB =
    new JToggleButton("<html><b>View/Fix model<br>in imod</b>");

  public AlignmentEstimationDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;
    contentPane = (JPanel) getContentPane();
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.X_AXIS));
    setTitle(
      "eTomo Fine Alignment Estimation: "
        + applicationManager.getFilesetName());

    if (applicationManager.isDualAxis()) {
      panelTiltalignA = new TiltalignPanel("a");
    }
    else {
      panelAlignEstB.setVisible(false);
      panelTiltalignA = new TiltalignPanel("");
    }
    panelTiltalignB = new TiltalignPanel("b");

    buttonExecute.setText("Done");

    //  Create the first panel
    panelButtonA.setLayout(new BoxLayout(panelButtonA, BoxLayout.Y_AXIS));
    panelButtonA.add(buttonTiltDefaultsA);
    panelButtonA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonA.add(buttonTiltEstA);
    panelButtonA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonA.add(buttonDistortionDefaultsA);
    panelButtonA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonA.add(buttonDistortionEstA);
    panelButtonA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonA.add(buttonImodA);

    panelAlignEstA.setLayout(new BoxLayout(panelAlignEstA, BoxLayout.X_AXIS));
    panelAlignEstA.setBorder(borderA.getBorder());

    panelAlignEstA.add(panelButtonA);
    panelAlignEstA.add(Box.createRigidArea(FixedDim.x5_y0));
    panelAlignEstA.add(panelTiltalignA.getContainer());

    //  Create the second panel
    panelButtonB.setLayout(new BoxLayout(panelButtonB, BoxLayout.Y_AXIS));
    panelButtonB.add(buttonTiltDefaultsB);
    panelButtonB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonB.add(buttonTiltEstB);
    panelButtonB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonB.add(buttonDistortionDefaultsB);
    panelButtonB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonB.add(buttonDistortionEstB);
    panelButtonB.add(Box.createRigidArea(FixedDim.x0_y10));
    panelButtonB.add(buttonImodB);

    panelAlignEstB.setLayout(new BoxLayout(panelAlignEstB, BoxLayout.X_AXIS));
    panelAlignEstB.setBorder(borderB.getBorder());

    panelAlignEstB.add(panelButtonB);
    panelAlignEstB.add(Box.createRigidArea(FixedDim.x5_y0));
    panelAlignEstB.add(panelTiltalignB.getContainer());

    panelAlignEst.setLayout(new BoxLayout(panelAlignEst, BoxLayout.X_AXIS));
    panelAlignEst.add(Box.createRigidArea(FixedDim.x10_y0));
    contentPane.add(Box.createHorizontalGlue());
    panelAlignEst.add(panelAlignEstA);
    contentPane.add(Box.createHorizontalGlue());
    panelAlignEst.add(Box.createRigidArea(FixedDim.x10_y0));
    panelAlignEst.add(panelAlignEstB);
    contentPane.add(Box.createHorizontalGlue());
    panelAlignEst.add(Box.createRigidArea(FixedDim.x10_y0));

    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    contentPane.add(panelAlignEst);

    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the action listeners to the buttons
    buttonTiltDefaultsA.addActionListener(new AlignementEstTiltDefaultsA(this));
    buttonTiltEstA.addActionListener(new AlignementEstTiltEstA(this));

    buttonDistortionDefaultsA.addActionListener(
      new AlignementEstDistortionDefaultsA(this));
    buttonDistortionEstA.addActionListener(
      new AlignementEstDistortionEstA(this));

    buttonImodA.addActionListener(new AlignementEstImodA(this));

    buttonTiltDefaultsB.addActionListener(new AlignementEstTiltDefaultsB(this));
    buttonTiltEstB.addActionListener(new AlignementEstTiltEstB(this));

    buttonDistortionDefaultsB.addActionListener(
      new AlignementEstDistortionDefaultsB(this));
    buttonDistortionEstB.addActionListener(
      new AlignementEstDistortionEstB(this));

    buttonImodB.addActionListener(new AlignementEstImodB(this));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    contentPane.addMouseListener(mouseAdapter);

    // Calcute the necessary window size
    setSize(new Dimension(400, 600));
    panelTiltalignA.setLargestTab();
    panelTiltalignB.setLargestTab();
    pack();
    panelTiltalignA.setFirstTab();
    panelTiltalignB.setFirstTab();
  }

  public void setTiltalignParams(
    TiltalignParam tiltalignParam,
    AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      panelTiltalignB.setParameters(tiltalignParam);
    }
    else {
      panelTiltalignA.setParameters(tiltalignParam);
    }

  }

  public void getTiltalignParams(TiltalignParam tiltalignParam, AxisID axisID)
    throws FortranInputSyntaxException {
    if (axisID == AxisID.SECOND) {
      try {
        panelTiltalignB.getParameters(tiltalignParam);
      }
      catch (FortranInputSyntaxException except) {
        String message = "Axis B: " + except.getMessage();
        throw new FortranInputSyntaxException(message);
      }

    }
    else {
      try {
        panelTiltalignA.getParameters(tiltalignParam);
      }
      catch (FortranInputSyntaxException except) {
        String message = "Axis A: " + except.getMessage();
        throw new FortranInputSyntaxException(message);
      }
    }

  }

  public void setEnabledB(boolean state) {
    panelAlignEstB.setEnabled(state);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "tiltalign", "xfproduct", "imod" };
    String[] manPage = { "tiltalign.html", "xfproduct.html", "imod.html" };

    String[] logFileLabel;
    String[] logFile;
    if (applicationManager.isDualAxis()) {
      logFileLabel = new String[2];
      logFileLabel[0] = "aligna";
      logFileLabel[1] = "alignb";
      logFile = new String[2];
      logFile[0] = "aligna.log";
      logFile[1] = "alignb.log";
    }
    else {
      logFileLabel = new String[1];
      logFileLabel[0] = "align";
      logFile = new String[1];
      logFile[0] = "align.log";
    }

    ContextPopup contextPopup =
      new ContextPopup(
        contentPane,
        mouseEvent,
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  //  Action funtions for process buttons
  void buttonTiltDefaultsAAction(ActionEvent event) {
    panelTiltalignA.rbTiltAngleAll.setSelected(true);
    panelTiltalignA.rbDistortionNone.setSelected(true);

    panelTiltalignA.updateEnabled();
  }

  void buttonTiltEstAAction(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.fineAlignment(AxisID.FIRST);
    }
    else {
      applicationManager.fineAlignment(AxisID.ONLY);
    }
  }

  void buttonDistortionDefaultsAAction(ActionEvent event) {
    panelTiltalignA.rbTiltAngleAutomap.setSelected(true);
    panelTiltalignA.ltfTiltAngleGroupSize.setText(10);

    panelTiltalignA.rbDistortionIndependent.setSelected(true);
    panelTiltalignA.rbXstretchAutomapLinear.setSelected(true);
    panelTiltalignA.ltfXstretchGroupSize.setText(7);
    panelTiltalignA.rbSkewAutomapLinear.setSelected(true);
    panelTiltalignA.ltfSkewGroupSize.setText(11);

    panelTiltalignA.updateEnabled();
  }

  void buttonDistortionEstAAction(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.fineAlignment(AxisID.FIRST);
    }
    else {
      applicationManager.fineAlignment(AxisID.ONLY);
    }
  }

  void buttonImodAAction(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.imodFixFiducials(AxisID.FIRST);
    }
    else {
      applicationManager.imodFixFiducials(AxisID.ONLY);
    }
  }

  void buttonTiltDefaultsBAction(ActionEvent event) {
    panelTiltalignB.rbTiltAngleAll.setSelected(true);
    panelTiltalignB.rbDistortionNone.setSelected(true);

    panelTiltalignB.updateEnabled();
  }

  void buttonTiltEstBAction(ActionEvent event) {
    applicationManager.fineAlignment(AxisID.SECOND);
  }

  void buttonDistortionDefaultsBAction(ActionEvent event) {
    panelTiltalignB.rbTiltAngleAutomap.setSelected(true);
    panelTiltalignB.ltfTiltAngleGroupSize.setText(10);

    panelTiltalignB.rbDistortionIndependent.setSelected(true);
    panelTiltalignB.rbXstretchAutomapLinear.setSelected(true);
    panelTiltalignB.ltfXstretchGroupSize.setText(7);
    panelTiltalignB.rbSkewAutomapLinear.setSelected(true);
    panelTiltalignB.ltfSkewGroupSize.setText(11);

    panelTiltalignB.updateEnabled();
  }

  void buttonDistortionEstBAction(ActionEvent event) {
    applicationManager.fineAlignment(AxisID.SECOND);
  }

  void buttonImodBAction(ActionEvent event) {
    applicationManager.imodFixFiducials(AxisID.SECOND);
  }

  //  Action function overides for exit buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneAlignmentEstimationDialog();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneAlignmentEstimationDialog();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneAlignmentEstimationDialog();
  }

}

class AlignementEstTiltDefaultsA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstTiltDefaultsA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTiltDefaultsAAction(event);
  }
}

class AlignementEstTiltEstA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstTiltEstA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTiltEstAAction(event);
  }
}

class AlignementEstDistortionDefaultsA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstDistortionDefaultsA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonDistortionDefaultsAAction(event);
  }
}

class AlignementEstDistortionEstA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstDistortionEstA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonDistortionEstAAction(event);
  }
}

class AlignementEstImodA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstImodA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodAAction(event);
  }
}

class AlignementEstTiltDefaultsB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstTiltDefaultsB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTiltDefaultsBAction(event);
  }
}

class AlignementEstTiltEstB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstTiltEstB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTiltEstBAction(event);
  }
}

class AlignementEstDistortionDefaultsB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstDistortionDefaultsB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonDistortionDefaultsBAction(event);
  }
}

class AlignementEstDistortionEstB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstDistortionEstB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonDistortionEstBAction(event);
  }
}

class AlignementEstImodB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignementEstImodB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodBAction(event);
  }
}
