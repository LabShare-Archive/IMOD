package etomo.ui;

import java.awt.GridLayout;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.TiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TransferfidParam;

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
 * <p> Revision 1.14  2003/01/06 17:35:43  rickg
 * <p> Check for dual axis before calling second axis panel
 * <p> or transferfid components in advanced
 * <p>
 * <p> Revision 1.13  2003/01/06 04:55:04  rickg
 * <p> Changed layout to a single transferfid panel
 * <p>
 * <p> Revision 1.12  2003/01/04 00:34:45  rickg
 * <p> bound actions listeners for transferfid buttons
 * <p> added getTransferFidParams method
 * <p> implemented button actions for transfer fiducials
 * <p>
 * <p> Revision 1.11  2002/12/31 23:13:01  rickg
 * <p> Implemented logic for transferfid buttons
 * <p>
 * <p> Revision 1.10  2002/12/20 01:25:05  rickg
 * <p> Adding fiducial transfer interface
 * <p>
 * <p> Revision 1.9  2002/12/19 00:28:34  rickg
 * <p> Advanced handling implemented
 * <p>
 * <p> Revision 1.8  2002/12/18 18:52:13  rickg
 * <p> Restructured buttons and layout
 * <p>
 * <p> Revision 1.7  2002/12/05 01:22:44  rickg
 * <p> Previous commit comment was incorrect
 * <p> Split advanced functionality from action handler
 * <p> so that it can be called at startup.
 * <p>
 * <p> Revision 1.6  2002/12/05 01:20:37  rickg
 * <p> Added isAdvanced stub
 * <p>
 * <p> Revision 1.5  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.4  2002/10/17 23:42:26  rickg
 * <p> Spaced buttons some
 * <p> Call default parameters methods in panelAlign objects
 * <p>
 * <p> Revision 1.3  2002/10/17 22:38:47  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
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

  private JPanel panelAlignEst = new JPanel();

  private JPanel panelAlignEstA = new JPanel();
  private BeveledBorder borderA = new BeveledBorder("Axis: A");

  private TiltalignPanel panelTiltalignA;

  private JPanel panelButtonA = new JPanel();

  private JToggleButton buttonComputeAlignmentA =
    new JToggleButton("<html><b>Compute<br>alignment</b>");

  private JButton buttonImodA =
    new JButton("<html><b>View/Edit model<br>in imod</b>");

  private JButton buttonView3DModelA =
    new JButton("<html><b>View 3D<br>model</b>");

  private JButton buttonViewResidualsA = 
    new JButton("<html><b>View residual<br>vectors</b>");  
    
  private JButton buttonTransferFiducialsA =
    new JButton("<html><b>Transfer fiducials<br>to the B axis</b>");

  private JPanel panelAlignEstB = new JPanel();
  private BeveledBorder borderB = new BeveledBorder("Axis: B");

  private TiltalignPanel panelTiltalignB;

  private JPanel panelButtonB = new JPanel();

  private JToggleButton buttonComputeAlignmentB =
    new JToggleButton("<html><b>Compute<br>alignment</b>");

  private JButton buttonView3DModelB =
    new JButton("<html><b>View 3D<br>model</b>");

  private JButton buttonImodB =
    new JButton("<html><b>View/Edit model<br>in imod<</b>");

  private JButton buttonViewResidualsB = 
    new JButton("<html><b>View residual<br>vectors</b>");  

  private JButton buttonTransferFiducialsB =
    new JButton("<html><b>Transfer fiducials<br>to the A axis</b>");

  //  There only needs to be one transfer fiducial panel
  private TransferfidPanel panelTransferFid;

  public AlignmentEstimationDialog(ApplicationManager appMgr) {
    super(appMgr);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    setTitle(
      "eTomo Fine Alignment Estimation: "
        + applicationManager.getFilesetName());

    if (applicationManager.isDualAxis()) {
      panelTiltalignA = new TiltalignPanel("a");
      panelTransferFid = new TransferfidPanel();
    }
    else {
      panelAlignEstB.setVisible(false);
      panelTiltalignA = new TiltalignPanel("");
    }
    panelTiltalignB = new TiltalignPanel("b");

    buttonExecute.setText("Done");

    //  Create the first panel
    GridLayout buttonLayout = new GridLayout(1,4);
    buttonLayout.setHgap(10);
    panelButtonA.setLayout(buttonLayout);
    
    panelButtonA.add(buttonComputeAlignmentA);
    panelButtonA.add(buttonImodA);
    panelButtonA.add(buttonViewResidualsA);
    panelButtonA.add(buttonView3DModelA);
    if (applicationManager.isDualAxis()) {
      buttonLayout.setColumns(5);
      panelButtonA.add(buttonTransferFiducialsA);
    }
    buttonView3DModelA.setEnabled(false);

    panelAlignEstA.setLayout(new BoxLayout(panelAlignEstA, BoxLayout.Y_AXIS));
    panelAlignEstA.setBorder(borderA.getBorder());

    panelAlignEstA.add(panelTiltalignA.getContainer());
    panelAlignEstA.add(Box.createRigidArea(FixedDim.x5_y0));
    if (applicationManager.isDualAxis()) {
      panelAlignEstA.add(panelTransferFid.getContainer());
      panelAlignEstA.add(Box.createRigidArea(FixedDim.x5_y0));
    }
    panelAlignEstA.add(panelButtonA);

    //  Create the second panel
    panelButtonB.setLayout(buttonLayout);
    panelButtonB.add(buttonComputeAlignmentB);
    panelButtonB.add(buttonImodB);
    panelButtonB.add(buttonViewResidualsB);
    panelButtonB.add(buttonView3DModelB);

    if (applicationManager.isDualAxis()) {
      panelButtonB.add(buttonTransferFiducialsB);
    }

    buttonView3DModelB.setEnabled(false);

    panelAlignEstB.setLayout(new BoxLayout(panelAlignEstB, BoxLayout.Y_AXIS));
    panelAlignEstB.setBorder(borderB.getBorder());

    panelAlignEstB.add(panelTiltalignB.getContainer());
    panelAlignEstB.add(Box.createRigidArea(FixedDim.x5_y0));

    panelAlignEstB.add(panelButtonB);

    panelAlignEst.setLayout(new BoxLayout(panelAlignEst, BoxLayout.X_AXIS));
    panelAlignEst.add(Box.createRigidArea(FixedDim.x10_y0));
    rootPanel.add(Box.createHorizontalGlue());
    panelAlignEst.add(panelAlignEstA);
    rootPanel.add(Box.createHorizontalGlue());
    panelAlignEst.add(Box.createRigidArea(FixedDim.x10_y0));
    panelAlignEst.add(panelAlignEstB);
    rootPanel.add(Box.createHorizontalGlue());
    panelAlignEst.add(Box.createRigidArea(FixedDim.x10_y0));

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(panelAlignEst);

    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the action listeners to the buttons
    buttonComputeAlignmentA.addActionListener(new AlignmentComputeA(this));
    buttonView3DModelA.addActionListener(new AlignmentView3DModelA(this));
    buttonViewResidualsA.addActionListener(new AlignmentViewResidualsA(this));
    buttonImodA.addActionListener(new AlignmentImodA(this));
    buttonTransferFiducialsA.addActionListener(new AlignmentTransferA(this));

    buttonComputeAlignmentB.addActionListener(new AlignmentComputeB(this));
    buttonView3DModelB.addActionListener(new AlignmentView3DModelB(this));
    buttonViewResidualsB.addActionListener(new AlignmentViewResidualsB(this));
    buttonImodB.addActionListener(new AlignmentImodB(this));
    buttonTransferFiducialsB.addActionListener(new AlignmentTransferB(this));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced state, this also executes a pack()
    updateAdvanced(isAdvanced);
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

  public void setTransferFidParams(TransferfidParam transferFidParam) {
    panelTransferFid.setParameters(transferFidParam);
  }

  public void getTransferFidParams(TransferfidParam transferFidParam) {
    panelTransferFid.getParameters(transferFidParam);
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
        rootPanel,
        mouseEvent,
        "FINAL ALIGNMENT",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  //  Action funtions for process buttons
  void buttonComputeAAction(ActionEvent event) {
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

  void buttonView3DModelAAction(ActionEvent event) {
  }

  void buttonResidualAAction(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.imodViewResiduals(AxisID.FIRST);
    }
    else {
      applicationManager.imodViewResiduals(AxisID.ONLY);
    }
  }
  
  void buttonTransferFiducialsAAction() {
    applicationManager.transferfid(AxisID.FIRST);
  }

  void buttonComputeBAction(ActionEvent event) {
    applicationManager.fineAlignment(AxisID.SECOND);
  }

  void buttonImodBAction(ActionEvent event) {
    applicationManager.imodFixFiducials(AxisID.SECOND);
  }

  void buttonView3DModelBAction(ActionEvent event) {

  }

  void buttonResidualBAction(ActionEvent event) {
    applicationManager.imodViewResiduals(AxisID.SECOND);
  }

  void buttonTransferFiducialsBAction() {
    applicationManager.transferfid(AxisID.SECOND);
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

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced(isAdvanced);
  }

  //  This is a separate function so it can be called at initialization time
  //  as well as from the button action above
  void updateAdvanced(boolean state) {
    panelTiltalignA.setAdvanced(isAdvanced);
    if (applicationManager.isDualAxis()) {
      panelTiltalignB.setAdvanced(isAdvanced);
      panelTransferFid.setAdvanced(isAdvanced);
    }
    pack();
  }
}

//  ActionListener classes for buttons
class AlignmentComputeA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentComputeA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonComputeAAction(event);
  }
}

class AlignmentView3DModelA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentView3DModelA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonView3DModelAAction(event);
  }
}

class AlignmentViewResidualsA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentViewResidualsA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonResidualAAction(event);
  }
}

class AlignmentImodA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentImodA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodAAction(event);
  }
}

class AlignmentTransferA implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentTransferA(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTransferFiducialsAAction();
  }
}

class AlignmentComputeB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentComputeB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonComputeBAction(event);
  }
}

class AlignmentView3DModelB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentView3DModelB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonView3DModelBAction(event);
  }
}

class AlignmentImodB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentImodB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonImodBAction(event);
  }
}

class AlignmentViewResidualsB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentViewResidualsB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonResidualBAction(event);
  }
}

class AlignmentTransferB implements ActionListener {

  AlignmentEstimationDialog adaptee;

  AlignmentTransferB(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonTransferFiducialsBAction();
  }
}
