package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
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
 * <p> Revision 2.2  2003/04/28 23:25:26  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.1  2003/02/24 23:23:02  rickg
 * <p> Corrected acition spelling
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.19.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.19  2003/01/10 20:46:34  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.18  2003/01/08 18:47:54  rickg
 * <p> Added a preferred size for the buttons so that the
 * <p> window size is more reasonable
 * <p>
 * <p> Revision 1.17  2003/01/08 05:28:25  rickg
 * <p> Still working to fix window layout :(
 * <p>
 * <p> Revision 1.16  2003/01/08 04:00:21  rickg
 * <p> Mods in progress
 * <p>
 * <p> Revision 1.15  2003/01/07 00:33:00  rickg
 * <p> Changed button panel layout to grid and added
 * <p> button to view residuals in imod
 * <p>
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

  private BeveledBorder border = new BeveledBorder("Fine Alignment");

  private TiltalignPanel panelTiltalign;

  private JPanel panelButton = new JPanel();

  private JToggleButton buttonComputeAlignment =
    new JToggleButton("<html><b>Compute alignment</b>");

  private JButton buttonImod =
    new JButton("<html><b>View/Edit model in 3dmod</b>");

  private JButton buttonView3DModel = new JButton("<html><b>View 3D model</b>");

  private JButton buttonViewResiduals =
    new JButton("<html><b>View residual vectors</b>");

  private JToggleButton buttonTransferFiducials =
    new JToggleButton("<html><b>Transfer fiducials to the other axis</b>");

  //  There only needs to be one transfer fiducial panel???
  private TransferfidPanel panelTransferFid;

  public AlignmentEstimationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    panelTiltalign = new TiltalignPanel(axisID);
    panelTransferFid = new TransferfidPanel();

    buttonExecute.setText("Done");

    //  Create the first tiltalign panel
    GridLayout buttonLayout = new GridLayout(1, 4);
    buttonLayout.setHgap(10);
    panelButton.setLayout(buttonLayout);

    Dimension dimButton = new Dimension(80, 60);
    buttonComputeAlignment.setPreferredSize(dimButton);
    buttonImod.setPreferredSize(dimButton);
    buttonViewResiduals.setPreferredSize(dimButton);
    buttonView3DModel.setPreferredSize(dimButton);
    buttonTransferFiducials.setPreferredSize(dimButton);
    panelButton.add(buttonComputeAlignment);
    panelButton.add(buttonImod);
    panelButton.add(buttonViewResiduals);
    panelButton.add(buttonView3DModel);
    if (applicationManager.isDualAxis()) {
      buttonLayout.setColumns(5);
      panelButton.add(buttonTransferFiducials);
    }

    panelAlignEst.setLayout(new BoxLayout(panelAlignEst, BoxLayout.Y_AXIS));
    panelAlignEst.setBorder(border.getBorder());

    panelAlignEst.add(panelTiltalign.getContainer());
    panelAlignEst.add(Box.createRigidArea(FixedDim.x5_y0));
    if (applicationManager.isDualAxis()) {
      panelAlignEst.add(panelTransferFid.getContainer());
      panelAlignEst.add(Box.createRigidArea(FixedDim.x5_y0));
    }

    panelAlignEst.add(panelButton);

    //  Construct the main panel from the alignment panel and exist buttons
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    //    rootPanel.setLayout(new BorderLayout());
    JScrollPane scrollPane = new JScrollPane(panelAlignEst);
    rootPanel.add(panelAlignEst, BorderLayout.CENTER);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons, BorderLayout.SOUTH);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the action listeners to the buttons
    AlignmentEstimationActionListner actionListener =
      new AlignmentEstimationActionListner(this);

    buttonComputeAlignment.addActionListener(actionListener);
    buttonView3DModel.addActionListener(actionListener);
    buttonViewResiduals.addActionListener(actionListener);
    buttonImod.addActionListener(actionListener);
    buttonTransferFiducials.addActionListener(actionListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced state
    updateAdvanced(isAdvanced);
    panelTiltalign.setFirstTab();
  }

  public void setTiltalignParams(TiltalignParam tiltalignParam) {
    panelTiltalign.setParameters(tiltalignParam);
  }

  public void getTiltalignParams(TiltalignParam tiltalignParam)
    throws FortranInputSyntaxException {
    try {
      panelTiltalign.getParameters(tiltalignParam);
    }
    catch (FortranInputSyntaxException except) {
      String message = "Axis: " + axisID.getExtension() + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }

  }

  public void setTransferFidParams(TransferfidParam transferFidParam) {
    panelTransferFid.setParameters(transferFidParam);
  }

  public void getTransferFidParams(TransferfidParam transferFidParam) {
    panelTransferFid.getParameters(transferFidParam);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "tiltalign", "xfproduct", "3dmod" };
    String[] manPage = { "tiltalign.html", "xfproduct.html", "3dmod.html" };

    String[] logFileLabel;
    String[] logFile;
    logFileLabel = new String[1];
    logFileLabel[0] = "align";
    logFile = new String[1];
    logFile[0] = "align" + axisID.getExtension() + ".log";

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

  //  Event handler for panel buttons
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(buttonComputeAlignment.getActionCommand())) {
      applicationManager.fineAlignment(axisID);
    }

    else if (command.equals(buttonImod.getActionCommand())) {
      applicationManager.imodFixFiducials(axisID);
    }

    else if (command.equals(buttonView3DModel.getActionCommand())) {
      applicationManager.imodView3DModel(axisID);
    }

    else if (command.equals(buttonViewResiduals.getActionCommand())) {
      applicationManager.imodViewResiduals(axisID);
    }

    else if (command.equals(buttonTransferFiducials.getActionCommand())) {
      applicationManager.transferfid(axisID);
    }
  }

  //  Action function overides for exit buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneAlignmentEstimationDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneAlignmentEstimationDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneAlignmentEstimationDialog(axisID);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced(isAdvanced);
  }

  //  This is a separate function so it can be called at initialization time
  //  as well as from the button action above
  void updateAdvanced(boolean state) {
    panelTiltalign.setAdvanced(isAdvanced);
    if (applicationManager.isDualAxis()) {
      panelTransferFid.setAdvanced(isAdvanced);
    }
    applicationManager.packMainWindow();
  }
}

//  ActionListener class for buttons
class AlignmentEstimationActionListner implements ActionListener {

  AlignmentEstimationDialog adaptee;
  AlignmentEstimationActionListner(AlignmentEstimationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
