package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import etomo.ApplicationManager;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltalignParam;
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
 * <p> Revision 3.5  2004/07/02 00:34:46  sueh
 * <p> bug# 461 construct TiltalignPanel with info from fid.xyz,
 * <p> preali header, and raw stack header
 * <p>
 * <p> Revision 3.4  2004/06/21 17:16:37  rickg
 * <p> Bug #461 z shift is scaled by the prealigned binning
 * <p>
 * <p> Revision 3.3  2004/06/05 00:51:25  sueh
 * <p> bug# 433 passing applicationManager to ContextPopup so that new
 * <p> versions of the ta* align logs can be generated.
 * <p>
 * <p> Revision 3.2  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.1  2004/01/30 22:44:22  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.14  2003/10/28 21:22:50  rickg
 * <p> Bug# 280 Tooltips
 * <p>
 * <p> Revision 2.13  2003/10/28 00:22:07  rickg
 * <p> Bug# 309 Capitalized align log tabs
 * <p>
 * <p> Revision 2.12  2003/10/28 00:13:41  rickg
 * <p> Bug# 309 Capitalized align log tabs
 * <p>
 * <p> Revision 2.11  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.10  2003/10/09 23:23:19  rickg
 * <p> Fixed context menu (needed to remove transferfid entries)
 * <p>
 * <p> Revision 2.9  2003/10/07 22:43:13  sueh
 * <p> bug251 moved transferfid from fine alignment dialog
 * <p> to fiducial model dialog
 * <p>
 * <p> Revision 2.8  2003/06/05 21:08:03  rickg
 * <p> Label change for transferfid button
 * <p>
 * <p> Revision 2.7  2003/06/05 04:42:01  rickg
 * <p> Button order swap
 * <p>
 * <p> Revision 2.6  2003/06/04 23:42:29  rickg
 * <p> Added independent labels for tabs
 * <p> Log file tab name changes
 * <p>
 * <p> Revision 2.5  2003/05/27 17:24:03  rickg
 * <p> Rearranged align log order and added locals
 * <p>
 * <p> Revision 2.4  2003/05/27 08:48:24  rickg
 * <p> Tabbed window for align log
 * <p>
 * <p> Revision 2.3  2003/05/23 22:50:13  rickg
 * <p> Removed any extensions from log file labels in context menu
 * <p>
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

public class AlignmentEstimationDialog extends ProcessDialog
  implements ContextMenu {

  public static final String rcsid = "$Id$";

  private JPanel pnlAlignEst = new JPanel();

  private BeveledBorder border = new BeveledBorder("Fine Alignment");

  private TiltalignPanel pnlTiltalign;

  private JPanel panelButton = new JPanel();

  private MultiLineToggleButton btnComputeAlignment = new MultiLineToggleButton(
    "<html><b>Compute Alignment</b>");

  private MultiLineButton btnImod = new MultiLineButton(
    "<html><b>View/Edit Fiducial Model</b>");

  private MultiLineButton btnView3DModel = new MultiLineButton(
    "<html><b>View 3D Model</b>");

  private MultiLineButton btnViewResiduals = new MultiLineButton(
    "<html><b>View Residual Vectors</b>");

  public AlignmentEstimationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    pnlTiltalign =
      new TiltalignPanel(
        axisID,
        appMgr.getFidXyz(axisID),
        appMgr.getMrcHeader(axisID, ".preali"),
        appMgr.getMrcHeader(axisID, ".st"));
    btnExecute.setText("Done");

    //  Create the first tiltalign panel
    panelButton.setLayout(new BoxLayout(panelButton, BoxLayout.Y_AXIS));

    Dimension dimButton = UIParameters.getButtonDimension();
    btnComputeAlignment.setPreferredSize(dimButton);
    btnComputeAlignment.setMaximumSize(dimButton);
    btnImod.setPreferredSize(dimButton);
    btnImod.setMaximumSize(dimButton);
    btnViewResiduals.setPreferredSize(dimButton);
    btnViewResiduals.setMaximumSize(dimButton);
    btnView3DModel.setPreferredSize(dimButton);
    btnView3DModel.setMaximumSize(dimButton);
    
    SpacedPanel topButtonPanel = new SpacedPanel(FixedDim.x10_y0);
    topButtonPanel.setLayout(new BoxLayout(topButtonPanel.getContainer(), BoxLayout.X_AXIS));
    topButtonPanel.add(btnComputeAlignment);
    //panelButton.add(Box.createRigidArea(FixedDim.x10_y0));
    topButtonPanel.add(btnImod);
    panelButton.add(topButtonPanel.getContainer());
    panelButton.add(Box.createRigidArea(FixedDim.x10_y0));
    SpacedPanel bottomButtonPanel = new SpacedPanel(FixedDim.x10_y0);
    bottomButtonPanel.setLayout(new BoxLayout(bottomButtonPanel.getContainer(), BoxLayout.X_AXIS));
    bottomButtonPanel.add(btnView3DModel);
    //panelButton.add(Box.createRigidArea(FixedDim.x10_y0));
    bottomButtonPanel.add(btnViewResiduals);
    panelButton.add(bottomButtonPanel.getContainer());
    
    pnlAlignEst.setLayout(new BoxLayout(pnlAlignEst, BoxLayout.Y_AXIS));
    pnlAlignEst.setBorder(border.getBorder());

    pnlAlignEst.add(pnlTiltalign.getContainer());
    pnlAlignEst.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlAlignEst.add(panelButton);

    //  Construct the main panel from the alignment panel and exist buttons
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    //    rootPanel.setLayout(new BorderLayout());
    JScrollPane scrollPane = new JScrollPane(pnlAlignEst);
    rootPanel.add(pnlAlignEst, BorderLayout.CENTER);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlExitButtons, BorderLayout.SOUTH);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the action listeners to the buttons
    AlignmentEstimationActionListner actionListener = new AlignmentEstimationActionListner(
      this);

    btnComputeAlignment.addActionListener(actionListener);
    btnView3DModel.addActionListener(actionListener);
    btnViewResiduals.addActionListener(actionListener);
    btnImod.addActionListener(actionListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    pnlTiltalign.getContainer().addMouseListener(mouseAdapter);

    // Set the default advanced state
    updateAdvanced(isAdvanced);
    pnlTiltalign.setFirstTab();
    setToolTipText();
  }

  public void setTiltalignParams(TiltalignParam tiltalignParam) {
    pnlTiltalign.setParameters(tiltalignParam);
  }

  public void getTiltalignParams(TiltalignParam tiltalignParam)
    throws FortranInputSyntaxException {
    try {
      pnlTiltalign.getParameters(tiltalignParam);
    }
    catch (FortranInputSyntaxException except) {
      String message = "Axis: " + axisID.getExtension() + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }

  }

  /**
   * Set the prealigned binning for handling the z shift parameter
   * @param binning
   */
  public void setPrealignedBinning(int binning) {
    pnlTiltalign.setPrealignedBinning(binning);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = {"Tiltalign", "Xfproduct", "3dmod"};
    String[] manPage = {"tiltalign.html", "xfproduct.html", "3dmod.html"};
    Vector logFileLabel = new Vector(1);
    String[] logWindowLabel = {"Align"};

    if (axisID != AxisID.ONLY) {
      logWindowLabel[0] = "Align Axis:" + axisID.getExtension();
    }
    String alignCommandName = logWindowLabel[0];

    String[] alignLabels = {"Errors", "Solution", "Surface Angles", "Locals",
        "Complete Log", "Large Residual", "Mappings", "Coordinates"};
    logFileLabel.add(alignLabels);

    Vector logFile = new Vector(1);
    String[] logFileList = new String[8];
    logFileList[0] = "taError" + axisID.getExtension() + ".log";
    logFileList[1] = "taSolution" + axisID.getExtension() + ".log";
    logFileList[2] = "taAngles" + axisID.getExtension() + ".log";
    logFileList[3] = "taLocals" + axisID.getExtension() + ".log";
    logFileList[4] = "align" + axisID.getExtension() + ".log";
    logFileList[5] = "taResiduals" + axisID.getExtension() + ".log";
    logFileList[6] = "taMappings" + axisID.getExtension() + ".log";
    logFileList[7] = "taCoordinates" + axisID.getExtension() + ".log";

    logFile.add(logFileList);

    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
      "FINAL ALIGNMENT", manPagelabel, manPage, logWindowLabel, logFileLabel,
      logFile, applicationManager, alignCommandName, axisID);
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
  private void updateAdvanced(boolean state) {
    pnlTiltalign.setAdvanced(isAdvanced);
    applicationManager.packMainWindow();
  }

  //  Event handler for panel buttons
  private void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(btnComputeAlignment.getActionCommand())) {
      applicationManager.fineAlignment(axisID);
    }

    else if (command.equals(btnImod.getActionCommand())) {
      applicationManager.imodFixFiducials(axisID);
    }

    else if (command.equals(btnView3DModel.getActionCommand())) {
      applicationManager.imodView3DModel(axisID);
    }

    else if (command.equals(btnViewResiduals.getActionCommand())) {
      applicationManager.imodViewResiduals(axisID);
    }
  }

  //	ActionListener class for buttons
  class AlignmentEstimationActionListner implements ActionListener {

    AlignmentEstimationDialog adaptee;

    AlignmentEstimationActionListner(AlignmentEstimationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text = "Run Tiltalign with current parameters.";
    btnComputeAlignment.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View fiducial model on the image stack in 3dmod.";
    btnImod.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View model of solved 3D locations of fiducial points in 3dmodv.";
    btnView3DModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Show model of residual vectors (exaggerated 10x) on the image stack.";
    btnViewResiduals.setToolTipText(tooltipFormatter.setText(text).format());
  }
}