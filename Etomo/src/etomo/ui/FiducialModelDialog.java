package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.ConstBeadtrackParam;
import etomo.comscript.TransferfidParam;
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
 * <p> $Log$
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.11  2003/10/30 21:09:41  rickg
 * <p> Bug# 340 Added context menu entry for transferfid man page
 * <p> JToggleButton -> MultilineToggleButton
 * <p>
 * <p> Revision 2.10  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.9  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.8  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.7  2003/10/15 01:34:20  sueh
 * <p> Bug277 added tooltips
 * <p>
 * <p> Revision 2.6  2003/10/10 23:17:01  sueh
 * <p> bug251 removing marks
 * <p>
 * <p> Revision 2.5  2003/10/09 22:49:42  sueh
 * <p> bug251 fixed some null reference problems with transferfid
 * <p> panel in single axis mode
 * <p>
 * <p> Revision 2.4  2003/10/07 22:43:13  sueh
 * <p> bug251 moved transferfid from fine alignment dialog
 * <p> to fiducial model dialog
 * <p>
 * <p> Revision 2.3  2003/05/19 04:31:36  rickg
 * <p> Toggle button for Fix Model
 * <p>
 * <p> Revision 2.2  2003/05/07 17:50:37  rickg
 * <p> Added beadtrack and track.log to context menu
 * <p>
 * <p> Revision 2.1  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.7.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.7  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.6  2002/12/19 06:02:57  rickg
 * <p> Implementing advanced parameters handling
 * <p>
 * <p> Revision 1.5  2002/12/19 00:30:05  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 22:39:42  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class FiducialModelDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlFiducialModel = new JPanel();

  private BeveledBorder border = new BeveledBorder("Fiducial Model Generation");
  private MultiLineToggleButton btnTransferFiducials = null;
  private MultiLineToggleButton btnSeed =
    new MultiLineToggleButton("<html><b>Seed Fiducial Model</b>");

  private TransferfidPanel pnlTransferfid = null;
  private BeadtrackPanel pnlBeadtrack;

  private MultiLineToggleButton btnTrack =
    new MultiLineToggleButton("<html><b>Track Fiducial Seed Model</b>");

  private MultiLineButton btnUseModel =
    new MultiLineButton("<html><b>Use Fiducial Model as Seed</b>");

  private MultiLineToggleButton btnFixModel =
    new MultiLineToggleButton("<html><b>Fix Fiducial Model</b>");

  private boolean transferfidEnabled = false;

  public FiducialModelDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    setToolTipText();
    fixRootPanel(rootSize);

    pnlBeadtrack = new BeadtrackPanel(axisID);

    buttonExecute.setText("Done");

    btnSeed.setAlignmentX(Component.CENTER_ALIGNMENT);
    Dimension dimButton = UIParameters.getButtonDimension();
    btnSeed.setPreferredSize(dimButton);
    btnSeed.setMaximumSize(dimButton);

    btnTrack.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTrack.setPreferredSize(dimButton);
    btnTrack.setMaximumSize(dimButton);

    btnUseModel.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnUseModel.setPreferredSize(dimButton);
    btnUseModel.setMaximumSize(dimButton);

    btnFixModel.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnFixModel.setPreferredSize(dimButton);
    btnFixModel.setMaximumSize(dimButton);

    pnlFiducialModel.setLayout(
      new BoxLayout(pnlFiducialModel, BoxLayout.Y_AXIS));

    pnlFiducialModel.setBorder(border.getBorder());

    if (applicationManager.isDualAxis()) {
      pnlTransferfid = new TransferfidPanel(true);
      pnlFiducialModel.add(pnlTransferfid.getContainer());
      pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));
    }

    pnlFiducialModel.add(btnSeed);
    pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlFiducialModel.add(pnlBeadtrack.getContainer());
    pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlFiducialModel.add(btnTrack);
    pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlFiducialModel.add(btnUseModel);
    pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlFiducialModel.add(btnFixModel);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(pnlFiducialModel);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //
    //  Action listener assignments for the buttons
    //
    btnSeed.addActionListener(new FiducialModelActionListener(this));
    btnTrack.addActionListener(new FiducialModelActionListener(this));
    btnUseModel.addActionListener(new FiducialModelActionListener(this));
    btnFixModel.addActionListener(new FiducialModelActionListener(this));

    if (applicationManager.isDualAxis()) {
      btnTransferFiducials = pnlTransferfid.getButton();
      if (btnTransferFiducials != null) {
        btnTransferFiducials.addActionListener(
          new FiducialModelActionListener(this));
      }
    }

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlFiducialModel.addMouseListener(mouseAdapter);

    //  Set the advanced state to the default
    updateAdvanced(isAdvanced);
    updateEnabled();
  }

  /**
   * Set the advanced state for the dialog box
   */
  public void updateAdvanced(boolean state) {
    pnlBeadtrack.setAdvanced(state);
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.setAdvanced(state);
    }

    applicationManager.packMainWindow();
  }

  public void updateEnabled() {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.setEnabled(transferfidEnabled);
    }
  }
  /**
   * Set the parameters for the specified beadtrack panel
   */
  public void setBeadtrackParams(ConstBeadtrackParam beadtrackParams) {
    pnlBeadtrack.setParameters(beadtrackParams);
  }

  public void setTransferFidParams(TransferfidParam transferFidParam) {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.setParameters(transferFidParam);
    }
  }

  /**
   * Get the parameters for the specified beadtrack command
   */
  public void getBeadtrackParams(BeadtrackParam beadtrackParams)
    throws FortranInputSyntaxException {
    pnlBeadtrack.getParameters(beadtrackParams);
  }

  public void getTransferFidParams(TransferfidParam transferFidParam) {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.getParameters(transferFidParam);
    }
  }

  public void setTransferfidEnabled(boolean fileExists) {
    transferfidEnabled = fileExists;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Transferfid", "Beadtrack", "3dmod" };
    String[] manPage = { "transferfid.html", "beadtrack.html", "3dmod.html" };

    String[] logFileLabel = { "Track", "Transferid" };
    String[] logFile = new String[2];
    logFile[0] = "track" + axisID.getExtension() + ".log";
    logFile[1] = "transferfid.log";

    //    ContextPopup contextPopup =
    new ContextPopup(
      pnlFiducialModel,
      mouseEvent,
      "GETTING FIDUCIAL",
      manPagelabel,
      manPage,
      logFileLabel,
      logFile);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Open new or existing seed model in 3dmod.";
    btnSeed.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Run Beadtrack to produce fiducial model from seed model.";
    btnTrack.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Turn the output of Beadtrack (fiducial model) into a new seed model.  "
        + "Your original seed model will be moved into an _orig.seed file."
        + "To use the new seed model, press Track Fiducial Seed Model.";
    btnUseModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Load fiducial model into 3dmod.";
    btnFixModel.setToolTipText(tooltipFormatter.setText(text).format());
  }

  //  Action function for buttons
  private void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(btnSeed.getActionCommand())) {
      applicationManager.imodSeedFiducials(axisID);
    }

    else if (command.equals(btnTrack.getActionCommand())) {
      applicationManager.fiducialModelTrack(axisID);
    }

    else if (command.equals(btnUseModel.getActionCommand())) {
      applicationManager.makeFiducialModelSeedModel(axisID);
      if (btnTrack.isSelected()) {
        btnTrack.setSelected(false);
      }
    }

    else if (command.equals(btnFixModel.getActionCommand())) {
      applicationManager.imodFixFiducials(axisID);
    }

    else if (command.equals(btnTransferFiducials.getActionCommand())) {
      applicationManager.transferfid(axisID);
    }
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneFiducialModelDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneFiducialModelDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneFiducialModelDialog(axisID);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced(isAdvanced);
  }

  //
  //	Action listener adapters
  //
  class FiducialModelActionListener implements ActionListener {

    FiducialModelDialog adaptee;

    FiducialModelActionListener(FiducialModelDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }
}
