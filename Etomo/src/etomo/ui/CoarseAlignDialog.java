/**
 * <p>Description: Panel to present the coarse alignment procedure</p>
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
 * <p> Revision 3.19  2005/04/22 00:14:25  sueh
 * <p> bug# 615 removed test raised beveled button.
 * <p>
 * <p> Revision 3.18  2005/04/21 20:32:08  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.17  2005/04/16 01:54:11  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.16  2005/04/07 22:03:22  sueh
 * <p> bug# 626 Added Make Distortion Corrected Stack button.
 * <p>
 * <p> Revision 3.15  2005/03/09 22:32:23  sueh
 * <p> bug# 533 Modify the context sensitive help so that it displays blendmont
 * <p> man pages and log files instead of newst when the view type is montage.
 * <p>
 * <p> Revision 3.14  2005/03/08 02:01:26  sueh
 * <p> bug# 533 Add btnEdgesMidas.
 * <p>
 * <p> Revision 3.13  2005/01/14 03:07:08  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.12  2004/12/02 20:37:18  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.11  2004/11/19 23:49:54  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.10.4.1  2004/10/11 02:11:10  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.10  2004/06/17 18:48:45  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.9  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.8  2004/05/25 23:24:32  rickg
 * <p> Bug #391 added fiducialess parameter interface and UI objects
 * <p>
 * <p> Revision 3.7  2004/04/23 19:38:43  rickg
 * <p> Method name change for opening 3dmod on the coarse
 * <p> aligned stack
 * <p>
 * <p> Revision 3.6  2004/04/07 21:03:10  rickg
 * <p> Fixed layout using UIUtilities
 * <p>
 * <p> Revision 3.5  2004/04/06 16:58:56  rickg
 * <p> Added fiducialess alignment methods
 * <p>
 * <p> Revision 3.4  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.3  2004/03/13 00:34:13  rickg
 * <p> Bug# 390 Add set/get prenewst parameters
 * <p>
 * <p> Revision 3.2  2004/02/05 04:35:07  rickg
 * <p> Added prenewst panel with binning
 * <p>
 * <p> Revision 3.1  2004/01/30 22:45:01  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.6  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.5  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.4  2003/10/14 21:56:05  sueh
 * <p> Bug273 add tooltips
 * <p>
 * <p> Revision 2.3  2003/05/23 22:14:55  rickg
 * <p> Added xcorr log file to context menu
 * <p>
 * <p> Revision 2.2  2003/05/19 22:06:43  rickg
 * <p> Fixed cross correlation button text
 * <p>
 * <p> Revision 2.1  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 00:28:41  rickg
 * <p> Advanced handling implemented
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 22:38:59  rickg
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

package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.comscript.TiltxcorrParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.ViewType;

public class CoarseAlignDialog extends ProcessDialog
    implements ContextMenu, FiducialessParams {
  public static final String rcsid = "$Id$";

  private JPanel pnlCoarseAlign = new JPanel();

  private CrossCorrelationPanel pnlCrossCorrelation;

  private MultiLineToggleButton btnCrossCorrelate = new MultiLineToggleButton(
    "Calculate Cross- Correlation");

  private PrenewstPanel pnlPrenewst;

  private MultiLineToggleButton btnCoarseAlign = new MultiLineToggleButton(
    "Generate Coarse Aligned Stack");

  private MultiLineToggleButton btnImod = new MultiLineToggleButton(
    "View Aligned<br>Stack In 3dmod");

  private JPanel pnlFiducialess = new JPanel();
  private JCheckBox cbFiducialess = new JCheckBox("Fiducialless alignment");
  private LabeledTextField ltfRotation = new LabeledTextField(
    "Tilt axis rotation:");

  private MultiLineToggleButton btnMidas = new MultiLineToggleButton(
    "<html><b>Fix Alignment<br>With Midas</b>");
  
  //Montaging
  private MultiLineToggleButton btnFixEdgesMidas = new MultiLineToggleButton(
  "Fix Edges With Midas");
  private MultiLineToggleButton btnDistortionCorrectedStack = new MultiLineToggleButton(
  "Make Distortion Corrected Stack");


  public CoarseAlignDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID, DialogType.COARSE_ALIGNMENT);
    ConstMetaData metaData = appMgr.getMetaData();
    setToolTipText();
    fixRootPanel(rootSize);
    pnlCrossCorrelation = new CrossCorrelationPanel(applicationManager, axisID);
    pnlPrenewst = new PrenewstPanel(applicationManager, axisID);
    btnExecute.setText("Done");

    pnlFiducialess.setLayout(new BoxLayout(pnlFiducialess, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlFiducialess, cbFiducialess);
    UIUtilities.addWithYSpace(pnlFiducialess, ltfRotation.getContainer());
    UIUtilities.alignComponentsX(pnlFiducialess, Component.LEFT_ALIGNMENT);

    pnlCoarseAlign.setLayout(new BoxLayout(pnlCoarseAlign, BoxLayout.Y_AXIS));
    pnlCoarseAlign.setBorder(new BeveledBorder("Coarse Alignment").getBorder());
    UIUtilities.addWithSpace(pnlCoarseAlign, pnlCrossCorrelation.getPanel(),
      FixedDim.x0_y10);
    UIUtilities
      .addWithSpace(pnlCoarseAlign, btnCrossCorrelate, FixedDim.x0_y10);
    if (metaData.getViewType() == ViewType.MONTAGE) {
      SpacedPanel pnlFixEdges = new SpacedPanel(FixedDim.x0_y10);
      pnlFixEdges.setLayout(new BoxLayout(pnlFixEdges.getContainer(), BoxLayout.Y_AXIS));
      pnlFixEdges.setBorder(new EtchedBorder("Fix Edges").getBorder());
      pnlFixEdges.addMultiLineButton(btnDistortionCorrectedStack);
      pnlFixEdges.addMultiLineButton(btnFixEdgesMidas);
      pnlCoarseAlign.add(pnlFixEdges.getContainer());
      if (!metaData.isDistortionCorrection()) {
        btnDistortionCorrectedStack.setEnabled(false);
      }
      setEnabledFixEdgesMidasButton();
    }
    UIUtilities.addWithSpace(pnlCoarseAlign, pnlPrenewst.getPanel(),
      FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlCoarseAlign, btnCoarseAlign, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlCoarseAlign, btnImod, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlCoarseAlign, pnlFiducialess, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlCoarseAlign, btnMidas, FixedDim.x0_y10);

    // Set the alignment and size of the UI objects
    UIUtilities.alignComponentsX(pnlCoarseAlign, Component.CENTER_ALIGNMENT);
    UIUtilities.setButtonSizeAll(pnlCoarseAlign, UIParameters.dimButton);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    UIUtilities.addWithSpace(rootPanel, pnlCoarseAlign, FixedDim.x0_y10);
    addExitButtons();

    //  Action listener assignment for the buttons
    ActionListener actionListener = new CoarseAlignActionListener(this);
    btnCrossCorrelate.addActionListener(actionListener);
    btnCoarseAlign.addActionListener(actionListener);
    btnImod.addActionListener(actionListener);
    btnMidas.addActionListener(actionListener);
    btnFixEdgesMidas.addActionListener(actionListener);
    btnDistortionCorrectedStack.addActionListener(actionListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCoarseAlign.addMouseListener(mouseAdapter);

    // Set the default advanced state for the window
    updateAdvanced();
  }
  
  /**
   * Enable Fix Edges with Midas button if there are no distortion correction files
   * in use or if the .dcst file (distortion corrected stack) has been created.
   *
   */
  public void setEnabledFixEdgesMidasButton() {
    if (btnDistortionCorrectedStack.isEnabled()
        && !BlendmontParam.getDistortionCorrectedFile(
            applicationManager.getPropertyUserDir(),
            applicationManager.getMetaData().getDatasetName(), axisID).exists()) {
      btnFixEdgesMidas.setEnabled(false);
    }
    else {
      btnFixEdgesMidas.setEnabled(true);
    }
  }

  /**
   * Set the parameters for the cross correlation panel
   */
  public void setCrossCorrelationParams(ConstTiltxcorrParam tiltXcorrParams) {
    pnlCrossCorrelation.setParameters(tiltXcorrParams);
  }

  /**
   * Get the parameters from the specified cross correlation panel
   */
  public void getCrossCorrelationParams(TiltxcorrParam tiltXcorrParams)
      throws FortranInputSyntaxException {
    pnlCrossCorrelation.getParameters(tiltXcorrParams);
  }

  /**
   * Set the prenewst params of the prenewst panel
   * @param prenewstParam
   */
  public void setPrenewstParams(ConstNewstParam prenewstParam) {
    pnlPrenewst.setParameters(prenewstParam);
  }

  /**
   * Get thre prenewst params from the prenewst panel
   * @param prenewstParam
   */
  public void getPrenewstParams(NewstParam prenewstParam) {
    pnlPrenewst.getParameters(prenewstParam);
  }

  public void setFiducialessAlignment(boolean state) {
    cbFiducialess.setSelected(state);
  }

  public boolean isFiducialessAlignment() {
    return cbFiducialess.isSelected();
  }

  public void setTiltAxisAngle(float tiltAxisAngle) {
    ltfRotation.setText(tiltAxisAngle);
  }

  public float getTiltAxisAngle() throws NumberFormatException {
    return Float.parseFloat(ltfRotation.getText());
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  void updateAdvanced() {
    pnlCrossCorrelation.setAdvanced(isAdvanced);
    pnlPrenewst.getPanel().setVisible(isAdvanced);
    applicationManager.packMainWindow(axisID);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String alignManpageLabel;
    String alignManpage;
    String alignLogfileLabel;
    String alignLogfile;
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      alignManpageLabel = "Blendmont";
      alignManpage = "blendmont";
      alignLogfileLabel = "Preblend";
      alignLogfile = "preblend";
    }
    else {
      alignManpageLabel = "Newstack";
      alignManpage = "newstack";
      alignLogfileLabel = "Prenewst";
      alignLogfile = "prenewst";
    }
    String[] manPagelabel = {"Tiltxcorr", "Xftoxg", alignManpageLabel, "3dmod", "Midas"};
    String[] manPage = {"tiltxcorr.html", "xftoxg.html", alignManpage + ".html", "3dmod.html",
        "midas.html"};
    String[] logFileLabel = {"Xcorr", alignLogfileLabel};
    String[] logFile = new String[2];
    logFile[0] = "xcorr" + axisID.getExtension() + ".log";
    logFile[1] = alignLogfile + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlCoarseAlign, mouseEvent,
      "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, applicationManager);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text = "Find alignment transformations between successive images by cross-correlation.";
    btnCrossCorrelate.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Use transformations to produce stack of aligned images.";
    btnCoarseAlign.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Use 3dmod to view the coarsely aligned images.";
    btnImod.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Enable or disable the processing flow using cross-correlation alignment only.";
    cbFiducialess.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Use Midas to adjust bad alignments.";
    btnMidas.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Initial rotation angle of tilt axis when viewing images in Midas.";
    ltfRotation.setToolTipText(tooltipFormatter.setText(text).format());
  }

  /**
   * Action function for process buttons
   * @param event
   */
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnCrossCorrelate.getActionCommand())) {
      applicationManager.crossCorrelate(axisID);
    }
    else if (command.equals(btnCoarseAlign.getActionCommand())) {
      applicationManager.coarseAlign(axisID);
    }
    else if (command.equals(btnImod.getActionCommand())) {
      applicationManager.imodCoarseAlign(axisID);
    }
    else if (command.equals(btnMidas.getActionCommand())) {
      applicationManager.midasRawStack(axisID);
    }
    else if (command.equals(btnFixEdgesMidas.getActionCommand())) {
      applicationManager.midasFixEdges(axisID);
    }
    else if (command.equals(btnDistortionCorrectedStack.getActionCommand())) {
      applicationManager.makeDistortionCorrectedStack(axisID);
    }
  }

  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  class CoarseAlignActionListener implements ActionListener {
    CoarseAlignDialog adaptee;

    CoarseAlignActionListener(CoarseAlignDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }
}