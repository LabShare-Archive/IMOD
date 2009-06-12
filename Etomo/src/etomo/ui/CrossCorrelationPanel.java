/**
 * <p>Description: Panel to modify the tiltxcorr parameters.</p>
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
 * <p> Revision 3.32  2009/03/24 20:27:13  sueh
 * <p> bug# 1201 Added angleOffset.
 * <p>
 * <p> Revision 3.31  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.30  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.29  2009/01/20 19:51:44  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.28  2008/05/28 02:49:46  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.27  2008/05/03 00:49:27  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.26  2007/09/10 20:42:24  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.25  2007/03/21 19:45:28  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.24  2007/03/01 01:28:53  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.23  2007/02/09 00:48:42  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.22  2006/11/07 22:34:29  sueh
 * <p> bug# 954 Completing tooltips for XMinAndMax and YMinAndMax.
 * <p>
 * <p> Revision 3.21  2006/07/04 18:47:23  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change the headers when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 3.20  2006/06/21 15:52:08  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.19  2006/03/30 16:48:02  sueh
 * <p> bug# 437 Passing the dialog type to getCrossCorrelateDIsplay().
 * <p>
 * <p> Revision 3.18  2006/03/27 21:01:10  sueh
 * <p> bug# 836 Moved btnCrossCorrelate to CrossCorrelationPanel.  Added a
 * <p> PanelHeader.
 * <p>
 * <p> Revision 3.17  2006/01/12 17:09:40  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.16  2006/01/03 23:34:36  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.15  2005/11/14 21:48:29  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.14  2005/08/27 22:35:36  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.13  2005/04/25 20:55:02  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.12  2005/02/11 16:45:29  sueh
 * <p> bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 * <p>
 * <p> Revision 3.11  2004/12/02 20:39:19  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.10  2004/11/19 23:50:37  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.9.4.1  2004/10/11 02:12:43  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.9  2004/05/03 18:03:49  sueh
 * <p> bug# 418 standardizing param gets and sets
 * <p>
 * <p> Revision 3.8  2004/04/07 21:03:10  rickg
 * <p> Fixed layout using UIUtilities
 * <p>
 * <p> Revision 3.7  2004/03/13 00:25:00  sueh
 * <p> bug# 412 Right justified checkboxes, changed labels.
 * <p>
 * <p> Revision 3.6  2004/03/12 20:09:33  sueh
 * <p> bug# 412 Added CrossCorrelationActionListener, cbAbsoluteCosineStretch,
 * <p> cbCumulativeCorrelation, cbNoCosineStretch, XMinAndMax, YMinAndMax,
 * <p> ltfTestOutput, updateCrossCorrelationPanel() - enables/disables fields.
 * <p> Removed ltfInputFile, ltfOutputFile.
 * <p>
 * <p> Revision 3.5  2004/03/11 19:43:45  sueh
 * <p> bug# 372 removing FilterSigma2, change text and order of FilterSigma1,
 * <p> FilterRadius2, FilterRadius1
 * <p>
 * <p> Revision 3.4  2004/02/05 04:49:22  rickg
 * <p> Added tiltxcorr border, simplified layout
 * <p>
 * <p> Revision 3.3  2004/01/30 01:30:26  sueh
 * <p> bug# 373 split filter parameters into four fields, changed
 * <p> tiltxcorrParam function calls
 * <p>
 * <p> Revision 3.2  2003/12/31 01:34:44  sueh
 * <p> bug# 372 tooltips moved to autodoc where possible, tooltips
 * <p> coming from autodoc
 * <p>
 * <p> Revision 3.1  2003/12/23 21:33:38  sueh
 * <p> bug# 372 Adding commented out test code.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/11/06 22:45:01  sueh
 * <p> cleaning up tasks
 * <p>
 * <p> Revision 2.6  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.5  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.4  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.3  2003/10/14 21:56:34  sueh
 * <p> Bug273 add tooltips
 * <p>
 * <p> Revision 2.2  2003/10/13 23:00:48  sueh
 * <p> removed PieceListFile, rename fields, move field to advanced
 * <p>
 * <p> Revision 2.1  2003/09/09 17:15:02  rickg
 * <p> Changed view list to view range
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
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
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltxcorrParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.ProcessResultDisplay;

final class CrossCorrelationPanel implements ContextMenu, Expandable,
    TiltXcorrDisplay {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlCrossCorrelation = new EtomoPanel();
  private final JPanel pnlBody = new JPanel();
  private final JPanel pnlAdvanced = new JPanel();
  private final JPanel pnlXMinAndMax = new JPanel();
  private final JPanel pnlYMinAndMax = new JPanel();
  private final ApplicationManager applicationManager;

  private final CheckBox cbExcludeCentralPeak = new CheckBox(
      "Exclude central peak due to fixed pattern noise");

  private final LabeledTextField ltfTestOutput = new LabeledTextField(
      "Test output: ");
  private final LabeledTextField ltfFilterSigma1 = new LabeledTextField(
      "Low frequency rolloff sigma: ");
  private final LabeledTextField ltfFilterRadius2 = new LabeledTextField(
      "High frequency cutoff radius: ");
  private final LabeledTextField ltfFilterSigma2 = new LabeledTextField(
      "High frequency rolloff sigma: ");
  private final LabeledTextField ltfTrim = new LabeledTextField(
      "Pixels to trim (x,y): ");
  private final LabeledTextField ltfXMin = new LabeledTextField("X axis min ");
  private final LabeledTextField ltfXMax = new LabeledTextField("Max ");
  private final LabeledTextField ltfYMin = new LabeledTextField("Y axis min ");
  private final LabeledTextField ltfYMax = new LabeledTextField("Max ");
  private final LabeledTextField ltfPadPercent = new LabeledTextField(
      "Pixels to pad (x,y): ");
  private final LabeledTextField ltfTaperPercent = new LabeledTextField(
      "Pixels to taper (x,y): ");
  private final CheckBox cbCumulativeCorrelation = new CheckBox(
      "Cumulative correlation");
  private final CheckBox cbAbsoluteCosineStretch = new CheckBox(
      "Absolute Cosine Stretch");
  private final CheckBox cbNoCosineStretch = new CheckBox("No Cosine Stretch");
  private final LabeledTextField ltfViewRange = new LabeledTextField(
      "View range (start,end): ");
  private final LabeledTextField ltfAngleOffset = new LabeledTextField(
      "Tilt angle offset: ");

  private final AxisID axisID;
  private final DialogType dialogType;

  private final MultiLineButton btnCrossCorrelate;
  private final CrossCorrelationActionListener actionListener;
  private final PanelHeader header;

  public CrossCorrelationPanel(ApplicationManager applicationManager,
      AxisID id, DialogType dialogType) {
    this.dialogType = dialogType;
    axisID = id;
    this.applicationManager = applicationManager;
    header = PanelHeader
        .getAdvancedBasicInstance("Tiltxcorr", this, dialogType);
    btnCrossCorrelate = (MultiLineButton) applicationManager
        .getProcessResultDisplayFactory(axisID).getCrossCorrelate();

    // Construct the min and max subpanels
    pnlXMinAndMax.setLayout(new BoxLayout(pnlXMinAndMax, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlXMinAndMax, ltfXMin.getContainer());
    UIUtilities.addWithXSpace(pnlXMinAndMax, ltfXMax.getContainer());

    pnlYMinAndMax.setLayout(new BoxLayout(pnlYMinAndMax, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlYMinAndMax, ltfYMin.getContainer());
    UIUtilities.addWithXSpace(pnlYMinAndMax, ltfYMax.getContainer());

    //  Construct the advanced panel
    pnlAdvanced.setLayout(new BoxLayout(pnlAdvanced, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlAdvanced, ltfAngleOffset.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, ltfFilterSigma1.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, ltfFilterRadius2.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, ltfFilterSigma2.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, ltfTrim.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, pnlXMinAndMax);
    UIUtilities.addWithYSpace(pnlAdvanced, pnlYMinAndMax);
    UIUtilities.addWithYSpace(pnlAdvanced, ltfPadPercent.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, ltfTaperPercent.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, cbCumulativeCorrelation);
    UIUtilities.addWithYSpace(pnlAdvanced, cbAbsoluteCosineStretch);
    UIUtilities.addWithYSpace(pnlAdvanced, cbNoCosineStretch);
    UIUtilities.addWithYSpace(pnlAdvanced, cbExcludeCentralPeak);
    UIUtilities.addWithYSpace(pnlAdvanced, ltfTestOutput.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced, ltfViewRange.getContainer());

    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlBody.add(pnlAdvanced);
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    btnCrossCorrelate.setSize();
    pnlBody.add(btnCrossCorrelate.getComponent());
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    UIUtilities.alignComponentsX(pnlAdvanced, Component.LEFT_ALIGNMENT);
    UIUtilities.alignComponentsX(pnlBody, Component.CENTER_ALIGNMENT);

    pnlCrossCorrelation.setLayout(new BoxLayout(pnlCrossCorrelation,
        BoxLayout.Y_AXIS));
    pnlCrossCorrelation.setBorder(BorderFactory.createEtchedBorder());
    pnlCrossCorrelation.add(header);
    pnlCrossCorrelation.add(pnlBody);
    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCrossCorrelation.addMouseListener(mouseAdapter);

    actionListener = new CrossCorrelationActionListener(this);
    cbCumulativeCorrelation.addActionListener(actionListener);
    cbNoCosineStretch.addActionListener(actionListener);
    btnCrossCorrelate.addActionListener(actionListener);
    setToolTipText();
  }

  void done() {
    btnCrossCorrelate.removeActionListener(actionListener);
  }

  /**
   * Update the header with the current advanced state
   */
  void updateAdvanced(boolean isAdvanced) {
    header.setAdvanced(isAdvanced);
  }

  void setAdvanced(boolean state) {
    pnlAdvanced.setVisible(state);
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      setAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  JPanel getPanel() {
    return pnlCrossCorrelation;
  }

  static ProcessResultDisplay getCrossCorrelateDisplay(DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance(
        "Calculate Cross- Correlation", dialogType);
  }

  /**
   * Set the field values for the panel from the ConstTiltxcorrParam object
   */
  public void setParameters(ConstTiltxcorrParam tiltXcorrParams) {
    cbExcludeCentralPeak.setSelected(tiltXcorrParams.getExcludeCentralPeak());
    ltfFilterSigma1.setText(tiltXcorrParams.getFilterSigma1String());
    ltfFilterRadius2.setText(tiltXcorrParams.getFilterRadius2String());
    ltfFilterSigma2.setText(tiltXcorrParams.getFilterSigma2String());
    ltfTrim.setText(tiltXcorrParams.getBordersInXandY());
    ltfXMin.setText(tiltXcorrParams.getXMinString());
    ltfXMax.setText(tiltXcorrParams.getXMaxString());
    ltfYMin.setText(tiltXcorrParams.getYMinString());
    ltfYMax.setText(tiltXcorrParams.getYMaxString());
    ltfPadPercent.setText(tiltXcorrParams.getPadsInXandYString());
    ltfTaperPercent.setText(tiltXcorrParams.getTaperPercentString());
    cbCumulativeCorrelation.setSelected(tiltXcorrParams
        .isCumulativeCorrelation());
    cbAbsoluteCosineStretch.setSelected(tiltXcorrParams
        .isAbsoluteCosineStretch());
    cbNoCosineStretch.setSelected(tiltXcorrParams.isNoCosineStretch());
    ltfTestOutput.setText(tiltXcorrParams.getTestOutput());
    ltfViewRange.setText(tiltXcorrParams.getStartingEndingViews());
    ltfAngleOffset.setText(tiltXcorrParams.getAngleOffset());
    updateCrossCorrelationPanel();
  }

  public void setParameters(BaseScreenState screenState) {
    //btnCrossCorrelate.setButtonState(screenState
    //    .getButtonState(btnCrossCorrelate.getButtonStateKey()));
    header.setButtonStates(screenState);
  }

  public void getParameters(BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  /**
   * Get the field values from the panel filling in the TiltxcorrParam object
   */
  public void getParameters(TiltxcorrParam tiltXcorrParams)
      throws FortranInputSyntaxException {
    tiltXcorrParams.setExcludeCentralPeak(cbExcludeCentralPeak.isSelected());
    tiltXcorrParams.setTestOutput(ltfTestOutput.getText());
    String currentParam = "unknown";
    try {
      currentParam = ltfFilterSigma1.getLabel();
      tiltXcorrParams.setFilterSigma1(ltfFilterSigma1.getText());
      currentParam = ltfFilterRadius2.getLabel();
      tiltXcorrParams.setFilterRadius2(ltfFilterRadius2.getText());
      currentParam = ltfFilterSigma2.getLabel();
      tiltXcorrParams.setFilterSigma2(ltfFilterSigma2.getText());
      currentParam = ltfTrim.getLabel();
      tiltXcorrParams.setBordersInXandY(ltfTrim.getText());
      currentParam = "X" + ltfXMin.getLabel();
      tiltXcorrParams.setXMin(ltfXMin.getText());
      currentParam = "X" + ltfXMax.getLabel();
      tiltXcorrParams.setXMax(ltfXMax.getText());
      currentParam = "Y" + ltfYMin.getLabel();
      tiltXcorrParams.setYMin(ltfYMin.getText());
      currentParam = "Y" + ltfYMax.getLabel();
      tiltXcorrParams.setYMax(ltfYMax.getText());
      currentParam = ltfPadPercent.getLabel();
      tiltXcorrParams.setPadsInXandY(ltfPadPercent.getText());
      currentParam = ltfTaperPercent.getLabel();
      tiltXcorrParams.setTapersInXandY(ltfTaperPercent.getText());
      currentParam = cbCumulativeCorrelation.getText();
      tiltXcorrParams.setCumulativeCorrelation(cbCumulativeCorrelation
          .isSelected());
      currentParam = cbAbsoluteCosineStretch.getText();
      tiltXcorrParams.setAbsoluteCosineStretch(cbAbsoluteCosineStretch
          .isSelected());
      currentParam = cbNoCosineStretch.getText();
      tiltXcorrParams.setNoCosineStretch(cbNoCosineStretch.isSelected());
      currentParam = ltfViewRange.getLabel();
      tiltXcorrParams.setStartingEndingViews(ltfViewRange.getText());
      currentParam = ltfAngleOffset.getLabel();
      tiltXcorrParams.setAngleOffset(ltfAngleOffset.getText());
    }
    catch (FortranInputSyntaxException except) {
      String message = currentParam + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
  }

  void setVisible(boolean state) {
    pnlCrossCorrelation.setVisible(state);
  }

  void setAlignmentX(float align) {
    pnlCrossCorrelation.setAlignmentX(align);
  }

  void updateCrossCorrelationPanel() {
    if (cbCumulativeCorrelation.isSelected() && !cbNoCosineStretch.isSelected()) {
      cbAbsoluteCosineStretch.setEnabled(true);
    }
    else {
      cbAbsoluteCosineStretch.setSelected(false);
      cbAbsoluteCosineStretch.setEnabled(false);
    }
  }

  //  Action functions for setup panel buttons
  protected void buttonAction(ActionEvent event) {
    if (event.getActionCommand().equals(btnCrossCorrelate.getActionCommand())) {
      applicationManager.preCrossCorrelate(axisID, btnCrossCorrelate, null,
          dialogType, this);
    }
    else {
      updateCrossCorrelationPanel();
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Tiltxcorr" };
    String[] manPage = { "tiltxcorr.html" };
    String[] logFileLabel = { "Xcorr" };
    String[] logFile = new String[1];
    logFile[0] = "xcorr" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlCrossCorrelation,
        mouseEvent, "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel,
        manPage, logFileLabel, logFile, applicationManager, axisID);
  }

  class CrossCorrelationActionListener implements ActionListener {

    CrossCorrelationPanel adaptee;

    public CrossCorrelationActionListener(CrossCorrelationPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }

  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc autodoc = null;

    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.TILTXCORR, axisID,
          applicationManager.getManagerKey());
      //autodoc.print();
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    ltfTestOutput
        .setToolTipText(EtomoAutodoc.getTooltip(autodoc, "TestOutput"));
    ltfFilterSigma1.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "FilterSigma1"));
    ltfFilterRadius2.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "FilterRadius2"));
    ltfFilterSigma2.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "FilterSigma2"));
    ltfTrim.setToolTipText(EtomoAutodoc.getTooltip(autodoc, "BordersInXandY"));
    text = EtomoAutodoc.getTooltip(autodoc, "XMinAndMax");
    if (text != null) {
      pnlXMinAndMax.setToolTipText(text);
      ltfXMin.setToolTipText(text);
      ltfXMax.setToolTipText(text);
    }
    text = EtomoAutodoc.getTooltip(autodoc, "YMinAndMax");
    if (text != null) {
      pnlYMinAndMax.setToolTipText(text);
      ltfYMin.setToolTipText(text);
      ltfYMax.setToolTipText(text);
    }
    ltfPadPercent.setToolTipText(EtomoAutodoc
        .getTooltip(autodoc, "PadsInXandY"));
    ltfTaperPercent.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "TapersInXandY"));
    cbCumulativeCorrelation.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "CumulativeCorrelation"));
    cbAbsoluteCosineStretch.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "AbsoluteCosineStretch"));
    cbNoCosineStretch.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "NoCosineStretch"));
    ltfViewRange.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "StartingEndingViews"));
    cbExcludeCentralPeak.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "ExcludeCentralPeak"));
    ltfAngleOffset.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        "AngleOffset"));
    btnCrossCorrelate
        .setToolTipText("Find alignment transformations between successive images by cross-correlation.");
  }
}