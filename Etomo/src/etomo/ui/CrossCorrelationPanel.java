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

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltxcorrParam;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;

public class CrossCorrelationPanel implements ContextMenu {
  public static final String rcsid = "$Id$";

  private JPanel pnlCrossCorrelation = new JPanel();
  private JPanel pnlAdvanced = new JPanel();
  private JPanel pnlXMinAndMax = new JPanel();
  private JPanel pnlYMinAndMax = new JPanel();
  private ApplicationManager applicationManager;

  private JCheckBox cbExcludeCentralPeak = new JCheckBox(
    "Exclude central peak due to fixed pattern noise");

  private LabeledTextField ltfTestOutput = new LabeledTextField("Test output: ");
  private LabeledTextField ltfFilterSigma1 = new LabeledTextField(
    "Low frequency rolloff sigma: ");
  private LabeledTextField ltfFilterRadius2 = new LabeledTextField(
    "High frequency cutoff radius: ");
  private LabeledTextField ltfFilterSigma2 = new LabeledTextField(
    "High frequency rolloff sigma: ");
  private LabeledTextField ltfTrim = new LabeledTextField(
    "Pixels to trim (x,y): ");
  private LabeledTextField ltfXMin = new LabeledTextField("X axis min ");
  private LabeledTextField ltfXMax = new LabeledTextField("Max ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y axis min ");
  private LabeledTextField ltfYMax = new LabeledTextField("Max ");
  private LabeledTextField ltfPadPercent = new LabeledTextField(
    "Pixels to pad (x,y): ");
  private LabeledTextField ltfTaperPercent = new LabeledTextField(
    "Pixels to taper (x,y): ");
  private JCheckBox cbCumulativeCorrelation = new JCheckBox(
    "Cumulative correlation");
  private JCheckBox cbAbsoluteCosineStretch = new JCheckBox(
    "Absolute Cosine Stretch");
  private JCheckBox cbNoCosineStretch = new JCheckBox("No Cosine Stretch");
  private LabeledTextField ltfViewRange = new LabeledTextField(
    "View range (start,end): ");

  AxisID axisID;

  public CrossCorrelationPanel(ApplicationManager applicationManager, AxisID id) {
    setToolTipText();
    axisID = id;
    this.applicationManager = applicationManager;

    // Construct the min and max subpanels
    pnlXMinAndMax.setLayout(new BoxLayout(pnlXMinAndMax, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlXMinAndMax, ltfXMin.getContainer());
    UIUtilities.addWithXSpace(pnlXMinAndMax, ltfXMax.getContainer());

    pnlYMinAndMax.setLayout(new BoxLayout(pnlYMinAndMax, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlYMinAndMax, ltfYMin.getContainer());
    UIUtilities.addWithXSpace(pnlYMinAndMax, ltfYMax.getContainer());

    //  Construct the advanced panel
    pnlAdvanced.setLayout(new BoxLayout(pnlAdvanced, BoxLayout.Y_AXIS));
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

    pnlCrossCorrelation.setLayout(new BoxLayout(pnlCrossCorrelation,
      BoxLayout.Y_AXIS));
    pnlCrossCorrelation.add(pnlAdvanced);
    // Since the whole panel is currently advanced we can put the border on the
    // advanced panel.  Thus it won't show up when the panel isn't visible.  
    pnlAdvanced.setBorder(new EtchedBorder("Tiltxcorr Parameters").getBorder());

    UIUtilities.alignComponentsX(pnlAdvanced, Component.LEFT_ALIGNMENT);
    
    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCrossCorrelation.addMouseListener(mouseAdapter);

    CrossCorrelationActionListener actionListener = new CrossCorrelationActionListener(
      this);
    cbCumulativeCorrelation.addActionListener(actionListener);
    cbNoCosineStretch.addActionListener(actionListener);
  }

  JPanel getPanel() {
    return pnlCrossCorrelation;
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
    updateCrossCorrelationPanel();
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
    }
    catch (FortranInputSyntaxException except) {
      String message = currentParam + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
  }

  void setVisible(boolean state) {
    pnlCrossCorrelation.setVisible(state);
  }

  void setAdvanced(boolean state) {
    pnlAdvanced.setVisible(state);
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
  private void buttonAction(ActionEvent event) {
    updateCrossCorrelationPanel();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = {"Tiltxcorr"};
    String[] manPage = {"tiltxcorr.html"};
    String[] logFileLabel = {"Xcorr"};
    String[] logFile = new String[1];
    logFile[0] = "xcorr" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlCrossCorrelation,
      mouseEvent, "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel,
      logFile, applicationManager);
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
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;

    try {
      autodoc = Autodoc.get(Autodoc.TILTXCORR);
      //autodoc.print();
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }

    text = EtomoAutodoc.getTooltip(autodoc, "TestOutput");
    if (text != null) {
      ltfTestOutput.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "FilterSigma1");
    if (text != null) {
      ltfFilterSigma1.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "FilterRadius2");
    if (text != null) {
      ltfFilterRadius2.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "FilterSigma2");
    if (text != null) {
      ltfFilterSigma2.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "BordersInXandY");
    if (text != null) {
      ltfTrim.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "XMinAndMax");
    if (text != null) {
      pnlXMinAndMax.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "YMinAndMax");
    if (text != null) {
      pnlYMinAndMax.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = EtomoAutodoc.getTooltip(autodoc, "PadsInXandY");
    if (text != null) {
      ltfPadPercent.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = EtomoAutodoc.getTooltip(autodoc, "TapersInXandY");
    if (text != null) {
      ltfTaperPercent.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = EtomoAutodoc.getTooltip(autodoc, "CumulativeCorrelation");
    if (text != null) {
      cbCumulativeCorrelation.setToolTipText(tooltipFormatter.setText(text)
        .format());
    }

    text = EtomoAutodoc.getTooltip(autodoc, "AbsoluteCosineStretch");
    if (text != null) {
      cbAbsoluteCosineStretch.setToolTipText(tooltipFormatter.setText(text)
        .format());
    }

    text = EtomoAutodoc.getTooltip(autodoc, "NoCosineStretch");
    if (text != null) {
      cbNoCosineStretch.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = EtomoAutodoc.getTooltip(autodoc, "StartingEndingViews");
    if (text != null) {
      ltfViewRange.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = EtomoAutodoc.getTooltip(autodoc, "ExcludeCentralPeak");
    if (text != null) {
      cbExcludeCentralPeak.setToolTipText(tooltipFormatter.setText(text)
        .format());
    }
  }

}