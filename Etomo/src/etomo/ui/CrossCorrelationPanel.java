package etomo.ui;

import java.awt.event.*;
import javax.swing.*;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.TiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import java.io.*;

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

public class CrossCorrelationPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlCrossCorrelation = new JPanel();
  private JPanel pnlAdvanced = new JPanel();
  private JPanel pnlXMinAndMax = new JPanel();
  private JPanel pnlYMinAndMax = new JPanel();

  private BeveledBorder borderCoarseAlignment =
    new BeveledBorder("Cross-Correlation");

  private JCheckBox cbExcludeCentralPeak =
    new JCheckBox("Exclude central peak due to fixed pattern noise");

  private LabeledTextField ltfTestOutput = new LabeledTextField("Test output: ");
  private LabeledTextField ltfFilterSigma1 =
    new LabeledTextField("Low frequency rolloff sigma: ");
  private LabeledTextField ltfFilterRadius2 =
    new LabeledTextField("High frequency cutoff radius: ");
  private LabeledTextField ltfFilterSigma2 =
    new LabeledTextField("High frequency rolloff sigma: ");
  private LabeledTextField ltfTrim = new LabeledTextField("Pixels to trim: ");
  private LabeledTextField ltfXMin = new LabeledTextField("Min ");
  private LabeledTextField ltfXMax = new LabeledTextField("max ");
  private JLabel lblXMinAndMax = new JLabel("pixels in X");
  private LabeledTextField ltfYMin = new LabeledTextField("Min ");
  private LabeledTextField ltfYMax = new LabeledTextField("max ");
  private JLabel lblYMinAndMax = new JLabel("pixels in Y");
  private LabeledTextField ltfPadPercent =
    new LabeledTextField("Pixels to pad: ");
  private LabeledTextField ltfTaperPercent =
    new LabeledTextField("Pixels to taper: ");
  private JCheckBox cbCumulativeCorrelation =
    new JCheckBox("Cumulative correlation");
  private JCheckBox cbAbsoluteCosineStretch = 
    new JCheckBox("Absolute Cosine Stretch");
  private JCheckBox cbNoCosineStretch = new JCheckBox("No Cosine Stretch");
  private LabeledTextField ltfViewRange = new LabeledTextField("View range: ");

  AxisID axisID;

  public CrossCorrelationPanel(AxisID id) {
    setToolTipText();
    axisID = id;
    pnlXMinAndMax.setLayout(new BoxLayout(pnlXMinAndMax, BoxLayout.X_AXIS));
    pnlXMinAndMax.add(ltfXMin.getContainer());
    pnlXMinAndMax.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlXMinAndMax.add(ltfXMax.getContainer());
    pnlXMinAndMax.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlXMinAndMax.add(lblXMinAndMax);
    
    pnlYMinAndMax.setLayout(new BoxLayout(pnlYMinAndMax, BoxLayout.X_AXIS));
    pnlYMinAndMax.add(ltfYMin.getContainer());
    pnlYMinAndMax.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlYMinAndMax.add(ltfYMax.getContainer());
    pnlYMinAndMax.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlYMinAndMax.add(lblYMinAndMax);
    
    pnlAdvanced.setLayout(new BoxLayout(pnlAdvanced, BoxLayout.Y_AXIS));
   
    cbExcludeCentralPeak.setAlignmentX((float) 0.5);
    pnlAdvanced.add(ltfFilterSigma1.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfFilterRadius2.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfFilterSigma2.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(Box.createHorizontalGlue());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfTrim.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(pnlXMinAndMax);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(pnlYMinAndMax);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfPadPercent.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfTaperPercent.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(cbCumulativeCorrelation);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(cbAbsoluteCosineStretch);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(cbNoCosineStretch);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(cbExcludeCentralPeak);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfTestOutput.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced.add(ltfViewRange.getContainer());
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x0_y5));
    
    pnlCrossCorrelation.setLayout(
      new BoxLayout(pnlCrossCorrelation, BoxLayout.Y_AXIS));
    pnlCrossCorrelation.add(pnlAdvanced);
    // Since the whole panel is currently advanced we can put the border on the
    // advanced panel.  Thus it won't show up when the panel isn't visible.  
    pnlAdvanced.setBorder(
      new EtchedBorder("Tiltxcorr Parameters").getBorder());
    
    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCrossCorrelation.addMouseListener(mouseAdapter);
    
    CrossCorrelationActionListener actionListener =
      new CrossCorrelationActionListener(this);
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
    ltfFilterSigma1.setText(tiltXcorrParams.getFilterSigma1());
    ltfFilterRadius2.setText(tiltXcorrParams.getFilterRadius2());
    ltfFilterSigma2.setText(tiltXcorrParams.getFilterSigma2());
    ltfTrim.setText(tiltXcorrParams.getBordersInXandY());
    ltfXMin.setText(tiltXcorrParams.getXMinString());
    ltfXMax.setText(tiltXcorrParams.getXMaxString());
    ltfYMin.setText(tiltXcorrParams.getYMinString());
    ltfYMax.setText(tiltXcorrParams.getYMaxString());
    ltfPadPercent.setText(tiltXcorrParams.getPadsInXandY());
    ltfTaperPercent.setText(tiltXcorrParams.getTaperPercent());
    cbCumulativeCorrelation.setSelected(tiltXcorrParams.isCumulativeCorrelation());
    cbAbsoluteCosineStretch.setSelected(tiltXcorrParams.isAbsoluteCosineStretch());
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
      tiltXcorrParams.setFilterSigma1(
        Double.parseDouble(ltfFilterSigma1.getText()));
      currentParam = ltfFilterRadius2.getLabel();
      tiltXcorrParams.setFilterRadius2(
        Double.parseDouble(ltfFilterRadius2.getText()));
      currentParam = ltfFilterSigma2.getLabel();
      tiltXcorrParams.setFilterSigma2(
        Double.parseDouble(ltfFilterSigma2.getText()));
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
      tiltXcorrParams.setCumulativeCorrelation(cbCumulativeCorrelation.isSelected());
      currentParam = cbAbsoluteCosineStretch.getText();
      tiltXcorrParams.setAbsoluteCosineStretch(cbAbsoluteCosineStretch.isSelected());
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
    String[] manPagelabel = { "Tiltxcorr" };
    String[] manPage = { "tiltxcorr.html" };
    String[] logFileLabel = { "Xcorr" };
    String[] logFile = new String[1];
    logFile[0] = "xcorr" + axisID.getExtension() + ".log";
    ContextPopup contextPopup =
      new ContextPopup(
        pnlCrossCorrelation,
        mouseEvent,
        "COARSE ALIGNMENT",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
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

    text = TooltipFormatter.getText(autodoc, "TestOutput");
    if (text != null) {
      ltfTestOutput.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "FilterSigma1");
    if (text != null) {
      ltfFilterSigma1.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "FilterRadius2");
    if (text != null) {
      ltfFilterRadius2.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "FilterSigma2");
    if (text != null) {
      ltfFilterSigma2.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "BordersInXandY");
    if (text != null) {
      ltfTrim.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "XMinAndMax");
    if (text != null) {
      pnlXMinAndMax.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "YMinAndMax");
    if (text != null) {
      pnlYMinAndMax.setToolTipText(tooltipFormatter.setText(text).format());
    }
    text = TooltipFormatter.getText(autodoc, "PadsInXandY");
    if (text != null) {
      ltfPadPercent.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "TapersInXandY");
    if (text != null) {
      ltfTaperPercent.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "CumulativeCorrelation");
    if (text != null) {
      cbCumulativeCorrelation.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "AbsoluteCosineStretch");
    if (text != null) {
      cbAbsoluteCosineStretch.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "NoCosineStretch");
    if (text != null) {
      cbNoCosineStretch.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "StartingEndingViews");
    if (text != null) {
      ltfViewRange.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "ExcludeCentralPeak");
    if (text != null) {
      cbExcludeCentralPeak.setToolTipText(
        tooltipFormatter.setText(text).format());
    }
  }

}
