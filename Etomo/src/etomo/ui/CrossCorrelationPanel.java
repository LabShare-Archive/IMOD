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

  private JPanel panelCrossCorrelation = new JPanel();
  private JPanel panelAdvanced = new JPanel();

  private BeveledBorder borderCoarseAlignment =
    new BeveledBorder("Cross-Correlation");

  private JCheckBox cbExcludeCentralPeak =
    new JCheckBox("Exclude central peak due to fixed pattern noise");

  private LabeledTextField ltfInputFile = new LabeledTextField("Input file: ");
  private LabeledTextField ltfOutputFile =
    new LabeledTextField("Output file: ");
  private LabeledTextField ltfFilterParams =
    new LabeledTextField("Filter parameters: ");
  private LabeledTextField ltfTrim = new LabeledTextField("Pixels to trim: ");
  private LabeledTextField ltfPadPercent =
    new LabeledTextField("Pixels to pad: ");
  private LabeledTextField ltfTaperPercent =
    new LabeledTextField("Pixels to taper: ");
  private LabeledTextField ltfViewRange = new LabeledTextField("View range: ");

  AxisID axisID;

  public CrossCorrelationPanel(AxisID id) {
    setToolTipText();
    axisID = id;
    panelAdvanced.setLayout(new BoxLayout(panelAdvanced, BoxLayout.Y_AXIS));
    cbExcludeCentralPeak.setAlignmentX((float) 0.5);
    panelAdvanced.add(cbExcludeCentralPeak);
    panelAdvanced.add(ltfInputFile.getContainer());
    panelAdvanced.add(ltfOutputFile.getContainer());
    //    panelAdvanced.add(ltfPieceListFile.getContainer());
    panelAdvanced.add(ltfFilterParams.getContainer());
    panelAdvanced.add(ltfTrim.getContainer());
    panelAdvanced.add(ltfPadPercent.getContainer());
    panelAdvanced.add(ltfTaperPercent.getContainer());
    panelAdvanced.add(ltfViewRange.getContainer());

    panelCrossCorrelation.setLayout(
      new BoxLayout(panelCrossCorrelation, BoxLayout.Y_AXIS));
    panelCrossCorrelation.add(panelAdvanced);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelCrossCorrelation.addMouseListener(mouseAdapter);

  }

  JPanel getPanel() {
    return panelCrossCorrelation;
  }

  /**
   * Set the field values for the panel from the ConstTiltxcorrParam object
   */
  public void setParameters(ConstTiltxcorrParam tiltXcorrParams) {
    cbExcludeCentralPeak.setSelected(tiltXcorrParams.getExcludeCentralPeak());
    ltfInputFile.setText(tiltXcorrParams.getInputFile());
    ltfOutputFile.setText(tiltXcorrParams.getOutputFile());
    ltfFilterParams.setText(tiltXcorrParams.getFilterParams());
    ltfTrim.setText(tiltXcorrParams.getTrim());
    ltfPadPercent.setText(tiltXcorrParams.getPadPercent());
    ltfTaperPercent.setText(tiltXcorrParams.getTaperPercent());
    ltfViewRange.setText(tiltXcorrParams.getViewRange());
  }

  /**
   * Get the field values from the panel filling in the TiltxcorrParam object
   */
  public void getParameters(TiltxcorrParam tiltXcorrParams)
    throws FortranInputSyntaxException {
    tiltXcorrParams.setExcludeCentralPeak(cbExcludeCentralPeak.isSelected());
    tiltXcorrParams.setInputFile(ltfInputFile.getText());
    tiltXcorrParams.setOutputFile(ltfOutputFile.getText());
    String currentParam = "unknown";
    try {
      currentParam = ltfFilterParams.getLabel();

      tiltXcorrParams.setFilterParams(ltfFilterParams.getText());
      currentParam = ltfTrim.getLabel();
      tiltXcorrParams.setTrim(ltfTrim.getText());
      currentParam = ltfPadPercent.getLabel();
      tiltXcorrParams.setPadPercent(ltfPadPercent.getText());
      currentParam = ltfTaperPercent.getLabel();
      tiltXcorrParams.setTaperPercent(ltfTaperPercent.getText());
      currentParam = ltfViewRange.getLabel();
      tiltXcorrParams.setViewRange(ltfViewRange.getText());
    }
    catch (FortranInputSyntaxException except) {
      String message = currentParam + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
  }

  void setVisible(boolean state) {
    panelCrossCorrelation.setVisible(state);
  }

  void setAdvanced(boolean state) {
    panelAdvanced.setVisible(state);
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
        panelCrossCorrelation,
        mouseEvent,
        "COARSE ALIGNMENT",
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
    
    text  = TooltipFormatter.getText(autodoc, "InputFile");
    if (text != null) {
      ltfInputFile.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text  = TooltipFormatter.getText(autodoc, "OutputFile");
    if (text != null) {
      ltfOutputFile.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text =
      "Sigma for low-frequency filter, sigma for high-frequency filter, 0, and "
        + "radius for start of high-frequency filter.";
    ltfFilterParams.setToolTipText(tooltipFormatter.setText(text).format());

    text  = TooltipFormatter.getText(autodoc, "BordersInXandY");
    if (text != null) {
      ltfTrim.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text  = TooltipFormatter.getText(autodoc, "PadsInXandY");
    if (text != null) {
      ltfPadPercent.setToolTipText(tooltipFormatter.setText(text).format());
    }

    text = TooltipFormatter.getText(autodoc, "TapersInXandY");
    if (text != null) {
      ltfTaperPercent.setToolTipText(tooltipFormatter.setText(text).format());
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
