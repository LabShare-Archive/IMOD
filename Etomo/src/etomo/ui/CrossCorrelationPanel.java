package etomo.ui;

import java.awt.event.*;
import javax.swing.*;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.TiltxcorrParam;
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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class CrossCorrelationPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private String logSuffix;

  private JPanel panelCrossCorrelation = new JPanel();
  private JPanel panelAdvanced = new JPanel();

  private BeveledBorder borderCoarseAlignment =
    new BeveledBorder("Cross-correlation");

  private JCheckBox chkBoxExcludeCentralPeak =
    new JCheckBox("Exclude central peak due to fixed pattern noise");

  private JToggleButton buttonCrossCorrelate =
    new JToggleButton("<html>Calculate<br>cross-correlation");

  private LabeledTextField ltfInputFile = new LabeledTextField("Input file: ");
  private LabeledTextField ltfOutputFile =
    new LabeledTextField("Output file: ");
  private LabeledTextField ltfPieceListFile =
    new LabeledTextField("Piece list file: ");
  private LabeledTextField ltfFilterParams =
    new LabeledTextField("Filter parameters: ");
  private LabeledTextField ltfTrim = new LabeledTextField("Trim: ");
  private LabeledTextField ltfPadPercent =
    new LabeledTextField("Pad percent: ");
  private LabeledTextField ltfTaperPercent =
    new LabeledTextField("Taper percent: ");
  private LabeledTextField ltfViewList = new LabeledTextField("View list: ");

  public CrossCorrelationPanel(String suffix) {
    logSuffix = suffix;
    panelAdvanced.setLayout(new BoxLayout(panelAdvanced, BoxLayout.Y_AXIS));

    panelAdvanced.add(ltfInputFile.getContainer());
    panelAdvanced.add(ltfOutputFile.getContainer());
    panelAdvanced.add(ltfPieceListFile.getContainer());
    panelAdvanced.add(ltfFilterParams.getContainer());
    panelAdvanced.add(ltfTrim.getContainer());
    panelAdvanced.add(ltfPadPercent.getContainer());
    panelAdvanced.add(ltfTaperPercent.getContainer());
    panelAdvanced.add(ltfViewList.getContainer());

    panelCrossCorrelation.setLayout(
      new BoxLayout(panelCrossCorrelation, BoxLayout.Y_AXIS));
    chkBoxExcludeCentralPeak.setAlignmentX((float) 0.5);
    buttonCrossCorrelate.setAlignmentX(0.5F);
    buttonCrossCorrelate.setPreferredSize(FixedDim.button2Line);
    buttonCrossCorrelate.setMaximumSize(FixedDim.button2Line);
    panelCrossCorrelation.add(chkBoxExcludeCentralPeak);
    panelCrossCorrelation.add(panelAdvanced);
    panelCrossCorrelation.add(buttonCrossCorrelate);

    //
    //  Mouse adapter for context menu
    //
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelCrossCorrelation.addMouseListener(mouseAdapter);

  }

  JPanel getPanel() {
    return panelCrossCorrelation;
  }

  public void setParameters(ConstTiltxcorrParam tiltXcorrParams) {
    chkBoxExcludeCentralPeak.setSelected(
      tiltXcorrParams.getExcludeCentralPeak());
    ltfInputFile.setText(tiltXcorrParams.getInputFile());
    ltfOutputFile.setText(tiltXcorrParams.getOutputFile());
    ltfPieceListFile.setText(tiltXcorrParams.getPieceListFile());
    ltfFilterParams.setText(tiltXcorrParams.getFilterParams());
    ltfTrim.setText(tiltXcorrParams.getTrim());
    ltfPadPercent.setText(tiltXcorrParams.getPadPercent());
    ltfTaperPercent.setText(tiltXcorrParams.getTaperPercent());
    ltfViewList.setText(tiltXcorrParams.getViewList());
  }

  public void getParameters(TiltxcorrParam tiltXcorrParams)
    throws FortranInputSyntaxException {
    tiltXcorrParams.setExcludeCentralPeak(
      chkBoxExcludeCentralPeak.isSelected());
    tiltXcorrParams.setInputFile(ltfInputFile.getText());
    tiltXcorrParams.setOutputFile(ltfOutputFile.getText());
    tiltXcorrParams.setPieceListFile(ltfPieceListFile.getText());
    String currentParam = "unknown";
    try {
      currentParam = "Filter parameters: ";
      tiltXcorrParams.setFilterParams(ltfFilterParams.getText());
      currentParam = "Trim parameters: ";
      tiltXcorrParams.setTrim(ltfTrim.getText());
      currentParam = "Padding percentage: ";
      tiltXcorrParams.setPadPercent(ltfPadPercent.getText());
      currentParam = "Taper percentage: ";
      tiltXcorrParams.setTaperPercent(ltfTaperPercent.getText());
      currentParam = "View list: ";
      tiltXcorrParams.setViewList(ltfViewList.getText());
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

  void setButtonState(boolean state) {
    buttonCrossCorrelate.setSelected(state);
  }

  void setButtonActionListener(ActionListener actionListener) {
    buttonCrossCorrelate.addActionListener(actionListener);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "tiltxcorr" };
    String[] manPage = { "tiltxcorr.html" };
    String[] logFileLabel = { "xcorr" };
    String[] logFile = new String[1];
    logFile[0] = "xcorr" + logSuffix + ".log";
    ContextPopup contextPopup =
      new ContextPopup(
        panelCrossCorrelation,
        mouseEvent,
        "Preliminary Steps",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

}
