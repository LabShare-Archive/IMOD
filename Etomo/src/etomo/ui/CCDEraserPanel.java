package etomo.ui;

import java.awt.Component;
import java.awt.event.*;
import javax.swing.*;

import etomo.comscript.ConstCCDEraserParam;
import etomo.comscript.CCDEraserParam;

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
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class CCDEraserPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel panelCCDEraser = new JPanel();

  private LabeledTextField ltfInputImage =
    new LabeledTextField("Input image file: ");
  private LabeledTextField ltfOutputImage =
    new LabeledTextField("Output image file: ");
  private LabeledTextField ltfGlobalReplacementList =
    new LabeledTextField("All section replacement list: ");
  private LabeledTextField ltfLocalReplacementList =
    new LabeledTextField("Line replacement list: ");
  private LabeledTextField ltfBorderPixels =
    new LabeledTextField("Border pixels: ");
  private LabeledTextField ltfPolynomialOrder =
    new LabeledTextField("Polynomial order: ");
  private JCheckBox chkboxIncludeAdjacentPoints =
    new JCheckBox("Include adjacent points");

  public CCDEraserPanel() {
    setToolTipText();
    panelCCDEraser.setLayout(new BoxLayout(panelCCDEraser, BoxLayout.Y_AXIS));

    chkboxIncludeAdjacentPoints.setAlignmentX(Component.CENTER_ALIGNMENT);
    panelCCDEraser.add(ltfInputImage.getContainer());
    panelCCDEraser.add(ltfOutputImage.getContainer());
    panelCCDEraser.add(ltfGlobalReplacementList.getContainer());
    panelCCDEraser.add(ltfLocalReplacementList.getContainer());
    panelCCDEraser.add(ltfBorderPixels.getContainer());
    panelCCDEraser.add(ltfPolynomialOrder.getContainer());
    panelCCDEraser.add(chkboxIncludeAdjacentPoints);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelCCDEraser.addMouseListener(mouseAdapter);
  }

  public void setParameters(ConstCCDEraserParam ccdEraserParams) {
    ltfInputImage.setText(ccdEraserParams.getInputFile());
    ltfOutputImage.setText(ccdEraserParams.getOutputFile());
    ltfGlobalReplacementList.setText(
      ccdEraserParams.getGlobalReplacementList());
    ltfLocalReplacementList.setText(ccdEraserParams.getlocalReplacementList());
    ltfBorderPixels.setText(ccdEraserParams.getBorderPixels());
    ltfPolynomialOrder.setText(ccdEraserParams.getPolynomialOrder());
    chkboxIncludeAdjacentPoints.setSelected(
      ccdEraserParams.getIncludeAdjacentPoints());
  }

  public void getParameters(CCDEraserParam ccdEraserParams) {
    ccdEraserParams.setInputFile(ltfInputImage.getText());
    ccdEraserParams.setOutputFile(ltfOutputImage.getText());
    ccdEraserParams.setGlobalReplacementList(
      ltfGlobalReplacementList.getText());
    ccdEraserParams.setLocalReplacementList(ltfLocalReplacementList.getText());
    ccdEraserParams.setBorderPixels(ltfBorderPixels.getText());
    ccdEraserParams.setPolynomialOrder(ltfPolynomialOrder.getText());
    ccdEraserParams.setIncludeAdjacentPoints(
      chkboxIncludeAdjacentPoints.isSelected());
  }

  public JPanel getContainer() {
    return panelCCDEraser;
  }

  public void setVisible(boolean state) {
    panelCCDEraser.setVisible(state);
  }

  /**
   * Makes the advanced components visible or invisible
   */
  void setAdvanced(boolean state) {
    ltfInputImage.setVisible(state);
    ltfOutputImage.setVisible(state);
    ltfGlobalReplacementList.setVisible(state);
    ltfLocalReplacementList.setVisible(state);
    ltfBorderPixels.setVisible(state);
    ltfPolynomialOrder.setVisible(state);
    chkboxIncludeAdjacentPoints.setVisible(state);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] label = { "ccdEraser" };
    String[] manPage = { "ccderaser.html" };
    ContextPopup contextPopup =
      new ContextPopup(
        panelCCDEraser,
        mouseEvent,
        "Preliminary Steps",
        label,
        manPage);
  }

  //  ToolTip string setup
  private void setToolTipText() {
    String line1, line2, line3, line4, line5, line6, line7;

    line1 = "<html>The input image stack filename.<br>";
    line2 = "This specifies the input image stack.  The default value of <br>";
    line3 = "the original source data will work unless you have modified<br>";
    line4 = "or renamed the orignal data.";
    ltfInputImage.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>The ouput image stack filename.<br>";
    line2 =
      "Leave blank to replace existing image stack file.  The default<br>";
    line3 = "processing expects the image stack file to be replaced so a <br>";
    line4 = "blank entry should work for most situations.";
    ltfOutputImage.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>The global replacemnt list.<br>";
    line2 = "A list of objects in the model file which specify the points<br>";
    line3 = "lines to be replaced in <b>ALL</b> projections of the stack.<br>";
    line4 = "You can use / to indicate that all objects in the model file<br>";
    line5 = "specify the pixels to be replaced.  A blank entry indicates<br>";
    line6 = "that no objects will be used for global replacement.<br>";
    line7 = "Ranges of objects are allowed";
    ltfGlobalReplacementList.setToolTipText(
      line1 + line2 + line3 + line4 + line5 + line6 + line7);

    line1 = "<html>The line replacemnt list.<br>";
    line2 = "A list of objects in the model file which specify lines<br>";
    line3 = "to be replaced in <b>SPECIFIC</b> projections of the stack.<br>";
    line4 = "You can use / to indicate that all objects in the model file<br>";
    line5 = "specify the lines to be replaced.  A blank entry indicates<br>";
    line6 = "that no objects will be used for line replacement.<br>";
    line7 = "Ranges of objects are allowed";
    ltfLocalReplacementList.setToolTipText(
      line1 + line2 + line3 + line4 + line5 + line6 + line7);

    line1 = "<html>The size of the border around the replaced pixels.<br>";
    line2 = "This specifes the number pixles are the replaced pixels to be<br>";
    line3 = "used in the interpolation.  Enter / for the default of 3 pixels";
    ltfBorderPixels.setToolTipText(line1 + line2 + line3);

    line1 = "<html>The order polynomial of the interpolation.<br>";
    line2 = "This specifes order of the two dimensional polynomial used to<br>";
    line3 = "interpolate the pixel being replaced.  Enter / for the<br>";
    line4 = "default of a second order polynomial";
    ltfPolynomialOrder.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>Include point adjacent to the specified points in the<br>";
    line2 =
      "polynomial fit.  <b>???</b>Selecting this check box will replace the erase<br>";
    line3 =
      "model points <b>as well as the adjacent points</b> with interpolated values.<br>";
    line4 =
      "Does this effectively increas the replacement patch by 1 pixel in all directions<br>";
    line5 = "<b>prior</b> to performing the interpolation<b>???</b>";
    chkboxIncludeAdjacentPoints.setToolTipText(
      line1 + line2 + line3 + line4 + line5);

  }
}
