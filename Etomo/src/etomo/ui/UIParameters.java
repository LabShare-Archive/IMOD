/**
 * <p>Description: Provides a static store for UI parameters</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.7  2006/07/20 17:23:46  sueh
 * <p> bug# 848 Made UIParameters a singleton.  Adjusting dimensions by
 * <p> UIParameters.fontSizeAdjustment.
 * <p>
 * <p> Revision 3.6  2005/12/23 02:24:31  sueh
 * <p> bug# 675 Split the test option functionality into headless and test.
 * <p>
 * <p> Revision 3.5  2005/08/10 20:50:25  sueh
 * <p> bug# 711 Made UIParameters constructor private.  Can't force it no be
 * <p> called since this is all static functions.  Recalc() requires a UI element,
 * <p> so return from it in test mode.
 * <p>
 * <p> Revision 3.4  2005/08/09 21:13:30  sueh
 * <p> bug# 711 Making sure that recalc() is called at least once before getting
 * <p> a dimension.
 * <p>
 * <p> Revision 3.3  2004/04/26 03:17:17  rickg
 * <p> Add a norrow button dimension
 * <p>
 * <p> Revision 3.2  2004/03/24 03:02:31  rickg
 * <p> Changed spinner size to only specify spinner region.  The
 * <p> panel and label should be handled automatically
 * <p>
 * <p> Revision 3.1  2004/02/20 23:52:10  sueh
 * <p> bug# 386 added FileField dimension and spinner dimension
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/10/16 21:49:05  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.ui;

import java.awt.Dimension;

import javax.swing.JCheckBox;

import etomo.EtomoDirector;

public final class UIParameters {
  public static final String rcsid = "$Id$";

  public static final UIParameters INSTANCE = new UIParameters();
  private static final float DEFAULT_FONT_SIZE = 12;
  
  private final Dimension dimButton = new Dimension();
  private final Dimension dimNarrowButton = new Dimension();
  private final Dimension dimSpinner = new Dimension();
  private final Dimension dimFileField = new Dimension();
  private final Dimension dimFileChooser = new Dimension();
  
  private float fontSize = DEFAULT_FONT_SIZE;
  private float fontSizeAdjustment = 1;
  private int numericWidth;
  private int sectionsWidth;
  private int integerTripletWidth;
  private int fourDigitWidth = 40;

  private UIParameters() {
    calcSizes();
  }
  
  public void setFontSize(int fontSize) {
    this.fontSize = fontSize;
    calcSizes();
  }

  /**
   * Return the size of a standard button
   * @return
   */
  Dimension getButtonDimension() {
    return new Dimension(dimButton);
  }

  Dimension getNarrowButtonDimension() {
    //  Return a safe copy of the Dimension
    return new Dimension(dimNarrowButton);
  }

  Dimension getSpinnerDimension() {
    return new Dimension(dimSpinner);
  }

  Dimension getFileFieldDimension() {
    return new Dimension(dimFileField);
  }
  
  Dimension getFileChooserDimension() {
    return new Dimension(dimFileChooser);
  }
  
  int getNumericWidth() {
    return numericWidth;
  }
  
  int getSectionsWidth() {
    return sectionsWidth;
  }
  
  int getIntegerTripletWidth() {
    return integerTripletWidth;
  }
  
  int getFourDigitWidth() {
    return fourDigitWidth;
  }
  
  /**
   * Get the amount to adjust a fields based on the current font size
   * @return
   */
  float getFontSizeAdjustment() {
    return fontSizeAdjustment;
  }

  /**
   * Sets size of objects given the current UI state.
   *
   */
  private void calcSizes() {
    if (EtomoDirector.getInstance().isHeadless()) {
      return;
    }
    //  Create a temporary check box and get its height
    JCheckBox temp = new JCheckBox();
    double height = temp.getPreferredSize().getHeight();
    fontSizeAdjustment = fontSize / DEFAULT_FONT_SIZE;
    dimButton.setSize(7 * height * fontSizeAdjustment, 2 * height * fontSizeAdjustment);
    dimNarrowButton.setSize(4 * height * fontSizeAdjustment, 2 * height * fontSizeAdjustment);
    dimSpinner.setSize(2 * height * fontSizeAdjustment, 1.05 * height * fontSizeAdjustment);
    dimFileField.setSize(20 * height * fontSizeAdjustment, 2 * height * fontSizeAdjustment);
    dimFileChooser.setSize(400 * fontSizeAdjustment, 400 * fontSizeAdjustment);
    numericWidth = (int) (50 * fontSizeAdjustment);
    sectionsWidth = (int) (75 * fontSizeAdjustment);
    integerTripletWidth = (int)(90 * fontSizeAdjustment);
    fourDigitWidth = (int)(40 * fontSizeAdjustment);
  }
}
