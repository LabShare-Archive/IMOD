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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.19  2008/02/19 00:48:02  sueh
 * <p> bug# 1078 Added fileWidth.
 * <p>
 * <p> Revision 3.18  2007/12/26 22:39:33  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 3.17  2007/11/06 20:33:37  sueh
 * <p> bug# 1047 Added getListWidth.
 * <p>
 * <p> Revision 3.16  2007/09/07 00:29:51  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.15  2007/06/14 19:38:01  sueh
 * <p> bug# 1020 Added wideNumericWidth.
 * <p>
 * <p> Revision 3.14  2007/06/06 22:06:17  sueh
 * <p> bug# 1010 Made numericWidth smaller.
 * <p>
 * <p> Revision 3.13  2007/04/02 21:53:32  sueh
 * <p> bug# 964 Added integerWidth.
 * <p>
 * <p> Revision 3.12  2007/03/03 01:08:25  sueh
 * <p> bug# 973 Added integerDoubletWidth.
 * <p>
 * <p> Revision 3.11  2006/08/08 19:59:06  sueh
 * <p> bug# 531 Made the done, advanced, etc buttons smaller.
 * <p>
 * <p> Revision 3.10  2006/07/31 21:47:11  sueh
 * <p> bug# 438 Added dimAxisButton
 * <p>
 * <p> Revision 3.9  2006/07/26 21:51:25  sueh
 * <p> bug# 907 Added a default height.
 * <p>
 * <p> Revision 3.8  2006/07/21 19:19:44  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
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
package etomo.ui.swing;

import java.awt.Dimension;

import javax.swing.JCheckBox;

import etomo.EtomoDirector;

public final class UIParameters {
  public static final String rcsid = "$Id$";

  public static final UIParameters INSTANCE = new UIParameters();
  private static final double DEFAULT_FONT_SIZE = 12;
  private static final double DEFAULT_HEIGHT = 21;

  private final Dimension dimButton = new Dimension();
  private final Dimension dimButtonSingleLine = new Dimension();
  private final Dimension dimNarrowButton = new Dimension();
  private final Dimension dimSpinner = new Dimension();
  private final Dimension dimFileField = new Dimension();
  private final Dimension dimFileChooser = new Dimension();
  private final Dimension dimAxisButton = new Dimension();

  private double fontSize = DEFAULT_FONT_SIZE;
  private double fontSizeAdjustment = 1;
  private int numericWidth;
  private int wideNumericWidth;
  private int sectionsWidth;
  private int integerTripletWidth;
  private int integerDoubletWidth;
  private int integerWidth;
  private int fourDigitWidth = 40;
  private int listWidth;
  private int fileWidth;

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

  Dimension getButtonSingleLineDimension() {
    return new Dimension(dimButtonSingleLine);
  }

  Dimension getNarrowButtonDimension() {
    // Return a safe copy of the Dimension
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

  Dimension getAxisButtonDimension() {
    return new Dimension(dimAxisButton);
  }

  int getNumericWidth() {
    return numericWidth;
  }

  int getWideNumericWidth() {
    return wideNumericWidth;
  }

  int getSectionsWidth() {
    return sectionsWidth;
  }

  int getIntegerTripletWidth() {
    return integerTripletWidth;
  }

  int getIntegerDoubletWidth() {
    return integerDoubletWidth;
  }

  int getIntegerWidth() {
    return integerWidth;
  }

  int getFourDigitWidth() {
    return fourDigitWidth;
  }

  int getListWidth() {
    return listWidth;
  }

  int getFileWidth() {
    return fileWidth;
  }

  /**
   * Get the amount to adjust a fields based on the current font size
   * @return
   */
  double getFontSizeAdjustment() {
    return fontSizeAdjustment;
  }

  /**
   * Sets size of objects given the current UI state.
   *
   */
  private void calcSizes() {
    double height;
    // Create a temporary check box and get its height
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      JCheckBox temp = new JCheckBox();
      height = temp.getPreferredSize().getHeight();
    }
    else {
      height = DEFAULT_HEIGHT;
    }
    fontSizeAdjustment = fontSize / DEFAULT_FONT_SIZE;
    dimButton.setSize(7 * height * fontSizeAdjustment, 2 * height * fontSizeAdjustment);
    dimButtonSingleLine.setSize(7 * height * fontSizeAdjustment, 1.25 * height * fontSizeAdjustment);
    dimNarrowButton.setSize(4 * height * fontSizeAdjustment, 1.25 * height
        * fontSizeAdjustment);
    dimAxisButton.setSize(3.6 * height * fontSizeAdjustment, 1.25 * height
        * fontSizeAdjustment);
    dimSpinner.setSize(2 * height * fontSizeAdjustment, 1.05 * height
        * fontSizeAdjustment);
    dimFileField.setSize(20 * height * fontSizeAdjustment, 2 * height
        * fontSizeAdjustment);
    dimFileChooser.setSize(400 * fontSizeAdjustment, 400 * fontSizeAdjustment);
    numericWidth = (int) (40 * fontSizeAdjustment);
    wideNumericWidth = (int) (50 * fontSizeAdjustment);
    sectionsWidth = (int) (75 * fontSizeAdjustment);
    integerTripletWidth = (int) (75 * fontSizeAdjustment);
    integerDoubletWidth = (int) (50 * fontSizeAdjustment);
    integerWidth = (int) (30 * fontSizeAdjustment);
    fourDigitWidth = (int) (40 * fontSizeAdjustment);
    listWidth = (int) (140 * fontSizeAdjustment);
    fileWidth = (int) (210 * fontSizeAdjustment);
  }
}
