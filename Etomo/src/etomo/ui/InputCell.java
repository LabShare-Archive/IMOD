package etomo.ui;

import java.awt.Component;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;
import javax.swing.plaf.ColorUIResource;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
abstract class InputCell {
  public static final String rcsid = "$Id$";

  private static boolean colorInitialized = false;
  protected static ColorUIResource foreground = null;
  protected static ColorUIResource notInUseForeground = null;
  private static ColorUIResource background = null;
  private static ColorUIResource disabledBackground = null;
  private static ColorUIResource highlightedBackground = null;
  private static ColorUIResource disabledhighlightedBackground = null;
  private static ColorUIResource warningBackground = null;
  private static ColorUIResource disabledWarningBackground = null;
  private static ColorUIResource errorBackground = null;
  private static ColorUIResource disabledErrorBackground = null;
  private static ColorUIResource headerBackground = null;

  protected boolean inUse = true;
  protected boolean enabled = true;
  private boolean highlighted = false;
  private boolean warning = false;
  private boolean error = false;
  private Font plainFont = null;
  private Font italicFont = null;
  private JPanel jpanelContainer = null;

  abstract protected Component getComponent();
  abstract protected void setForeground();
  abstract int getWidth();

  /**
   * constructor should not call any overridable or abstract function
   * or any function that calls overridable or abstract functions.
   * The inheritor should add the following calls to its constructor:
   *  setBackground();
   *  setForeground();
   *  setFont();
   */
  InputCell() {
    initializeColor();
  }

  final protected void setFont() {
    plainFont = getComponent().getFont();
    italicFont = new Font(plainFont.getFontName(), Font.ITALIC, plainFont
        .getSize());
  }

  final void add(JPanel panel, GridBagLayout layout,
      GridBagConstraints constraints) {
    layout.setConstraints(getComponent(), constraints);
    panel.add(getComponent());
    jpanelContainer = panel;
  }

  final void remove() {
    if (jpanelContainer != null) {
      jpanelContainer.remove(getComponent());
      jpanelContainer = null;
    }
  }
  
  void setEnabled(boolean enabled) {
    this.enabled = enabled;
    getComponent().setEnabled(enabled);
    setBackground();
  }

  final void setHighlighted(boolean highlighted) {
    this.highlighted = highlighted;
    setBackground();
  }

  final void setInUse(boolean inUse) {
    this.inUse = inUse;
    setForeground();
  }

  final protected void setBackground() {
    if (highlighted) {
      if (enabled) {
        setBackground(highlightedBackground);
      }
      else {
        setBackground(disabledhighlightedBackground);
      }
    }
    else if (warning) {
      if (enabled) {
        setBackground(warningBackground);
      }
      else {
        setBackground(disabledWarningBackground);
      }
    }
    else if (error) {
      if (enabled) {
        setBackground(errorBackground);
      }
      else {
        setBackground(disabledErrorBackground);
      }
    }
    else if (enabled) {
      setBackground(background);
    }
    else {
      setBackground(disabledBackground);
    }
  }

  protected void setBackground(ColorUIResource color) {
    getComponent().setBackground(color);
  }

  final void setWarning(boolean warning) {
    //if switching from error to warning, turn off error first
    if (warning && error) {
      this.warning = false;//prevent recursion
      setError(false);
    }
    this.warning = warning;
    setBackground();
  }
  
  final void setError(boolean error) {
    //if switching from warning to error, turn off warning first
    if (error && warning) {
      this.error = false;//prevent recursion
      setWarning(false);
    }
    this.error = error;
    setBackground();
  }

  private final static ColorUIResource subtract(ColorUIResource color, ColorUIResource subtractColor) {
    return new ColorUIResource(color.getRed() - subtractColor.getRed(),
                               color.getGreen() - subtractColor.getGreen(),
                               color.getBlue() - subtractColor.getBlue());
  }
  
  private final static ColorUIResource add(ColorUIResource color, ColorUIResource subtractColor) {
    return new ColorUIResource(color.getRed() + subtractColor.getRed(),
                               color.getGreen() + subtractColor.getGreen(),
                               color.getBlue() + subtractColor.getBlue());
  }
  
  private final static void initializeColor() {
    if (colorInitialized) {
      return;
    }
    colorInitialized = true;
    ColorUIResource greyout = new ColorUIResource(25, 25, 25);
    foreground = new ColorUIResource(0, 0, 0);
    notInUseForeground = new ColorUIResource(102, 102, 102);
    background = new ColorUIResource(255, 255, 255);
    disabledBackground = subtract(background, greyout);
    highlightedBackground = new ColorUIResource(204, 255, 255);
    disabledhighlightedBackground = subtract(highlightedBackground, greyout);
    warningBackground = new ColorUIResource(255, 255, 204);
    disabledWarningBackground = subtract(warningBackground, greyout);
    errorBackground = new ColorUIResource(255, 204, 204);
    disabledErrorBackground = subtract(errorBackground, greyout);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2005/07/19 22:32:40  sueh
 * <p> bug# 532 changing the look of inUse == false to greyed out text.
 * <p> Changing the look of error == true to red background.  Added warning
 * <p> boolean.  Setting color internally instead of relying on UI color.
 * <p>
 * <p> Revision 1.1  2005/07/01 21:20:08  sueh
 * <p> bug# 619 Pulled an ancestor class (InputCell) out of FieldCell because we
 * <p> need several types of input cells.  InputCell handles states and colors
 * <p> but not display fields.
 * <p> </p>
 */