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
  protected static ColorUIResource errorForeground = null;
  private static ColorUIResource background = null;
  private static ColorUIResource disabledBackground = null;
  private static ColorUIResource highlightedBackground = null;
  private static ColorUIResource disabledhighlightedBackground = null;
  private static ColorUIResource headerBackground = null;

  private boolean inUse = true;
  protected boolean enabled = true;
  private boolean highlighted = false;
  protected boolean error = false;
  private Font plainFont = null;
  private Font italicFont = null;
  private JPanel jpanelContainer = null;

  abstract protected Component getComponent();
  abstract protected void setForeground();

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
    if (inUse) {
      getComponent().setFont(plainFont);
    }
    else {
      getComponent().setFont(italicFont);
    }
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

  final void setError(boolean error) {
    this.error = error;
    setForeground();
  }

  private final static void initializeColor() {
    if (colorInitialized) {
      return;
    }
    colorInitialized = true;
    foreground = new ColorUIResource(0, 0, 0);

    errorForeground = new ColorUIResource(153, 0, 0);

    background = UIUtilities.getDefaultUIColor("Table.backgroundvalue");
    if (background == null) {
      background = new ColorUIResource(255, 255, 255);
    }

    highlightedBackground = UIUtilities
        .getDefaultUIColor("Table.selectionBackgroundvalue");
    if (highlightedBackground == null) {
      highlightedBackground = new ColorUIResource(204, 255, 255);
    }

    headerBackground = UIUtilities
        .getDefaultUIColor("TableHeader.backgroundvalue");
    if (headerBackground == null) {
      headerBackground = new ColorUIResource(204, 204, 204);
    }

    int backgroundRed = background.getRed();
    int backgroundGreen = background.getGreen();
    int backgroundBlue = background.getBlue();
    int red = backgroundRed - headerBackground.getRed();
    int green = backgroundGreen - headerBackground.getGreen();
    int blue = backgroundBlue - headerBackground.getBlue();
    int greyoutValue = (red + green + blue) / 6;

    disabledBackground = new ColorUIResource(backgroundRed - greyoutValue,
        backgroundGreen - greyoutValue, backgroundBlue - greyoutValue);

    disabledhighlightedBackground = new ColorUIResource(highlightedBackground
        .getRed()
        - greyoutValue, highlightedBackground.getGreen() - greyoutValue,
        highlightedBackground.getBlue() - greyoutValue);
  }
}
/**
 * <p> $Log$ </p>
 */