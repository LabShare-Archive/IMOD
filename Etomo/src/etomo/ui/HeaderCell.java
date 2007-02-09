package etomo.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.plaf.ColorUIResource;

import etomo.type.EtomoNumber;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
class HeaderCell {
  public static final String rcsid = "$Id$";

  private static final ColorUIResource background = new ColorUIResource(204,
      204, 204);
  private static final ColorUIResource greyout = UIUtilities.subtractColor(
      UIUtilities.BACKGROUND, background);
  private static final ColorUIResource warningBackground = UIUtilities
      .subtractColor(UIUtilities.WARNING_BACKGROUND, greyout);

  private JButton cell;
  private JPanel jpanelContainer = null;
  private String text = "";
  private final boolean controlColor;
  private String pad = "";

  public String toString() {
    return text;
  }

  HeaderCell() {
    this(null, -1, true);
  }

  HeaderCell(String text) {
    this(text, -1, true);
  }

  HeaderCell(String text, boolean controlColor) {
    this(text, -1, controlColor);
  }

  HeaderCell(int width) {
    this(null, width, true);
  }

  HeaderCell(String text, int width) {
    this(text, width, true);
  }

  private HeaderCell(String text, int width, boolean controlColor) {
    this.text = text;
    this.controlColor = controlColor;
    if (text == null) {
      cell = new JButton();
    }
    else {
      cell = new JButton(formatText());
    }
    cell.setBorder(BorderFactory.createEtchedBorder());
    cell.setEnabled(false);
    if (width > 0) {
      Dimension size = cell.getPreferredSize();
      size.width = width;
      cell.setPreferredSize(size);
    }
    if (controlColor) {
      cell.setBackground(background);
    }
  }

  final void setWarning(boolean warning, String toolTipText) {
    setWarning(warning);
    setToolTipText(toolTipText);
  }

  final void setWarning(boolean warning) {
    //Can't change the color if not controlling color
    if (!controlColor) {
      return;
    }
    if (warning) {
      cell.setBackground(warningBackground);
    }
    else {
      cell.setBackground(background);
    }
  }

  void setBorderPainted(boolean borderPainted) {
    cell.setBorderPainted(borderPainted);
  }

  HeaderCell add(JPanel panel, GridBagLayout layout,
      GridBagConstraints constraints) {
    layout.setConstraints(cell, constraints);
    panel.add(cell);
    jpanelContainer = panel;
    return this;
  }

  void remove() {
    if (jpanelContainer != null) {
      jpanelContainer.remove(cell);
      jpanelContainer = null;
    }
  }

  String getText() {
    return text;
  }

  int getInt() {
    return new EtomoNumber().set(text).getInt();
  }

  void setText(String text) {
    this.text = text;
    cell.setText(formatText());
  }

  final void setText() {
    text = "";
    setText("");
  }

  final int getHeight() {
    return cell.getHeight() + cell.getBorder().getBorderInsets(cell).bottom - 1;
  }

  final int getWidth() {
    return cell.getWidth();
  }

  final void setToolTipText(String text) {
    cell.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  final void pad() {
    if (text==null) {
      return;
    }
    pad = " ";
    cell.setText(formatText());
  }

  private String formatText() {
    return "<html><b>" + text + pad + "</b>";
  }
}
/**
 * * <p> $Log$
 * * <p> Revision 1.14  2007/02/05 23:37:05  sueh
 * * <p> bug# 962 Added getInt().
 * * <p>
 * * <p> Revision 1.13  2006/10/17 20:19:18  sueh
 * * <p> bug# 919  Adding setWarning().  Changing boolean fixedColor to controlColor.
 * * <p>
 * * <p> Revision 1.12  2006/06/29 20:06:57  sueh
 * * <p> bug# 880 Added HeaderCell(int).
 * * <p>
 * * <p> Revision 1.11  2006/04/10 19:08:43  sueh
 * * <p> bug# 846 Added fixedColor parameter to constructor to preserve the color
 * * <p> relationships in the tables.
 * * <p>
 * * <p> Revision 1.10  2006/01/27 18:41:58  sueh
 * * <p> bug# 801 Need to set text to "" when clearing the button text
 * * <p>
 * * <p> Revision 1.9  2005/12/14 20:56:56  sueh
 * * <p> bug# 784 Added setToolTipText().
 * * <p>
 * * <p> Revision 1.8  2005/11/29 22:45:49  sueh
 * * <p> bug# bug# 757 Added setText(void) to blank out the header label.
 * * <p>
 * * <p> Revision 1.7  2005/11/04 00:54:05  sueh
 * * <p> bug# 732 Added getWidth().
 * * <p>
 * <p> Revision 1.6  2005/08/04 20:10:46  sueh
 * <p> bug# 532 Fixed getHeight() and removed getBorderHeight().
 * <p>
 * <p> Revision 1.5  2005/07/21 22:19:07  sueh
 * <p> bug# 532 Fixed test.  It wasn't being set on construction
 * <p>
 * <p> Revision 1.4  2005/07/11 23:00:57  sueh
 * <p> bug# 619 Added functions:  getBorderHeight and getHeight so that the
 * <p> height of the processor table can be calculated.
 * <p>
 * <p> Revision 1.3  2005/07/06 23:36:09  sueh
 * <p> bug# 619 Added setBorderPainted() so that a borderless header cell can
 * <p> be created.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:55:38  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/29 22:14:28  sueh
 * <p> bug# 520 Removed color settings.  They are unecessary, since the color
 * <p> is never changed.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/22 16:39:20  sueh
 * <p> bug# 520 Added getText().  Return the original text string before is it
 * <p> wrapped in html.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/13 23:08:37  sueh
 * <p> bug# 520 Changed the add() functions.  No longer relying on the Table
 * <p> interface because a FieldCell instance could be added to different panels.
 * <p> The Add() functions remember what panel they where added to.  The
 * <p> remove() requires no parameters.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/01 19:56:55  sueh
 * <p> bug# 520 A header designed designed to be used with a gridbag layout.
 * <p> It can be used with any ui object with implements Table.  It is actually a
 * <p> disabled button with bolded text and an etched border.  It uses
 * <p> TableHeader colors.
 * <p> </p>
 */

