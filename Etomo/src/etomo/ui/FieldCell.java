package etomo.ui;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.plaf.ColorUIResource;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
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
* <p> Revision 1.1.2.2  2004/10/13 23:06:57  sueh
* <p> bug# 520 Changed the add() functions.  No longer relaying on the Table
* <p> interface because a FieldCell instance could be added to different panels.
* <p> The Add() functions remember what panel they where added to.  The
* <p> remove() requires no parameters.
* <p>
* <p> Revision 1.1.2.1  2004/10/01 19:53:42  sueh
* <p> bug# 520 A text field designed designed to be used with a gridbag layout.
* <p> It can be used with any ui object with implements Table.  It has three
* <p> state variables: isUse (lightens foreground when not in use), enabled
* <p> darkens background when not enabled), and highlighted (uses table
* <p> selection color for background when highlighted, setting it darker when
* <p> field is disabled).
* <p> </p>
*/
public class FieldCell {
  public static  final String  rcsid =  "$Id$";
  
  private static ColorUIResource foreground = null;
  private static ColorUIResource background = null;
  private static ColorUIResource disabledBackground = null;
  private static ColorUIResource highlightedBackground = null;
  private static ColorUIResource disabledhighlightedBackground = null;
  private static ColorUIResource headerBackground = null;
  
  private JTextField cell;
  private boolean inUse = true;
  private boolean enabled = true;
  private boolean highlighted = false;
  private Font plainFont = null;
  private Font italicFont = null;
  private JPanel jpanelContainer = null;
  
  FieldCell() {
    initializeColor();
    cell = new JTextField();
    cell.setBorder(BorderFactory.createEtchedBorder());
    cell.setDisabledTextColor(foreground);
    setColor();
    plainFont = cell.getFont();
    italicFont = new Font(plainFont.getFontName(), Font.ITALIC, plainFont.getSize());
  }
  
  void add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    layout.setConstraints(cell, constraints);
    panel.add(cell);
    jpanelContainer = panel;
  }
  
  void remove() {
    if (jpanelContainer != null) {
      jpanelContainer.remove(cell);
      jpanelContainer = null;
    }
  }
  
  void setText(String text) {
    cell.setText(text);
  }
  
  String getText() {
    return cell.getText();
  }
  
  void setEnabled(boolean enabled) {
    this.enabled = enabled;
    cell.setEnabled(enabled);
    setColor();
  }
  
  void setHighlighted(boolean highlighted) {
    this.highlighted = highlighted;
    setColor();
  }
  
  void setInUse(boolean inUse) {
    this.inUse = inUse;
    if (inUse) {
      cell.setFont(plainFont);
    }
    else {
      cell.setFont(italicFont);
    }
  }
    
  private void setColor() {
    if (highlighted) {
      if (enabled) {
        cell.setBackground(highlightedBackground);
      }
      else {
        cell.setBackground(disabledhighlightedBackground);
      }
    }
    else if (enabled) {
      cell.setBackground(background);
    }
    else {
      cell.setBackground(disabledBackground);
    }
  }
  
  private void initializeColor() {
    if (foreground == null) {
      foreground = UIUtilities.getDefaultUIColor("ToggleButton.foregroundvalue");
      if (foreground == null) {
        foreground = new ColorUIResource(0, 0, 0);
      }
    }
    if (background == null) {
      background = UIUtilities.getDefaultUIColor("Table.backgroundvalue");
      if (background == null) {
        background = new ColorUIResource(255, 255, 255);
      }
    }
    if (highlightedBackground == null) {
      highlightedBackground = UIUtilities.getDefaultUIColor("Table.selectionBackgroundvalue");
      if (highlightedBackground == null) {
        highlightedBackground = new ColorUIResource(204, 255, 255);
      }
    }
    
    if (headerBackground == null) {
      headerBackground = UIUtilities.getDefaultUIColor("TableHeader.backgroundvalue");
      if (headerBackground == null) {
        headerBackground = new ColorUIResource(204, 204, 204);
      }
    }
    int backgroundRed = background.getRed();
    int backgroundGreen = background.getGreen();
    int backgroundBlue = background.getBlue();
    int red = backgroundRed - headerBackground.getRed();
    int green = backgroundGreen - headerBackground.getGreen();
    int blue = backgroundBlue - headerBackground.getBlue();
    int greyoutValue = (red + green + blue) / 6;
    
    if (disabledBackground == null) {
      disabledBackground = new ColorUIResource(backgroundRed - greyoutValue,
          backgroundGreen - greyoutValue, backgroundBlue - greyoutValue);
    }
    if (disabledhighlightedBackground == null) {
      disabledhighlightedBackground = new ColorUIResource(highlightedBackground
          .getRed()
          - greyoutValue, highlightedBackground.getGreen() - greyoutValue,
          highlightedBackground.getBlue() - greyoutValue);
    }

  }
}
