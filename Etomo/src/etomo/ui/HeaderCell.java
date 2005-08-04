package etomo.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;

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
class HeaderCell {
  public static  final String  rcsid =  "$Id$";
  
  private JButton cell;
  private JPanel jpanelContainer = null;
  private String text = "";
  
  HeaderCell() {
    this(null, -1);
  }
  
  HeaderCell(String text) {
    this(text, -1);
  }
  
  HeaderCell(String text, int width) {
    this.text = text;
    if (text == null) {
      cell = new JButton();
    }
    else {
      cell = new JButton("<html><b>" + text + "</b>");
    }
    cell.setBorder(BorderFactory.createEtchedBorder());
    cell.setEnabled(false);
    if (width > 0) {
      Dimension size = cell.getPreferredSize();
      size.width = width;
      cell.setPreferredSize(size);
    }
  }
  
  void setBorderPainted(boolean borderPainted) {
    cell.setBorderPainted(borderPainted);
  }
  
  HeaderCell add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
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
  
  void setText(String text) {
    this.text = text;
    cell.setText("<html><b>" + text + "</b>");
  }
  
  final int getHeight() {
    return cell.getHeight() + cell.getBorder().getBorderInsets(cell).bottom - 1;
  }
}
