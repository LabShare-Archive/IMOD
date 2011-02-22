package etomo.ui.swing;

import java.awt.Color;

import etomo.util.InvalidParameterException;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.2  2006/07/31 21:43:23  sueh
 * <p> bug# 438 Made labels optional
 * <p>
 * <p> Revision 3.1  2003/11/10 07:40:06  rickg
 * <p> Now throws exceptions for invalid parameters
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/03/20 17:29:53  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ColoredStateText {
  public static final String rcsid = "$Id$";

  private String[] labels = null;
  private Color[] colors;
  private int nItems = 0;
  private int currentSelected = -1;

  public ColoredStateText(String[] labels, Color[] colors)
      throws InvalidParameterException {
    nItems = labels.length;
    if (nItems != colors.length) {
      throw new InvalidParameterException(
          "The length of the labels and colors arrays do not match");
    }
    this.labels = labels;
    this.colors = colors;
  }

  public ColoredStateText(Color[] colors) {
    nItems = colors.length;
    this.colors = colors;
  }

  public void setSelected(int index) throws InvalidParameterException {
    if (index < 0 & index >= nItems) {
      throw new InvalidParameterException("Index out of range, nItems: "
          + String.valueOf(nItems) + " index: " + String.valueOf(index));
    }
    currentSelected = index;
  }

  public int getSelected() {
    return currentSelected;
  }

  public String getSelectedText() {
    if (labels == null) {
      return null;
    }
    return labels[currentSelected];
  }

  public Color getSelectedColor() {
    return colors[currentSelected];
  }
}