package etomo.ui;
import java.awt.Color;

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

  private String[] labels;
  private Color[] colors;
  private int nItems = 0;
  private int currentSelected = -1;

  public ColoredStateText(String[] labels, Color[] colors) {
    nItems = labels.length;
    // TODO throw an exception if the two arrays are not of equal length
    this.labels = labels;
    this.colors = colors;
  }

  public void setSelected(int index) {

    //TODO should throw a parameter out of range exception
    if (index < 0 & index >= nItems) {
      index = -1;
    }
    currentSelected = index;
  }

  public int getSelected() {
    return currentSelected;
  }

  public String getSelectedText() {
    return labels[currentSelected];
  }

  public Color getSelectedColor() {
    return colors[currentSelected];
  }
}