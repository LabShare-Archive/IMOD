package etomo.ui.swing;

import java.awt.*;
import java.awt.event.MouseListener;

import javax.swing.*;

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
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
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
public class HighlightList {
  public static final String rcsid = "$Id$";

  JPanel panel = new JPanel();
  JLabel[] labels;
  int nItems = 0;
  int currentSelected = -1;
  Color unselected = new Color(128, 128, 128);
  Color selected = new Color(160, 0, 64);

  public HighlightList(String[] items) {
    nItems = items.length;
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    labels = new JLabel[nItems];

    for (int i = 0; i < nItems; i++) {
      labels[i] = new JLabel(items[i]);
      labels[i].setText(items[i]);
      labels[i].setForeground(unselected);
      panel.add(labels[i]);
      panel.add(Box.createRigidArea(FixedDim.x0_y5));
    }
  }

  public JPanel getPanel() {
    return panel;
  }

  public void setSelected(int index) {
    if (index >= 0 & index < nItems) {
      //
      //  Deselect the current item
      //
      if (currentSelected != -1) {
        labels[currentSelected].setForeground(unselected);
      }
      //
      //  Select the new item
      //
      currentSelected = index;
      labels[currentSelected].setForeground(selected);
    }
  }

  public int getSelected() {
    return currentSelected;
  }

  public String getSelectedText() {
    return labels[currentSelected].getText();
  }

  public void addMouseListener(MouseListener listener) {
    panel.addMouseListener(listener);
    for (int i = 0; i < labels.length; i++) {
      labels[i].addMouseListener(listener);
    }
  }
}
