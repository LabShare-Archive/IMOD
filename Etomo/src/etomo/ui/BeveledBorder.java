package etomo.ui;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

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
public class BeveledBorder {
  public static final String rcsid =
    "$Id$";

  Border border;
  TitledBorder titledBorder;
  static Color highlightOuter = new Color(248, 254, 255);
  static Color shadowOuter = new Color(121, 124, 136);
  static Color shadowInner = new Color(84, 86, 95);

  public BeveledBorder(String title) {
    titledBorder =
      new TitledBorder(
        BorderFactory.createEtchedBorder(highlightOuter, shadowOuter),
        title);

    border =
      BorderFactory.createBevelBorder(
        BevelBorder.LOWERED,
        highlightOuter,
        Color.white,
        shadowOuter,
        shadowOuter);
    titledBorder.setBorder(border);
  }

  public TitledBorder getBorder() {
    return titledBorder;
  }

}
