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
 * <p> $Log$ </p>
 */
public class BeveledBorder {
  public static final String rcsid = "$Id$";

  Border border;
  TitledBorder titledBorder;
  static Color highlightOuter = new Color(248, 254, 255);
  static Color shadowOuter = new Color(121, 124, 136);
  static Color shadowInner = new Color(84, 86, 95);

  public BeveledBorder(String title) {
    titledBorder = new
      TitledBorder(BorderFactory.createEtchedBorder(highlightOuter,
	shadowOuter), title);

    border = BorderFactory.createBevelBorder(BevelBorder.LOWERED,
					     highlightOuter,
					     Color.white,
					     shadowOuter,
					     shadowOuter);
    titledBorder.setBorder(border);
  }

  public Border getBorder() {
    return titledBorder;
  }

}
