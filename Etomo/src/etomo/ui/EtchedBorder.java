package etomo.ui;

import java.awt.Color;

import javax.swing.BorderFactory;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

/*
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
 */

public class EtchedBorder {
  public static final String rcsid = "$Id$";

  private TitledBorder titledBorder;
  // TODO: these should be gotten from the app some how
  private static final Color highlight = new Color(248, 254, 255);
  private static final Color shadow = new Color(121, 124, 136);

  public EtchedBorder(String title) {
    titledBorder =
      new TitledBorder(
        BorderFactory.createEtchedBorder(highlight, shadow),
        title);
  }

  public Border getBorder() {
    return titledBorder;
  }

}