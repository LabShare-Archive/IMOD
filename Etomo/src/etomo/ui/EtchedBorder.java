package etomo.ui;

import java.awt.Color;

import javax.swing.BorderFactory;
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
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.3  2003/03/20 17:30:10  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 1.2  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 1.1  2003/02/24 23:21:44  rickg
 * <p> Initial revision
 * <p>
 */

public class EtchedBorder {
  public static final String rcsid =
    "$Id$";

  private TitledBorder titledBorder;
  // TODO these should be gotten from the app some how
  private static final Color highlight = new Color(248, 254, 255);
  private static final Color shadow = new Color(121, 124, 136);

  public EtchedBorder(String title) {
    titledBorder =
      new TitledBorder(
        BorderFactory.createEtchedBorder(highlight, shadow),
        title);
  }

  public TitledBorder getBorder() {
    return titledBorder;
  }

}