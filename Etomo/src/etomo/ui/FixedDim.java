package etomo.ui;

import java.awt.Dimension;

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
 * <p> Revision 3.7  2006/07/21 19:02:36  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 3.6  2005/08/10 20:43:29  sueh
 * <p> bug# 711 Moved button sizing to MultiLineButton.  SetSize() sets the
 * <p> standard button size.
 * <p>
 * <p> Revision 3.5  2005/07/01 21:16:59  sueh
 * <p> bug# 619 Added x70_y0.
 * <p>
 * <p> Revision 3.4  2005/03/24 17:51:55  sueh
 * <p> bug# 621 Added a standard width for 4 digits.
 * <p>
 * <p> Revision 3.3  2005/01/05 00:02:20  sueh
 * <p> bug# 567 added widths for integer pairs and triplets
 * <p>
 * <p> Revision 3.2  2004/12/30 18:37:51  sueh
 * <p> bug# 567 Add a 25 pixel x spacing variable.
 * <p>
 * <p> Revision 3.1  2004/11/19 23:54:15  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.0.6.2  2004/09/21 17:58:09  sueh
 * <p> bug# 520 Correcting a typo.
 * <p>
 * <p> Revision 3.0.6.1  2004/09/16 18:30:33  sueh
 * <p> bug# 520 added widths for SectionTablePanel
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/18 17:46:46  rickg
 * <p> Added new dimension constansts
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class FixedDim {
  public static final String rcsid =
    "$Id$";

  public final static Dimension x5_y0 = new Dimension(5, 0);
  public final static Dimension x10_y0 = new Dimension(10, 0);
  public final static Dimension x20_y0 = new Dimension(20, 0);
  public final static Dimension x40_y0 = new Dimension(40, 0);
  public final static Dimension x70_y0 = new Dimension(70, 0);
  public final static Dimension x200_y0 = new Dimension(200, 0);
  public final static Dimension x0_y5 = new Dimension(0, 5);
  public final static Dimension x0_y10 = new Dimension(0, 10);
  public final static Dimension x0_y20 = new Dimension(0, 20);
  public final static Dimension x0_y40 = new Dimension(0, 40);
  public final static Dimension folderButton = new Dimension(32, 32);
}
