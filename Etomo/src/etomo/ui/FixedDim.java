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
  public final static Dimension x0_y5 = new Dimension(0, 5);
  public final static Dimension x0_y10 = new Dimension(0, 10);
  public final static Dimension x0_y20 = new Dimension(0, 20);
  public final static Dimension x0_y40 = new Dimension(0, 40);
  public final static Dimension fileChooser = new Dimension(400, 400);
  public final static Dimension folderButton = new Dimension(32, 32);
  public final static Dimension button2Line = new Dimension(150, 50);
  public final static int numericWidth = 50;
  public final static int sectionsWidth = 75;
  public final static int highlighterWidth = 40;
  public final static int rowNumberWidth = 30;
}
