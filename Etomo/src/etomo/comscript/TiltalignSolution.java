package etomo.comscript;

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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/12/10 21:35:56  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltalignSolution {
  public static final String rcsid = "$Id$";

  public int type;
  public FortranInputString referenceView;
  public FortranInputString params;
  public StringList additionalGroups;

  public TiltalignSolution() {
    params = new FortranInputString(2);
    params.setIntegerType(0, true);
    params.setIntegerType(1, true);
    referenceView = new FortranInputString(1);
    referenceView.setIntegerType(0, true);
    additionalGroups = new StringList(0);
  }

  /**
   * Copy constructor
   */
  public TiltalignSolution(TiltalignSolution src) {
    type = src.type;
    referenceView = src.referenceView;
    params = new FortranInputString(src.params);
    additionalGroups = new StringList(src.additionalGroups);
  }
}
