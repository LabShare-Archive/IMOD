package etomo.type;

/**
 * <p>Description: View type definitions</p>
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
 * <p> Revision 3.1  2007/12/26 22:20:01  sueh
 * <p> bug# 1052 Added getValue() to return an alternative value for ViewType.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ViewType implements EnumeratedType {
  public static final String rcsid = "$Id$";

  public static final ViewType SINGLE_VIEW = new ViewType("Single View", "single", 0);
  public static final ViewType MONTAGE = new ViewType("Montage", "montage", 1);

  public static final ViewType DEFAULT = SINGLE_VIEW;

  private final EtomoNumber index = new EtomoNumber();

  private final String title;
  private final String paramValue;

  private ViewType(final String title, final String paramValue, final int index) {
    this.title = title;
    this.paramValue = paramValue;
    this.index.set(index);
  }

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return title;
  }

  public String getParamValue() {
    return paramValue;
  }

  public ConstEtomoNumber getValue() {
    return index;
  }

  public boolean isDefault() {
    return this == DEFAULT;
  }

  public String getLabel() {
    return null;
  }

  /**
   * Takes a string representation of an ViewType type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString() or getParamValue().
   */
  public static ViewType fromString(final String name) {
    if (name.compareToIgnoreCase(SINGLE_VIEW.toString()) == 0) {
      return SINGLE_VIEW;
    }
    if (name.compareToIgnoreCase(MONTAGE.toString()) == 0) {
      return MONTAGE;
    }
    if (name.compareToIgnoreCase(SINGLE_VIEW.getParamValue()) == 0) {
      return SINGLE_VIEW;
    }
    if (name.compareToIgnoreCase(MONTAGE.getParamValue()) == 0) {
      return MONTAGE;
    }
    return null;
  }

  public static ViewType getInstance(final EnumeratedType enumeratedType) {
    if (enumeratedType == SINGLE_VIEW) {
      return SINGLE_VIEW;
    }
    if (enumeratedType == MONTAGE) {
      return MONTAGE;
    }
    return DEFAULT;
  }
}
