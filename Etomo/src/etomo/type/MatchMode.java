package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class MatchMode {
  public static final String rcsid = "$Id$";

  public static final MatchMode B_TO_A = new MatchMode();
  public static final MatchMode A_TO_B = new MatchMode();

  private static final String B_TO_A_STRING = "B_TO_A";
  private static final String A_TO_B_STRING = "A_TO_B";

  private MatchMode() {
  }

  public String toString() {
    if (this == B_TO_A) {
      return B_TO_A_STRING;
    }
    if (this == A_TO_B) {
      return A_TO_B_STRING;
    }
    return "";
  }

  public static MatchMode getInstance(String string) {
    if (string == null) {
      return null;
    }
    if (string.equals(B_TO_A_STRING)) {
      return B_TO_A;
    }
    if (string.equals(A_TO_B_STRING)) {
      return A_TO_B;
    }
    return null;
  }

  public static MatchMode getInstance(boolean matchBToA) {
    if (matchBToA) {
      return B_TO_A;
    }
    return A_TO_B;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2006/03/16 01:54:18  sueh
 * <p> bug# 828 Enum to handle the match direction in Combine.
 * <p> </p>
 */
