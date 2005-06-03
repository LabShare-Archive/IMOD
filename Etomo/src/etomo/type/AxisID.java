package etomo.type;

import etomo.EtomoDirector;

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
 * <p> Revision 1.1.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class AxisID {
  public static final String rcsid =
    "$Id$";

  private static final String ONLY_AXIS_NAME = "Only";
  
  private final String name;
  private final boolean selfTest = EtomoDirector.getInstance().isSelfTest();

  private AxisID(String name) {
    this.name = name;
  }

  public static final AxisID ONLY = new AxisID(ONLY_AXIS_NAME);
  public static final AxisID FIRST = new AxisID("First");
  public static final AxisID SECOND = new AxisID("Second");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Returns the extension associated with the specific AxisID.  Used for
   * creating file names.
   */
  public String getExtension() {
    runSelfTest();
    return getStorageExtension();
  }
  
  /**
   * Returns the extension associated with the specific AxisID.  Should not be
   * used for creating file names.
   */
  public String getStorageExtension() {
    if (this == ONLY) {
      return "";
    }
    if (this == FIRST) {
      return "a";
    }
    if (this == SECOND) {
      return "b";
    }
    return "ERROR";
  }
  /**
   * Takes a string representation of an AxisID type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString().
   */
  public static AxisID fromString(String name) {
    if (name.compareToIgnoreCase(ONLY.toString()) == 0) {
      return ONLY;
    }
    if (name.compareToIgnoreCase(FIRST.toString()) == 0) {
      return FIRST;
    }
    if (name.compareToIgnoreCase(SECOND.toString()) == 0) {
      return SECOND;
    }

    return null;
  }
  
  private void runSelfTest() {
    if (!selfTest) {
      return;
    }
    selfTest();
  }
  
  public void selfTest() {
    //state is always getExtension
    //make sure that the extension is correct, or the file name will be wrong
    AxisType axisType = null;
    try {
      axisType = EtomoDirector.getInstance().getCurrentManager()
          .getBaseMetaData().getAxisType();
    }
    catch (IllegalStateException e) {
      return;
    }
    if (axisType == AxisType.SINGLE_AXIS && !name.equals(ONLY_AXIS_NAME)) {
      throw new IllegalStateException(
          "AxisID should be 'Only' when AxisType is single.  AxisID = "
              + name);
    }
  }
}
