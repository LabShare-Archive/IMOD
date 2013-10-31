package etomo.logic;

/**
* <p>Description: For backwards compatibility with previous version which encorporated
* elongated.</p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class ClusteredPointsAllowed {
  public static final String rcsid = "$Id:$";
  public static final ClusteredPointsAllowed CLUSTERED = new ClusteredPointsAllowed(1);
  /**
   * @deprecated - no longer valid values
   */
  private static final ClusteredPointsAllowed ELONGATED_ONE_THIRD = new ClusteredPointsAllowed(
      2);
  /**
   * @deprecated - no longer valid values
   */
  private static final ClusteredPointsAllowed ELONGATED_TWO_THIRDS = new ClusteredPointsAllowed(
      3);
  /**
   * @deprecated - no longer valid values
   */
  private static final ClusteredPointsAllowed ELONGATED_ALL = new ClusteredPointsAllowed(
      4);

  private final int value;

  private ClusteredPointsAllowed(final int value) {
    this.value = value;
  }

  public static ClusteredPointsAllowed getInstance(final int value) {
    if (value == CLUSTERED.value) {
      return CLUSTERED;
    }
    if (value == ELONGATED_ONE_THIRD.value) {
      return ELONGATED_ONE_THIRD;
    }
    if (value == ELONGATED_TWO_THIRDS.value) {
      return ELONGATED_TWO_THIRDS;
    }
    if (value == ELONGATED_ALL.value) {
      return ELONGATED_ALL;
    }
    return null;
  }

  /**
   * @deprecated
   * @param displayValue
   * @return
   */
  public static ClusteredPointsAllowed getInstanceFromDisplayValue(
      final Number displayValue) {
    if (displayValue == null) {
      return null;
    }
    return getInstance(displayValue.intValue() + 1);
  }

  public boolean isElongated() {
    return this != CLUSTERED;
  }

  public int getValue() {
    return value;
  }

  /**
   * for backwards compatibility
   */
  public int convertToDisplayValue() {
    return value - 1;
  }

  public String toString() {
    return Integer.toString(value);
  }
}
