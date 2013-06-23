package etomo.logic;

import etomo.type.EtomoNumber;

/**
* <p>Description: Translates between the multi-particle reference level stored in the
* .prm file and the list of particle counts displayed on the screen.  The particle counts
* are 2^k, where k is the reference level.</p>
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
public final class MultiparticleReference {
  public static final String rcsid = "$Id:$";

  private static final int MIN_LEVEL = 2;
  private static final int MAX_LEVEL = 10;
  public static final int DEFAULT_LEVEL = 5;

  public static int getNumEntries() {
    return MAX_LEVEL - MIN_LEVEL + 1;
  }

  /**
   * Converts the (corrected) index to a level and then to a particle count.
   * @param index
   * @return
   */
  public static Integer getParticleCount(int index) {
    int numEntries = getNumEntries();
    if (index < 0) {
      index = 0;
    }
    else if (index >= numEntries) {
      index = numEntries - 1;
    }
    return new Integer((int) (Math.pow(2, index + MIN_LEVEL)));
  }

  /**
   * Converts the default level to an index.
   * @return
   */
  public static int getDefaultIndex() {
    return DEFAULT_LEVEL - MIN_LEVEL;
  }

  /**
   * Calculates the index of the reference in the list of particle counts.
   * @param reference
   * @param index - parameter for returning the index (must not be null)
   * @return known value (no warning)
   */
  public static boolean convertLevelToIndex(final String level, final EtomoNumber index) {
    if (level == null) {
      index.set(getDefaultIndex());
      return true;
    }
    EtomoNumber nLevel = new EtomoNumber();
    nLevel.setCeiling(MAX_LEVEL);
    nLevel.setFloor(MIN_LEVEL);
    nLevel.set(level);
    if (!nLevel.isValid()) {
      index.set(getDefaultIndex());
      return true;
    }
    // calculate index
    index.set(nLevel.getInt() - MIN_LEVEL);
    if (nLevel.isValueAltered()) {
      return false;
    }
    return true;
  }

  /**
   * Calculates the index of the reference in the list of particle counts.
   * @param reference
   * @return int
   */
  public static int convertLevelToIndex(final int level) {
    EtomoNumber nLevel = new EtomoNumber();
    nLevel.setCeiling(MAX_LEVEL);
    nLevel.setFloor(MIN_LEVEL);
    nLevel.set(level);
    // calculate index
    return nLevel.getInt() - MIN_LEVEL;
  }

  /**
   * Returns a reference.
   * @param index - index to the list of particles counts.
   * @return
   */
  public static String convertIndexToLevel(int index) {
    EtomoNumber nLevel = new EtomoNumber();
    nLevel.setCeiling(MAX_LEVEL);
    nLevel.setFloor(MIN_LEVEL);
    nLevel.set(index + MIN_LEVEL);
    return nLevel.toString();
  }
}
