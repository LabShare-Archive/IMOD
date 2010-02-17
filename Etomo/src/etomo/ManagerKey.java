package etomo;

import etomo.util.UniqueKey;

/**
 * <p>Description: A mutable container for a UniqueKey.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2009/03/17 00:24:08  sueh
 * <p> bug# 1186 Mutable class that holds a UniqueKey.
 * <p> </p>
 */
final class ManagerKey {
  public static final String rcsid = "$Id$";

  private UniqueKey uniqueKey = null;

  /**
   * Find out if it two manager keys contains the same unique key values.
   * @param managerKey
   * @return
   */
  public boolean equals(ManagerKey managerKey) {
    //If managerKey is false, then Etomo is not running yet.  Return false.
    if (managerKey == null) {
      return false;
    }
    //If one is null and the other is not null, return false.
    if ((uniqueKey == null) != (managerKey.uniqueKey == null)) {
      return false;
    }
    //If both are false, only return true if they are the same instance.  Not
    //sure if this is the right idea.  The other options is to always return
    //true in this case.
    if (uniqueKey == null) {
      return this == managerKey;
    }
    //If both are not null, use UniqueKey.equals.
    return uniqueKey.equals(managerKey.uniqueKey);
  }

  void setKey(UniqueKey key) {
    uniqueKey = key;
  }

  public UniqueKey getKey() {
    return uniqueKey;
  }

  public String toString() {
    return "[" + uniqueKey + "]";
  }
}
