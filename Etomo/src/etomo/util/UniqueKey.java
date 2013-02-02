package etomo.util;

/**
 * <p>Description: key that is unique to an array.  The values in this key
 * are immutable.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2005/08/22 18:23:00  sueh
 * <p> bug# 532 Moved HashedArray to UniqueHashedArray.  Added a simpler
 * <p> HashedArray class which does not use UniqueKey.
 * <p>
 * <p> Revision 1.3  2004/12/04 01:01:54  sueh
 * <p> Added comments.
 * <p>
 * <p> Revision 1.2  2004/11/20 00:15:20  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/28 22:19:56  sueh
 * <p> bug# 520 Clarified code by improving names for parameters.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/13 19:20:23  sueh
 * <p> bug# 520 An immutable key which is unque to the HashedArray passed
 * <p> to it.  Contains a hashCode() function.
 * <p> </p>
 */
public class UniqueKey {
  public static final String rcsid = "$Id$";

  private final String name;
  private final int count;

  UniqueKey(String name, UniqueHashedArray keyedStorage) {
    this.name = name;
    //make instance unique in keyedStorage by making count one more then the
    //largest stored key with the same name
    int tempCount = 0;
    for (int i = 0; i < keyedStorage.size(); i++) {
      UniqueKey storedKey = (UniqueKey) keyedStorage.getKey(i);
      if (storedKey.name.equals(name)) {
        tempCount = storedKey.count + 1;
      }
    }
    count = tempCount;
  }

  public boolean equals(UniqueKey that) {
    if (that == null) {
      return false;
    }
    if (name.equals(that.name) && count == that.count) {
      return true;
    }
    return false;
  }

  public String getName() {
    return name;
  }

  public int hashCode() {
    Integer intCount = new Integer(this.count);
    return name.hashCode() + intCount.hashCode();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  private String paramString() {
    return ",name=" + name + ",count =" + count;
  }
}
