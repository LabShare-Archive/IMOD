package etomo.util;

import java.util.Hashtable;
import java.util.Vector;

/**
* <p>Description: A list of name, value pairs that can be accessed by keys or 
* indexes and can have non-unique names.</p>
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
* <p> Revision 1.10  2006/11/29 00:22:34  sueh
* <p> bug# 934 Corrected comment.
* <p>
* <p> Revision 1.9  2005/09/09 21:48:15  sueh
* <p> bug# 532 Handling null from stderr and stdout.
* <p>
* <p> Revision 1.8  2005/09/01 18:07:25  sueh
* <p> bug# 532  Added invariant self testing.  In remove(), return if the key isn't
* <p> found.
* <p>
* <p> Revision 1.7  2005/08/31 17:19:36  sueh
* <p> bug# 532 Ack!  Have to reindex the hashtable when remove an element
* <p> from the array.
* <p>
* <p> Revision 1.6  2005/08/30 19:24:34  sueh
* <p> bug# 532 Changed class so a remove(Object key) function could be added.
* <p> Using valueArray (an array of values) and indexMap (a hashtable to the
* <p> index of valueArray) allows more functionality.
* <p>
* <p> Revision 1.5  2005/08/22 18:22:41  sueh
* <p> bug# 532 Moved HashedArray to UniqueHashedArray.  Added a simpler
* <p> HashedArray class which does not use UniqueKey.
* <p>
* <p> Revision 1.4  2005/02/07 23:00:03  sueh
* <p> bug# 594 Fixed rekey to preserve array order.  Added add(UniqueKey, Object),
* <p> to add with an existing key.  Added rekey(UniqueKey, UniqueKey) to
* <p> rekey to an existing key.
* <p>
* <p> Revision 1.3  2005/01/22 04:09:01  sueh
* <p> bug# 509, bug# 591  Commenting functions.
* <p>
* <p> Revision 1.2  2004/11/20 00:11:14  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.2  2004/10/28 22:19:27  sueh
* <p> bug# 520 Clarified code by improving names for functions, member
* <p> variables, and parameters.
* <p>
* <p> Revision 1.1.2.1  2004/09/13 19:16:28  sueh
* <p> bug# 520 A list of name, value pairs the can be accessed by index or key.
* <p> Has two add functions: one that creates a new unique key and one that
* <p> adds to an an existing key.  Used for EtomoDirect.managerList and the
* <p> Window menu in MainFrame.  Can a create a new HashedArray with keys
* <p> for an existing HashedArray, but no values.  Can remove elements.
* <p> </p>
*/
public class HashedArray {
  public static final String rcsid = "$Id$";

  private final Hashtable valueMap = new Hashtable();
  private final Vector keyArray = new Vector();

  void selfTestInvariants() {
    if (!Utilities.isSelfTest()) {
      return;
    }
    //collections should be the same size
    if (valueMap.size() != keyArray.size()) {
      throw new IllegalStateException("sizes are different:" + "valueMap="
          + valueMap.size() + ",keyArray=" + keyArray.size());
    }
    //a key in keyArray must exist in valueMap
    for (int i = 0; i < keyArray.size(); i++) {
      if (!valueMap.containsKey(keyArray.get(i))) {
        throw new IllegalStateException("a key in keyArray is not in valueMap:" + "key="
            + keyArray.get(i));
      }
    }
  }

  void selfTestAdd(Object key, Object value) {
    if (!Utilities.isSelfTest()) {
      return;
    }
    if (!valueMap.containsKey(key)) {
      throw new IllegalStateException("The added key is not in valueMap:" + "key=" + key
          + ",value=" + value);
    }
    if (valueMap.get(key) != value) {
      throw new IllegalStateException("valueMap is mapped to the wrong value:" + "key="
          + key + ",value=" + value + ",valueMap.get(key)=" + valueMap.get(key));
    }
    if (!keyArray.contains(key)) {
      throw new IllegalStateException("The added key is not in keyArray:" + "key=" + key
          + ",value=" + value);
    }
  }

  /**
   * Add a new value with key
   * @param key
   * @param value
   * @return
   */
  public synchronized void add(Object key, Object value) {
    if (key == null) {
      return;
    }
    if (valueMap.put(key, value) == null) {
      keyArray.add(key);
    }
    selfTestInvariants();
    selfTestAdd(key, value);
  }

  public synchronized void add(Object key) {
    add(key, key);
  }

  public synchronized void remove(Object key) {
    if (key == null) {
      return;
    }
    if (!valueMap.containsKey(key)) {
      return;
    }
    valueMap.remove(key);
    for (int i = 0; i < keyArray.size(); i++) {
      if (keyArray.get(i).hashCode() == key.hashCode()) {
        keyArray.remove(i);
      }
    }
    selfTestInvariants();
  }

  public synchronized Object get(Object key) {
    if (key == null) {
      return null;
    }
    return valueMap.get(key);
  }

  public Object get(int index) {
    if (index < 0) {
      return null;
    }
    Object key = keyArray.get(index);
    if (key == null) {
      return null;
    }
    return valueMap.get(key);
  }

  public int size() {
    return valueMap.size();
  }

  public synchronized boolean containsKey(Object key) {
    return valueMap.containsKey(key);
  }
}
