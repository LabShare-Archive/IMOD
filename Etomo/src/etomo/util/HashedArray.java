package etomo.util;

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
public class HashedArray extends ConstHashedArray {
  public static  final String  rcsid =  "$Id$";
  
  public HashedArray() {
    super();
  }
  
  protected HashedArray(Vector keyArray) {
    super(keyArray);
  }
  
  /**
   * Add a new value with a new uniqueKey
   * @param keyName
   * @param value
   * @return
   */
  public synchronized UniqueKey add(String keyName, Object value) {
    UniqueKey key = new UniqueKey(keyName, this);
    keyArray.add(key);
    map.put(key, value);
    return key;
  }
  
  /**
   * Set an existing UniqueKey to a new value by index
   * @param keyIndex
   * @param value
   * @return
   */
  public synchronized UniqueKey set(int keyIndex, Object value) {
    UniqueKey key = (UniqueKey) keyArray.get(keyIndex);
    map.remove(key);
    map.put(key, value);
    return key;
  }
  
  public synchronized Object remove(UniqueKey key) {
    for (int i = 0; i < keyArray.size(); i++) {
      if (keyArray.get(i).equals(key)) {
        keyArray.remove(i);
      }
    }
    return map.remove(key);
  }
  
  public synchronized UniqueKey rekey(UniqueKey oldKey, String newKeyName) {
    Object value = remove(oldKey);
    return add(newKeyName, value);
  }
}
