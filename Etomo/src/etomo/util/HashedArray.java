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
  public static  final String  rcsid =  "$Id$";
  
  Hashtable map = null;
  Vector keyArray = null;
  
  public HashedArray() {
    map = new Hashtable();
    keyArray = new Vector();
  }
  
  /**
   * Add a new value with unique key is creates from keyName
   * @param keyName
   * @param value
   * @return
   */
  public synchronized void add(String key, Object value) {
    keyArray.add(key);
    map.put(key, value);
  }
  
  public Object get(Object key) {
    if (key == null) {
      return null;
    }
    return map.get(key);
  }
  
  public Object get(int index) {
    if (index < 0) {
      return null;
    }
    if (index >= keyArray.size()) {
      return null;
    }
    Object key = keyArray.get(index);
    if (key == null) {
      return null;
    }
    return map.get(key);
  }
  
  public int size() {
    return keyArray.size();
  }
  
  public boolean contains(Object value) {
    return map.contains(value);
  }
}
