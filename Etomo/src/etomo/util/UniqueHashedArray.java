package etomo.util;

import java.util.Hashtable;
import java.util.Vector;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class UniqueHashedArray {
  public static final String rcsid = "$Id$";

  Hashtable map = null;
  Vector keyArray = null;

  public UniqueHashedArray() {
    map = new Hashtable();
    keyArray = new Vector();
  }

  protected UniqueHashedArray(Vector keyArray) {
    map = new Hashtable();
    this.keyArray = new Vector(keyArray);
  }

  /**
   * Add a new value with unique key is creates from keyName
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
   * Adds a new value and key.
   * @param key
   * @param value
   * @throws IllegalStateException if key is not unique in the UniqueHashedArray
   * instance.
   * @return
   */
  public synchronized UniqueKey add(UniqueKey key, Object value) {
    if (get(key) != null) {
      throw new IllegalStateException("Key, " + key + ", is not unique.");
    }
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
    return rekey(oldKey, new UniqueKey(newKeyName, this));
  }

  public synchronized UniqueKey rekey(UniqueKey oldKey, UniqueKey newKey) {
    int index = getIndex(oldKey);
    Object value = map.remove(oldKey);
    map.put(newKey, value);
    keyArray.set(index, newKey);
    return newKey;
  }

  public Object get(UniqueKey key) {
    if (key == null) {
      return null;
    }
    Object object = map.get(key);
    return object;
  }

  public Object get(int index) {
    if (index < 0) {
      return null;
    }
    if (index >= keyArray.size()) {
      return null;
    }
    UniqueKey key = (UniqueKey) keyArray.get(index);
    if (key == null) {
      return null;
    }
    return map.get(key);
  }

  public UniqueKey getKey(int index) {
    if (index < 0) {
      return null;
    }
    return (UniqueKey) keyArray.get(index);
  }

  public int getIndex(UniqueKey key) {
    if (key == null) {
      return -1;
    }
    for (int i = 0; i < keyArray.size(); i++) {
      if (key.equals(keyArray.get(i))) {
        return i;
      }
    }
    return -1;
  }

  public int size() {
    return keyArray.size();
  }

  //FIXME are the elements in the new array copies?  Should they be?  
  //If they aren't
  //copies, should this function be in UniqueHashedArray?
  public UniqueHashedArray getEmptyUniqueHashedArray() {
    return new UniqueHashedArray(keyArray);
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    StringBuffer buffer = new StringBuffer(",map=");
    for (int i = 0; i < keyArray.size(); i++) {
      UniqueKey key = (UniqueKey) keyArray.get(i);
      buffer.append("\nkey=" + key);
      if (key != null) {
        buffer.append(",value=" + map.get(key));
      }
    }
    return buffer.toString();
  }

}
/**
 * <p> $Log$
 * <p> Revision 1.1  2005/08/22 18:22:51  sueh
 * <p> bug# 532 Moved HashedArray to UniqueHashedArray.  Added a simpler
 * <p> HashedArray class which does not use UniqueKey.
 * <p> </p>
 */
