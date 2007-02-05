package etomo.type;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

/**
 * <p>Description: A storable keyed list whose keys must be integers.  IntKeyList
 * finds the first and last keys with its put() function.  It saves the first
 * and last values in Properties and therefore can save and retrieve the entire
 * list from Properties without knowing anything about it.  IntKeyList is
 * implemented with a hash, not an array.  This should allow it to be lightly
 * populated (key values don't have to be numerically contiguous).</p>
 * 
 * </p>Before the list is loaded from a dialog using the put() function, the
 * reset() function should be called to clear out the list and reset the start
 * and end keys.  The list can be changed completely between loading and
 * storing without keeping track of anything because the store() function
 * calls the remove() function before storing.</p>
 * 
 * <p>If the first or last keys are missing from Properties, they are assumed to
 * be 0.  The last key must not be greater then the first key.<p>
 * 
 * <p>Copyright: Copyright 2006, 2007</p>
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
public final class IntKeyList implements ConstIntKeyList {
  public static final String rcsid = "$Id$";
  private static final int DEFAULT_START_KEY = 0;
  private static final String FIRST_KEY = "First";
  private static final String LAST_KEY = "Last";
  private final HashMap list = new HashMap();
  private final EtomoNumber firstKey = new EtomoNumber(FIRST_KEY);
  private final EtomoNumber lastKey = new EtomoNumber(LAST_KEY);
  int startKey = DEFAULT_START_KEY;
  private String listKey = null;
  //If etomoNumberType is null, then the value is a string
  private EtomoNumber.Type numericType = null;
  private boolean debug = false;

  public String toString() {
    return "[firstKey=" + firstKey + ",lastKey=" + lastKey + ",list=" + list
        + "]";
  }

  /**
   * Creates an instance without an instance key.  Will be able to load, remove,
   * or store.
   */
  public IntKeyList() {
    firstKey.setDisplayValue(DEFAULT_START_KEY);
    lastKey.setDisplayValue(DEFAULT_START_KEY);
  }

  public IntKeyList(String listKey) {
    this();
    this.listKey = listKey;
  }

  public static IntKeyList getNumericInstance() {
    IntKeyList instance = new IntKeyList();
    instance.numericType = EtomoNumber.Type.getDefault();
    return instance;
  }

  public static IntKeyList getNumericInstance(String listKey) {
    IntKeyList instance = new IntKeyList(listKey);
    instance.numericType = EtomoNumber.Type.getDefault();
    return instance;
  }

  public synchronized void reset() {
    list.clear();
    firstKey.reset();
    lastKey.reset();
    startKey = DEFAULT_START_KEY;
  }

  /**
   * Reset with a non default startKey.  The startKey is only used when lastKey
   * is null and put(String) or put(ConstEtomoNumber) is used.  This way a list
   * can be built, without reference to keys, but still controlling the
   * firstKey.
   * @param startKey
   */
  public synchronized void reset(int startKey) {
    reset();
    this.startKey = startKey;
  }

  public boolean isEmpty() {
    return list.isEmpty();
  }

  public void set(ConstIntKeyList intKeyList) {
    if (intKeyList==null) {
      return;
    }
    for (int i = intKeyList.getFirstKey(); i <= intKeyList.getLastKey(); i++) {
      String value = intKeyList.get(i);
      if (value != null) {
        put(i, value);
      }
    }
  }

  public synchronized void put(int key, String value) {
    adjustFirstLastKeys(key);
    list.put(buildKey(key), new Pair(key, value, numericType));
  }

  public synchronized void put(int key, ConstEtomoNumber value) {
    adjustFirstLastKeys(key);
    list.put(buildKey(key), new Pair(key, value, numericType));
  }

  /**
   * puts the value, generates its own key (lastKey+1)
   * @param value
   */
  public synchronized void put(ConstEtomoNumber value) {
    int key;
    if (lastKey.isNull()) {
      key = startKey;
    }
    else {
      key = getLastKey() + 1;
    }
    adjustFirstLastKeys(key);
    list.put(buildKey(key), new Pair(key, value, numericType));
  }

  public int getFirstKey() {
    return firstKey.getInt();
  }

  public int getLastKey() {
    return lastKey.getInt();
  }

  public String get(int key) {
    Pair pair = (Pair) list.get(buildKey(key));
    if (pair == null) {
      return null;
    }
    return pair.getValue();
  }

  public ConstEtomoNumber getNumeric(int key) {
    if (debug) {
      System.err.println("getNumeric:numericType=" + numericType);
    }
    Pair pair = (Pair) list.get(buildKey(key));
    if (debug) {
      System.err.println("getNumeric:key=" + key + ",pair=" + pair);
    }
    if (pair == null) {
      return null;
    }
    if (numericType == null) {
      EtomoNumber number = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      number.set(pair.getValue());
      return number;
    }
    return pair.getNumericValue();
  }

  public void store(Properties props, String prepend) {
    if (listKey == null) {
      return;
    }
    remove(props, prepend);
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    if (firstKey.gt(lastKey)) {
      throw new IllegalStateException(
          "StartKey must be not be greater then endKey.\nstartKey=" + firstKey
              + ",endKey=" + lastKey);
    }
    firstKey.store(props, prepend);
    lastKey.store(props, prepend);
    if (list.isEmpty()) {
      return;
    }
    Iterator i = list.values().iterator();
    while (i.hasNext()) {
      Pair pair = (Pair) i.next();
      if (pair != null && !pair.isNull()) {
        props.setProperty(group + pair.getKey(), pair.getValue());
      }
    }
  }

  public void load(Properties props, String prepend) {
    if (listKey == null) {
      return;
    }
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    firstKey.load(props, prepend);
    lastKey.load(props, prepend);
    for (int i = getFirstKey(); i <= getLastKey(); i++) {
      String value = props.getProperty(group + String.valueOf(i));
      if (value != null) {
        list.put(String.valueOf(i), new Pair(i, value, numericType));
      }
    }
  }

  public Walker getWalker() {
    return new Walker(this);
  }

  public boolean containsKey(int key) {
    return list.containsKey(buildKey(key));
  }

  public int size() {
    return list.size();
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  private void remove(Properties props, String prepend) {
    if (listKey == null) {
      return;
    }
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    EtomoNumber oldFirstKey = new EtomoNumber(firstKey);
    oldFirstKey.load(props, prepend);
    EtomoNumber oldLastKey = new EtomoNumber(lastKey);
    oldFirstKey.load(props, prepend);
    for (int i = oldFirstKey.getInt(); i <= oldLastKey.getInt(); i++) {
      props.remove(group + String.valueOf(i));
    }
  }

  private final String getPrepend(String prepend) {
    if (prepend == "") {
      return listKey;
    }
    else {
      return prepend + "." + listKey;
    }
  }

  private void adjustFirstLastKeys(int key) {
    if (debug) {
      System.err.println("adjustFirstLastKeys:key=" + key + ",firstKey="
          + firstKey + ",lastKey=" + lastKey);
    }
    if (firstKey.isNull() || firstKey.gt(key)) {
      firstKey.set(key);
    }
    if (lastKey.isNull() || lastKey.lt(key)) {
      lastKey.set(key);
    }
    if (debug) {
      System.err.println("firstKey=" + firstKey + ",lastKey=" + lastKey);
    }
  }

  private String buildKey(int key) {
    if (debug) {
      System.err.println("buildKey:key=" + key + ",String.valueOf(key)="
          + String.valueOf(key));
    }
    return String.valueOf(key);
  }

  public static final class Walker {
    private boolean started = false;
    private boolean debug = false;
    private int key;
    private final ConstIntKeyList list;

    /**
     * Increments the key until it points to the next value.  Returns true if
     * there are values left.  
     * @return
     */
    public boolean hasNext() {
      start();
      if (debug) {
        System.err.println("hasNext:key=" + key);
      }
      boolean hasNext = false;
      while (!hasNext && key <= list.getLastKey()) {
        hasNext = list.containsKey(key);
        if (!hasNext) {
          key++;
        }
      }
      if (debug) {
        System.err.println("hasNext:key=" + key + ",hasNext=" + hasNext);
      }
      return hasNext;
    }

    /**
     * Increments the key until it points to the next value.  Gets the value
     * and increments the key.
     * @return
     */
    public ConstEtomoNumber nextNumeric() {
      start();
      if (debug) {
        System.err.println("nextNumeric:key=" + key);
      }
      ConstEtomoNumber value = null;
      while (value == null && key <= list.getLastKey()) {
        value = list.getNumeric(key++);
        if (debug) {
          System.err.println("nextNumeric:key=" + key + ",value=" + value);
        }
      }
      if (debug) {
        System.err.println("nextNumeric:key=" + key + ",value=" + value);
      }
      return value;
    }

    public ConstEtomoNumber getLastNumeric() {
      return list.getNumeric(list.getLastKey());
    }

    public int size() {
      return list.size();
    }

    public int getFirstKey() {
      return list.getFirstKey();
    }

    public String toString() {
      if (started) {
        return "[key=" + key + ",list=" + list + "]";
      }
      return "[list=" + list + "]";
    }

    public void setDebug(boolean debug) {
      this.debug = debug;
    }

    Walker(ConstIntKeyList list) {
      this.list = list;
    }

    private void start() {
      if (!started) {
        key = list.getFirstKey();
        started = true;
        if (debug) {
          System.err.println("start:key=" + key);
        }
      }
    }
  }

  private static final class Pair {
    private final int key;
    private final String stringValue;
    private final EtomoNumber numericValue;
    private final EtomoNumber.Type numericType;

    public String toString() {
      if (numericType == null) {
        return "[key=" + key + ",value=" + stringValue + "]";
      }
      return "[key=" + key + ",value=" + numericValue + "]";
    }

    int getKey() {
      return key;
    }

    String getValue() {
      if (numericType == null) {
        return stringValue;
      }
      return numericValue.toString();
    }

    ConstEtomoNumber getNumericValue() {
      if (numericType == null) {
        return null;
      }
      return numericValue;
    }

    boolean isNull() {
      if (numericType == null) {
        return stringValue == null || stringValue.matches("\\s*");
      }
      return numericValue.isNull();
    }

    private Pair(int key, String value, EtomoNumber.Type etomoNumberType) {
      this.key = key;
      this.numericType = etomoNumberType;
      if (etomoNumberType == null) {
        stringValue = value;
        numericValue = null;
      }
      else {
        numericValue = new EtomoNumber(etomoNumberType).set(value);
        stringValue = null;
      }
    }

    private Pair(int key, ConstEtomoNumber value,
        EtomoNumber.Type etomoNumberType) {
      this.key = key;
      this.numericType = etomoNumberType;
      if (etomoNumberType == null) {
        if (value == null) {
          stringValue = "";
        }
        else {
          stringValue = value.toString();
        }
        numericValue = null;
      }
      else {
        numericValue = new EtomoNumber(etomoNumberType);
        numericValue.set(value);
        stringValue = null;
      }
    }
  }
}
