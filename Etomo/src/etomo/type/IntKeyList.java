package etomo.type;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * <p>Description: A storable keyed list whose keys must be integers.  IntKeyList
 * finds the first and last keys with its put() function.  It saves the first
 * and last values in Properties and therefore can save and retrieve the entire
 * list from Properties without knowing anything about it.  IntKeyList is
 * implemented with a hash, not an array.  This should allow it to be lightly
 * populated (key values don't have to be numerically contiguous).</p>
 * 
 * </p>Before the list is loaded from a dialog using the set() function, the
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
 * <p> $Log$
 * <p> Revision 1.10  2009/12/01 00:23:54  sueh
 * <p> bug# 1285 Added Walker.isEmpty.
 * <p>
 * <p> Revision 1.9  2009/10/30 20:53:58  sueh
 * <p> bug# 1285 Added debugging.
 * <p>
 * <p> Revision 1.8  2009/02/05 23:41:43  sueh
 * <p> bug# 1148 Changed put(ConstEtomoNumber) to add since it acts like the
 * <p> list function of the same name.  Added more functions for string
 * <p> instance.  Added containsValue().
 * <p>
 * <p> Revision 1.7  2008/05/22 00:15:57  sueh
 * <p> bug# 1110 Corrected a comment in class description.
 * <p>
 * <p> Revision 1.6  2007/06/06 20:42:37  sueh
 * <p> bug# 1016 Added getPrepend(String, String tempKey), load(Properties,
 * <p> String, String tempKey), and remove(Properties, String, String tempKey) to
 * <p> allow an IntKeyList property to be tranferred to another key.
 * <p>
 * <p> Revision 1.5  2007/05/16 01:46:16  sueh
 * <p> bug# 964 Added RowKey.toString().
 * <p>
 * <p> Revision 1.4  2007/05/11 16:00:05  sueh
 * <p> bug# 964 Added set(String[]), which saves the values of the parameter
 * <p> using the indexes as keys.
 * <p>
 * <p> Revision 1.3  2007/04/26 02:46:35  sueh
 * <p> bug# 964 Reformatted
 * <p>
 * <p> Revision 1.2  2007/03/01 01:25:01  sueh
 * <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
 * <p> in IntKeyList.
 * <p>
 * <p> Revision 1.1  2007/02/05 23:27:39  sueh
 * <p> bug# 962 Storable, hashable array which may be lightly populated.
 * <p> </p>
 */
public final class IntKeyList implements ConstIntKeyList {
  public static final String rcsid = "$Id$";

  private final Map map = new HashMap();
  private final RowKey rowKey = new RowKey();
  //If etomoNumberType is null, then the value is a string
  private final EtomoNumber.Type etomoNumberType;
  private String listKey = null;
  private boolean debug = false;

  public String toString() {
    return "[map=" + map + "]";
  }

  private IntKeyList(String listKey, EtomoNumber.Type etomoNumberType) {
    this.listKey = listKey;
    this.etomoNumberType = etomoNumberType;
  }

  public static IntKeyList getStringInstance() {
    return new IntKeyList(null, null);
  }

  public static IntKeyList getStringInstance(String listKey) {
    return new IntKeyList(listKey, null);
  }

  public static IntKeyList getNumberInstance() {
    return new IntKeyList(null, EtomoNumber.Type.getDefault());
  }

  public static IntKeyList getNumberInstance(String listKey) {
    return new IntKeyList(listKey, EtomoNumber.Type.getDefault());
  }

  public static IntKeyList getNumberInstance(String listKey,
      EtomoNumber.Type etomoNumberType) {
    if (etomoNumberType == null) {
      etomoNumberType = EtomoNumber.Type.getDefault();
    }
    return new IntKeyList(listKey, etomoNumberType);
  }

  public synchronized void reset() {
    map.clear();
    rowKey.reset();
  }

  /**
   * Clear with a non default startKey.  The startKey is only used when lastKey
   * is null and put(String) or put(ConstEtomoNumber) is used.  This way a list
   * can be built, without reference to keys, but still controlling the
   * firstKey.
   * @param startKey
   */
  public synchronized void reset(final int startKey) {
    reset();
    rowKey.reset(startKey);
  }

  public boolean isEmpty() {
    return map.isEmpty();
  }

  public void set(final String[] input) {
    if (input == null) {
      return;
    }
    for (int i = 0; i < input.length; i++) {
      if (input[i] != null) {
        put(i, input[i]);
      }
    }
  }

  public void set(final List input) {
    if (input == null) {
      return;
    }
    for (int i = 0; i < input.size(); i++) {
      Object object = input.get(i);
      if (object != null) {
        put(i, object);
      }
    }
  }

  /**
   * Added intKeyList to the instance.  This function does not replace the
   * existing list, though it may replace individual elements.  If you want to
   * replace the existing list, call reset before calling this function.
   * @param intKeyList
   */
  public void set(final ConstIntKeyList intKeyList) {
    if (intKeyList == null) {
      return;
    }
    for (int i = intKeyList.getFirstKey(); i <= intKeyList.getLastKey(); i++) {
      String value = intKeyList.getString(i);
      if (value != null) {
        put(i, value);
      }
    }
  }

  public synchronized void put(final int key, final String value) {
    rowKey.adjustFirstLastKeys(key);
    map.put(buildKey(key), new Pair(key, value, etomoNumberType));
    if (debug) {
      System.out.println("map.size()=" + map.size());
    }
  }

  public synchronized void put(final int key, final Object value) {
    rowKey.adjustFirstLastKeys(key);
    Pair pair = null;
    if (etomoNumberType == null) {
      pair = new Pair(key, (String) value, etomoNumberType);
    }
    else {
      pair = new Pair(key, (ConstEtomoNumber) value, etomoNumberType);
    }
    map.put(buildKey(key), pair);
    if (debug) {
      System.out.println("map.size()=" + map.size());
    }
  }

  public synchronized void put(int key, ConstEtomoNumber value) {
    rowKey.adjustFirstLastKeys(key);
    map.put(buildKey(key), new Pair(key, value, etomoNumberType));
    if (debug) {
      System.out.println("map.size()=" + map.size());
    }
  }

  public synchronized void put(int key, int value) {
    rowKey.adjustFirstLastKeys(key);
    map.put(buildKey(key), new Pair(key, value, etomoNumberType));
    if (debug) {
      System.out.println("map.size()=" + map.size());
    }
  }

  /**
   * puts the value, generates its own key (lastKey+1)
   * @param value
   */
  public synchronized void add(ConstEtomoNumber value) {
    int key = rowKey.genKey();
    map.put(buildKey(key), new Pair(key, value, etomoNumberType));
    if (debug) {
      System.out.println("map.size()=" + map.size());
    }
  }

  /**
   * puts the value, generates its own key (lastKey+1)
   * @param value
   */
  public synchronized void add(String value) {
    int key = rowKey.genKey();
    map.put(buildKey(key), new Pair(key, value, etomoNumberType));
    if (debug) {
      System.out.println("map.size()=" + map.size());
    }
  }

  public int getFirstKey() {
    return rowKey.getFirstKey();
  }

  public int getLastKey() {
    return rowKey.getLastKey();
  }

  public String getString(int key) {
    Pair pair = (Pair) map.get(buildKey(key));
    if (pair == null) {
      return null;
    }
    return pair.getValueString();
  }

  public ConstEtomoNumber getEtomoNumber(int key) {
    Pair pair = (Pair) map.get(buildKey(key));
    if (pair == null) {
      return null;
    }
    EtomoNumber etomoNumber;
    if (etomoNumberType == null) {
      etomoNumber = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      etomoNumber.set(pair.getValueString());
      return etomoNumber;
    }
    etomoNumber = new EtomoNumber(etomoNumberType).set(pair.getValueNumber());
    return etomoNumber;
  }

  public void store(Properties props, String prepend) {
    if (listKey == null) {
      return;
    }
    //Remove everything from this map from properties.  This means that
    //renumbering is unnecessary.
    remove(props, prepend);
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    rowKey.store(props, prepend);
    if (map.isEmpty()) {
      return;
    }
    Iterator i = map.values().iterator();
    while (i.hasNext()) {
      Pair pair = (Pair) i.next();
      if (pair != null && !pair.isNull()) {
        props.setProperty(group + pair.getKey(), pair.getValueString());
      }
    }
  }

  public void load(Properties props, String prepend) {
    if (debug) {
      System.out.println("load:prepend=" + prepend);
    }
    if (listKey == null) {
      return;
    }
    prepend = getPrepend(prepend);
    if (debug) {
      System.out.println("prepend=" + prepend);
    }
    String group = prepend + ".";
    rowKey.load(props, prepend);
    for (int i = getFirstKey(); i <= getLastKey(); i++) {
      String value = props.getProperty(group + String.valueOf(i));
      if (value != null) {
        map.put(String.valueOf(i), new Pair(i, value, etomoNumberType));
        if (debug) {
          System.out.println("map.size()=" + map.size());
        }
      }
    }
  }

  public void load(Properties props, String prepend, String tempKey) {
    if (listKey == null) {
      return;
    }
    prepend = getPrepend(prepend, tempKey);
    String group = prepend + ".";
    rowKey.load(props, prepend);
    for (int i = getFirstKey(); i <= getLastKey(); i++) {
      String value = props.getProperty(group + String.valueOf(i));
      if (value != null) {
        map.put(String.valueOf(i), new Pair(i, value, etomoNumberType));
        if (debug) {
          System.out.println("map.size()=" + map.size());
        }
      }
    }
  }

  public void remove(Properties props, String prepend, String tempKey) {
    if (tempKey == null) {
      return;
    }
    prepend = getPrepend(prepend, tempKey);
    String group = prepend + ".";
    RowKey oldRowKey = new RowKey(rowKey);
    oldRowKey.load(props, prepend);
    for (int i = oldRowKey.getFirstKey(); i <= oldRowKey.getLastKey(); i++) {
      props.remove(group + String.valueOf(i));
    }
    oldRowKey.remove(props, prepend);
  }

  /**
   * Iterator that doesn't allow changes to the IntKeyList
   */
  public Walker getWalker() {
    return new Walker(this);
  }

  public boolean containsKey(int key) {
    return map.containsKey(buildKey(key));
  }

  public boolean containsValue(String element) {
    return map.containsValue(element);
  }

  public int size() {
    return map.size();
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
    rowKey.setDebug(debug);
  }

  /**
   * Remove the data associated with the keys of this instance, without changing
   * the value of this instance.
   * @param props
   * @param prepend
   */
  private void remove(Properties props, String prepend) {
    if (listKey == null) {
      return;
    }
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    RowKey oldRowKey = new RowKey(rowKey);
    oldRowKey.load(props, prepend);
    for (int i = oldRowKey.getFirstKey(); i <= oldRowKey.getLastKey(); i++) {
      props.remove(group + String.valueOf(i));
    }
    oldRowKey.remove(props, prepend);
  }

  private final String getPrepend(String prepend) {
    if (prepend == "") {
      return listKey;
    }
    else {
      return prepend + "." + listKey;
    }
  }

  private final String getPrepend(String prepend, String tempKey) {
    if (prepend == "") {
      return tempKey;
    }
    else {
      return prepend + "." + tempKey;
    }
  }

  static String buildKey(int key) {
    return String.valueOf(key);
  }

  public static final class Walker {
    private boolean started = false;
    private boolean debug = false;
    private int key;
    private final ConstIntKeyList list;

    public boolean isEmpty() {
      return list.isEmpty();
    }

    /**
     * Increments the key until it points to the next value.  Returns true if
     * there are values left.  
     * @return
     */
    public boolean hasNext() {
      start();
      boolean hasNext = false;
      while (!hasNext && key <= list.getLastKey()) {
        hasNext = list.containsKey(key);
        if (!hasNext) {
          key++;
        }
      }
      return hasNext;
    }

    /**
     * Increments the key until it points to the next value.  Gets the value
     * and increments the key.
     * @return
     */
    public ConstEtomoNumber nextEtomoNumber() {
      start();
      ConstEtomoNumber value = null;
      while (value == null && key <= list.getLastKey()) {
        value = list.getEtomoNumber(key++);
      }
      return value;
    }

    /**
     * returns the next value as a string
     * @return
     */
    public String nextString() {
      start();
      String value = null;
      while (value == null && key <= list.getLastKey()) {
        value = list.getString(key++);
      }
      return value;
    }

    public ConstEtomoNumber getLastEtomoNumber() {
      return list.getEtomoNumber(list.getLastKey());
    }

    public int size() {
      if (debug) {
        System.out.println("list.size()=" + list.size());
      }
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

    /**
     * Start walking through the list
     */
    private void start() {
      if (!started) {
        key = list.getFirstKey();
        started = true;
      }
    }
  }

  private static final class Pair {
    private final int key;
    private final String stringValue;
    private final Number number;
    private final EtomoNumber.Type numericType;

    private Pair(int key, String value, EtomoNumber.Type etomoNumberType) {
      this.key = key;
      this.numericType = etomoNumberType;
      if (etomoNumberType == null) {
        stringValue = value;
        number = null;
      }
      else {
        number = new EtomoNumber(etomoNumberType).set(value).getNumber();
        stringValue = null;
      }
    }

    private Pair(int key, int value, EtomoNumber.Type etomoNumberType) {
      this(key, new Integer(value), etomoNumberType);
    }

    private Pair(int key, ConstEtomoNumber value, EtomoNumber.Type etomoNumberType) {
      this(key, value.getNumber(), etomoNumberType);
    }

    private Pair(int key, Number value, EtomoNumber.Type etomoNumberType) {
      this.key = key;
      this.numericType = etomoNumberType;
      if (etomoNumberType == null) {
        if (value == null) {
          stringValue = "";
        }
        else {
          stringValue = value.toString();
        }
        number = null;
      }
      else {
        number = value;
        stringValue = null;
      }
    }

    public String toString() {
      if (numericType == null) {
        return "[key=" + key + ",value=" + stringValue + "]";
      }
      return "[key=" + key + ",value=" + number + "]";
    }

    int getKey() {
      return key;
    }

    String getValueString() {
      if (numericType == null) {
        return stringValue;
      }
      return number.toString();
    }

    Number getValueNumber() {
      if (numericType == null) {
        return null;
      }
      return number;
    }

    boolean isNull() {
      if (numericType == null) {
        return stringValue == null || stringValue.matches("\\s*");
      }
      return new EtomoNumber(numericType).set(number).isNull();
    }
  }

  private static final class RowKey {
    private static final String FIRST_KEY = "First";
    private static final String LAST_KEY = "Last";
    private static final int DEFAULT_START_KEY = 0;

    private final EtomoNumber firstKey = new EtomoNumber(FIRST_KEY);
    private final EtomoNumber lastKey = new EtomoNumber(LAST_KEY);

    private boolean debug = false;

    private int startKey = DEFAULT_START_KEY;

    private RowKey() {
      firstKey.setDisplayValue(DEFAULT_START_KEY);
      lastKey.setDisplayValue(DEFAULT_START_KEY);
    }

    private RowKey(RowKey rowKey) {
      firstKey.set(rowKey.firstKey);
      lastKey.set(rowKey.lastKey);
      startKey = rowKey.startKey;
    }

    public String toString() {
      return "[firstKey=" + firstKey + ",lastKey=" + lastKey;
    }

    void reset() {
      firstKey.reset();
      lastKey.reset();
      startKey = DEFAULT_START_KEY;
    }

    public void setDebug(boolean debug) {
      this.debug = debug;
    }

    void reset(int startKey) {
      this.startKey = startKey;
    }

    int genKey() {
      int key;
      if (lastKey.isNull()) {
        key = startKey;
      }
      else {
        key = getLastKey() + 1;
      }
      adjustFirstLastKeys(key);
      return key;
    }

    int getLastKey() {
      return lastKey.getInt();
    }

    void adjustFirstLastKeys(int key) {
      if (firstKey.isNull() || firstKey.gt(key)) {
        firstKey.set(key);
      }
      if (lastKey.isNull() || lastKey.lt(key)) {
        lastKey.set(key);
      }
    }

    int getFirstKey() {
      return firstKey.getInt();
    }

    void store(Properties props, String prepend) {
      if (firstKey.gt(lastKey)) {
        throw new IllegalStateException(
            "StartKey must be not be greater then endKey.\nstartKey=" + firstKey
                + ",endKey=" + lastKey);
      }
      firstKey.store(props, prepend);
      lastKey.store(props, prepend);
    }

    void load(Properties props, String prepend) {
      firstKey.load(props, prepend);
      lastKey.load(props, prepend);
      if (debug) {
        System.out.println("firstKey=" + firstKey + ",lastKey=" + lastKey);
      }
    }

    void remove(Properties props, String prepend) {
      firstKey.remove(props, prepend);
      lastKey.remove(props, prepend);
    }

  }
}
