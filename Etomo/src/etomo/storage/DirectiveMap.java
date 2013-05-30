package etomo.storage;

import java.util.Map;
import java.util.NoSuchElementException;
import java.util.TreeMap;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class DirectiveMap {
  public static final String rcsid = "$Id:$";

  private final Map<String, Directive> map = new TreeMap<String, Directive>();

  public String toString() {
    return map.toString();
  }

  public DirectiveMap() {
  }

  public void put(final String key, final Directive value) {
    map.put(key, value);
  }

  public void clear() {
    map.clear();
  }

  public Directive get(final String key) {
    if (key == null) {
      return null;
    }
    return map.get(key);
  }

  public KeySet keySet(final DirectiveType type) {
    return new KeySet(map.keySet(), type);
  }

  public static final class KeySet {
    private final java.util.Set keySet;
    private final DirectiveType type;

    private KeySet(final java.util.Set<String> keySet, final DirectiveType type) {
      this.keySet = keySet;
      this.type = type;
    }

    public Iterator iterator() {
      return new Iterator(keySet, type);
    }
  }

  public static final class Iterator {
    private final java.util.Iterator<String> iterator;
    private final DirectiveType type;

    private String saveKey = null;

    private Iterator(final java.util.Set<String> keySet, final DirectiveType type) {
      this.iterator = keySet.iterator();
      this.type = type;
    }

    /**
     * Can be run multiple times without incrementing the iterator.
     * @return true if there is at least one key left that matches type.
     */
    public boolean hasNext() {
      if (type == null) {
        return iterator.hasNext();
      }
      // Use next() to check if any keys match type. If saveKey is already set, then
      // hasNext() was already run successfully.
      if (saveKey != null) {
        return true;
      }
      try {
        saveKey = next();
        return saveKey != null;
      }
      catch (NoSuchElementException e) {
        return false;
      }
    }

    /**
     * Increments the iterator.
     * @return the next element in the iteration that matches type.
     * 
     * @exception NoSuchElementException iteration has no more elements.
     */
    public String next() {
      if (type == null) {
        return iterator.next();
      }
      // If saveKey is set, then hasNext() was run, so use the key saved by hasNext.
      if (saveKey != null) {
        String temp = saveKey;
        // Set saveKey to null so that iterator will be incremented next time next() is
        // called.
        saveKey = null;
        return temp;
      }
      // Call next() until a key is returned that matches type.
      String key = null;
      while ((key = iterator.next()) != null) {
        if (DirectiveName.equals(key, type)) {
          return key;
        }
      }
      return null;
    }
  }
}
