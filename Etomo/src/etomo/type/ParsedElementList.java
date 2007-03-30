package etomo.type;

import java.util.HashMap;
import java.util.Map;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
final class ParsedElementList {
  public static final String rcsid = "$Id$";

  private Map map = new HashMap();
  private int size = 0;

  int size() {
    return size;
  }

  synchronized void add(ParsedElement element) {
    map.put(newKey(), element);
  }

  ParsedElement get(int index) {
    return (ParsedElement) map.get(getKey(index));
  }
  
  synchronized void set(int index,ParsedElement element) {
    map.put(getKey(index),element);
  }

  synchronized void clear() {
    map.clear();
    size = 0;
  }
  
  synchronized void addEmptyElement() {
    newKey();
  }

  private Integer newKey() {
    return getKey(size++);
  }

  private Integer getKey(int key) {
    return new Integer(key);
  }
}
