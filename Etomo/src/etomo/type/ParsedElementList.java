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
 * <p> $Log$
 * <p> Revision 1.1  2007/03/30 23:45:55  sueh
 * <p> bug# 964 Expandable array that allows sparse population.
 * <p> </p>
 */
final class ParsedElementList {
  public static final String rcsid = "$Id$";

  private Map map = new HashMap();
  private int size = 0;
  private boolean emptyElements = false;

  int size() {
    return size;
  }

  synchronized void add(ParsedElement element) {
    map.put(newKey(), element);
  }

  ParsedElement get(int index) {
    ParsedElement element = (ParsedElement) map.get(getKey(index));
    if (element == null) {
      return new EmptyParsedElement();
    }
    return element;
  }

  boolean isEmptyElements() {
    return emptyElements;
  }

  synchronized void set(int index, ParsedElement element) {
    map.put(getKey(index), element);
  }

  synchronized void clear() {
    map.clear();
    size = 0;
    emptyElements = false;
  }

  synchronized void addEmptyElement() {
    add(new EmptyParsedElement());
    emptyElements = true;
  }

  private Integer newKey() {
    return getKey(size++);
  }

  private Integer getKey(int key) {
    return new Integer(key);
  }
}
