package etomo.type;

import java.util.HashMap;
import java.util.Map;

import etomo.ui.Token;
import etomo.util.PrimativeTokenizer;

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
 * <p> Revision 1.3  2007/04/09 21:09:03  sueh
 * <p> bug# 964 Fixed a bug where the size was incorrect.  Made EmptyParsedElement
 * <p> a private inner class, so that null would not have to be returned.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:55:30  sueh
 * <p> bug# 964 Added boolean emptyElements to remember that empty elements where
 * <p> added.  Adding EmptyParsedElement instead of nothing when adding empty elements.
 * <p>
 * <p> Revision 1.1  2007/03/30 23:45:55  sueh
 * <p> bug# 964 Expandable array that allows sparse population.
 * <p> </p>
 */
final class ParsedElementList {
  public static final String rcsid = "$Id$";

  private Map map = new HashMap();
  private int size = 0;

  public String toString() {
    return "[map:" + map + "]";
  }

  int size() {
    return size;
  }

  /**
   * Add an element.  The key is the current size.  Size is incremented by one.
   * @param element
   */
  synchronized void add(ParsedElement element) {
    map.put(getKey(size++), element);
  }

  ParsedElement get(int index) {
    ParsedElement element = (ParsedElement) map.get(getKey(index));
    if (element == null) {
      return new EmptyParsedElement();
    }
    return element;
  }

  /**
   * Add or change an element.  Puts the element in the map using index as the
   * key.  If the location is larger then the current size, the size will be
   * increased.  If not every index up to size has an element associated with it,
   * then the empty locations contain empty elements.
   * @param index - location of the element
   * @param element
   */
  synchronized void set(int index, ParsedElement element) {
    map.put(getKey(index), element);
    //if index is equal to size, this is the same a calling add()
    if (index == size) {
      size++;
    }
    //if index is ahead of size
    else if (index > size) {
      size += index;
    }
  }

  synchronized void clear() {
    map.clear();
    size = 0;
  }

  synchronized void addEmptyElement() {
    size++;
  }

  private Integer getKey(int key) {
    return new Integer(key);
  }

  /**
   * Class that allows ParsedElementList to return null when getting an element.
   * EmptyParsedElement isn't available outside of ParsedElementList, but it
   * extends ParsedElement and behaves like an empty ParsedElement that is not a
   * collection.
   */
  private static final class EmptyParsedElement extends ParsedElement {
    public static final String rcsid = "$Id$";

    boolean valid = true;
    
    public String toString() {
      return "[empty]";
    }

    public String getRawString() {
      return "";
    }
    
    public void setRawString(String input) {
    }

    ConstEtomoNumber getRawNumber() {
      return new EtomoNumber();
    }

    Token parse(Token token, PrimativeTokenizer tokenizer) {
      valid=true;
      return null;
    }

    ParsedElement getElement(int index) {
      return this;
    }

    int size() {
      return 0;
    }

    boolean isEmpty() {
      return true;
    }

    String getParsableString() {
      return "";
    }

    boolean isCollection() {
      return false;
    }
    
    void fail() {
      valid = false;
    }
  }
}
