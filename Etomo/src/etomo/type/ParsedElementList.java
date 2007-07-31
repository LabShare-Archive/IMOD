package etomo.type;

import java.util.HashMap;
import java.util.List;
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
 * <p> Revision 1.8  2007/05/11 16:04:00  sueh
 * <p> bug# 964 Added EmptyParsedElement.getArray(List), which does nothing.
 * <p>
 * <p> Revision 1.7  2007/04/26 02:47:25  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.6  2007/04/19 21:46:10  sueh
 * <p> bug# 964 Made EmptyParsedElement a singleton.  Added move and remove.
 * <p>
 * <p> Revision 1.5  2007/04/13 21:51:25  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:16:30  sueh
 * <p> bug# 964 Made getRawString public.
 * <p>
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
      return EmptyParsedElement.INSTANCE;
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
      size = index + 1;
    }
  }

  synchronized void clear() {
    map.clear();
    size = 0;
  }

  synchronized void addEmptyElement() {
    size++;
  }

  synchronized void move(int fromIndex, int toIndex) {
    ParsedElement element = remove(fromIndex);
    set(toIndex, element);
  }

  public ParsedElement remove(int index) {
    return (ParsedElement) map.remove(getKey(index));
  }

  private Integer getKey(int key) {
    return new Integer(key);
  }

  /**
   * Class that allows ParsedElementList to return null when getting an element.
   * EmptyParsedElement isn't available outside of ParsedElementList, but it
   * extends ParsedElement and behaves like an empty ParsedElement that is not a
   * collection.
   * @singleton
   * @immutable
   */
  static final class EmptyParsedElement extends ParsedElement {
    public static final String rcsid = "$Id$";

    static final EmptyParsedElement INSTANCE = new EmptyParsedElement();
    
    private boolean debug = false;

    private EmptyParsedElement() {
    }

    public String toString() {
      return "[empty]";
    }

    public String getRawString() {
      return "";
    }
    
    public String getRawString(int index) {
      return "";
    }
    
    boolean ge(int number) {
      return false;
    }

    public void setRawString(String input) {
    }
    
    public  void setRawString(int index,String string) {
    }

    public Number getRawNumber() {
      return new EtomoNumber().getNumber();
    }

    public void setDebug(boolean debug) {
      this.debug = debug;
    }

    public void moveElement(int fromIndex, int toIndex) {
    }

    public ParsedElement getElement(int index) {
      return this;
    }

    public void setRawString(int index, float number) {
    }
    
    List getArray(List parsedNumberArray) {
      return parsedNumberArray;
    }
    
    void setDefaultValue(int numberIndex,Integer[] defaultValueArray) {
    }

    Token parse(Token token, PrimativeTokenizer tokenizer) {
      return null;
    }
    
    void removeElement(int index) {
    }

    int size() {
      return 0;
    }

    public boolean isEmpty() {
      return true;
    }
    
    boolean isDefaultedEmpty() {
      return true;
    }

    String getParsableString() {
      return "";
    }
    
    boolean hasParsedNumberSyntax() {
      return true;
    }

    boolean isCollection() {
      return false;
    }

    void fail() {
    }
  }
}
