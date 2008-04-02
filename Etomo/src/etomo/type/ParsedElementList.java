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
 * <p> Revision 1.10  2007/11/06 19:48:33  sueh
 * <p> bug# 1047 added validate.
 * <p>
 * <p> Revision 1.9  2007/07/31 20:40:26  sueh
 * <p> bug# 1028 added ge(int).
 * <p>
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

  private final ParsedElementType type;

  private Map map = new HashMap();
  private int size = 0;

  ParsedElementList(ParsedElementType type) {
    this.type = type;
  }

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
      return ParsedEmptyElement.getInstance(type);
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

  public ParsedElement remove(int index) {
    return (ParsedElement) map.remove(getKey(index));
  }

  private Integer getKey(int key) {
    return new Integer(key);
  }
}
