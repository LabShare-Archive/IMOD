package etomo.comscript;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 * <p>Title: </p>
 */

public class StringList {
  public static final String rcsid =
    "$Id$";
  String[] elements;

  public StringList(int nElements) {
    elements = new String[nElements];
  }

  /**
   * Copy constructor
   */
  public StringList(StringList src) {
    elements = new String[src.getNElements()];
    for (int i = 0; i < elements.length; i++) {
      elements[i] = src.get(i);
    }
  }

  public void setNElements(int nElements) {
    //  Allocate a new string array
    elements = new String[nElements];
  }

  public void set(int index, String value) {
    elements[index] = value;
  }

  public String get(int index) {
    return elements[index];
  }

  public int getNElements() {
    return elements.length;
  }

  public String toString() {
    if (elements == null || elements.length == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < elements.length; i++) {
      buffer.append(elements[i]);
      buffer.append(" ");
    }
    return buffer.toString();
  }

  /**
   * Parse a space delimited string into the StringList
   */
  public void parseString(String newList) {
    //  If the string is only white space set the StringList to the null set
    if (newList.matches("\\s*")) {
      elements = new String[0];
      return;
    }
    elements = newList.split(" +");
  }
}
