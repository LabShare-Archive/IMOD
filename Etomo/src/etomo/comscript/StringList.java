package etomo.comscript;

import java.util.ArrayList;

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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
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

  public StringList() {
    elements = new String[0];
  }
  
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
  
  public StringList(String[] stringArray) {
    parseString(stringArray);
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
  
  /**
   * Parse a space delimited string into the StringList
   */
  public void parseString(String[] newList) {
    //  If the string is only white space set the StringList to the null set
    if (newList == null || newList.length == 0) {
      elements = new String[0];
      return;
    }
    ArrayList elementArray = new ArrayList();
    for (int i = 0; i < newList.length; i++) {
      if (!newList[i].matches("\\s*")) {
        String[] stringArray = newList[i].split(" +");
        for (int stringIndex = 0; stringIndex < stringArray.length; stringIndex++) {
          elementArray.add(stringArray[stringIndex]);
        }
      }
    }
    if (elementArray.size() == 0) {
      elements = new String[0];
    }
    else if (elementArray.size() == 1) {
      elements = new String[1];
      elements[0] = (String) elementArray.get(0);
    }
    else {
      elements = (String[]) elementArray.toArray(new String[elementArray.size()]);
    }
  }
}
