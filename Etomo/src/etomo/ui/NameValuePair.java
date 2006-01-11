package etomo.ui;

import java.util.ArrayList;
import java.util.Vector;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class NameValuePair {
  public static  final String  rcsid =  "$Id$";
  
  private final Vector names = new Vector();
  private final Token value; //may be null

  NameValuePair(Attribute attrib, Token value) {
    this.value = value;
    //set names
    ArrayList list = new ArrayList();
    while (attrib != null) {
      list.add(attrib.getNameToken());
      WriteOnlyAttributeMap parent = attrib.getParent();
      if (parent instanceof Attribute) {
        attrib = (Attribute) parent;
      }
      else {
        attrib = null;
      }
    }
    for (int i = list.size() - 1; i >= 0; i--) {
      names.add(list.get(i));
    }
  }
  
  final int levels() {
    return names.size();
  }
  
  final boolean equalsName(String name, int index) {
    if (name == null || index >= names.size()) {
      return false;
    }
    String key = ((Token) names.get(index)).getKey();
    return key.equals(Token.convertToKey(name));
  }
  
  final String getName(int index) {
    if (index >= names.size()) {
      return null;
    }
    return ((Token) names.get(index)).getValues();
  }
  
  final String getValue() {
    if (value == null) {
      return null;
    }
    return value.getFormattedValues(false);
  }
  
  public String getString() {
    StringBuffer buffer = new StringBuffer();
    if (names.size() >= 1) {
      buffer.append(((Token) names.get(0)).getValues());
    }
    for (int i = 1; i < names.size(); i++) {
      buffer.append(AutodocTokenizer.SEPARATOR_CHAR);
      buffer.append(((Token) names.get(i)).getValues());
    }
    buffer.append(" " + AutodocTokenizer.DEFAULT_DELIMITER + " ");
    if (value != null) {
      buffer.append(value.getFormattedValues(false));
    }
    return buffer.toString();
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "names=" + names + ",\nvalue=" + value + "," + super.toString();
  }
}
/**
* <p> $Log$ </p>
*/