package etomo.type;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.ui.Token;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: A description of an array.  A correct array descriptor
 * consists of the start, increment, and end values or the start and end values
 * (increment is assumed to be 1).  The increment may be negative which means
 * that the start value should be >= to the end value.  If the increment is
 * positive, the start value should be <= to the end value.  If the increment is
 * zero, the array described is [start, end].  If the descriptor contains only
 * one value, the array described is [value].  Values after the first three
 * values are ignored.
 * 
 * The descriptor member variable should only contain ParsedNumbers.  The string
 * representation of an array descriptor uses ":" to divide the values and does
 * not use open and close symbols such as square brackets.
 * 
 * The array descriptor is only used as an element of a ParsedArray.</p>
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
 * <p> Revision 1.11  2007/07/31 16:30:00  sueh
 * <p> bug# 1033 In getArray(List) include the last number in the arrray, if it is one of the
 * <p> number specified by the start and increment numbers.
 * <p>
 * <p> Revision 1.10  2007/07/24 04:04:33  sueh
 * <p> bug# 1030 Fixed getArray(List); it was handling the last number incorrectly.
 * <p>
 * <p> Revision 1.9  2007/05/11 16:02:06  sueh
 * <p> bug# 964 Added getArray(List), which converts an array descriptor into
 * <p> an array.
 * <p>
 * <p> Revision 1.8  2007/05/03 21:08:59  sueh
 * <p> bug# 964 Fixed bug in hasParsedNumberSyntax().
 * <p>
 * <p> Revision 1.7  2007/04/26 02:47:06  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.6  2007/04/19 21:42:42  sueh
 * <p> bug# 964 Added boolean compact.  A compact instance ignores all the empty
 * <p> numbers when writing to the .prm file.
 * <p>
 * <p> Revision 1.5  2007/04/13 21:51:03  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:14:25  sueh
 * <p> bug# 964 Added setRawString(String), which parses a list of numbers separated
 * <p> by :'s.
 * <p>
 * <p> Revision 1.3  2007/04/09 21:00:29  sueh
 * <p> bug# 964 Added parsing.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:54:24  sueh
 * <p> bug# 964 Added isCollection().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:41:13  sueh
 * <p> bug# 964 Class to parse Matlab array descriptors.
 * <p> </p>
 */

public final class ParsedArrayDescriptor extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final Character DIVIDER_SYMBOL = new Character(':');

  private final ParsedElementList descriptor = new ParsedElementList();

  private final EtomoNumber.Type etomoNumberType;

  /**
   * Use defaultValueArray to set each element's defaultValue when the defaultValueArray
   * is available and contains an entry under the same index as the element.
   */
  private Integer[] defaultValueArray = null;

  /**
   * When compact is true, only place non-empty elements in the parsable string.
   */
  private final boolean compact;

  private boolean debug = false;
  private boolean valid = true;

  ParsedArrayDescriptor(EtomoNumber.Type etomoNumberType, boolean compact) {
    this.etomoNumberType = etomoNumberType;
    this.compact = compact;
  }

  public String toString() {
    return "[descriptor:" + descriptor + "]";
  }

  public int size() {
    return descriptor.size();
  }

  void fail() {
    valid = false;
  }

  public boolean isEmpty() {
    return size() == 0;
  }

  public ParsedElement getElement(int index) {
    return descriptor.get(index);
  }

  public String getRawString() {
    return getString(false);
  }

  /**
   * Returns true only when all numbers in the array are greater then or equal
   * to the number parameter.  Expands any array descriptors into the arrays
   * they represent.
   */
  public boolean ge(int number) {
    List array = getArray(null);
    boolean greaterOrEqual = true;
    for (int i = 0; i < array.size(); i++) {
      if (!((ParsedElement) array.get(i)).ge(number)) {
        greaterOrEqual = false;
        break;
      }
    }
    return greaterOrEqual;
  }

  /**
   * Append an array of non-empty ParsedNumbers described by this.descriptor.
   * @param parsedNumberArray the array to be added to and returned - may be null
   * @return parsedNumberArray
   */
  List getArray(List parsedNumberArray) {
    if (descriptor.size() == 0) {
      return parsedNumberArray;
    }
    //exclude empty descriptor numbers
    ParsedElementList list = new ParsedElementList();
    for (int i = 0; i < descriptor.size(); i++) {
      if (!descriptor.get(i).isEmpty()) {
        list.add(descriptor.get(i));
      }
    }
    if (list.size() == 0) {
      return parsedNumberArray;
    }
    if (parsedNumberArray == null) {
      parsedNumberArray = new ArrayList();
    }
    //the first number in the descriptor is the first number in the array
    parsedNumberArray.add(list.get(0));
    //if there is only one number, then we are done
    if (list.size() == 1) {
      return parsedNumberArray;
    }
    //two or three numbers means that it is a descriptor, so expand the descriptor
    //into the list of numbers
    EtomoNumber increment = new EtomoNumber(etomoNumberType);
    ParsedNumber lastNumber = null;
    if (list.size() == 2) {
      increment.set(1);
      lastNumber = (ParsedNumber) list.get(1);
    }
    else {
      increment.set(list.get(1).getRawNumber());
      lastNumber = (ParsedNumber) list.get(2);
    }
    //if the increment is 0, return the first and last number
    //not sure if this is right, but it makes sense
    if (increment.equals(0)) {
      parsedNumberArray.add(lastNumber);
      return parsedNumberArray;
    }
    //increment the number and save the result until you get to the last number
    //the increment can be positive or negative
    ParsedNumber curNumber = (ParsedNumber) list.get(0);
    ParsedNumber prevNumber = curNumber;
    boolean done = false;
    while (!done) {
      prevNumber = curNumber;
      curNumber = new ParsedNumber(etomoNumberType);
      curNumber.setRawString(prevNumber.getRawNumber());
      curNumber.plus(increment);
      if ((increment.isPositive() && curNumber.le(lastNumber))
          || (increment.isNegative() && curNumber.ge(lastNumber))) {
        parsedNumberArray.add(curNumber);
      }
      else {
        done = true;
      }
    }
    /* Parser ignores the last number, so don't add it
     if (!curNumber.equals(lastNumber)) {
     parsedNumberArray.add(lastNumber);
     }*/
    return parsedNumberArray;
  }

  /**
   * Clear the descriptor member variable, and add each element of the parsedArrayDescriptor
   * to the descriptor.
   * @param parsedArrayDescriptor
   */
  public void set(ParsedElement parsedArrayDescriptor) {
    parsedArrayDescriptor.setDebug(debug);
    parsedArrayDescriptor.setDefaultValue(0, defaultValueArray);
    descriptor.clear();
    for (int i = 0; i < parsedArrayDescriptor.size(); i++) {
      descriptor.add(parsedArrayDescriptor.getElement(i));
    }
  }

  public void setRawString(int index, String string) {
    if (index < 0) {
      return;
    }
    ParsedNumber element = new ParsedNumber(etomoNumberType);
    element.setDebug(debug);
    element.setDefaultValue(index, defaultValueArray);
    element.setRawString(string);
    descriptor.set(index, element);
  }

  public void setRawString(int index, float number) {
    ParsedNumber element = new ParsedNumber(etomoNumberType);
    element.setDebug(debug);
    element.setDefaultValue(index, defaultValueArray);
    element.setRawString(number);
    descriptor.set(index, element);
  }

  public void moveElement(int fromIndex, int toIndex) {
    descriptor.move(fromIndex, toIndex);
  }

  /**
   * input is a collection, since it is not indexed, so it is a semi-raw string
   * - it has :'s
   */
  public void setRawString(String input) {
    descriptor.clear();
    valid = true;
    if (input == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(input);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      token = tokenizer.next();
      parse(token, tokenizer);
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
  }

  public Number getRawNumber() {
    return descriptor.get(0).getRawNumber();
  }

  public Number getRawNumber(int index) {
    return descriptor.get(index).getRawNumber();
  }

  public String getRawString(int index) {
    return descriptor.get(index).getRawString();
  }

  public EtomoNumber.Type getEtomoNumberType() {
    return etomoNumberType;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
    for (int i = 0; i < descriptor.size(); i++) {
      descriptor.get(i).setDebug(debug);
    }
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    descriptor.clear();
    valid = true;
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until a divider isn't found, this should be the end of the list
    while (dividerFound && valid) {
      try {
        //parse an element.
        token = parseElement(token, tokenizer);
        //whitespace is not allowed in a file descriptor and it may be used as
        //an array divider, so it shouldn't be removed
        dividerFound = false;
        //if the divider symbol is found, continue parsing elements
        if (token != null
            && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          dividerFound = true;
          token = tokenizer.next();
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        fail();
      }
    }
    return token;
  }

  void removeElement(int index) {
    descriptor.remove(index);
  }

  boolean isCollection() {
    return true;
  }

  boolean isDefaultedEmpty() {
    for (int i = 0; i < descriptor.size(); i++) {
      if (!descriptor.get(i).isDefaultedEmpty()) {
        return false;
      }
    }
    return true;
  }

  String getParsableString() {
    return getString(true);
  }

  /**
   * Return true when the array is empty or the array has 1 element and that
   * element has parsed number syntax.
   */
  boolean hasParsedNumberSyntax() {
    int size = descriptor.size();
    if (size == 0) {
      return true;
    }
    if (size == 1 && descriptor.get(0).hasParsedNumberSyntax()) {
      return true;
    }
    if (!compact) {
      return false;
    }
    //compact descriptor show only non-empty numbers
    int elementCount = 0;
    for (int i = 0; i < size; i++) {
      if (!descriptor.get(i).isDefaultedEmpty()) {
        elementCount++;
        if (elementCount > 1) {
          return false;
        }
      }
    }
    return true;
  }

  /**
   * Sets defaultValueArray and calls setDefaultValue() for each element in the
   * descriptor.
   * @param numberIndex - not used by a collection
   */
  void setDefaultValue(int numberIndex, Integer[] defaultValueArray) {
    for (int i = 0; i < descriptor.size(); i++) {
      descriptor.get(i).setDefaultValue(i, defaultValueArray);
    }
  }

  /**
   * return the elements of the collection separated by :'s.  If compact is
   * true and a parsable string is being created, exclude empty elements.
   * @param parsable - true when creating a parsable string
   */
  private String getString(boolean parsable) {
    StringBuffer buffer = new StringBuffer();
    boolean emptyString = true;
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (!(compact && parsable) || !element.isDefaultedEmpty()) {
        if (!emptyString) {
          buffer.append(DIVIDER_SYMBOL.charValue());
        }
        emptyString = false;
        if (parsable) {
          buffer.append(descriptor.get(i).getParsableString());
        }
        else {
          buffer.append(descriptor.get(i).getRawString());
        }
      }
    }
    return buffer.toString();
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    //parse a number
    ParsedNumber element = new ParsedNumber(etomoNumberType);
    element.setDebug(debug);
    element.setDefaultValue(descriptor.size(), defaultValueArray);
    token = element.parse(token, tokenizer);
    descriptor.add(element);
    return token;
  }
}
