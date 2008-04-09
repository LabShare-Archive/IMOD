package etomo.type;

import java.io.IOException;

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
 * <p> Revision 1.2  2008/04/02 02:20:10  sueh
 * <p> bug# 1097 Moved functionality out of ParsedDescriptor and into child
 * <p> classes.  This is because the child class are now less similar to each
 * <p> other.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:49:43  sueh
 * <p> bug# 1047 Class to parsed iterator descriptors such as 4-9 and 5-2.
 * <p> </p>
 */
final class ParsedIteratorDescriptor extends ParsedDescriptor {
  public static final String rcsid = "$Id$";

  static final Character DIVIDER_SYMBOL = new Character('-');
  private static final ParsedElementType type = ParsedElementType.NON_MATLAB;

  private final EtomoNumber.Type etomoNumberType = EtomoNumber.Type.INTEGER;
  private final ParsedElementList descriptor = new ParsedElementList(type);

  private boolean dividerParsed = false;

  ParsedIteratorDescriptor() {
  }

  public boolean isEmpty() {
    return size() == 0;
  }

  public String toString() {
    return "[descriptor:" + descriptor + "]";
  }

  void removeElement(int index) {
    descriptor.remove(index);
  }

  ParsedElement getElement(final int index) {
    return descriptor.get(index);
  }

  void clear() {
    descriptor.clear();
  }

  boolean wasDividerParsed() {
    return dividerParsed;
  }

  Token parse(Token token, final PrimativeTokenizer tokenizer) {
    tokenizer.setDebug(isDebug());
    descriptor.clear();
    resetFailed();
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until a divider isn't found, this should be the end of the descriptor
    while (dividerFound && !isFailed()) {
      try {
        //parse an element.
        token = parseElement(token, tokenizer);
        //whitespace is not allowed in a file descriptor and it may be used as
        //an array divider, so it shouldn't be removed
        dividerFound = false;
        //if the divider symbol is found, continue parsing elements
        if (token != null
            && token.equals(Token.Type.SYMBOL, getDividerSymbol().charValue())) {
          dividerFound = true;
          //Until the first divider is found this may not be a descriptor.
          dividerParsed = true;
          token = tokenizer.next();
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        fail(e.getMessage());
      }
    }
    return token;
  }

  Token parseElement(Token token, final PrimativeTokenizer tokenizer) {
    //parse a number
    ParsedNumber element = ParsedNumber.getArrayInstance(type, etomoNumberType);
    element.setDebug(isDebug());
    token = element.parse(token, tokenizer);
    descriptor.add(element);
    return token;
  }

  /**
   * Clear the descriptor member variable, and add each element of the parsedDescriptor
   * to the descriptor.
   * @param parsedDescriptor
   */
  public void set(final ParsedElement parsedDescriptor) {
    parsedDescriptor.setDebug(isDebug());
    descriptor.clear();
    for (int i = 0; i < parsedDescriptor.size(); i++) {
      descriptor.add(parsedDescriptor.getElement(i));
    }
  }

  void setRawString(final int index, final String string) {
    if (index < 0) {
      return;
    }
    ParsedNumber element = ParsedNumber.getArrayInstance(type, etomoNumberType);
    element.setDebug(isDebug());
    element.setRawString(string);
    descriptor.set(index, element);
  }

  void setRawString(final int index, final float number) {
    ParsedNumber element = ParsedNumber.getArrayInstance(type, etomoNumberType);
    element.setDebug(isDebug());
    element.setRawString(number);
    descriptor.set(index, element);
  }

  int size() {
    return descriptor.size();
  }

  String validate() {
    for (int i = 0; i < descriptor.size(); i++) {
      String errorMessage = descriptor.get(i).validate();
      if (errorMessage != null) {
        return errorMessage;
      }
    }
    return getFailedMessage();
  }

  EtomoNumber.Type getEtomoNumberType() {
    return etomoNumberType;
  }

  /**
   * input is a collection, since it is not indexed, so it is a semi-raw string
   * - it has :'s
   */
  void setRawString(final String input) {
    descriptor.clear();
    resetFailed();
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
      fail(e.getMessage());
    }
  }

  /**
   * Append an array of non-empty ParsedNumbers described by this.descriptor.
   * Construct parsedNumberExpandedArray if it is null.
   * @param parsedNumberExpandedArray the array to be added to and returned
   * @return parsedNumberExpandedArray
   */
  ParsedElementList getParsedNumberExpandedArray(ParsedElementList parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ParsedElementList(type);
    }
    if (descriptor.size() == 0) {
      return parsedNumberExpandedArray;
    }
    //exclude empty descriptor numbers
    ParsedElementList list = new ParsedElementList(type);
    for (int i = 0; i < descriptor.size(); i++) {
      if (!descriptor.get(i).isEmpty()) {
        list.add(descriptor.get(i));
      }
    }
    if (list.size() == 0) {
      return parsedNumberExpandedArray;
    }
    //the first number in the descriptor is the first number in the array
    parsedNumberExpandedArray.add(list.get(0));
    //if there is only one number, then we are done
    if (list.size() == 1) {
      return parsedNumberExpandedArray;
    }
    //two or three numbers means that it is a descriptor, so expand the descriptor
    //into the list of numbers
    EtomoNumber increment = new EtomoNumber(etomoNumberType);
    ParsedNumber lastNumber = null;
    if (list.size() == 2) {
      ParsedNumber first = (ParsedNumber) list.get(0);
      ParsedNumber second = (ParsedNumber) list.get(1);
      //If there are two numbers and they are the same, then the the array expands
      //into the one number
      if (first.equals(second)) {
        return parsedNumberExpandedArray;
      }
      else {
        increment.set(getIncrement((ParsedNumber) list.get(0),
            (ParsedNumber) list.get(1)));
      }
      lastNumber = (ParsedNumber) list.get(1);
    }
    else {
      increment.set(list.get(1).getRawNumber());
      lastNumber = (ParsedNumber) list.get(2);
    }
    //if the increment is 0, return the first and last number
    //not sure if this is right.
    if (increment.equals(0)) {
      parsedNumberExpandedArray.add(lastNumber);
      return parsedNumberExpandedArray;
    }
    //increment the number and save the result until you get to the last number
    //the increment can be positive or negative
    ParsedNumber curNumber = (ParsedNumber) list.get(0);
    ParsedNumber prevNumber = curNumber;
    boolean done = false;
    while (!done) {
      prevNumber = curNumber;
      curNumber = ParsedNumber.getArrayInstance(type, etomoNumberType);
      curNumber.setRawString(prevNumber.getRawNumber());
      curNumber.plus(increment);
      if ((increment.isPositive() && curNumber.le(lastNumber))
          || (increment.isNegative() && curNumber.ge(lastNumber))) {
        parsedNumberExpandedArray.add(curNumber);
      }
      else {
        done = true;
      }
    }
    //Matlab parser ignores the last number, so don't add it.  With the
    //iterator descriptor the increment is always 1 and only integers are
    //allowed, so that the last number will automatically be added.
    return parsedNumberExpandedArray;
  }

  final Number getRawNumber(final int index) {
    return descriptor.get(index).getRawNumber();
  }

  /**
   * return the elements of the collection separated by dividers.  If compact is
   * true and a parsable string is being created, exclude empty elements.
   * @param parsable - true when creating a parsable string
   */
  String getString(final boolean parsable) {
    StringBuffer buffer = new StringBuffer();
    boolean emptyString = true;
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (!emptyString) {
        buffer.append(getDividerSymbol().charValue());
      }
      emptyString = false;
      if (parsable) {
        buffer.append(descriptor.get(i).getParsableString());
      }
      else {
        buffer.append(descriptor.get(i).getRawString());
      }
    }
    return buffer.toString();
  }

  boolean isCompact() {
    return false;
  }

  boolean isValid() {
    return true;
  }

  Character getDividerSymbol() {
    return DIVIDER_SYMBOL;
  }

  boolean isIteratorDescriptor() {
    return true;
  }

  int getIncrement(final ParsedNumber first, final ParsedNumber last) {
    if (first.gt(last)) {
      return -1;
    }
    return 1;
  }
}
