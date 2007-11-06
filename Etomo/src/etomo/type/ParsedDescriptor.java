package etomo.type;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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
 * <p> $Log$ </p>
 */
abstract class ParsedDescriptor extends ParsedElement {
  public static final String rcsid = "$Id$";

  final ParsedElementList descriptor = new ParsedElementList();

  final EtomoNumber.Type etomoNumberType;

  /**
   * Use defaultValueArray to set each element's defaultValue when the defaultValueArray
   * is available and contains an entry under the same index as the element.
   */
  private Integer[] defaultValueArray = null;

  ParsedDescriptor(final EtomoNumber.Type etomoNumberType) {
    this.etomoNumberType = etomoNumberType;
  }

  final public Number getRawNumber() {
    return descriptor.get(0).getRawNumber();
  }

  final public String getRawString() {
    return getString(false);
  }

  final public String getRawString(int index) {
    return descriptor.get(index).getRawString();
  }

  final public boolean isEmpty() {
    return size() == 0;
  }

  final public String toString() {
    return "[descriptor:" + descriptor + "]";
  }

  abstract boolean isCompact();

  abstract Character getDividerSymbol();

  abstract boolean isIteratorDescriptor();

  abstract int getIncrement(ParsedNumber first, ParsedNumber last);

  static Character getDividerSymbol(boolean usingIteratorDescriptor) {
    if (usingIteratorDescriptor) {
      return ParsedIteratorDescriptor.DIVIDER_SYMBOL;
    }
    return ParsedArrayDescriptor.DIVIDER_SYMBOL;
  }

  final void moveElement(int fromIndex, int toIndex) {
    descriptor.move(fromIndex, toIndex);
  }

  final Token parse(Token token, PrimativeTokenizer tokenizer) {
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

  final Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    if (isDebug()) {
      System.out.println("ParsedDescriptor.parseElement:token=" + token);
    }
    //parse a number
    ParsedNumber element = new ParsedNumber(etomoNumberType,
        isIteratorDescriptor());
    element.setDebug(isDebug());
    element.setDefaultValue(descriptor.size(), defaultValueArray);
    token = element.parse(token, tokenizer);
    descriptor.add(element);
    return token;
  }

  /**
   * Returns true only when all numbers in the array are greater then or equal
   * to the number parameter.  Expands any array descriptors into the arrays
   * they represent.
   */
  final boolean ge(final int number) {
    List expandedArray = getParsedNumberExpandedArray(null);
    boolean greaterOrEqual = true;
    for (int i = 0; i < expandedArray.size(); i++) {
      if (!((ParsedElement) expandedArray.get(i)).ge(number)) {
        greaterOrEqual = false;
        break;
      }
    }
    return greaterOrEqual;
  }

  final boolean isCollection() {
    return true;
  }

  final boolean isDefaultedEmpty() {
    for (int i = 0; i < descriptor.size(); i++) {
      if (!descriptor.get(i).isDefaultedEmpty()) {
        return false;
      }
    }
    return true;
  }

  final ParsedElement getElement(final int index) {
    return descriptor.get(index);
  }

  final String getParsableString() {
    return getString(true);
  }

  final void removeElement(int index) {
    descriptor.remove(index);
  }

  /**
   * Clear the descriptor member variable, and add each element of the parsedDescriptor
   * to the descriptor.
   * @param parsedDescriptor
   */
  final void set(ParsedElement parsedDescriptor) {
    parsedDescriptor.setDebug(isDebug());
    parsedDescriptor.setDefaultValue(0, defaultValueArray);
    descriptor.clear();
    for (int i = 0; i < parsedDescriptor.size(); i++) {
      descriptor.add(parsedDescriptor.getElement(i));
    }
  }

  final void setRawString(int index, String string) {
    if (index < 0) {
      return;
    }
    ParsedNumber element = new ParsedNumber(etomoNumberType,
        isIteratorDescriptor());
    element.setDebug(isDebug());
    element.setDefaultValue(index, defaultValueArray);
    element.setRawString(string);
    descriptor.set(index, element);
  }

  final void setRawString(int index, float number) {
    ParsedNumber element = new ParsedNumber(etomoNumberType,
        isIteratorDescriptor());
    element.setDebug(isDebug());
    element.setDefaultValue(index, defaultValueArray);
    element.setRawString(number);
    descriptor.set(index, element);
  }

  final int size() {
    return descriptor.size();
  }

  final String validate() {
    for (int i = 0; i < descriptor.size(); i++) {
      String errorMessage = descriptor.get(i).validate();
      if (errorMessage != null) {
        return errorMessage;
      }
    }
    return getFailedMessage();
  }

  final void setDebug(boolean debug) {
    super.setDebug(debug);
    for (int i = 0; i < descriptor.size(); i++) {
      descriptor.get(i).setDebug(debug);
    }
  }

  final EtomoNumber.Type getEtomoNumberType() {
    return etomoNumberType;
  }

  /**
   * input is a collection, since it is not indexed, so it is a semi-raw string
   * - it has :'s
   */
  final void setRawString(String input) {
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
   * Sets defaultValueArray and calls setDefaultValue() for each element in the
   * descriptor.
   * @param numberIndex - not used by a collection
   */
  final void setDefaultValue(int numberIndex, Integer[] defaultValueArray) {
    for (int i = 0; i < descriptor.size(); i++) {
      descriptor.get(i).setDefaultValue(i, defaultValueArray);
    }
  }

  /**
   * Append an array of non-empty ParsedNumbers described by this.descriptor.
   * Construct parsedNumberExpandedArray if it is null.
   * @param parsedNumberExpandedArray the array to be added to and returned
   * @return parsedNumberExpandedArray
   */
  final List getParsedNumberExpandedArray(List parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ArrayList();
    }
    if (descriptor.size() == 0) {
      return parsedNumberExpandedArray;
    }
    //exclude empty descriptor numbers
    ParsedElementList list = new ParsedElementList();
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
      curNumber = new ParsedNumber(etomoNumberType, isIteratorDescriptor());
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
   * return the elements of the collection separated by :'s.  If compact is
   * true and a parsable string is being created, exclude empty elements.
   * @param parsable - true when creating a parsable string
   */
  final String getString(boolean parsable) {
    StringBuffer buffer = new StringBuffer();
    boolean emptyString = true;
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (!(isCompact() && parsable) || !element.isDefaultedEmpty()) {
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
    }
    return buffer.toString();
  }

  /**
   * Return true when the array is empty or the array has 1 element and that
   * element has parsed number syntax.
   */
  final boolean hasParsedNumberSyntax() {
    int size = descriptor.size();
    if (size == 0) {
      return true;
    }
    if (size == 1 && descriptor.get(0).hasParsedNumberSyntax()) {
      return true;
    }
    if (!isCompact()) {
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
}
