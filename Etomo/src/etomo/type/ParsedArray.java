package etomo.type;

import java.io.IOException;

import etomo.storage.autodoc.ReadOnlyAttribute;
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
 * <p> Revision 1.5  2007/04/13 21:50:51  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:14:05  sueh
 * <p> bug# 964 Added setRawString(String), which parses a list of numbers separated
 * <p> by whitespace or commas.
 * <p>
 * <p> Revision 1.3  2007/04/09 20:59:35  sueh
 * <p> bug# 964 Added missing tokenizer.next() call to parseArray.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:54:01  sueh
 * <p> bug# 964 In getRawString(), using a "," when some of the elements are empty -
 * <p> so that the location of the existing elements isn't lost.  Using ParsedElement.isCollection()
 * <p> to decide whether to use "," in other cases:  using a comma when one of the
 * <p> elements is an array descriptor.
 * <p>
 * <p> Revision 1.1  2007/03/30 23:40:59  sueh
 * <p> bug# 964 Class to parse Matlab arrays.
 * <p> </p>
 */
public final class ParsedArray extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final Character OPEN_SYMBOL = new Character('[');
  static final Character CLOSE_SYMBOL = new Character(']');
  static final Character DIVIDER_SYMBOL = new Character(',');

  private final ParsedElementList array = new ParsedElementList();

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;
  private boolean valid = true;
  private boolean debug = false;

  /**
   * flexibleSyntax allows a ParsedArray to look like a number when it contains
   * 0 or more elements and doesn't contain a collection.
   */
  private boolean flexibleSyntax = false;

  /**
   * No effect on ParsedArray.  Used to set ParsedArrayDescriptor.compact.
   */
  private boolean compactDescriptor = false;

  public ParsedArray() {
  }

  public ParsedArray(EtomoNumber.Type etomoNumberType) {
    this.etomoNumberType = etomoNumberType;
  }

  public ParsedArray(EtomoNumber.Type etomoNumberType, int defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = new Integer(defaultValue);
  }

  public ParsedArray(EtomoNumber.Type etomoNumberType, Integer defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = defaultValue;
  }

  public static ParsedArray getFlexibleInstance(EtomoNumber.Type etomoNumberType) {
    ParsedArray instance = new ParsedArray(etomoNumberType);
    instance.flexibleSyntax = true;
    return instance;
  }

  public static ParsedArray getCompactDescriptorInstance(
      EtomoNumber.Type etomoNumberType) {
    ParsedArray instance = new ParsedArray(etomoNumberType);
    instance.compactDescriptor = true;
    return instance;
  }

  static ParsedArray getFlexibleInstance(EtomoNumber.Type etomoNumberType,
      Integer defaultValue) {
    ParsedArray instance = new ParsedArray(etomoNumberType, defaultValue);
    instance.flexibleSyntax = true;
    return instance;
  }

  /**
   * This is a array only if starts with "[" (ignoring whitespace).
   * @param attribute
   * @return
   */
  static boolean isArray(Token token) {
    if (token == null) {
      return false;
    }
    if (token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
      return true;
    }
    return false;
  }

  public void parse(ReadOnlyAttribute attribute) {
    array.clear();
    valid = true;
    if (attribute == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(attribute.getValue());
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

  public String getRawString(int index) {
    return array.get(index).getRawString();
  }

  public Number getRawNumber() {
    return array.get(0).getRawNumber();
  }

  public boolean isEmpty(int index) {
    return array.get(index).isEmpty();
  }

  public void setRawString(int arrayIndex, int descriptorIndex, String string) {
    if (string != null && !string.matches("\\s*")) {
      insureDescriptorExists(arrayIndex);
    }
    array.get(arrayIndex).setRawString(descriptorIndex, string);
  }
  
  public ParsedElement getDescriptor() {
    for (int i= 0;i<array.size();i++) {
      ParsedElement element = array.get(i);
      if (element.isCollection()) {
        return element;
      }
    }
    return ParsedElementList.EmptyParsedElement.INSTANCE;
  }
  
  public int getDescriptorIndex() {
    for (int i= 0;i<array.size();i++) {
      ParsedElement element = array.get(i);
      if (element.isCollection()) {
        return i;
      }
    }
    return -1;
  }
  
  public void clear() {
    array.clear();
  }

  public void setRawString(int arrayIndex, int descriptorIndex, float number) {
    insureDescriptorExists(arrayIndex);
    array.get(arrayIndex).setRawString(descriptorIndex, number);
  }

  public ParsedElement getElement(int arrayIndex, int descriptorIndex) {
    return array.get(arrayIndex).getElement(descriptorIndex);
  }

  public void moveElement(int arrayIndex, int descriptorFromIndex,
      int descriptorToIndex) {
    array.get(arrayIndex).moveElement(descriptorFromIndex, descriptorToIndex);
  }

  public String getRawString(int arrayIndex, int descriptorIndex) {
    return array.get(arrayIndex).getRawString(descriptorIndex);
  }

  public void setRawString(int index, String string) {
    array.remove(index);
    if (string == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(string);
    StringBuffer buffer = new StringBuffer();
    try {
      parseElement(tokenizer.next(), tokenizer, index);
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
  }

  public void setRawString(int index, float number) {
    ParsedNumber element = new ParsedNumber(etomoNumberType, defaultValue);
    element.setRawString(number);
    array.set(index, element);
  }

  public void setRawString(int index, String number, String fieldDescription) {
    ParsedNumber element = new ParsedNumber(etomoNumberType, defaultValue);
    element.setRawString(number, fieldDescription);
    array.set(index, element);
  }

  /**
   * Clear the array member variable, and add each element of the ParsedElement
   * to the array.
   * @param ParsedElement
   */
  public void setElement(ParsedElement elementArray) {
    array.clear();
    for (int i = 0; i < elementArray.size(); i++) {
      array.add(elementArray.getElement(i));
    }
  }

  public void setElement(int index, ParsedElement element) {
    if (debug) {
      System.out.println("index=" + index + ",element=" + element);
    }
    array.set(index, element);
  }

  public int size() {
    return array.size();
  }

  public ParsedElement getElement(int index) {
    return array.get(index);
  }

  public void moveElement(int fromIndex, int toIndex) {
    array.move(fromIndex, toIndex);
  }

  void fail() {
    valid = false;
  }

  /**
   * parse the entire array
   * @return the token that is current when the array is parsed
   */
  Token parse(Token token, PrimativeTokenizer tokenizer) {
    array.clear();
    valid = true;
    if (token == null) {
      return token;
    }
    try {
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null) {
        return token;
      }
      if (!token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
        if (!flexibleSyntax) {
          valid = false;
          return token;
        }
        //parse this as a number because it is a number and flexibleSyntax is on
        ParsedNumber number = new ParsedNumber(etomoNumberType, defaultValue);
        token = number.parse(token, tokenizer);
        setElement(0, number);
        return token;
      }
      token = tokenizer.next();
      //remove any whitespace before the first element
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseArray(token, tokenizer, -1);
      if (!valid) {
        return token;
      }
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null
          || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
        valid = false;
        return token;
      }
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
    return token;
  }

  /**
   * Raw strings go to the screen
   */
  public String getRawString() {
    return getRawString(null, -1);
  }

  public String getRawStrings(int startIndex) {
    return getRawString(null, startIndex);
  }

  /**
   * Create a ParsedArrayDescriptor in the position specified by index, if the
   * position is not a collection.  This creates the correct format and corrects
   * it if it changes.
   * @param arrayIndex
   */
  private void insureDescriptorExists(int index) {
    ParsedElement element = array.get(index);
    if (!element.isCollection()) {
      //this position must contain a descriptor
      ParsedArrayDescriptor descriptor;
      if (compactDescriptor) {
        descriptor = ParsedArrayDescriptor.getCompactInstance(etomoNumberType,
            defaultValue);
      }
      else {
        descriptor = new ParsedArrayDescriptor(etomoNumberType, defaultValue);
      }
      array.set(index, descriptor);
    }
  }

  /**
   * Raw strings may go to the screen
   * @param compatibleWithParsedNumber
   * @return
   */
  private String getRawString(EtomoBoolean2 compatibleWithParsedNumber,
      int startIndex) {
    return getString(compatibleWithParsedNumber, false, startIndex);
  }

  /**
   * Asking for a raw string or a parsable string.  A raw string and a parsable
   * string from a collection returns basically the same thing.  The
   * elements are separated by a divider but the "[" and "]" are not added.
   * If parsable is true, the divider may be a space.
   * @param compatibleWithParsedNumber is true if the returned string is empty or 1 number
   * @param parsable - true when getting parsable string, false when getting a raw string
   * @param index - start index
   */
  private String getString(EtomoBoolean2 compatibleWithParsedNumber,
      boolean parsable, int startIndex) {
    if (startIndex == -1) {
      startIndex = 0;
    }
    if (compatibleWithParsedNumber != null) {
      compatibleWithParsedNumber.set(true);
    }
    StringBuffer buffer = new StringBuffer();
    //only use spaces as dividers when there are no empty elements and no array
    //descriptors
    boolean elementIsCollection = false;
    boolean emptyElement = false;
    boolean empty = true;//set to false when a non-empty element is found
    boolean usingDividerSymbol = false;
    for (int i = startIndex; i < array.size(); i++) {
      if (i > startIndex) {
        //use the divider symbol if this is a raw string or there are missing
        //elements or collection elements
        if (usingDividerSymbol || !parsable || emptyElement || elementIsCollection) {
          buffer.append(DIVIDER_SYMBOL + " ");
          usingDividerSymbol = true;
        }
        else {
          buffer.append(" ");
        }
      }
      ParsedElement element = array.get(i);
      elementIsCollection = element.isCollection();
      emptyElement = element.isEmpty();
      if (!emptyElement) {
        empty = false;
      }
      //Turn off compatibleWithParsedNumber if there
      //are multiple non-empty elements or 1 or more collection elements.
      if (compatibleWithParsedNumber != null && compatibleWithParsedNumber.is()) {
        if (elementIsCollection || (i > 0 && !emptyElement)) {
          compatibleWithParsedNumber.set(false);
        }
      }
      if (parsable) {
        buffer.append(element.getParsableString());
      }
      else {
        buffer.append(element.getRawString());
      }
    }
    if (empty) {
      return "";
    }
    return buffer.toString();
  }

  /**
   * raw strings come from the screen
   * input may be a collection, since it is not indexed, so treat it as  a semi-
   * raw string (it may have commas)
   */
  public void setRawString(String input) {
    array.clear();
    valid = true;
    if (input == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(input);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      token = tokenizer.next();
      //raw strings shouldn't have brackets
      //place input into array starting from the beginning of the list
      parseArray(token, tokenizer, -1);
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
  }

  /**
   * Parse one or more elements in an array.  Place them in ParsedArray.array
   * starting at index.
   * @param index
   * @param input
   */
  public void setRawStrings(int index, String input) {
    PrimativeTokenizer tokenizer = createTokenizer(input);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      token = tokenizer.next();
      //raw strings shouldn't have brackets
      parseArray(token, tokenizer, index);
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
  }

  /**
   * @return true if the size of the list is null.  Empty elements in the list
   * do not affect this result
   */
  public boolean isEmpty() {
    return size() == 0;
  }

  boolean isCollection() {
    return true;
  }

  /**
   * Parsable strings are saved to the .prm file.
   * Returns [] if the list is empty.  Returns a list surrounded by brackets if
   * there are multiple elements.
   * If flexibleSyntax is true and there is only one element, which is not a
   * collection, return the rawString (no brackets), so that the parsableString
   * looks like a ParsedNumber.
   */
  public String getParsableString() {
    EtomoBoolean2 compatibleWithParsedNumber = new EtomoBoolean2();
    String string = getString(compatibleWithParsedNumber, true, -1);
    StringBuffer buffer = new StringBuffer();
    if (flexibleSyntax && compatibleWithParsedNumber.is()) {
      //return a parsable number
      return string;
    }
    buffer = new StringBuffer(OPEN_SYMBOL.toString());
    buffer.append(string);
    buffer.append(CLOSE_SYMBOL.toString());
    return buffer.toString();
  }

  public String toString() {
    return "[array:" + array + "]";
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
    for (int i = 0; i < array.size(); i++) {
      array.get(i).setDebug(debug);
    }
  }

  private Token parseArray(Token token, PrimativeTokenizer tokenizer, int index) {
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until the end of the array
    //can't just check dividerFound, because whitespace can act as a divider,
    //but isn't always the divider
    while (dividerFound && valid && token != null && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      try {
        //parse an element
        token = parseElement(token, tokenizer, index++);
        //whitespace may be used as a divider, but it may not always be the divider -
        //it could just be whitespace at the end of the array or whitespace
        //before the divider
        dividerFound = false;
        if (token != null && token.is(Token.Type.WHITESPACE)) {
          dividerFound = true;
          token = tokenizer.next();
        }
        if (token != null
            && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          dividerFound = true;
          token = tokenizer.next();
          //A "," is being used as a divider character so remove whitespace
          //after divider
          if (token != null && token.is(Token.Type.WHITESPACE)) {
            token = tokenizer.next();
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        valid = false;
      }
    }
    return token;
  }

  /**
   * Adds or sets either a ParsedArrayDescriptor or a ParsedNumber.
   * @param token
   * @param tokenizer
   * @param index adds element when index is -1
   * @return current token when done parsing the element
   */
  private Token parseElement(Token token, PrimativeTokenizer tokenizer,
      int index) {
    //end of list
    if (token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      //This probably doesn't matter because its only necessary to keep track of
      //location of existing elements in a lightly populated array.  Its not really
      //necessary to track the size of the array.  But this does preserve the
      //the original string:
      //[] means no elements, [,] means two elements.  [1,,] means three elements
      if (array.size() > 0) {
        array.addEmptyElement();
      }
      return token;
    }
    //found empty element
    if (token.is(Token.Type.WHITESPACE)
        || token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
      array.addEmptyElement();
      return token;
    }
    //may have found an element
    //Array descriptors don't have their own open and close symbols, so they
    //look like numbers until to you get to the first divider (":").
    ParsedArrayDescriptor descriptor;
    if (compactDescriptor) {
      descriptor = ParsedArrayDescriptor.getCompactInstance(etomoNumberType,
          defaultValue);
    }
    else {
      descriptor = new ParsedArrayDescriptor(etomoNumberType, defaultValue);
    }
    token = descriptor.parse(token, tokenizer);
    if (descriptor.size() == 0) {
      //There's nothing there, so its an empty element
      array.addEmptyElement();
    }
    ParsedElement element;
    if (descriptor.size() == 1) {
      //If there is only one number, then there was no ":" and its really a ParsedNumber.
      element = descriptor.getElement(0);
    }
    else {
      element = descriptor;
    }
    if (index == -1) {
      array.add(element);
    }
    else {
      array.set(index, element);
    }
    return token;
  }
}
