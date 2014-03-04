package etomo.type;

import java.io.IOException;

import etomo.ui.swing.Token;
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
 * <p> Revision 1.8  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.7  2009/09/05 00:32:23  sueh
 * <p> bug# 1256 Removed ParsedIteratorDescriptor.
 * <p>
 * <p> Revision 1.6  2008/09/10 21:05:17  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.
 * <p> arsedElementList will no longer create an empty element, so null returns
 * <p> will happen.  Make sure that the type is matlab before creating a
 * <p> ParsedArrayDescriptor.
 * <p>
 * <p> Revision 1.5  2008/06/20 19:59:19  sueh
 * <p> bug# 1119 ParsedArrayDescriptor can be either Matlab or non-Matlab now, so I need to explicitly choose an iterator array when I need one.
 * <p>
 * <p> Revision 1.4  2008/04/15 21:23:08  sueh
 * <p> bug# 1105 Simplified setting the default.  Added debug and default to
 * <p> constructor.  Move setDebug() to child classes.  Moved generic descriptor
 * <p> code to ParsedDescriptor.
 * <p>
 * <p> Revision 1.3  2008/04/08 23:57:52  sueh
 * <p> bug# 1105 Changed the array used in getParsedNumberExpandedArray
 * <p> to a ParsedElementList because it always holds ParsedNumbers.
 * <p>
 * <p> Revision 1.2  2008/04/02 02:05:13  sueh
 * <p> Moved functionality out of ParsedDescriptor and into child classes.  This
 * <p> is because the child class are now less similar to each other.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:44:20  sueh
 * <p> bug# 1047 Moved most of the code in ParsedArrayDescriptor to parent class
 * <p> ParsedDescriptor so that ParsedIteratorDescriptor.
 * <p> </p>
 */
abstract class ParsedDescriptor extends ParsedElement {
  public static final String rcsid = "$Id$";

  private final EtomoNumber.Type etomoNumberType;
  final ParsedElementList descriptor;// for use by children classes
  private final ParsedElementType type;

  private boolean dividerParsed = false;
  private EtomoNumber defaultValue = null;

  abstract Character getDividerSymbol();

  abstract boolean isDebug();

  ParsedDescriptor(ParsedElementType type, EtomoNumber.Type etomoNumberType,
      boolean debug, EtomoNumber defaultValue, final String descr) {
    super(descr);
    this.type = type;
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = defaultValue;
    descriptor = new ParsedElementList(type, etomoNumberType, debug, defaultValue, descr);
    setDebug(debug);
  }

  static Character getDividerSymbol(ParsedElementType type) {
    return ParsedArrayDescriptor.DIVIDER_SYMBOL;
  }

  /**
   * If the type is ParsedElementType.NON_MATLAB_ITERATOR_ARRAY, return an
   * instance of ParsedIteratorDescriptor.  If the type is any kind of matlab
   * number, return an instance of ParsedArrayDescriptor.  These are the only
   * types where an array descriptor of some kind is valid.  If the type is
   * anything else, return null.  This is because many arrays outside of
   * MatlabParam do not use iterators or any other kind of array descriptor.
   * @param type
   * @param etomoNumberType
   * @param debug
   * @param defaultValue
   * @return
   */
  static ParsedDescriptor getInstance(ParsedElementType type,
      EtomoNumber.Type etomoNumberType, boolean debug, EtomoNumber defaultValue,
      final String descr) {
    if (debug) {
      System.out.println("ParsedDescriptor.getInstance");
    }
    if (type.isMatlab()) {
      return new ParsedArrayDescriptor(etomoNumberType, debug, defaultValue, descr);
    }
    return null;
  }

  public final String toString() {
    return "[descriptor:" + descriptor + "]";
  }

  final void removeElement(int index) {
    descriptor.remove(index);
  }

  public final void clear() {
    descriptor.clear();
  }

  /**
   * Returns true if the tokenizer passed to parse() contained a divider.
   * Otherwise returns false.
   * @return
   */
  final boolean wasDividerParsed() {
    return dividerParsed;
  }

  /**
   * Don't like having this available to everything in the package but need for
   * children classes to be able to turn on dividerParsed.
   */
  final void setDividerParsed() {
    dividerParsed = true;
  }

  /**
   * Parse the array descriptor.  Also figures out if this actually is an array.
   * Whitespace are not allowed around the divider in an array descriptor.
   * descriptor
   */
  Token parse(Token token, PrimativeTokenizer tokenizer) {
    if (isDebug()) {
      System.out.println("ParsedDescriptor.parse:token=" + token);
    }
    clear();
    if (token == null) {
      return null;
    }
    try {
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      boolean dividerFound = true;
      // loop until the end of the array descriptor.
      while (dividerFound && !isFailed() && token != null && !token.is(Token.Type.EOL)
          && !token.is(Token.Type.EOF)
          && !token.equals(Token.Type.SYMBOL, ParsedList.CLOSE_SYMBOL.charValue())
          && !token.equals(Token.Type.SYMBOL, ParsedArray.CLOSE_SYMBOL.charValue())) {
        // parse an element
        token = parseElement(token, tokenizer);
        if (isDebug()) {
          System.out.println("ParsedDescriptor.parse:descriptor=" + descriptor);
        }
        // Find the divider.
        dividerFound = false;
        if (token != null
            && (token.equals(Token.Type.SYMBOL, getDividerSymbol().charValue()))) {
          // Until the first divider is found this may not be a descriptor.
          setDividerParsed();
          dividerFound = true;
          token = tokenizer.next();
        }
        // Don't worry about whitespace after the divider. It should be handled
        // by the element.
      }
      if (validate() != null) {
        clear();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    return token;
  }

  /**
   * Sets defaultValue and calls setDefaultValue() for each element in the
   * array.
   */
  final void setDefault(EtomoNumber input) {
    defaultValue = input;
    descriptor.setDefault(input);
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (element != null) {
        element.setDefault(defaultValue);
      }
    }
  }

  public final void setDefault(int input) {
    if (defaultValue == null) {
      defaultValue = new EtomoNumber(etomoNumberType);
    }
    defaultValue.set(input);
    descriptor.setDefault(defaultValue);
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (element != null) {
        element.setDefault(defaultValue);
      }
    }
  }

  final Token parseElement(Token token, final PrimativeTokenizer tokenizer) {
    // parse a number
    ParsedNumber element = ParsedNumber.getInstance(type, etomoNumberType, isDebug(),
        defaultValue, descr);
    element.setDebug(isDebug());
    element.setDefault(defaultValue);
    token = element.parse(token, tokenizer);
    if (isDebug()) {
      System.out.println("ParsedDescriptor.parse:element=" + element);
    }
    descriptor.add(element);
    return token;
  }

  /**
   * Clear the descriptor and add the elements in the descriptor to the elements
   * in the input.  If an element is a collection, set the
   * elements in the collection element, because a
   * descriptor cannot contain elements that are collections.
   * 
   * @param input
   */
  public final void set(final ParsedElement input) {
    clear();
    if (input == null) {
      return;
    }
    input.setDebug(isDebug());
    input.setDefault(defaultValue);
    append(input);
  }

  /**
   * Add the elements in the descriptor to the elements in the input.  If an
   * element is a collection, append the elements in the collection element,
   * because a descriptor cannot contain elements that are collections.
   * 
   * @param input
   */
  private void append(final ParsedElement input) {
    if (input == null) {
      return;
    }
    int inputIndex = 0;
    while (inputIndex < input.size()) {
      ParsedElement element = input.getElement(inputIndex++);
      if (element.isCollection()) {
        append(element);
      }
      else {
        descriptor.add(element);
      }
    }
  }

  void setRawString(final int index, final String string) {
    if (index < 0) {
      return;
    }
    ParsedNumber element = ParsedNumber.getInstance(type, etomoNumberType, isDebug(),
        defaultValue, descr);
    element.setRawString(string);
    descriptor.set(index, element);
  }

  void setRawString(final int index, final double number) {
    ParsedNumber element = ParsedNumber.getInstance(type, etomoNumberType, isDebug(),
        defaultValue, descr);
    element.setRawString(number);
    descriptor.set(index, element);
  }

  int size() {
    return descriptor.size();
  }

  /**
   * @return null if valid.
   */
  public String validate() {
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      String errorMessage = null;
      if (element != null) {
        errorMessage = element.validate();
      }
      if (errorMessage != null) {
        return errorMessage;
      }
    }
    return getFailedMessage();
  }

  final EtomoNumber.Type getEtomoNumberType() {
    return etomoNumberType;
  }

  final ParsedElementType getType() {
    return type;
  }

  final EtomoNumber getDefault() {
    return defaultValue;
  }

  /**
   * input is a collection, since it is not indexed, so it is a semi-raw string
   * - it has :'s or -'s
   */
  final void setRawString(final String input) {
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
  ParsedElementList getParsedNumberExpandedArray(
      ParsedElementList parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ParsedElementList(type, etomoNumberType, isDebug(),
          defaultValue, descr);
    }
    if (descriptor.size() == 0) {
      return parsedNumberExpandedArray;
    }
    // exclude empty descriptor numbers
    ParsedElementList list = new ParsedElementList(type, etomoNumberType, isDebug(),
        defaultValue, descr);
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (element != null && !element.isEmpty()) {
        list.add(element);
      }
    }
    if (list.size() == 0) {
      return parsedNumberExpandedArray;
    }
    // the first number in the descriptor is the first number in the array
    parsedNumberExpandedArray.add(list.get(0));
    // if there is only one number, then we are done
    if (list.size() == 1) {
      return parsedNumberExpandedArray;
    }
    // two or three numbers means that it is a descriptor, so expand the descriptor
    // into the list of numbers
    EtomoNumber increment = new EtomoNumber(etomoNumberType);
    ParsedNumber lastNumber = null;
    if (list.size() == 2) {
      ParsedNumber first = (ParsedNumber) list.get(0);
      ParsedNumber second = (ParsedNumber) list.get(1);
      // If there are two numbers and they are the same, then the the array expands
      // into the one number
      if (first.equals(second)) {
        return parsedNumberExpandedArray;
      }
      else {
        increment
            .set(getIncrement((ParsedNumber) list.get(0), (ParsedNumber) list.get(1)));
      }
      lastNumber = (ParsedNumber) list.get(1);
    }
    else {
      increment.set(list.get(1).getRawNumber());
      lastNumber = (ParsedNumber) list.get(2);
    }
    // if the increment is 0, return the first and last number
    // not sure if this is right.
    if (increment.equals(0)) {
      parsedNumberExpandedArray.add(lastNumber);
      return parsedNumberExpandedArray;
    }
    // increment the number and save the result until you get to the last number
    // the increment can be positive or negative
    ParsedNumber curNumber = (ParsedNumber) list.get(0);
    ParsedNumber prevNumber = curNumber;
    boolean done = false;
    while (!done) {
      prevNumber = curNumber;
      curNumber = ParsedNumber.getInstance(type, etomoNumberType, isDebug(),
          defaultValue, descr);
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
    // Matlab parser ignores the last number, so don't add it. With the
    // iterator descriptor the increment is always 1 and only integers are
    // allowed, so that the last number will automatically be added.
    return parsedNumberExpandedArray;
  }

  /**
   * return the elements of the collection separated by dividers.  If compact is
   * true and a parsable string is being created, exclude empty elements.
   * @param parsable - true when creating a parsable string
   */
  String getString(final boolean parsable) {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      if (element != null) {
        String string;
        if (parsable) {
          string = element.getParsableString();
        }
        else {
          string = element.getRawString();
        }
        if (buffer.length() > 0 && !string.matches("\\s*")) {
          buffer.append(getDividerSymbol().charValue());
        }
        buffer.append(string);
      }
    }
    return buffer.toString();
  }

  final int getIncrement(final ParsedNumber first, final ParsedNumber last) {
    if (first.gt(last)) {
      return -1;
    }
    return 1;
  }

  public final String getRawString() {
    return getString(false);
  }

  public final String getRawString(int index) {
    ParsedElement element = getElement(index);
    if (element != null) {
      return element.getRawString();
    }
    return "";
  }

  final boolean isDescriptor() {
    return true;
  }

  final boolean isCollection() {
    return true;
  }

  public ParsedElement getElement(int index) {
    return descriptor.get(index);
  }

  final String getParsableString() {
    return getString(true);
  }

  /**
   * Returns true only when all numbers in the array are greater then or equal
   * to the number parameter.  Expands any array descriptors into the arrays
   * they represent.
   */
  final boolean ge(final int number) {
    ParsedElementList expandedArray = getParsedNumberExpandedArray(null);
    boolean greaterOrEqual = true;
    for (int i = 0; i < expandedArray.size(); i++) {
      ParsedElement element = expandedArray.get(i);
      if (element != null && !element.ge(number)) {
        greaterOrEqual = false;
        break;
      }
    }
    return greaterOrEqual;
  }

  /**
   * Returns true only when all numbers in the array are equal
   * to the number parameter.  Expands any array descriptors into the arrays
   * they represent.
   */
  public final boolean equals(final int number) {
    ParsedElementList expandedArray = getParsedNumberExpandedArray(null);
    boolean equal = true;
    for (int i = 0; i < expandedArray.size(); i++) {
      ParsedElement element = expandedArray.get(i);
      if (element != null && !element.equals(number)) {
        equal = false;
        break;
      }
    }
    return equal;
  }
}
