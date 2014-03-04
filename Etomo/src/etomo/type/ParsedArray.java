package etomo.type;

import java.io.IOException;
import java.util.Properties;

import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.swing.Token;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: A regular array in Matlab when compact is true.</p>
 * 
 * <H4>Matlab Variable Syntax</H4>
 * 
 * <H5>Regular array</H5><UL>
 * <LI>Delimiters: [] (required)
 * <LI>Divider: "," or " "
 * <LI>Empty array: []
 * <LI>Elements:  array descriptors, numbers
 * <LI>Empty element: NaN (number)</UL>
 * 
 * @see ParsedList
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
 * <p> Revision 1.22  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.21  2009/11/20 16:56:46  sueh
 * <p> bug# 1282 Added addElement(ParsedElement).
 * <p>
 * <p> Revision 1.20  2009/09/05 00:31:59  sueh
 * <p> bug# 1256 Removed instance types associated with non-matlab iterator
 * <p> arrays.
 * <p>
 * <p> Revision 1.19  2008/09/10 21:00:32  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.  ParsedElementList will no longer create an empty element, so null returns will happen.  Handle lstThreshold with more flexibility.  Added functions the handle array descriptors which are simpler to call.  Can get the other items in lstThreshold without knowing where they are now; just added an exclude index to the getString function to exclude the first array descriptor.
 * <p>
 * <p> Revision 1.18  2008/06/20 18:58:15  sueh
 * <p> bug# 1119 ParsedArrayDescriptor can be either Matlab or non-Matlab now, so I need to explicitly choose an iterator array when I need one.
 * <p>
 * <p> Revision 1.17  2008/04/15 21:18:40  sueh
 * <p> bug# 1105 Simplified setting the default.  Added debug and default to
 * <p> constructor.  Move setDebug() to child classes.
 * <p>
 * <p> Revision 1.16  2008/04/08 23:55:37  sueh
 * <p> bug# 1105 Changed the array used in ParsedElement to a
 * <p> ParsedElementList because it always holds ParsedNumbers.  Fixed
 * <p> parsing; it was eating up the delimiter whitespace before it could be
 * <p> recognized.
 * <p>
 * <p> Revision 1.15  2008/04/02 02:02:16  sueh
 * <p> bug# 1097 Matching Mallab's syntax.  Making non-Matlab syntax the
 * <p> default in the ParsedElements classes.  This is because matlab uses
 * <p> "NaN", which is unhealthy for Etomo and IMOD.
 * <p>
 * <p> Revision 1.14  2007/11/06 19:38:53  sueh
 * <p> bug# 1047 Made class compatible with ParsedIteratorDescriptor.
 * <p>
 * <p> Revision 1.13  2007/07/31 20:39:52  sueh
 * <p> bug# 1028 added ge(int).
 * <p>
 * <p> Revision 1.12  2007/07/24 04:03:52  sueh
 * <p> bug# 1030 Changed ParsedArray.getParsableStringArray to
 * <p> getPaddedStringArray.  The function is only being used by lstThresholds and it
 * <p> needs padded strings.  Also fixed bugs in the function; it was handling the last
 * <p> number incorrectly.
 * <p>
 * <p> Revision 1.11  2007/07/10 00:31:44  sueh
 * <p> bug# 1022 Added a comment.
 * <p>
 * <p> Revision 1.10  2007/05/11 16:01:12  sueh
 * <p> bug# 964 Added getParsableStringArray(), which expands an array,
 * <p> converting the array descriptors into arrays, and return the resulting array
 * <p> in a String[].
 * <p>
 * <p> Revision 1.9  2007/05/03 21:08:44  sueh
 * <p> bug# 964 Added getFlexibleInstance() and getFlexibleCompactInstance(Type).  Fixed bug in hasParsedNumberSyntax().
 * <p>
 * <p> Revision 1.8  2007/04/26 02:46:56  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.7  2007/04/20 20:52:49  sueh
 * <p> bug# 964 Fixed a bug in parseArray:  don't increment index if it is set to -1.
 * <p> Fixed getString.
 * <p>
 * <p> Revision 1.6  2007/04/19 21:41:39  sueh
 * <p> bug# 964 Added support for flexible syntax, where the string in the .prm file can
 * <p> be either an array or a number.  Instead of handling this in MatlabParamFile,
 * <p> will always store in the array, but the string that is writen to the .prm file will look
 * <p> like a number if flexibleSyntax is turned on.  Added a way to write to an array
 * <p> descriptor inside of an array (setRawString(int,int,String).
 * <p>
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
  static final Character DIVIDER_SYMBOL = ParsedList.DIVIDER_SYMBOL;

  private final ParsedElementList array;
  private final ParsedElementType type;
  private final EtomoNumber.Type etomoNumberType;
  private final String key;

  private EtomoNumber defaultValue = null;
  private boolean debug = false;

  private ParsedArray(ParsedElementType type, EtomoNumber.Type etomoNumberType,
      String key, boolean debug, EtomoNumber defaultValue, final String descr) {
    super(descr);
    this.etomoNumberType = etomoNumberType;
    this.type = type.toArrayInstance();
    this.key = key;
    this.defaultValue = defaultValue;
    this.debug = debug;
    array = new ParsedElementList(type, etomoNumberType, debug, defaultValue, descr);
    setDebug(debug);
  }

  public static ParsedArray getInstance(ParsedElementType type, final String descr) {
    return new ParsedArray(type, null, null, false, null, descr);
  }

  public static ParsedArray getMatlabInstance(final String descr) {
    return new ParsedArray(ParsedElementType.MATLAB_ARRAY, null, null, false, null, descr);
  }

  public static ParsedArray getMatlabInstance(EtomoNumber.Type etomoNumberType,
      final String descr) {
    return new ParsedArray(ParsedElementType.MATLAB_ARRAY, etomoNumberType, null, false,
        null, descr);
  }

  public static ParsedArray getInstance(EtomoNumber.Type etomoNumberType,
      final String descr) {
    return new ParsedArray(ParsedElementType.NON_MATLAB_ARRAY, etomoNumberType, null,
        false, null, descr);
  }

  public static ParsedArray getInstance(EtomoNumber.Type etomoNumberType, String key,
      final String descr) {
    return new ParsedArray(ParsedElementType.NON_MATLAB_ARRAY, etomoNumberType, key,
        false, null, descr);
  }

  static ParsedArray getInstance(ParsedElementType type,
      EtomoNumber.Type etomoNumberType, boolean debug, EtomoNumber defaultValue,
      final String descr) {
    return new ParsedArray(type, etomoNumberType, null, debug, defaultValue, descr);
  }

  /**
   * Get the array stored in this.array, excluding empty ParsedNumbers and
   * expanding array descriptors.  Pad numbers with zeros.
   * @return parsable string version of array
   */
  public String[] getPaddedStringExpandedArray() {
    ParsedElementList expandedArray = getParsedNumberExpandedArray(null);
    if (expandedArray.size() == 0) {
      return new String[0];
    }
    int maxDigits = 0;
    StringBuffer[] bufferArray = new StringBuffer[expandedArray.size()];
    for (int i = 0; i < expandedArray.size(); i++) {
      bufferArray[i] = new StringBuffer();
      ParsedNumber number = (ParsedNumber) expandedArray.get(i);
      if (number != null) {
        bufferArray[i].append(number.getParsableString());
        maxDigits = Math.max(maxDigits, bufferArray[i].length());
      }
    }
    String[] returnArray = new String[expandedArray.size()];
    for (int i = 0; i < expandedArray.size(); i++) {
      int padding = maxDigits - bufferArray[i].length();
      for (int j = 0; j < padding; j++) {
        bufferArray[i].insert(0, '0');
      }
      returnArray[i] = bufferArray[i].toString();
    }
    return returnArray;
  }

  public void setDebug(boolean input) {
    debug = input;
    array.setDebug(input);
    for (int i = 0; i < size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null) {
        element.setDebug(input);
      }
    }
  }

  public void parse(ReadOnlyAttribute attribute) {
    array.clear();
    resetFailed();
    if (attribute == null) {
      setMissingAttribute();
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(attribute.getValue());
    try {
      parse(tokenizer.next(), tokenizer);
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  /**
   * @return an error message if invalid, otherwise null
   */
  public String validate() {
    String errorMessage = null;
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null) {
        errorMessage = element.validate();
      }
      if (errorMessage != null) {
        return errorMessage;
      }
    }
    return getFailedMessage();
  }

  public String getRawString(int index) {
    ParsedElement element = array.get(index);
    if (element != null) {
      return element.getRawString();
    }
    return "";
  }

  public boolean isEmpty(int index) {
    ParsedElement element = array.get(index);
    if (element != null) {
      return element.isEmpty();
    }
    return true;
  }

  public void setRawStringStart(String string) {
    ParsedArrayDescriptor descriptor = getAddFirstArrayDescriptor(string);
    if (descriptor != null) {
      descriptor.setRawStringStart(string);
    }
    return;
  }

  public void setRawStringEnd(String string) {
    ParsedArrayDescriptor descriptor = getAddFirstArrayDescriptor(string);
    if (descriptor != null) {
      descriptor.setRawStringEnd(string);
    }
    return;
  }

  public void setRawStringIncrement(String string) {
    ParsedArrayDescriptor descriptor = getAddFirstArrayDescriptor(string);
    if (descriptor != null) {
      descriptor.setRawStringIncrement(string);
    }
    return;
  }

  /**
   * Gets the first array descriptor in the array.  If there is no array
   * descriptor, it adds one, but only if addIfNumber is a number.
   * @param createIfNumber
   * @return
   */
  private ParsedArrayDescriptor getAddFirstArrayDescriptor(String addIfNumber) {
    ParsedArrayDescriptor descriptor = getFirstArrayDescriptor();
    if (descriptor == null) {
      ParsedNumber number = ParsedNumber.getInstance(type, etomoNumberType, debug,
          defaultValue, descr);
      // If the string doesn't have a number in it, don't bother to create the
      // descriptor. Descriptors can't use "NaN", so there is no point to
      // creating an empty one.
      number.setRawString(addIfNumber);
      if (number.isEmpty()) {
        return null;
      }
      descriptor = new ParsedArrayDescriptor(etomoNumberType, debug, defaultValue, descr);
      array.add(descriptor);
    }
    return descriptor;
  }

  public String getRawStringStart() {
    ParsedArrayDescriptor descriptor = getFirstArrayDescriptor();
    if (descriptor != null) {
      return descriptor.getRawStringStart();
    }
    return "";
  }

  public String getRawStringIncrement() {
    ParsedArrayDescriptor descriptor = getFirstArrayDescriptor();
    if (descriptor != null) {
      return descriptor.getRawStringIncrement();
    }
    return "";
  }

  public String getRawStringEnd() {
    ParsedArrayDescriptor descriptor = getFirstArrayDescriptor();
    if (descriptor != null) {
      return descriptor.getRawStringEnd();
    }
    return "";
  }

  /**
   * raw strings come from the screen
   * input may be a collection, since it is not indexed, so treat it as  a semi-
   * raw string (it may have commas)
   */
  public void setRawString(String input) {
    if (debug) {
      System.out.println("ParsedArray.setRawString:input=" + input);
    }
    array.clear();
    resetFailed();
    if (input == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(input);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      token = tokenizer.next();
      // raw strings shouldn't have brackets
      // place input into array starting from the beginning of the list
      parseArray(token, tokenizer);
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  /**
   * Parse one or more elements in an array.
   * @param index
   * @param input
   */
  public void setRawStrings(String input) {
    PrimativeTokenizer tokenizer = createTokenizer(input);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      token = tokenizer.next();
      // raw strings shouldn't have brackets so start with parseArray, not parse.
      parseArray(token, tokenizer);
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  /* public void setMinArraySize(int input) { array.setMinSize(input); } */

  public void clear() {
    array.clear();
  }

  private ParsedElement getElement(int arrayIndex, int descriptorIndex) {
    ParsedElement element = array.get(arrayIndex);
    if (element != null) {
      return element.getElement(descriptorIndex);
    }
    return null;
  }

  /**
   * Raw strings go to the screen
   */
  public String getRawString() {
    return getString(false, -1, -1);
  }

  public String getRawStringsExceptFirstArrayDescriptor() {
    int firstArrayDescriptorIndex = getFirstArrayDescriptorIndex();
    return getString(false, 0, firstArrayDescriptorIndex);
  }

  private ParsedArrayDescriptor getFirstArrayDescriptor() {
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null && element.isDescriptor()) {
        return (ParsedArrayDescriptor) element;
      }
    }
    return null;
  }

  private int getFirstArrayDescriptorIndex() {
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null && element.isDescriptor()) {
        return i;
      }
    }
    return -1;
  }

  public String getRawString(int arrayIndex, int descriptorIndex) {
    ParsedElement element = array.get(arrayIndex);
    if (element != null) {
      return element.getRawString(descriptorIndex);
    }
    return "";
  }

  public void setRawString(int index, String string) {
    PrimativeTokenizer tokenizer = createTokenizer(string);
    try {
      parseElement(tokenizer.next(), tokenizer, index);
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  /**
   * Returns true only when all numbers in the array are greater then or equal
   * to the number parameter.
   */
  public boolean ge(int number) {
    boolean greaterOrEqual = true;
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null && !element.ge(number)) {
        greaterOrEqual = false;
        break;
      }
    }
    return greaterOrEqual;
  }

  /**
   * Returns true only when all numbers in the array are equal
   * to the number parameter.
   */
  public boolean equals(int number) {
    boolean equal = true;
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null && !element.equals(number)) {
        equal = false;
        break;
      }
    }
    return equal;
  }

  /**
   * Parsable strings are saved to the .prm file.
   * Returns [] if the list is empty.  Returns a list surrounded by brackets.
   */
  public String getParsableString() {
    StringBuffer buffer = new StringBuffer();
    buffer = new StringBuffer(OPEN_SYMBOL.toString());
    buffer.append(getString(true, -1, -1));
    buffer.append(CLOSE_SYMBOL.toString());
    return buffer.toString();
  }

  public String toString() {
    return "[array:" + array + "]";
  }

  /**
   * Clear the array member variable, and add each element of the ParsedElement
   * to the array.
   * @param parsedArray
   */
  public void set(ParsedElement input) {
    array.clear();
    if (input == null) {
      return;
    }
    input.setDebug(debug);
    input.setDefault(defaultValue);
    for (int i = 0; i < input.size(); i++) {
      array.add(input.getElement(i));
    }
  }

  public int size() {
    return array.size();
  }

  public ParsedElement getElement(int index) {
    return array.get(index);
  }

  /**
   * This is a array only if starts with "[" (strip whitespace before calling).
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

  void setRawString(int index, double number) {
    ParsedNumber element;
    element = ParsedNumber.getInstance(type, etomoNumberType, debug, defaultValue, descr);
    element.setRawString(number);
    array.set(index, element);
  }

  void setElement(int index, ParsedElement element) {
    element.setDebug(debug);
    element.setDefault(defaultValue);
    array.set(index, element);
  }

  public void addElement(ParsedElement element) {
    element.setDebug(debug);
    element.setDefault(defaultValue);
    array.add(element);
  }

  void store(final Properties props, String prepend) {
    StringProperty property = new StringProperty(key);
    property.set(getRawString());
    property.store(props, prepend);
  }

  void load(final Properties props, String prepend) {
    StringProperty property = new StringProperty(key);
    property.load(props, prepend);
    setRawString(property.toString());
  }

  /**
   * Create a list of non-null ParsedNumber based on this.array.  Expand the
   * array descriptors to create the entire array.
   * @return list of ParsedNumbers
   */
  public ParsedElementList getParsedNumberExpandedArray(
      ParsedElementList parsedNumberExpandedArray) {
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null) {
        parsedNumberExpandedArray = element
            .getParsedNumberExpandedArray(parsedNumberExpandedArray);
      }
    }
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ParsedElementList(type, etomoNumberType, debug,
          defaultValue, descr);
    }
    return parsedNumberExpandedArray;
  }

  boolean isDescriptor() {
    return false;
  }

  boolean isCollection() {
    return true;
  }

  void removeElement(int index) {
    array.remove(index);
  }

  /**
   * Sets defaultValue and calls setDefaultValue() for each element in the
   * array.
   */
  void setDefault(EtomoNumber input) {
    defaultValue = input;
    array.setDefault(input);
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null) {
        element.setDefault(defaultValue);
      }
    }
  }

  public void setDefault(int input) {
    if (defaultValue == null) {
      defaultValue = new EtomoNumber(etomoNumberType);
    }
    defaultValue.set(input);
    array.setDefault(defaultValue);
    for (int i = 0; i < array.size(); i++) {
      ParsedElement element = array.get(i);
      if (element != null) {
        element.setDefault(defaultValue);
      }
    }
  }

  /**
   * parse the entire array
   * @return the token that is current when the array is parsed
   */
  Token parse(Token token, PrimativeTokenizer tokenizer) {
    array.clear();
    resetFailed();
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
        fail("Missing delimiter: '" + OPEN_SYMBOL + "'");
        return token;
      }
      token = tokenizer.next();
      // Remove any whitespace before the first element.
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseArray(token, tokenizer);
      if (isFailed()) {
        return token;
      }
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
        fail("Missing delimiter: '" + CLOSE_SYMBOL + "'");
        return token;
      }
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    return token;
  }

  /**
   * Returns a raw string or a parsable string.  A raw string and a parsable
   * string from a collection returns basically the same thing but the elements'
   * parsable strings may be different.  Will return the whole string when
   * startIndex is equal to -1 or 0.  Otherwise returns a partial string
   * starting at startIndex.
   * @param parsable - true when getting parsable string, false when getting a raw string
   * @param startIndex - start index
   */
  private String getString(final boolean parsable, int startIndex, int exclusionIndex) {
    if (startIndex == -1) {
      startIndex = 0;
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = startIndex; i < array.size(); i++) {
      if (exclusionIndex != i) {
        ParsedElement element = array.get(i);
        if (debug) {
          System.out.println("i=" + i + ",element=" + element);
        }
        if (element != null) {
          String string;
          if (parsable) {
            string = element.getParsableString();
          }
          else {
            string = element.getRawString();
          }
          if (buffer.length() > 0) {
            buffer.append(DIVIDER_SYMBOL + " ");
          }
          if (parsable && string.matches("\\s*")) {
            buffer.append("NaN");
          }
          else {
            buffer.append(string);
          }
        }
      }
    }
    return buffer.toString();
  }

  private Token parseArray(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    // loop until the end of the array
    // can't just check dividerFound, because whitespace can act as a divider,
    // but isn't always the divider
    while (dividerFound && !isFailed() && token != null && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      if (debug) {
        System.out.println("ParsedArray.parseArray:while");
      }
      try {
        // parse an element
        token = parseElement(token, tokenizer);
        // Find the divider.
        // Whitespace may be used as a divider or the divider may be preceded by
        // whitespace.
        dividerFound = false;
        if (token != null
            && (token.is(Token.Type.WHITESPACE) || token.equals(Token.Type.SYMBOL,
                DIVIDER_SYMBOL.charValue()))) {
          dividerFound = true;
          token = tokenizer.next();
        }
        if (dividerFound) {
          // If whitespace was found, it may precede the divider.
          if (token != null
              && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
            token = tokenizer.next();
          }
        }
        // Don't worry about whitespace after the divider. It should be handled
        // by the element.
      }
      catch (IOException e) {
        e.printStackTrace();
        fail(e.getMessage());
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
  private Token parseElement(Token token, PrimativeTokenizer tokenizer, final int index) {
    // parse element
    // Array descriptors don't have their own open and close symbols, so they
    // look like numbers until to you get to the first divider (":"or "-").
    if (debug) {
      System.out.println("ParsedArray.parseElement:token=" + token + ",type=" + type
          + ",index:" + index);
    }
    ParsedElement element;
    // First assume that there might be an array descriptor.
    ParsedDescriptor descriptor = ParsedDescriptor.getInstance(type, etomoNumberType,
        debug, defaultValue, descr);
    if (descriptor != null) {
      descriptor.setDebug(debug);
      token = descriptor.parse(token, tokenizer);
      // create the correct type of element
      if (descriptor.isEmpty()) {
        // There's nothing there, so its an empty element
        if (index == -1) {
          array.addEmptyElement();
        }
        else {
          array.setEmptyElement(index);
        }
        return token;
      }
      else if (descriptor.wasDividerParsed()) {
        element = descriptor;
      }
      else {
        // If the divider was not found then it is not a descriptor.
        element = descriptor.getElement(0);
      }
    }
    else {
      // ParsedDescriptor would not return an instance so the type is not a type
      // that can have an array descriptor or iterator.
      element = ParsedNumber.getInstance(type, etomoNumberType, debug, defaultValue,
          descr);
      token = element.parse(token, tokenizer);
    }
    if (index == -1) {
      array.add(element);
    }
    else {
      array.set(index, element);
    }
    return token;
  }

  /**
   * Adds either a ParsedArrayDescriptor or a ParsedNumber.
   * @param token
   * @param tokenizer
   * @return current token when done parsing the element
   */
  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    return parseElement(token, tokenizer, -1);
  }
}
