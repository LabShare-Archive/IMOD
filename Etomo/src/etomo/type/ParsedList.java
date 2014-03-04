package etomo.type;

import java.io.IOException;

import etomo.storage.LogFile;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.swing.Token;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: A cell array in Matlab.  An expandable array of elements.
 * The array does not have to be fully populated.
 * Use parse and toString functions to load and retrieve parsable data.
 * The add, set, and get functions refer to raw data.
 * ListParser has two types.  When the STRING type is set, ListParser only parses
 * quoted strings.  When the NUMERIC type is set, ListParser only parses numbers and arrays.</P>
 * 
 * <H4>Matlab Variable Syntax</H4>
 * 
 * <H5>Cell array</H5><UL>
 * <LI>Delimiters: {} (required)
 * <LI>Dividers: "," or " "
 * <LI>Empty cell array: {}
 * <LI>Cells:  cell arrays, regular arrays, array descriptors, numbers, strings
 * <LI>Empty cell:<UL>
 *   <LI>{} (cell array)
 *   <LI>[] (regular array or number)
 *   <LI>'' (string or number)
 *   <LI>NaN (number)</UL></UL>
 * 
 * <H5>Regular array</H5><UL>
 * <LI>Delimiters: [] (required)
 * <LI>Divider: "," or " "
 * <LI>Empty array: []
 * <LI>Elements:  array descriptors, numbers
 * <LI>Empty element: NaN (number)</UL>
 * 
 * <H5>Array descriptor</H5><UL>
 * <LI>Delimiters: none
 * <LI>Divider: ":"
 * <LI>Empty descriptor:<UL>
 *   <LI>With 0 elements: illegal
 *   <LI>With 2 elements (j:k): empty if j > k
 *   <LI>With 3 elements (j:i:k): empty if<UL>
 *     <LI>i == 0 or 
 *     <LI>(i > 0 and j > k) or 
 *     <LI>(i < 0 and j < k)</UL></UL>
 * <LI>Elements: numbers
 * <LI>Number of elements: 2, or 3<UL>
 *   <LI>2 elements (j:k): [j,j+1,...,k]
 *   <LI>3 elements (j:i:k): [j,j+i,j+2i,...,k]</UL>
 * <LI>Empty element: With 2 elements, i is assumed to be 1.</UL>
 * 
 * <H5>Number</H5><UL>
 * <LI>Delimiters: [] or '' (optional - cannot be used inside a regular array)
 * <LI>Empty number: [], '', or NaN</UL>
 * 
 * <H5>String</H5><UL>
 * <LI>Delimiters: ''  (required)
 * <LI>Empty string: ''</UL>
 * 
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
 * <p> Revision 1.18  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.17  2009/02/04 23:30:30  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.16  2008/10/10 20:43:05  sueh
 * <p> bug# 1142 Added clear function.
 * <p>
 * <p> Revision 1.15  2008/09/10 21:08:00  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.
 * <p> arsedElementList will no longer create an empty element, so null returns
 * <p> will happen.
 * <p>
 * <p> Revision 1.14  2008/08/21 00:07:02  sueh
 * <p> bug# 1132 Updated matlab definition comment with info about
 * <p> ParsedArrayDescriptor.getIncrement.  Will hopefully be able to take it out
 * <p> when bug# 1135 is done.
 * <p>
 * <p> Revision 1.13  2008/06/20 20:02:09  sueh
 * <p> bug# 1119 For clarity _NUMBER to MATLAB and NON_MATLAB.
 * <p>
 * <p> Revision 1.12  2008/04/15 21:28:33  sueh
 * <p> bug# 1105 Simplified setting the default.  Added debug and default to
 * <p> constructor.  Added setMinArraySize.
 * <p>
 * <p> Revision 1.11  2008/04/09 00:00:53  sueh
 * <p> bug# 1105 Changed the array used in getParsedNumberExpandedArray
 * <p> to a ParsedElementList because it always holds ParsedNumbers.  Fixed
 * <p> parsing; it was eating up the delimiter whitespace before it could be
 * <p> recognized.
 * <p>
 * <p> Revision 1.10  2008/04/02 02:21:22  sueh
 * <p> bug# 1097 Matching Matlab's syntax.  This simplifies many of the
 * <p> ParsedElement classes because there where too many special cases
 * <p> before.  Now it just follows Matlab's syntax instead of trying to imitate
 * <p> exactly how a field happened to be entered by hand in the .prm file.
 * <p>
 * <p> Revision 1.9  2007/11/06 19:50:28  sueh
 * <p> bug# 1047 Made class compatible with ParsedArray.
 * <p>
 * <p> Revision 1.8  2007/07/10 00:31:52  sueh
 * <p> bug# 1022 Added a comment.
 * <p>
 * <p> Revision 1.7  2007/05/03 21:10:27  sueh
 * <p> bug# 964 Added boolean compactArray.
 * <p>
 * <p> Revision 1.6  2007/04/26 02:47:39  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.5  2007/04/19 21:55:15  sueh
 * <p> bug# 964 Added support for flexible syntax, where an array string in the .prm file
 * <p> can be either an array or a number.  Instead of handling this in MatlabParamFile,
 * <p> will always store in the array, but the string that is writen to the .prm file will look
 * <p> like a number if flexibleSyntax is turned on.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:19:21  sueh
 * <p> bug# 964 Added getRawString(int).
 * <p>
 * <p> Revision 1.3  2007/04/09 21:09:46  sueh
 * <p> bug# 964 Added missing tokenizer.next() call to parseList.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:56:33  sueh
 * <p> bug# 964 Removed getRawString(int) because it is not in use.
 * <p>
 * <p> Revision 1.1  2007/03/30 23:46:15  sueh
 * <p> bug# 964 Parses a Matlab list.
 * <p> </p>
 */
public final class ParsedList {
  public static final String rcsid = "$Id$";

  static final Character OPEN_SYMBOL = new Character('{');
  static final Character CLOSE_SYMBOL = new Character('}');
  static final Character DIVIDER_SYMBOL = new Character(',');

  private final ParsedElementType type;
  private final ParsedElementList list;
  private final EtomoNumber.Type etomoNumberType;
  private final String descr;

  /**
   * Place the entire array into parsed arrays and the individual elements into
   * parsed numbers
   */
  private EtomoNumber defaultValue = null;

  private boolean failed = false;
  private boolean debug = false;
  private String failedMessage = null;
  private boolean missingAttribute = false;

  private ParsedList(ParsedElementType type, EtomoNumber.Type etomoNumberType,
      final String descr) {
    this.type = type;
    this.descr = descr;
    list = new ParsedElementList(type, etomoNumberType, debug, defaultValue, descr);
    this.etomoNumberType = etomoNumberType;
  }

  public static ParsedList getMatlabInstance(final String descr) {
    return new ParsedList(ParsedElementType.MATLAB_NUMBER, null, descr);
  }

  public static ParsedList getMatlabInstance(EtomoNumber.Type etomoNumberType,
      final String descr) {
    return new ParsedList(ParsedElementType.MATLAB_NUMBER, etomoNumberType, descr);
  }

  public static ParsedList getStringInstance(final String descr) {
    return new ParsedList(ParsedElementType.STRING, null, descr);
  }

  /**
   * This is a list only if starts with "{" (ignoring whitespace).
   * @param attribute
   * @return 
   */
  public static boolean isList(ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return false;
    }
    String value = attribute.getValue();
    if (value == null) {
      return false;
    }
    return value.trim().charAt(0) == OPEN_SYMBOL.charValue();
  }

  /**
   * This is a string list only if starts with "{'" (ignoring whitespace).
   * @param attribute
   * @return
   */
  public static boolean isStringList(ReadOnlyAttribute attribute) {
    if (isList(attribute)) {
      // String off the list character and check for the string character
      return ParsedQuotedString.isQuotedString(attribute.getValue().trim().substring(1));
    }
    return false;
  }

  public void setDefault(int input) {
    if (defaultValue == null) {
      defaultValue = new EtomoNumber(etomoNumberType);
    }
    defaultValue.set(input);
    list.setDefault(defaultValue);
    for (int i = 0; i < list.size(); i++) {
      ParsedElement element = list.get(i);
      if (element != null) {
        element.setDefault(defaultValue);
      }
    }
  }

  public void clear() {
    list.clear();
  }

  public int size() {
    return list.size();
  }

  public String toString() {
    return "[list:" + list + "]";
  }

  public ParsedElement getElement(int index) {
    return list.get(index);
  }

  public String getRawString(int index) {
    ParsedElement element = list.get(index);
    if (element != null) {
      return element.getRawString();
    }
    return "";
  }

  public void addElement(ParsedElement element) {
    element.setDebug(debug);
    element.setDefault(defaultValue);
    list.add(element);
  }

  public void setDebug(boolean input) {
    debug = input;
    list.setDebug(input);
    for (int i = 0; i < list.size(); i++) {
      ParsedElement element = list.get(i);
      if (element != null) {
        element.setDebug(debug);
      }
    }
  }

  /**
   * Only effects elements of the list, not list size.
   * @param input
   */
  public String getParsableString() {
    StringBuffer buffer = new StringBuffer();
    buffer.append(OPEN_SYMBOL);
    for (int i = 0; i < list.size(); i++) {
      ParsedElement element = list.get(i);
      if (element != null) {
        String string;
        string = element.getParsableString();
        if (buffer.length() > 1) {
          buffer.append(DIVIDER_SYMBOL + " ");
        }
        if (string.matches("\\s*")) {
          buffer.append(ParsedArray.OPEN_SYMBOL.toString()
              + ParsedArray.CLOSE_SYMBOL.toString());
        }
        else {
          buffer.append(string);
        }
      }
    }
    buffer.append(CLOSE_SYMBOL);
    return buffer.toString();
  }

  /**
   * Parse attribute value and set the list member variable.  May set the valid
   * member variable.
   * @param attribute
   * @param etomoNumberType
   */
  public void parse(ReadOnlyAttribute attribute) {
    list.clear();
    resetFailed();
    if (attribute == null) {
      missingAttribute = true;
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(attribute.getValue());
    Token token = null;
    try {
      token = tokenizer.next();
      if (token == null) {
        return;
      }
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null || !token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
        fail("Missing delimiter: '" + OPEN_SYMBOL + "'");
        return;
      }
      token = tokenizer.next();
      // remove any whitespace before the first element
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseList(token, tokenizer);
      if (isFailed()) {
        return;
      }
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      // if the close symbol wasn't found, fail
      if (token == null || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
        fail("Missing delimiter: '" + CLOSE_SYMBOL + "'");
        return;
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  private PrimativeTokenizer createTokenizer(String value) {
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(value);
    try {
      tokenizer.initialize();
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    return tokenizer;
  }

  private Token parseList(Token token, PrimativeTokenizer tokenizer) {
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

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    try {
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    if (token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
      // Found an empty element.
      list.add(ParsedNumber.getInstance(type, etomoNumberType, isDebug(), defaultValue,
          descr));
      return token;
    }
    // May have found an element.
    ParsedElement element;
    if (type == ParsedElementType.STRING) {
      element = ParsedQuotedString.getInstance(isDebug(), descr);
      token = element.parse(token, tokenizer);
    }
    else if (ParsedArray.isArray(token)) {
      element = ParsedArray.getInstance(type, etomoNumberType, isDebug(), defaultValue,
          descr);
      token = element.parse(token, tokenizer);
    }
    else {
      // Array descriptors don't have their own open and close symbols, so they
      // look like numbers until to you get to the first divider (":"or "-").
      ParsedDescriptor descriptor = ParsedDescriptor.getInstance(type, etomoNumberType,
          isDebug(), defaultValue, descr);
      token = descriptor.parse(token, tokenizer);
      // create the correct type of element
      if (descriptor.isEmpty()) {
        // There's nothing there, so its an empty element
        list.addEmptyElement();
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
    list.add(element);
    return token;
  }

  /**
   * @return an error message if invalid, otherwise null
   */
  public String validate() {
    String errorMessage = null;
    for (int i = 0; i < list.size(); i++) {
      ParsedElement element = list.get(i);
      if (element != null) {
        errorMessage = element.validate();
      }
      if (errorMessage != null) {
        return errorMessage;
      }
    }
    return getFailedMessage();
  }

  /**
   * Returns null if not failed, otherwise returns a string.
   * @return
   */
  final String getFailedMessage() {
    if (!failed || !missingAttribute) {
      return null;
    }
    if (failedMessage == null) {
      return (descr != null ? descr : "") + ": Unable to parse.";
    }
    return failedMessage;
  }

  private boolean isDebug() {
    return debug;
  }

  final boolean isFailed() {
    return failed;
  }

  final void resetFailed() {
    failed = false;
    failedMessage = null;
    missingAttribute = false;
  }

  final void fail(final String message) {
    failed = true;
    failedMessage = (descr != null ? descr : "") + ": " + message;
  }

  private static final class Type {
    private static final Type NUMERIC = new Type();
    private static final Type STRING = new Type();

    private Type() {
    }
  }
}
