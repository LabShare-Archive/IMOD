package etomo.type;

import java.io.IOException;

import etomo.storage.LogFile;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.Token;
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

  /**
   * Place the entire array into parsed arrays and the individual elements into
   * parsed numbers
   */
  private Integer[] defaultValueArray = null;

  private boolean failed = false;
  private boolean debug = false;

  private ParsedList(ParsedElementType type, EtomoNumber.Type etomoNumberType) {
    this.type = type;
    list = new ParsedElementList(type);
    this.etomoNumberType = etomoNumberType;
  }

  public static ParsedList getMatlabInstance() {
    return new ParsedList(ParsedElementType.MATLAB, null);
  }

  public static ParsedList getMatlabInstance(EtomoNumber.Type etomoNumberType) {
    return new ParsedList(ParsedElementType.MATLAB, etomoNumberType);
  }

  public static ParsedList getStringInstance() {
    return new ParsedList(ParsedElementType.STRING, null);
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
    if (value.trim().charAt(0) == OPEN_SYMBOL.charValue()) {
      return true;
    }
    return false;
  }

  public void setDefaultValue(Integer[] defaultValueArray) {
    this.defaultValueArray = defaultValueArray;
    for (int i = 0; i < list.size(); i++) {
      list.get(i).setDefaultValue(i, defaultValueArray);
    }
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
    return list.get(index).getRawString();
  }

  public void addElement(ParsedElement element) {
    element.setDebug(debug);
    element.setDefaultValue(list.size(), defaultValueArray);
    list.add(element);
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
    for (int i = 0; i < list.size(); i++) {
      list.get(i).setDebug(debug);
    }
  }

  public String getParsableString() {
    StringBuffer buffer = new StringBuffer(OPEN_SYMBOL.toString());
    for (int i = 0; i < list.size(); i++) {
      if (i > 0) {
        buffer.append(DIVIDER_SYMBOL.toString() + " ");
      }
      buffer.append(list.get(i).getParsableString());
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
      if (token == null
          || !token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
        fail();
        return;
      }
      token = tokenizer.next();
      //remove any whitespace before the first element
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
      //if the close symbol wasn't found, fail
      if (token == null
          || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
        fail();
        return;
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail();
    }
  }

  private PrimativeTokenizer createTokenizer(String value) {
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(value);
    try {
      tokenizer.initialize();
    }
    catch (IOException e) {
      e.printStackTrace();
      fail();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
      fail();
    }
    return tokenizer;
  }

  private Token parseList(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until a divider isn't found, this should be the end of the list
    while (dividerFound && !isFailed()) {
      try {
        //parse an element.
        token = parseElement(token, tokenizer);
        //remove any whitespace
        if (token != null && token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        dividerFound = false;
        //if the divider symbol is found, continue parsing elements
        if (token != null
            && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          dividerFound = true;
          token = tokenizer.next();
          //remove whitespace after divider
          if (token != null && token.is(Token.Type.WHITESPACE)) {
            token = tokenizer.next();
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        fail();
        return token;
      }
    }
    return token;
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    //end of list
    if (token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      //Empty element at end of list.  This preserves the the original string.
      //{} means no elements, {,} means two elements.  {'tomo',,} means three elements
      //This is not parsable by matlab and would be added to a .prm file as:
      //{}, {NaN,NaN}, and {1,NaN,NaN}.
      if (list.size() > 0) {
        list.add(ParsedEmptyElement.getInstance(type));
      }
      return token;
    }
    if (token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
      //Found an empty element.
      list.add(ParsedEmptyElement.getInstance(type));
      return token;
    }
    //May have found an element.
    ParsedElement element;
    if (type == ParsedElementType.STRING) {
      element = new ParsedQuotedString();
      element.setDebug(isDebug());
      token = element.parse(token, tokenizer);
    }
    else if (ParsedArray.isArray(token)) {
      element = ParsedArray.getInstance(type, etomoNumberType);
      element.setDebug(isDebug());
      token = element.parse(token, tokenizer);
    }
    else {
      //Array descriptors don't have their own open and close symbols, so they
      //look like numbers until to you get to the first divider (":"or "-").
      ParsedDescriptor descriptor = ParsedDescriptor.getInstance(type,
          etomoNumberType);
      descriptor.setDebug(isDebug());
      token = descriptor.parse(token, tokenizer);
      //create the correct type of element
      if (descriptor.isEmpty()) {
        //There's nothing there, so its an empty element
        list.addEmptyElement();
        return token;
      }
      else if (descriptor.wasDividerParsed()) {
        element = descriptor;
      }
      else {
        //If the divider was not found then it is not a descriptor.
        element = descriptor.getElement(0);
      }
    }
    element.setDefaultValue(list.size(), defaultValueArray);
    list.add(element);
    return token;
  }

  private boolean isDebug() {
    return debug;
  }

  final boolean isFailed() {
    return failed;
  }

  final void resetFailed() {
    failed = false;
  }

  final void fail() {
    failed = true;
  }

  private static final class Type {
    private static final Type NUMERIC = new Type();
    private static final Type STRING = new Type();

    private Type() {
    }
  }
}
