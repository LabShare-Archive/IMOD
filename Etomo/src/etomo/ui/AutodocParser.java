package etomo.ui;

import java.io.File;
//import java.io.FileReader;
//import java.lang.IllegalArgumentException;
import java.io.IOException;
//import java.lang.IllegalStateException;
//import java.io.StreamTokenizer;
import java.io.FileNotFoundException;
import java.util.Vector;

/**
* <p>Description:</p>
*
* <p>Copyright: Copyright © 2002, 2003</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$
* <p> $Revision 1.1  2003/12/22 23:48:50  sueh
* <p> $bug# 372 parser and preprocessor for autodoc files.  Stores
* <p> $autodoc data.
* <p> $$ </p>
*/

public class AutodocParser {
  public static final String rcsid =
    "$$Id$$";

  private AutodocTokenizer tokenizer;
  private String name = null;
  private File file = null;
  private Vector line = null;
  private int lineIndex = 0;
  private Autodoc autodoc = null;
  Token token = null;
  //Preprocessor flags
  private boolean startOfLine = true;
  private boolean delimiterInLine = false;
  private boolean emptyLine = false;

  public AutodocParser(Autodoc autodoc, File file) {
    if (autodoc == null) {
      throw new IllegalArgumentException("autodoc is null.");
    }
    if (file == null) {
      throw new IllegalArgumentException("file is null.");
    }
    this.autodoc = autodoc;
    name = new String(autodoc.getName());
    this.file = file;
    tokenizer = new AutodocTokenizer(file);
  }

  public void initialize() throws FileNotFoundException, IOException {
    tokenizer.initialize();
  }

  /**
   * Autodoc => { (emptyLine) | comment | attribute | section }
   * 
   * @throws IOException
   */
  public void parse() throws IOException {
    do {
      nextToken();
      if (emptyLine) {
        break;
      }
      if (comment()) {
        break;
      }
      if (section()) {
        break;
      }
      if (attribute()) {
        break;
      }
    }
    while (!token.is(Token.EOF));
  }

  /**
   * comment => (startOfLine) {1 WHITESPACE 1} COMMENT { \EOL\ } EOL
   * 
   * @return true if comment found
   * @throws IOException
   */
  private boolean comment() throws IOException {
    if (!startOfLine) {
      return false;
    }
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    if (!token.is(Token.COMMENT)) {
      return false;
    }
    nextToken();
    while (!token.is(Token.EOL) && !token.is(Token.EOF)) {
      nextToken();
    }

    return true;
  }

  /**
   * section => (startOfLine && delimiterInLine) {1 WHITESPACE 1} OPEN sectionHeader CLOSE { EmptyLine | attribute }
   * 
   * @return true if section found
   * @throws IOException
   */
  private boolean section() throws IOException {
    if (!startOfLine || !delimiterInLine) {
      return false;
    }
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    if (!token.is(Token.OPEN)) {
      return false;
    }
    nextToken();
    if (!sectionHeader()) {
      return false;
    }
    if (!token.is(Token.CLOSE)) {
      return false;
    }
    nextToken();
    while (!token.is(Token.EOF)) {
      if (emptyLine) {
        nextToken();
      }
      else {
        if (!attribute()) {
          return true;
        }
      }
    }

    return true;
  }

  /**
   * sectionHeader => (!startOfLine && delimiterInLine) WORD {1 WHITESPACE 1} DELIMITER {1 WHITESPACE 1} sectionName
   * 
   * @return true if sectionHeader found
   * @throws IOException
   */
  private boolean sectionHeader() throws IOException {
    if (startOfLine || !delimiterInLine) {
      return false;
    }
    if (!token.is(Token.WORD)) {
      return false;
    }
    nextToken();
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    nextToken();
    if (!token.is(Token.DELIMITER)) {
      return false;
    }
    nextToken();
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    if (!sectionName()) {
      return false;
    }
    return true;
  }

  /**
   * sectionName = (!startOfLine && delimiterInLine) [ \CLOSE,WHITESPACE,EOL\ ]
   * 
   * @return true if sectionName found
   */
  private boolean sectionName() throws IOException {
    if (startOfLine || !delimiterInLine) {
      return false;
    }
    boolean found = false;
    while (!token.is(Token.CLOSE)
      && !token.is(Token.WHITESPACE)
      && !token.is(Token.EOL)
      && !token.is(Token.EOF)) {
      found = true;
      nextToken();
    }
    if (!found) {
      return false;
    }
    return true;
  }

  /**
   * attribute => (startOfLine && delimiterInLine) ( ( WORD { SEPARATOR attribute } ) | ( {1 WHITESPACE 1} DELIMITER {1 WHITESPACE 1} {1 value 1} ) )
   * 
   * @return true if attribute found
   * @throws IOException
   */
  private boolean attribute() throws IOException {
    if (!startOfLine || !delimiterInLine) {
      return false;
    }
    if (!token.is(Token.WORD)) {
      return false;
    }
    nextToken();
    if (token.is(Token.SEPARATOR)) {
      while (token.is(Token.SEPARATOR) && !token.is(Token.EOF)) {
        if (!attribute()) {
          return false;
        }
      }
    }
    else {
      if (token.is(Token.WHITESPACE)) {
        nextToken();
      }
      if (!token.is(Token.DELIMITER)) {
        return false;
      }
      nextToken();
      if (token.is(Token.WHITESPACE)) {
        nextToken();
      }
      value();
    }

    return true;
  }

  /**
   * value => { \EOL\ } EOL { (!delimiterInLine && !emptyLine) { \DELIMITER,EOL\ } EOL }
   * 
   * @return true if value found
   * @throws IOException
   */
  private boolean value() throws IOException {
    boolean found = false;
    while (!token.is(Token.EOL) && !token.is(Token.EOF)) {
      found = true;
      nextToken();
    }
    if (!token.is(Token.EOF)) {
      nextToken();
    }
    while (!delimiterInLine && !emptyLine && !token.is(Token.EOF)) {
      while (!token.is(Token.DELIMITER)
        && !token.is(Token.EOL)
        && !token.is(Token.EOF)) {
        found = true;
        nextToken();
      }
      if (token.is(Token.DELIMITER)) {
        return false;
      }
      if (!token.is(Token.EOF)) {
        nextToken();
      }
    }
    return found;
  }

  /**
   * set token level preprocessor flag: startOfLine
   */
  private void nextToken() throws IOException {
    if (line == null || lineIndex == line.size()) {
      preprocess();
    }
    token = (Token) line.get(lineIndex);
    lineIndex++;
    if (startOfLine
      && (!token.is(Token.WHITESPACE)
        && !token.is(Token.EOL)
        && !token.is(Token.EOF))) {
      startOfLine = false;
    }
  }

  /**
   * set line level preprocessor flags: emptyLine, delimiterInLine
   */
  private void preprocess() throws IOException {
    Token token = null;
    if (line == null) {
      line = new Vector();
    }
    else {
      line.clear();
    }
    lineIndex = 0;
    startOfLine = true;
    delimiterInLine = false;
    emptyLine = true;
    do {
      token = new Token(tokenizer.next());
      if (emptyLine
        && (!token.is(Token.WHITESPACE)
          && !token.is(Token.EOL)
          && !token.is(Token.EOF))) {
        emptyLine = false;
      }
      if (token.is(Token.DELIMITER)) {
        delimiterInLine = true;
      }
      line.add(token);
      ;
    }
    while (!token.is(Token.EOL) && !token.is(Token.EOF));
  }

  public void testStreamTokenizer(boolean tokens) throws IOException {
    tokenizer.testStreamTokenizer(tokens);
  }

  public void testPrimativeTokenizer(boolean tokens) throws IOException {
    tokenizer.testPrimativeTokenizer(tokens);
  }

  public void testAutodocTokenizer(boolean tokens) throws IOException {
    tokenizer.initialize();
    Token token = null;
    do {
      token = tokenizer.next();
      if (tokens) {
        System.out.println(token.toString());
      }
      else if (token.is(Token.EOL)) {
        System.out.println();
      }
      else if (!token.is(Token.EOF)) {
        System.out.print(token.getValue());
      }
    }
    while (!token.is(Token.EOF));
  }

  public void testPreprocessor(boolean tokens) throws IOException {
    if (tokens) {
      System.out.println("(type,value):startOfLine,emptyLine,delimiterInLine");
    }
    tokenizer.initialize();
    do {
      nextToken();
      if (tokens) {
        System.out.println(
          token.toString()
            + ":"
            + startOfLine
            + ","
            + emptyLine
            + ","
            + delimiterInLine);
      }
      else if (token.is(Token.EOL)) {
        System.out.println();
      }
      else if (!token.is(Token.EOF)) {
        System.out.print(token.getValue());
      }
    }
    while (!token.is(Token.EOF));
  }

}

/*  public void parse() throws IOException {
    if (token == null) {
      initialize();
    }
    boolean versionFound = false;
    boolean pipFound = false;

    getEol(zeroOrMore);
    while (tokenEquals(AutodocTokenizer.DELIMITER_CHANGE_KEYWORD)
      || tokenEquals(AutodocTokenizer.VERSION_KEYWORD)
      || tokenEquals(AutodocTokenizer.PIP_KEYWORD)
      || tokenEquals(AutodocTokenizer.WORD)) {
      if (tokenEquals(AutodocTokenizer.VERSION_KEYWORD)) {
        versionFound = attributePair();
      }
      else if (tokenEquals(AutodocTokenizer.PIP_KEYWORD)) {
        pipFound = attributePair();
      }
      else if (tokenEquals(AutodocTokenizer.DELIMITER_CHANGE_KEYWORD)) {
        delimiterChange();
      }
      else {
        attributePair();
      }
      if (failed) {
        return;
      }
      getEol(zeroOrMore);
    }

    while (tokenEquals(AutodocTokenizer.OPEN_BRACKET)) {
      section();
    }

    if (!versionFound) {
      fail("Version information not found");
    }
    else if (!pipFound) {
      fail("PIP setting not found");
    }
  }

  private boolean attributePair() throws IOException {
    if (failIf(isEof(), true, "unexpected end of file in an attribute")) {
      return false;
    }
    if (failIf(token.isSol(),
      false,
      "only whitespace may appear on a line before an attribute",
      token.getValue())) {
      return false;
    }
    if (!tokenEquals(AutodocTokenizer.DELIMITER_CHANGE_KEYWORD)
      && !tokenEquals(AutodocTokenizer.VERSION_KEYWORD)
      && !tokenEquals(AutodocTokenizer.PIP_KEYWORD)
      && !tokenEquals(AutodocTokenizer.WORD)) {
      fail("invalid attribute name", token.getValue());
      return false;
    }
    String attributeName = token.getValue();

    token.next();
    if (failIf(isEof(), true, "unexpected end of file in an attribute")) {
      return false;
    }
    if (failIf(tokenEquals(AutodocTokenizer.DELIMITER),
      false,
      "illegal delimiter in an attribute",
      token.getValue())) {
      return false;
    }

    token.next();
    if (failIf(isEof(), true, "unexpected end of file in an attribute")) {
      return false;
    }
    String value = fullValue();
    if (failed) {
      return false;
    }
    if (!map.setAttributePair(attributeName, value) && !processErrorLevel()) {
      return false;
    }

    return !failed;
  }

  private boolean delimiterChange() throws IOException {
    if (failIf(isEof(), true,
      "unexpected end of file in a delimiter change")) {
      return false;
    }
    if (failIf(token.isSol(), false,
      "only whitespace may appear on a line before a delimiter change",
      token.getValue())) {
      return false;
    }
    if (failIf(tokenEquals(AutodocTokenizer.DELIMITER_CHANGE_KEYWORD),
      false,
      "invalid attribute name in a delimiter change",
      token.getValue())) {
      return false;
    }
    String attributeName = token.getValue();

    token.next();
    if (failIf(isEof(),
      true,
      "unexpected end of file in a delimiter change.")) {
      return false;
    }
    if (failIf(tokenEquals(AutodocTokenizer.DELIMITER),
      false,
      "illegal delimiter in a delimiter change",
      token.getValue())) {
      return false;
    }

    token.next();
    if (isEol()) {
      token.next();
    }
    if (failIf(tokenEquals(AutodocTokenizer.SPECIAL_CHARACTER),
      false,
      "illegal new delimiter character",
      token.getValue())) {
      return false;
    }
    StringBuffer value = new StringBuffer();
    value.append(token.getValue());
    while (tokenEquals(AutodocTokenizer.SPECIAL_CHARACTER)) {
      value.append(token.getValue());
      token.next();
    }
    if (!isEol() && !isEof()) {
      fail(
        "only whitespace may appear on a line after a delimiter change",
        token.getValue());
      return false;
    }

    if (!map.setAttributePair(attributeName, value.toString()) && !processErrorLevel()) {
      return false;
    }
    changeDelimiter(value.toString());

    token.next();
    return true;

  }

  private void section() throws IOException {
    token.next();
    while (!tokenEquals(AutodocTokenizer.OPEN_BRACKET) && !isEof() && !tokenEquals(AutodocTokenizer.ERROR)) {
      token.next();
    }
  }

  private String fullValue() throws IOException {
    StringBuffer value = new StringBuffer();
    while (!isEol() && !isEof() && !tokenEquals(AutodocTokenizer.ERROR)) {
      token.next();
      value.append(token.getValue());
    }
    return value.toString();
  }


  private boolean isEol() {
    return tokenEquals(AutodocTokenizer.END_OF_LINE);
  }


  private boolean getEol(String amount) throws IOException {
    if (amount == null) {
      throw new IllegalArgumentException("amount is null.");
    }
    if (amount.equals(zeroOrMore)) {
      while (tokenEquals(AutodocTokenizer.END_OF_LINE)) {
        token.next();
      }
      return true;
    }
    throw new IllegalArgumentException(
      "amount has an illegal value, " + amount + ".");
  }

  private boolean isEof() {
    return tokenEquals(AutodocTokenizer.END_OF_FILE);
  }

  private boolean tokenEquals(int type) {
    return token.getType() == type;
  }

  private void changeDelimiter(String newDelimiter) {
    token.setDelimiterString(newDelimiter);
  }

  private void warn(String message) {
    if (message == null) {
      throw new IllegalArgumentException("autodocFile is null.");
    }
    errorMessage = new String("Warning in " + fileName + ", line# " + token.getLineno() + ": " + message + ".");
  }
  
  private void fail(String message) {
    if (message == null) {
      throw new IllegalArgumentException("autodocFile is null.");
    }
    errorMessage =
      new String("Error in " + fileName + ", line# " + token.getLineno() + ": " + message + ".");
    failed = true;
    System.err.println(errorMessage);
  }

  private void fail(String message, String tokenValue) {
    if (message == null) {
      throw new IllegalArgumentException("autodocFile is null.");
    }
    if (tokenValue == null) {
      fail(message);
    }
    failed = true;
    errorMessage =
      new String("Error in " + fileName + ", line# " + token.getLineno() + ", " + tokenValue + ": " + message + ".");
    System.err.println(errorMessage);
  }

  private boolean failIf(
    boolean test,
    boolean failureState,
    String message,
    String tokenValue) {
    if (test == failureState) {
      fail(message, tokenValue);
      return true;
    }
    return false;
  }

  private boolean failIf(boolean test, boolean failureState, String message) {
    if (test == failureState) {
      fail(message);
      return true;
    }
    return false;
  }
  
  private boolean processErrorLevel() {
    int errorLevel = map.getErrorLevel();
    if (errorLevel == AutodocMap.NO_ERROR) {
      throw new IllegalStateException("Incorrect error level, " + errorLevel + " in AutodocMap.");
    }
    else if (errorLevel == AutodocMap.WARNING) {
      warn(map.getErrorMessage());
    }
    else if (errorLevel == AutodocMap.ERROR) {
      fail(map.getErrorMessage());
      return false;
    }
    else {
      throw new IllegalStateException("Unknown error level, " + errorLevel + "In AutodocMap.");
    }
    return true;
  }

  public AutodocMap getMap() {
    map.lock();
    AutodocMap constMap = (AutodocMap) map;
    return constMap;
  }

  public final boolean isFailed() {
    return failed;
  }

  public final String getErrorMessage() {
    return errorMessage;
  }
*/
