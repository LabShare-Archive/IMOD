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
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$$ </p>
*/

public class AutodocParser {
  public static final String rcsid = "$$Id$$";

  private static final String zeroOrMore = new String("zeroOrMore");
  private AutodocTokenizer tokenizer;
  private String name = null;
  private File file = null;
  private Vector line = null;
  private int lineIndex = 0;
  private Autodoc autodoc = null;
  private boolean startOfLine = true;
  private boolean delimiterInLine = false;

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
  
  
  public void parse() {
  }
  
  private boolean emptyLine() {
    return true;
  }
  
  private boolean attribute() {
    return true; 
  }
  
  private boolean group() {
    return true;
  }
  
  private boolean comment() {
    return true;
  }
  
  private boolean value() {
    return true;
  }
  
  private boolean nameValuePair() {
    return true;
  }
  
  private boolean oneLineValue() {
    return true;
  }
  
  private Token getToken() throws IOException {
    if (lineIndex == line.size()) {
      preprocess();
    }
    Token token = (Token) line.get(lineIndex);
    if (startOfLine && !token.is(Token.WHITESPACE)) {
      startOfLine = false;
    }
    return null;
  }
  
  private void preprocess() throws IOException {
    Token token = null;
    if (line == null) {
      line = new Vector();
    }
    else {
      line.clear();
      lineIndex = 0;
      startOfLine = true;
      delimiterInLine = false;
    }
    do {
      token = new Token(tokenizer.next());
      if (token.is(Token.DELIMITER)) {
        delimiterInLine = true;
      }
      line.add(lineIndex, token);
    } while (!token.is(Token.EOL) || !token.is(Token.EOF));
  }
  
  public void testStreamTokenizer(boolean tokens) throws IOException {
    tokenizer.testStreamTokenizer(tokens);
  }
  
  public void testPrimativeTokenizer(boolean tokens) throws IOException {
    tokenizer.testPrimativeTokenizer(tokens);
  }
  
  public void testAutodocTokenizer(boolean tokens) throws IOException {
    tokenizer.initialize();
    Token token = tokenizer.next();
    while (!token.is(Token.EOF)) {
      if (tokens) {
        System.out.println(token.toString());
      }
      if (token.is(Token.EOL) && !tokens) {
        System.out.println();
      }
      else {
        if (!tokens) {
          System.out.print(token.getValue());
        }
      }
      token = tokenizer.next();
    }
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
