package etomo.ui;

import java.io.File;
//import java.io.FileReader;
//import java.lang.IllegalArgumentException;
import java.io.IOException;
import java.lang.IllegalStateException;
//import java.io.StreamTokenizer;
import java.io.FileNotFoundException;
import java.util.Vector;

/**
* <p>Description:
* Parses an autodoc file.  Finds and saves autodoc elements in an Autodoc
* object.
* 
* AutodocParser is not case sensitive.  It stores all text in the original case.
* It retains the original whitespace, except for end of line.  It substitute one
* space for each end of line character in a multi-line value.  Comments are not
* stored.  Messages about syntax errors are sent to System.err.
*
* To Use:
* Construct the class with an Autodoc.
* Run initialize().
* Run parse().
* 
* Testing:
* Do not call initialize() when testing.
* Call test() to test this class.
* Call testPreprocessor() to test only the preprocessor in this class.
* Call testAutodocTokenizer() to test the AutodocTokenizer.
* Call testPrimativeTokenizer() to test the PrimativeTokenizer.
* Call testStreamTokenizer() to test the StreamTokenizer.
*
*
* Language Definition:
*
* Syntax of language definition:
* => equals
* TOKEN
* {  0 or more  }
* [  1 or more  ]
* {n  0 up to n  n}
* (  group together  )
* (boolean value)
* \any token except these\
* | => or
* & => and
* EOL => EOL & EOF
* 
* Definition:
* Autodoc => { (emptyLine) | comment | attribute | section }
* 
* comment => (startOfLine) {1 WHITESPACE 1} COMMENT { \EOL\ } EOL
* 
* section => (startOfLine && delimiterInLine) {1 WHITESPACE 1} OPEN 
*            sectionHeader CLOSE { EmptyLine | attribute }
* 
* sectionHeader => (!startOfLine && delimiterInLine) WORD {1 WHITESPACE 1}
*                  DELIMITER {1 WHITESPACE 1} sectionName
* 
* sectionName = (!startOfLine && delimiterInLine) [ \CLOSE & WHITESPACE & EOL\ ]
* 
* attribute => (startOfLine && delimiterInLine)
*              ( ( WORD { SEPARATOR attribute } ) |
*                ( {1 WHITESPACE 1} DELIMITER {1 WHITESPACE 1} {1 value 1} )
*              )
* 
* value => { \EOL\ } EOL { (!delimiterInLine && !emptyLine && !comment)
*                          { \DELIMITER & EOL\ } EOL
*                        }
* 
* Required Elements:
* Top level attributes (meta data):  Version and Pip.
* 
* Flags:
* The KeyValueDelimiter attribute changes the delimiter string for subsequent
* lines.  It can be used as often as needed and placed anywhere in the autodoc
* file.
* 
* Preprocessor:
* Sets flags decribing the line and position of the current token: emptyLine,
* delimiterInLine, and startOfLine.
* 
* Postprocessor:
* Checks for required elements.
* 
* </p>
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
* <p> $Revision 1.4  2003/12/31 17:47:41  sueh
* <p> $bug# 372 add doc, get file from Autodoc
* <p> $
* <p> $Revision 1.3  2003/12/31 01:27:04  sueh
* <p> $bug# 372 save recognized data, testing, delimiter change on
* <p> $the fly, checking for meta data
* <p> $
* <p> $Revision 1.2  2003/12/23 21:32:25  sueh
* <p> $bug# 372 Reformating.  Creating parser.  Test function for
* <p> $preprocessor.  Fixing preprocessor.
* <p> $
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
  private Vector prevLine = null;
  private int tokenIndex = 0;
  private Autodoc autodoc = null;
  private Token token = null;
  private Token prevToken = null;
  private boolean parsed = false;

  //error flags
  private boolean errorsFound = false;
  private boolean error = false;

  //testing flags
  private boolean test = false;
  private boolean detailedTest = false;
  private boolean testWithTokens = false;
  private int lastTokenType = Token.NULL;
  private int testIndent = -1;

  //Preprocessor flags
  private boolean startOfLine = true;
  private boolean delimiterInLine = false;
  private boolean emptyLine = false;
  private int lineNum = 0;

  //Postprocessor flags
  private boolean versionFound = false;
  private boolean pipFound = false;

  //Values
  private Token sectionTypeStart = null;
  private Token sectionTypeEnd = null;
  private Token sectionNameStart = null;
  private Token sectionNameEnd = null;
  private Token attributeNameStart = null;
  private Token attributeNameEnd = null;
  private Token valueStart = null;
  private Token valueEnd = null;

  public AutodocParser(Autodoc autodoc) {
    if (autodoc == null) {
      throw new IllegalArgumentException("autodoc is null.");
    }
    this.autodoc = autodoc;
    name = new String(autodoc.getName());
    file = autodoc.getFile();
    tokenizer = new AutodocTokenizer(file);
  }

  public void initialize() throws FileNotFoundException, IOException {
    tokenizer.initialize();
  }

  /**
   * Parses an autodoc file.
   * This function can be run only once per instance of the object.
   * @throws IOException
   */
  public void parse() throws IOException {
    if (parsed) {
      return;
    }
    parsed = true;
    testStartFunction("parse");
    nextToken();
    while (!token.is(Token.EOF)) {
      error = false;
      if (emptyLine) {
        nextToken();
        continue;
      }
      if (testEndFunction(comment())) {
        continue;
      }
      if (testEndFunction(section())) {
        continue;
      }
      if (testEndFunction(attribute(autodoc))) {
        processMetaData();
        continue;
      }
      reportError("Unknown statement.");
    }
    postprocess();
    testEndFunction(true);
  }

  /**
   * Parses a comment.
   * @return true if comment found
   * @throws IOException
   */
  private boolean comment() throws IOException {
    testStartFunction("comment");
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
    if (!token.is(Token.EOF)) {
      nextToken();
    }
    return true;
  }

  /**
   * Parses a section.
   * @return true if section found
   * @throws IOException
   */
  private boolean section() throws IOException {
    AttributeCollection section = null;
    testStartFunction("section");
    if (!startOfLine) {
      return false;
    }
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    if (!token.is(Token.OPEN)) {
      return false;
    }
    if (!delimiterInLine) {
      reportError(
        "A section header must contain a delimiter (\""
          + tokenizer.getDelimiterString()
          + "\").");
      return false;
    }
    nextToken();
    section = sectionHeader();
    if (testEndFunction(section == null)) {
      return false;
    }
    if (!token.is(Token.CLOSE)) {
      reportError(
        "A section header must end with '"
          + AutodocTokenizer.CLOSE_CHAR
          + "'.");
      return false;
    }
    nextToken();
    while (!token.is(Token.EOF)) {
      if (token.is(Token.WHITESPACE) || token.is(Token.EOL) || emptyLine) {
        nextToken();
      }
      else {
        if (!testEndFunction(attribute(section))) {
          return true;
        }
      }
    }

    return true;
  }

  /**
   * @return true if sectionHeader found
   * @throws IOException
   */
  private Section sectionHeader() throws IOException {
    testStartFunction("sectionHeader");
    if (startOfLine) {
      reportError(
        "A section header must start with '"
          + AutodocTokenizer.OPEN_CHAR
          + "'.");
      return null;
    }
    if (!delimiterInLine) {
      reportError(
        "A section header must contain a delimiter (\""
          + tokenizer.getDelimiterString()
          + "\").");
      return null;
    }
    if (!token.is(Token.WORD) && !token.is(Token.KEYWORD)) {
      reportError("A section header must contain a section type.");
      return null;
    }
    sectionTypeStart = token;
    sectionTypeEnd = sectionTypeStart;
    nextToken();
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    if (!token.is(Token.DELIMITER)) {
      reportError(
        "A section header must contain a delimiter (\""
          + tokenizer.getDelimiterString()
          + "\").");
      return null;
    }
    nextToken();
    if (token.is(Token.WHITESPACE)) {
      nextToken();
    }
    if (!testEndFunction(sectionName())) {
      reportError("A section header must contain a section name.");
      return null;
    }
    return autodoc.addSection(sectionTypeStart, sectionNameStart);
  }

  /**
   * @return true if sectionName found
   */
  private boolean sectionName() throws IOException {
    testStartFunction("sectionName");
    if (startOfLine) {
      reportError(
        "A section header must start with '"
          + AutodocTokenizer.OPEN_CHAR
          + "'.");
      return false;
    }
    if (!delimiterInLine) {
      reportError(
        "A section header must contain a delimiter (\""
          + tokenizer.getDelimiterString()
          + "\").");
      return false;
    }
    sectionNameStart = token;
    sectionNameEnd = sectionNameStart;
    boolean found = false;
    while (!token.is(Token.CLOSE)
      && !token.is(Token.WHITESPACE)
      && !token.is(Token.EOL)
      && !token.is(Token.EOF)) {
      found = true;
      nextToken();
      sectionNameEnd = sectionNameEnd.setNext(token);
    }
    sectionNameEnd = sectionNameEnd.dropFromList();
    if (!found) {
      sectionNameStart = null;
      reportError("A section header must contain a section name.");
      return false;
    }
    return true;
  }

  /**
   * @return true if attribute found
   * @throws IOException
   */
  private boolean attribute(AttributeCollection attributeCollection) throws IOException {
    Attribute attribute = null;
    testStartFunction("attribute");
    if (!delimiterInLine) {
      reportError(
        "A multi-line value cannot contain embedded empty lines or comments starting with '"
          + AutodocTokenizer.COMMENT_CHAR
          + "'.");
      return false;
    }
    if (!token.is(Token.WORD) && !token.is(Token.KEYWORD)) {
      return false;
    }
    attributeNameStart = token;
    attributeNameEnd = attributeNameStart;
    attribute = (Attribute) attributeCollection.addAttribute(attributeNameStart);
    nextToken();
    if (token.is(Token.SEPARATOR)) {
      while (token.is(Token.SEPARATOR)) {
        nextToken();
        if (!testEndFunction(attribute(attribute))) {
          reportError(
            "An attribute names cannot end with a separator ('"
              + AutodocTokenizer.SEPARATOR_CHAR
              + "').");
          return false;
        }
      }
    }
    else {
      if (token.is(Token.WHITESPACE)) {
        nextToken();
      }
      if (!token.is(Token.DELIMITER)) {
        reportError(
          "A full-line value cannot contain the delimiter string (\""
            + tokenizer.getDelimiterString()
            + "\").");
        return false;
      }
      nextToken();
      if (token.is(Token.WHITESPACE)) {
        nextToken();
      }
      boolean oneLine =
        attributeNameStart.equals(
          Token.KEYWORD,
          AutodocTokenizer.DELIMITER_KEYWORD);
      testEndFunction(value(attribute, oneLine));
    }

    return true;
  }

  /**
   * @return true if value found
   * @throws IOException
   */
  private boolean value(Attribute attribute, boolean oneLine)
    throws IOException {
    testStartFunction("value");
    valueStart = token;
    valueEnd = valueStart;
    boolean found = false;
    while (!token.is(Token.EOL) && !token.is(Token.EOF)) {
      found = true;
      nextToken();
      valueEnd = valueEnd.setNext(token);
    }
    if (!oneLine) {
      nextToken();
      while (!delimiterInLine
        && !emptyLine
        && !token.is(Token.EOF)
        && !testEndFunction(comment())) {
        valueEnd = valueEnd.setNext(token);
        while (!token.is(Token.DELIMITER)
          && !token.is(Token.EOL)
          && !token.is(Token.EOF)) {
          found = true;
          nextToken();
          valueEnd = valueEnd.setNext(token);
        }
        if (token.is(Token.DELIMITER)) {
          valueStart = null;
          reportError(
            "A full-line value cannot contain the delimiter string (\""
              + tokenizer.getDelimiterString()
              + "\").");
          return false;
        }
        nextToken();
      }
    }
    if (found) {
      valueEnd = valueEnd.dropFromList();
      attribute.setValue(valueStart);
      if (oneLine) {
        if (attributeNameStart
          .equals(Token.KEYWORD, AutodocTokenizer.DELIMITER_KEYWORD)) {
          tokenizer.setDelimiterString(valueStart.getValue(true));
        }
        nextToken();
      }

    }
    else {
      valueStart = null;
    }
    return found;
  }

  private void reportError(String message) throws IOException {
    if (error) {
      return;
    }
    if (message == null) {
      message = "Unknown error.";
    }
    error = true;
    String errorMessage =
      new String("Line# " + lineNum + ": " + token + ": " + message);
    int errorIndex = tokenIndex - 1;
    int size = line.size();
    Token token = null;
    String value = null;
    int errorPosition = 0;
    int index = 0;
    StringBuffer printLine = new StringBuffer(124);
    StringBuffer carat = new StringBuffer(124);
    for (index = 0; index <= errorIndex && index < size - 1; index++) {
      token = (Token) line.get(index);
      value = token.getValue();
      if (index < errorIndex) {
        errorPosition += value.length();
      }
      printLine.append(value);
    }
    for (index = errorIndex + 1; index < size - 1; index++) {
      token = (Token) line.get(index);
      printLine.append(token.getValue());
    }
    for (index = 0; index < errorPosition; index++) {
      carat.append(' ');
    }
    carat.append("^");
    if (!errorsFound) {
      errorsFound = true;
      System.err.println("Errors in " + name);
      System.err.println();
    }
    System.err.println(errorMessage);
    System.err.println(printLine);
    System.err.println(carat);
    System.err.println();
    if (detailedTest) {
      throw new IllegalStateException();
    }
    tokenIndex = line.size();
    nextToken();
  }

  /**
   * set token level preprocessor flag: startOfLine
   */
  private void nextToken() throws IOException {
    if (line == null || tokenIndex == line.size()) {
      preprocess();
      tokenIndex = 0;
    }
    if (test && tokenIndex == 0) {
      printTestLine();
    }
    prevToken = token;
    token = (Token) line.get(tokenIndex);
    if (tokenIndex == 0
      || (tokenIndex == 1 && prevToken.is(Token.WHITESPACE))) {
      startOfLine = true;
    }
    else {
      startOfLine = false;
    }
    tokenIndex++;
    if (detailedTest) {
      System.out.println(
        tokenIndex
          + ":"
          + token
          + ",startOfLine="
          + startOfLine
          + ",emptyLine="
          + emptyLine
          + ",delimiterInLine="
          + delimiterInLine);
    }
  }

  /**
   * set line level preprocessor flags: emptyLine, delimiterInLine, lineNum
   */
  private void preprocess() throws IOException {
    Token token = null;
    lineNum++;
    if (line != null) {
      prevLine = line;
    }
    line = new Vector();
    delimiterInLine = false;
    emptyLine = true;
    do {
      token = tokenizer.next();
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
    }
    while (!token.is(Token.EOL) && !token.is(Token.EOF));
  }

  //postprocessor

  private void processMetaData() {
    if (attributeNameStart
      .equals(Token.KEYWORD, AutodocTokenizer.VERSION_KEYWORD)) {
      versionFound = true;
      return;
    }
    if (attributeNameStart
      .equals(Token.KEYWORD, AutodocTokenizer.PIP_KEYWORD)) {
      pipFound = true;
      return;
    }
  }

  private void postprocess() {
    if (!versionFound) {
      System.err.println();
      System.err.println("Missing meta data:  Version not found.");
      System.err.println();
    }
    if (!pipFound) {
      System.err.println();
      System.err.println("Missing meta data:  Pip not found.");
      System.err.println();
    }
  }

  public void testStreamTokenizer(boolean tokens) throws IOException {
    tokenizer.testStreamTokenizer(tokens);
  }

  public void testPrimativeTokenizer(boolean tokens) throws IOException {
    tokenizer.testPrimativeTokenizer(tokens);
  }

  public void testAutodocTokenizer(boolean tokens) throws IOException {
    tokenizer.test(tokens);
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
  
  /**
   * 
   * @param tokens: display tokens rather then text
   * @throws IOException
   */
  public void test(boolean tokens) throws IOException {
    test = true;
    testWithTokens = tokens;
    initialize();
    parse();
  }

  /**
   * 
   * @param tokens: display tokens rather then text.
   * @param details: display more information and throw an exception as the
   *                 first error.
   * @throws IOException
   */
  public void test(boolean tokens, boolean details) throws IOException {
    detailedTest = true;
    test(tokens);
  }

  private void testStartFunction(String functionName) {
    if (test) {
      testIndent++;
      printTestIndent();
      System.out.println(functionName + " {");
    }
  }

  private boolean testEndFunction(boolean result) {
    //System.out.println("in testEndFunction");
    if (test) {
      printTestIndent();
      if (result) {
        System.out.println("} succeeded");
      }
      else {
        System.out.println("} failed");
      }
      testIndent--;
    }
    return result;
  }

  private void printTestLine() {
    Token token = null;
    int size = line.size();
    printTestIndent();
    System.out.print("\tLine# " + lineNum + ": ");
    for (int index = 0; index < size; index++) {
      token = (Token) line.get(index);
      if (testWithTokens) {
        System.out.print(token);
      }
      else if (!token.is(Token.EOL) && !token.is(Token.EOF)) {
        System.out.print(token.getValue());
      }
    }
    System.out.println();
  }

  private void printTestIndent() {
    for (int i = 0; i < testIndent; i++) {
      System.out.print("\t");
    }
  }

}
