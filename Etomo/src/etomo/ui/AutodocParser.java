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
* stored.  Messages about syntax errors are sent to System.err.  It is extremely
* important to keep the language definition up to date.
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
*            sectionHeader CLOSE { EmptyLine | comment | attribute }
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
* value => { \EOL\ } EOL  
*          { (!delimiterInLine && !emptyLine && !comment)
*            (startOfLine) {1 (breakInLine) {1 WHITESPACE 1} BREAK {1 INDENT 1} 1}
*            { \DELIMITER & EOL\ } EOL
*          }
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
* <p> $Revision 1.7  2005/09/01 17:59:47  sueh
* <p> $bug# 532 Allow a comment inside a section.  Changed the language
* <p> $definition and section().
* <p> $
* <p> $Revision 1.6  2005/02/15 19:51:54  sueh
* <p> $bug# 602 Preprocessor:  Converting BREAK token to WORD when it is
* <p> $not at the beginning of a line.  Converting the spaces following a BREAK
* <p> $to an INDENT.
* <p> $Parcer:  Saving BREAKS and INDENTS.  In value(), ignoring
* <p> $WHITESPACE that preceeds a BREAK.
* <p> $
* <p> $Revision 1.5  2004/01/01 00:45:17  sueh
* <p> $bug# 372 correcting interface name
* <p> $
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
  private boolean breakInLine = false;
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
    file = autodoc.getAutodocFile();
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
    //System.out.println("0token="+token);
    if (parsed) {
      return;
    }
    parsed = true;
    testStartFunction("parse");
    nextToken();
    //System.out.println("1token="+token);
    while (!token.is(Token.EOF)) {
      error = false;
      if (emptyLine) {
        nextToken();
        //System.out.println("2token="+token);
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
    //System.out.println("0token="+token);
    AttributeCollection section = null;
    testStartFunction("section");
    if (!startOfLine) {
      return false;
    }
    if (token.is(Token.WHITESPACE)) {
      nextToken();
      //System.out.println("1token="+token);
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
    //System.out.println("2token="+token);
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
    //System.out.println("3token="+token);
    while (!token.is(Token.EOF)) {
      if (token.is(Token.WHITESPACE) || token.is(Token.EOL) || emptyLine) {
        nextToken();
        //System.out.println("4token="+token);
      }
      else if (!testEndFunction(comment())) {
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
    //System.out.println("0token="+token);
    Attribute attribute = null;
    testStartFunction("attribute");
    if (!delimiterInLine) {
      reportError(
        "A multi-line value cannot contain embedded empty lines or comments starting with '"
          + AutodocTokenizer.COMMENT_CHAR
          + "'.");
      //System.out.println("1return false");
      return false;
    }
    if (!token.is(Token.WORD) && !token.is(Token.KEYWORD)) {
      //System.out.println("2return false");
      //new Exception().printStackTrace();
      return false;
    }
    attributeNameStart = token;
    attributeNameEnd = attributeNameStart;
    attribute = (Attribute) attributeCollection.addAttribute(attributeNameStart);
    nextToken();
    //System.out.println("1token="+token);
    if (token.is(Token.SEPARATOR)) {
      while (token.is(Token.SEPARATOR)) {
        nextToken();
        //System.out.println("2token="+token);
        if (!testEndFunction(attribute(attribute))) {
          reportError(
            "An attribute names cannot end with a separator ('"
              + AutodocTokenizer.SEPARATOR_CHAR
              + "').");
          //System.out.println("3return false");
          return false;
        }
      }
    }
    else {
      if (token.is(Token.WHITESPACE)) {
        nextToken();
        //System.out.println("3token="+token);
      }
      if (!token.is(Token.DELIMITER)) {
        reportError(
          "A full-line value cannot contain the delimiter string (\""
            + tokenizer.getDelimiterString()
            + "\").");
        //System.out.println("4return false");
        return false;
      }
      nextToken();
      //System.out.println("4token="+token);
      if (token.is(Token.WHITESPACE)) {
        nextToken();
        //System.out.println("5token="+token);
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
   * 
   * @param attribute
   * @param oneLine - if its a known one line attribute, such as the keyword to
   * set the delimiter
   * @return
   * @throws IOException
   */
  private boolean value(Attribute attribute, boolean oneLine)
    throws IOException {
    //System.out.println("attribute="+attribute+",oneLine="+oneLine+",token="+token);
    testStartFunction("value");
    valueStart = token;
    valueEnd = valueStart;
    boolean found = false;
    //{ \EOL\ }
    while (!token.is(Token.EOL) && !token.is(Token.EOF)) {
      found = true;
      nextToken();
      //System.out.println("1token="+token);
      valueEnd = valueEnd.setNext(token);
    }
    //{ (!delimiterInLine && !emptyLine && !comment)
    //            (startOfLine) {1 (breakInLine) {1 WHITESPACE 1} BREAK {1 INDENT 1} 1}
    //            { \DELIMITER & EOL\ } EOL
    //}
    if (!oneLine) {
      nextToken();
      //System.out.println("2token="+token);
      while (!delimiterInLine
        && !emptyLine
        && !token.is(Token.EOF)
        && !testEndFunction(comment())) {
        valueEnd = valueEnd.setNext(token);
        //(startOfLine) {1 (breakInLine) {1 WHITESPACE 1} BREAK {1 INDENT 1} 1}
        if (startOfLine && breakInLine) {
          //ignore whitespace before a break
          if (token.is(Token.WHITESPACE)) {
            valueEnd = valueEnd.dropFromList();
            nextToken();
            //System.out.println("3token="+token);
          }
          if (!token.is(Token.BREAK)) {
            reportError("breakInLine is true, but BREAK is missing (" + token + ").");
              return false;
          }
          //found break
          nextToken();
          //System.out.println("4token="+token);
          valueEnd = valueEnd.setNext(token);
          //find optional indent
          if (token.is(Token.INDENT)) {
            nextToken();
            //System.out.println("5token="+token);
            valueEnd = valueEnd.setNext(token);
          }
        }
        while (!token.is(Token.DELIMITER)
          && !token.is(Token.EOL)
          && !token.is(Token.EOF)) {
          found = true;
          nextToken();
          //System.out.println("6token="+token);
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
        //System.out.println("7token="+token);
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
        System.out.println("8token="+token);
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
      if (test && line != null) {
        printTestLine();
      }
      preprocess();
      tokenIndex = 0;
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
          + delimiterInLine
          + ",breakInLine="
          + breakInLine);
    }
  }

  /**
   * set line level preprocessor flags: emptyLine, delimiterInLine, lineNum, breakInLine
   */
  private void preprocess() throws IOException {
    boolean breakDetected = false;
    Token token = null;
    lineNum++;
    if (line != null) {
      prevLine = line;
    }
    line = new Vector();
    delimiterInLine = false;
    emptyLine = true;
    breakInLine = false;
    do {
      token = tokenizer.next();
      //Handle an indent following a break:
      //If the next token is whitespace, make all the spaces following the break
      //character into an indent token.
      if (breakDetected) {
        breakDetected = false;
        if (token.is(Token.WHITESPACE)) {
          int indentSize = token.numberOf(' ', 0);
          if (indentSize != 0) {
            if (indentSize == token.length()) {
              token.set(Token.INDENT);
            }
            else {
              line.add(token.split(Token.INDENT, 0, indentSize));
            }
          }
        }
      }
      //BREAK token only functions as a break if it is at the beginning of the
      //line, except for whitespace.
      if (token.is(Token.BREAK)) {
        if (emptyLine) {
          breakDetected = true;
          breakInLine = true;
        }
        else {
          token.set(Token.WORD);
        }
      }
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
      System.out.println("(type,value):startOfLine,emptyLine,delimiterInLine,breakInLine");
    }
    tokenizer.initialize();
    do {
      nextToken();
      if (tokens) {
        System.out.println(token.toString() + ":" + startOfLine + ","
            + emptyLine + "," + delimiterInLine + "," + breakInLine);
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
