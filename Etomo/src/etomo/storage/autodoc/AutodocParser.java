package etomo.storage.autodoc;

import java.io.File;
//import java.io.FileReader;
//import java.lang.IllegalArgumentException;
import java.io.IOException;
import java.lang.IllegalStateException;
//import java.io.StreamTokenizer;
import java.io.FileNotFoundException;
import java.util.Vector;

import etomo.ui.Token;

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
 * Valid Autodoc Tokens:
 * 
 * Special Character Tokens:
 * OPEN => [
 * CLOSE => ]
 * SEPARATOR => .
 * BREAK => ^ (formatting character - ignored except in valueline at startline)
 * COMMENT => #
 * 
 * Other Tokens:
 * EOL => end of line
 * EOF => end of file
 * DELIMETER => A definable string of non-alphnumeric characters.  May not
 *              include characters used by Special Character Tokens.  The default
 *              is "=".
 * WHITESPACE => The largest possible string of whitespace.
 * WORD => The largest possible string of everything not used by other Autodoc
 *         tokens.
 * KEYWORD => A WORD that match an autodoc keyword:  Version, Pip, KeyValueDelimiter
 *
 *
 * Language definition for the parser:
 * Syntax of language definition:
 * => equals
 * TOKEN
 * {  0 or more  }
 * [  1 or more  ]
 * {n  0 up to n  n}
 * (  group together  )
 * (boolean value)
 * \any token except these\
 * -optional-
 * | => or
 * & => and
 * ^ => beginning of the line
 * 
 * Preprocessor flags:
 * DelimiterInLine
 * 
 * Definition:
 * 
 * Autodoc => { emptyLine | comment | pair | section } EOF
 * 
 * section => startLineDelimiter OPEN sectionHeader CLOSE -WHITESPACE- ( EOL | EOF ) 
 *            { emptyLine | comment | pair | subSection }
 *            
 * 
 * sectionHeader => -WHITESPACE- sectionType -WHITESPACE- DELIMITER -WHITESPACE-
 *                  sectionName -WHITESPACE-
 *                  
 * sectionType => ( WORD | KEYWORD )
 * 
 * sectionName = [ \CLOSE & WHITESPACE & EOL & EOF\ ]
 *               
 * subSection => startLineDelimiter OPEN OPEN sectionHeader CLOSE CLOSE ( EOL | EOF )
 *               { emptyLine | comment | pair }
 *               -subSectionClose-
 *               
 * subSectionClose -> startLine OPEN OPEN CLOSE CLOSE
 *               
 * pair => startLineDelimiter name -WHITESPACE- DELIMITER -WHITESPACE- value
 *         
 * name => attribute { SEPARATOR attribute }
 *         
 * attribute => ( WORD | KEYWORD )
 * 
 * value => { \EOL & EOF\ } ( EOL | EOF ) { valueline }  
 *  
 * valueLine => startLineDelimiter !emptyLine !comment -indent- 
 *              { \DELIMITER & EOL & EOF\ } ( EOL | EOF )
 *          
 * indent => startLine BREAK -WHITESPACE-
 * 
 * These elements are not saved:
 *               
 * comment => startline COMMENT { \EOL\ } ( EOL | EOF )
 * 
 * emptyLine => emptyline => startLine ( EOL | EOF )
 * 
 * startLineDelimiter => (DelimiterInLine == true) startLine
 * 
 * startLineWithOutDelimiter => (DelimiterInLine == false) startLine
 * 
 * startLine => ^ -WHITESPACE-
 *
 *
 * Non-embedded EOL and WHITESPACE are stripped from values.
 * Optional WHITESPACE is ignored in other elements.
 *               
 * Required pairs:
 * Top level attributes (meta data):  Version and Pip.
 * 
 * Flags:
 * The KeyValueDelimiter attribute changes the delimiter string for subsequent
 * lines.  It can be used as often as needed and placed anywhere in the autodoc
 * file.  The value of a KeyValueDelimiter must be one line
 * 
 * Preprocessor:
 * Sets flags decribing the current line: delimiterInLine and line number.
 * 
 * Postprocessor:
 * Checks for required elements.
 * 
 * </p>
 * 
 * <p>Copyright: Copyright 2002 - 2006</p>
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
 * <p> $Revision 1.4  2006/06/14 00:22:19  sueh
 * <p> $bug# 852 Rewrote the autodoc language definitions to make them simpler and
 * <p> $easier to understand.  Fixed an incorrect definition in Attribute.  Added a
 * <p> $definition for sub-sections.  Rewrote the parser to conform to the new definitions.
 * <p> $Added LinkList inner class to handle elements made of multiple Tokens.
 * <p> $Removes all the preprocessor flags except for delimiterInLine.  Handling
 * <p> $BREAK in the parser instead of the preprocessor.  Fixed the internal test
 * <p> $print statements so that they are clearer.
 * <p> $
 * <p> $Revision 1.3  2006/06/05 18:05:47  sueh
 * <p> $bug# 766 Removed commented out print statements.
 * <p> $
 * <p> $Revision 1.2  2006/05/01 21:16:57  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.1  2006/01/12 17:02:37  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p> $
 * <p> $Revision 1.10  2006/01/11 21:57:36  sueh
 * <p> $bug# 675 Replaced AttributeCollection with WriteOnlyAttributeMap.
 * <p> $
 * <p> $Revision 1.9  2005/11/10 18:15:22  sueh
 * <p> $bug# 733 Changed the missing meta data warning to a single line warning.
 * <p> $
 * <p> $Revision 1.8  2005/09/21 16:36:58  sueh
 * <p> $bug# 532 Changed Autodoc.getFile() to getAutodocFile().
 * <p> $
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

final class AutodocParser {
  public static final String rcsid = "$$Id$$";

  private AutodocTokenizer tokenizer;
  private String name = null;
  private File file = null;
  private final Vector line = new Vector();
  private int tokenIndex = 0;
  private Autodoc autodoc = null;
  private Token token = null;
  private Token prevToken = null;
  private Token prevPrevToken = null;
  private boolean parsed = false;

  //error flags
  private boolean errorsFound = false;
  private boolean error = false;

  //testing flags
  private boolean test = false;
  private boolean detailedTest = false;
  private boolean testWithTokens = false;
  private Token.Type lastTokenType = Token.Type.NULL;
  private int testIndent = -1;

  //Preprocessor flags
  private boolean delimiterInLine = false;
  private int lineNum = 0;
  private int lastLinePrinted = 0;

  //Postprocessor flags
  private boolean versionFound = false;
  private boolean pipFound = false;

  AutodocParser(Autodoc autodoc) {
    if (autodoc == null) {
      throw new IllegalArgumentException("autodoc is null.");
    }
    this.autodoc = autodoc;
    name = new String(autodoc.getName());
    file = autodoc.getAutodocFile();
    tokenizer = new AutodocTokenizer(file);
  }

  void initialize() throws FileNotFoundException, IOException {
    tokenizer.initialize();
  }

  boolean isError() {
    return error;
  }

  /**
   * Parses an autodoc file.
   * This function can be run only once per instance of the object.
   * @throws IOException
   */
  void parse() throws IOException {
    autodoc();
  }

  /**
   * Autodoc => { emptyLine | comment | pair | section } EOF
   * 
   * Parses an autodoc file.
   * This function can be run only once per instance of the object.
   * @throws IOException
   */
  private void autodoc() throws IOException {
    if (parsed) {
      return;
    }
    parsed = true;
    nextToken();
    while (!token.is(Token.Type.EOF)) {
      if (emptyLine()) {
      }
      else if (comment()) {
      }
      else if (section()) {
      }
      else if (pair(autodoc)) {
      }
      else {
        reportError("Unknown statement.");
      }
    }
    postprocess();
  }

  /** 
   * emptyLine => emptyLine => startLine ( EOL | EOF )
   * 
   * @return
   * @throws IOException
   */
  private boolean emptyLine() throws IOException {
    if (!startLine()
        || (!matchToken(Token.Type.EOL) && !matchToken(Token.Type.EOF))) {
      //not an empty line
      return false;
    }
    testStartFunction("emptyline");
    testEndFunction("emptyline", true);
    return true;
  }

  /**
   * startLineDelimiter => (DelimiterInLine == true) startLine
   * 
   * runs startLine if the delimiter is there, otherwise returns false
   */
  private boolean startLineDelimiter() throws IOException {
    if (!delimiterInLine) {
      //delimiter is missing, so fail start line
      return false;
    }
    return startLine();
  }

  /**
   * startLineWithOutDelimiter => (DelimiterInLine == false) startLine
   * 
   * runs startLine if the delimiter is there, otherwise returns false
   */
  private boolean startLineWithOutDelimiter() throws IOException {
    if (delimiterInLine) {
      //delimiter is missing, so fail start line
      return false;
    }
    return startLine();
  }

  /**

   * 
   * startLine => ^ -WHITESPACE-
   * 
   * @return true if the token is the first token on the line, or the first
   * token except for whitespace.
   * StartLine will eat only the first whitespace of the line.
   * StartLine can be called more then once per line.
   * StartLine does not print a test line.
   * @throws IOException
   */
  private boolean startLine() throws IOException {
    if (prevToken == null || prevToken.is(Token.Type.EOL)) {
      //start of line - first whitespace may not have been eaten
      //eat up the optional whitespace at the start of the line
      if (test) {
        printTestLine();
      }
      matchToken(Token.Type.WHITESPACE);
      return true;
    }
    if ((prevPrevToken == null || prevPrevToken.is(Token.Type.EOL))
        && prevToken.is(Token.Type.WHITESPACE)) {
      //start of line - whitespace was eaten by a previous call to startLine()
      return true;
    }
    //not start of line
    return false;
  }

  /**
   * comment => startLine COMMENT { \EOL\ } EOL
   * 
   * Parses a comment.
   * @return true if comment found
   * @throws IOException
   */
  private boolean comment() throws IOException {
    if (!startLine() || !matchToken(Token.Type.COMMENT)) {
      //not a comment
      return false;
    }
    testStartFunction("comment");
    //eat up comment line
    while (!token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
      nextToken();
    }
    //eat up the EOL
    matchToken(Token.Type.EOL);
    testEndFunction("comment", true);
    return true;
  }

  /**
   * section => startLineDelimiter OPEN sectionHeader CLOSE -WHITESPACE- ( EOL | EOF )
   *            { emptyline | comment | pair | subsection }
   *            
   * Parses a section.
   * @return true if section found
   * @throws IOException
   */
  private boolean section() throws IOException {
    WriteOnlyAttributeMap section = null;
    if (!startLineDelimiter()) {
      //not a section
      return false;
    }
    if (!matchToken(Token.Type.OPEN)) {
      //not a section
      return false;
    }
    testStartFunction("section");
    //save the new section
    section = sectionHeader();
    if (section == null) {
      testEndFunction("section", false);
      return false;
    }
    if (!matchToken(Token.Type.CLOSE)) {
      //bad section
      reportError("A section header must end with \""
          + AutodocTokenizer.CLOSE_CHAR + "\".");
      testEndFunction("section", false);
      return false;
    }
    matchToken(Token.Type.WHITESPACE);
    if (!matchToken(Token.Type.EOL) && !matchToken(Token.Type.EOF)) {
      //bad section
      reportError("A section header must end with \""
          + AutodocTokenizer.CLOSE_CHAR + "\".");
      testEndFunction("section", false);
      return false;
    }
    while (!token.is(Token.Type.EOF)) {
      //look for elements in the section
      if (emptyLine()) {
      }
      else if (comment()) {
      }
      else if (subSection()) {
      }
      else if (pair(section)) {
      }
      else {
        //end of section
        testEndFunction("section", true);
        return true;
      }
    }
    //empty section
    testEndFunction("section", true);
    return true;
  }

  private boolean subSection() throws IOException {
    return false;
  }

  /**
   * Returns true and calls nextToken(), if token is tokenType.
   * If token is not tokenType, returns false and does not call nextToken().
   * @param tokenType
   * @return
   */
  private boolean matchToken(Token.Type tokenType) throws IOException {
    if (token.is(tokenType)) {
      nextToken();
      return true;
    }
    return false;
  }

  /**
   * sectionHeader => -WHITESPACE- sectionType -WHITESPACE- DELIMITER -WHITESPACE-
   *                  sectionName -WHITESPACE-
   * 
   * @return a Section if sectionHeader found, otherwise return null
   * @throws IOException
   */
  private Section sectionHeader() throws IOException {
    testStartFunction("sectionHeader");
    matchToken(Token.Type.WHITESPACE);
    //get the section type
    Token type = sectionType();
    if (type == null) {
      //bad section header
      reportError("A section header must contain a section type.");
      testEndFunction("sectionHeader", false);
      return null;
    }
    matchToken(Token.Type.WHITESPACE);
    if (!matchToken(Token.Type.DELIMITER)) {
      //bad section header
      reportError("A section header must contain a delimiter (\""
          + tokenizer.getDelimiterString() + "\").");
      testEndFunction("sectionHeader", false);
      return null;
    }
    matchToken(Token.Type.WHITESPACE);
    //get the section name
    LinkList name = sectionName();
    if (name.size() == 0) {
      //bad section header
      reportError("A section header must contain a section name.");
      testEndFunction("sectionHeader", false);
      return null;
    }
    matchToken(Token.Type.WHITESPACE);
    testEndFunction("sectionHeader", true);
    return autodoc.addSection(type, name.getList());
  }

  /**
   * sectionType => ( WORD | KEYWORD )
   * 
   * @return section type or null
   */
  private Token sectionType() throws IOException {
    testStartFunction("sectionType");
    if (matchToken(Token.Type.WORD) || matchToken(Token.Type.KEYWORD)) {
      //return the section type
      testEndFunction("sectionType", true);
      return prevToken;
    }
    //did not find section type
    testEndFunction("sectionType", false);
    return null;
  }

  /**
   * sectionName = [ \CLOSE & WHITESPACE & EOL\ ]
   * 
   * @return section name or null
   */
  private LinkList sectionName() throws IOException {
    testStartFunction("sectionName");
    //section name may contain multiple tokens
    LinkList name = new LinkList(token);
    //link the section name together
    while (!token.is(Token.Type.CLOSE) && !token.is(Token.Type.WHITESPACE)
        && !token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
      name.append(token);
      nextToken();
    }
    if (name.size() == 0) {
      //bad section name
      reportError("Missing section name.");
      testEndFunction("sectionName", false);
      return null;
    }
    testEndFunction("sectionName", true);
    return name;
  }

  /**
   * pair => startLineDelimiter name -WHITESPACE- DELIMITER -WHITESPACE- value
   *         
   * Adds an attribute tree to the attribute map.
   * @throws IOException
   */
  private boolean pair(WriteOnlyAttributeMap attributeMap) throws IOException {
    if (!startLineDelimiter() || token.is(Token.Type.OPEN)) {
      //not a pair
      return false;
    }
    testStartFunction("pair");
    Attribute leafAttribute = name(attributeMap);
    if (leafAttribute == null) {
      //bad pair
      testEndFunction("pair", false);
      return false;
    }
    matchToken(Token.Type.WHITESPACE);
    if (!matchToken(Token.Type.DELIMITER)) {
      //bad pair
      testEndFunction("pair", false);
      return false;
    }
    matchToken(Token.Type.WHITESPACE);
    //attach the value to the last attribute
    value(leafAttribute);
    testEndFunction("pair", true);
    return true;
  }

  /**
   * name => attribute { SEPARATOR attribute }
   * 
   * @throws IOException
   */
  private Attribute name(WriteOnlyAttributeMap attributeMap) throws IOException {
    testStartFunction("name");
    Attribute attributeTree = attribute(attributeMap);
    if (attributeTree == null) {
      //bad name
      reportError("Missing attribute.");
      testEndFunction("name", false);
    }
    while (matchToken(Token.Type.SEPARATOR)) {
      //add the attribute to the map, point to the child attribute
      attributeTree = attribute(attributeTree);
      if (attributeTree == null) {
        //bad name
        reportError("Attribute must follow separator (\""
            + AutodocTokenizer.SEPARATOR_CHAR + "\")" + ".");
        testEndFunction("name", false);
      }
    }
    testEndFunction("name", true);
    return attributeTree;
  }

  /**
   * attribute => ( WORD | KEYWORD )
   * 
   * Adds attributes to an attribute map.  An attribute map is a tree of
   * attributes.
   * @return attributeMap or null
   */
  private Attribute attribute(WriteOnlyAttributeMap attributeMap)
      throws IOException {
    testStartFunction("attribute");
    if (matchToken(Token.Type.WORD) || matchToken(Token.Type.KEYWORD)) {
      //add the attribute to the map
      testEndFunction("attribute", true);
      //return the new attribute
      return (Attribute) attributeMap.addAttribute(prevToken);
    }
    //did not find attribute
    testEndFunction("attribute", false);
    return null;
  }

  /**
   * value => { \EOL & EOF\ } ( EOL | EOF ) { valueLine }  
   * 
   * sets the value in the attribute, if the value exists
   * @throws IOException
   */
  private void value(Attribute attribute) throws IOException {
    testStartFunction("value");
    //values can be made of multiple tokens, so use a link list
    LinkList value = new LinkList(token);
    while (!token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
      //add the token to the value link list
      value.append(token);
      nextToken();
    }
    //Finished the first line of the value (excluding the EOL) if this is
    //delimiter reassignment, it must be done now, or the following pair will be
    //mistaken for part of this value.
    processMetaData(attribute, value.getList());
    value.append(token);
    nextToken();
    //look for value lines
    while (!error && valueLine(value)) {
    }
    //Strip non-embedded EOL, EOF, and WHITESPACE
    while (value.size() > 0
        && (value.isFirstElement(Token.Type.WHITESPACE)
            || value.isFirstElement(Token.Type.EOL) || value
            .isFirstElement(Token.Type.EOF))) {
      value.dropFirstElement();
    }
    while (value.size() > 0
        && (value.isLastElement(Token.Type.WHITESPACE)
            || value.isLastElement(Token.Type.EOL) || value
            .isLastElement(Token.Type.EOF))) {
      value.dropLastElement();
    }
    attribute.setValue(value.getList());
    testEndFunction("value", true);
    return;
  }

  /**
   * valueLine => startLine !emptyLine !comment -indent-
   *              { \DELIMITER & EOL & EOF\ } ( EOL | EOF )
   *              
   * Adds to the end of a value link list
   * returns the new end of the link list
   * @throws IOException
   */
  private boolean valueLine(LinkList value) throws IOException {
    if (!startLineWithOutDelimiter() || token.is(Token.Type.EOL)
        || token.is(Token.Type.EOF) || token.is(Token.Type.COMMENT)) {
      //not a value line
      return false;
    }
    testStartFunction("valueLine");
    //add the option indent to the value link list
    indent(value);
    while (!token.is(Token.Type.DELIMITER) && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)) {
      //add the token to the value link list
      value.append(token);
      nextToken();
    }
    if (token.is(Token.Type.DELIMITER)) {
      //bad value line
      reportError("A value line cannot contain the delimiter string (\""
          + tokenizer.getDelimiterString() + "\").");
      testEndFunction("valueLine", false);
      return false;
    }
    value.append(token);
    nextToken();
    testEndFunction("valueLine", true);
    return true;
  }

  /**
   * indent => startLine BREAK -WHITESPACE-
   * 
   * looks for the BREAK token at the start of the line
   * @throws IOException
   */
  private void indent(LinkList value) throws IOException {
    if (!startLine() || !matchToken(Token.Type.BREAK)) {
      //not an indent
      return;
    }
    testStartFunction("indent");
    //activate BREAK so it can be used in formatting as a new line character
    prevToken.setActive(true);
    //add the break character to the value link list
    value.append(prevToken);
    if (matchToken(Token.Type.WHITESPACE)) {
      //white space was matched - this is the indentation
      value.append(prevToken);
    }
    testEndFunction("indent", true);
    return;
  }

  private void reportError(String message) throws IOException {
    if (message == null) {
      message = "Unknown error.";
    }
    error = true;
    String errorMessage = new String("Line# " + lineNum + ": " + token + ": "
        + message);
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
   * get the next token
   * set prevToken and prevPrevToken
   */
  private void nextToken() throws IOException {
    if (token != null && token.is(Token.Type.EOF)) {
      return;
    }
    if (line == null || tokenIndex == line.size()) {
      preprocess();
      tokenIndex = 0;
    }
    prevPrevToken = prevToken;
    prevToken = token;
    token = (Token) line.get(tokenIndex);
    tokenIndex++;
    if (detailedTest) {
      System.err.println(tokenIndex + ":" + token + ",delimiterInLine="
          + delimiterInLine);
    }
  }

  /**
   * Set line level preprocessor flag delimiterInLine.
   * Also set the line number
   */
  private void preprocess() throws IOException {
    Token token = null;
    lineNum++;
    line.clear();
    delimiterInLine = false;
    do {
      token = tokenizer.next();
      if (token.is(Token.Type.DELIMITER)) {
        delimiterInLine = true;
      }
      line.add(token);
    } while (!token.is(Token.Type.EOL) && !token.is(Token.Type.EOF));
  }

  //postprocessor

  private void processMetaData(Attribute attribute, Token value) {
    if (!attribute.isBase()) {
      return;
    }
    Token name = attribute.getNameToken();
    if (attribute.isGlobal()) {
      if (name.equals(Token.Type.KEYWORD, AutodocTokenizer.VERSION_KEYWORD)) {
        versionFound = true;
        return;
      }
      if (name.equals(Token.Type.KEYWORD, AutodocTokenizer.PIP_KEYWORD)) {
        pipFound = true;
        return;
      }
    }
    if (name.equals(Token.Type.KEYWORD, AutodocTokenizer.DELIMITER_KEYWORD)) {
      tokenizer.setDelimiterString(value.getValues());
    }
  }

  private void postprocess() {
    if (!versionFound) {
      System.err.println("Warning:  missing meta data - Version not found.");
    }
  }

  void testStreamTokenizer(boolean tokens) throws IOException {
    tokenizer.testStreamTokenizer(tokens);
  }

  void testPrimativeTokenizer(boolean tokens) throws IOException {
    tokenizer.testPrimativeTokenizer(tokens);
  }

  void testAutodocTokenizer(boolean tokens) throws IOException {
    tokenizer.test(tokens);
  }

  void testPreprocessor(boolean tokens) throws IOException {
    if (tokens) {
      System.err.println("(type,value):delimiterInLine");
    }
    tokenizer.initialize();
    do {
      nextToken();
      if (tokens) {
        System.err.println(token.toString() + ":" + delimiterInLine);
      }
      else if (token.is(Token.Type.EOL)) {
        System.err.println();
      }
      else if (!token.is(Token.Type.EOF)) {
        System.err.print(token.getValue());
      }
    } while (!token.is(Token.Type.EOF));
  }

  /**
   * 
   * @param tokens: display tokens rather then text
   * @throws IOException
   */
  void test(boolean tokens) throws IOException {
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
  void test(boolean tokens, boolean details) throws IOException {
    detailedTest = true;
    test(tokens);
  }

  private void testStartFunction(String functionName) {
    if (test) {
      testIndent++;
      printTestIndent();
      System.err.println(functionName + " {");
    }
  }

  private boolean testEndFunction(String functionName, boolean result) {
    if (test) {
      printTestIndent();
      if (result) {
        System.err.println(functionName + " } succeeded");
      }
      else {
        System.err.println(functionName + " } failed");
      }
      testIndent--;
    }
    return result;
  }

  private void printTestLine() {
    if (lastLinePrinted == lineNum) {
      return;
    }
    lastLinePrinted = lineNum;
    Token token = null;
    int size = line.size();
    printTestIndent();
    System.err.print("Line# " + lineNum + ": ");
    for (int index = 0; index < size; index++) {
      token = (Token) line.get(index);
      if (testWithTokens) {
        System.err.print(token);
      }
      else if (!token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
        System.err.print(token.getValue());
      }
    }
    System.err.println();
  }

  private void printTestIndent() {
    for (int i = 0; i < testIndent; i++) {
      System.err.print("\t");
    }
  }

  private final static class LinkList {
    Token list;
    Token endList;
    int size;

    LinkList(Token start) {
      list = start;
      endList = list;
      size = 1;
    }

    void append(Token token) {
      if (endList == null) {
        list = token;
        endList = token;
        size = 1;
      }
      else {
        endList = endList.setNext(token);
        size++;
      }
    }

    int size() {
      return size;
    }

    boolean isFirstElement(Token.Type compareType) {
      if (list == null) {
        return false;
      }
      return list.is(compareType);
    }

    boolean isLastElement(Token.Type compareType) {
      if (endList == null) {
        return false;
      }
      return endList.is(compareType);
    }

    void dropFirstElement() {
      if (list == null) {
        return;
      }
      list = list.dropFromList();
      if (list == null) {
        endList = null;
        size = 0;
      }
      else {
        size--;
      }
    }

    void dropLastElement() {
      if (endList == null) {
        return;
      }
      endList = endList.dropFromList();
      if (endList == null) {
        list = null;
        size = 0;
      }
      else {
        size--;
      }
    }

    Token getList() {
      return list;
    }
  }
}
