package etomo.type;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Formatter;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.ui.swing.Token;
import etomo.ui.swing.UIHarness;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: 
 * IterationList may contain array descriptors in the form start-end.
 *   Example: "2,4 - 9,10".
 * 
 * NUMBER => a whole positive number (largest possible)
 * DIVIDER => ,
 * DASH => -
 * WHITESPACE => The largest possible string of whitespace.
 * EOF => end of input
 * 
 * Language definition for the parser:
 * Syntax of language definition:
 * => equals
 * TOKEN
 * {  0 or more  }
 * (  group together  )
 * -optional-
 * | => or
 *  
 * iterator =>  -WHITESPACE- -( element { -WHITESPACE- DIVIDER -WHITESPACE- element } -WHITESPACE- )- EOF
 * 
 * element => NUMBER -(-WHITESPACE- DASH -WHITESPACE- NUMBER )-
 * 
 * This parser fails on the first error.
 * 
 * Can return the list as written (for nad_eed_3d) or can expand the ranges
 * (for 3dmod).
 * 
 * </p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 * <p> Revision 1.3  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.2  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.1  2009/09/05 00:10:52  sueh
 * <p> bug# 1256 Simple parser that parses iterator lists.
 * <p></p>
 */
public final class IteratorParser {
  public static final String rcsid = "$Id$";

  private static final String DIVIDER = ",";
  private static final String DASH = "-";

  private final BaseManager manager;
  private final AxisID axisID;
  private final String description;

  private PrimativeTokenizer tokenizer = null;
  private Token token = null;
  private String prevPrevPrevValue = null;
  private String prevPrevValue = null;
  private String prevValue = null;
  private boolean valid = true;
  private IteratorElementList iteratorElementList = null;

  public IteratorParser(BaseManager manager, AxisID axisID, String description) {
    this.manager = manager;
    this.axisID = axisID;
    this.description = description;
  }

  /**
   * Parses an iterator.
   * @throws IOException
   */
  public void parse(String input, IteratorElementList iteratorElementList) {
    //Reset parser.
    tokenizer = null;
    token = null;
    valid = true;
    this.iteratorElementList = iteratorElementList;
    if (iteratorElementList == null) {
      this.iteratorElementList = new IteratorElementList(manager, axisID, description);
    }
    tokenizer = PrimativeTokenizer.getNumericInstance(input);
    try {
      valid = false;
      tokenizer.initialize();
      valid = true;
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to parse " + description
          + ".  FileNotFoundException: " + e.getMessage(), "Etomo Error", axisID);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to parse " + description
          + ".  LogFile.LockException: " + e.getMessage(), "Etomo Error", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to parse " + description
          + ".  IOException: " + e.getMessage(), "Etomo Error", axisID);
    }
    iterator();
  }

  /**
   * iterator => -WHITESPACE- -( element -WHITESPACE- { DIVIDER -WHITESPACE- element  -WHITESPACE- } )- EOF
   * 
   * Parses an iterator
   */
  private void iterator() {
    nextToken();
    if (token.is(Token.Type.WHITESPACE)) {
      nextToken();
    }
    if (token.is(Token.Type.EOF)) {
      return;
    }
    if (!element()) {
      return;
    }
    if (token.is(Token.Type.WHITESPACE)) {
      nextToken();
    }
    while (!token.is(Token.Type.EOF)) {
      if (!token.equals(Token.Type.SYMBOL, DIVIDER)) {
        reportError(DIVIDER);
        return;
      }
      nextToken();
      if (token.is(Token.Type.WHITESPACE)) {
        nextToken();
      }
      if (!element()) {
        return;
      }
      if (token.is(Token.Type.WHITESPACE)) {
        nextToken();
      }
    }
  }

  /**
   * element => NUMBER -(-WHITESPACE- DASH -WHITESPACE- NUMBER )-
   * 
   * @return true if successful
   */
  private boolean element() {
    if (!token.is(Token.Type.NUMERIC)) {
      reportError(Token.Type.NUMERIC.toString());
      return false;
    }
    //Found an element - save it.
    String first = token.getValue();
    nextToken();
    if (token.is(Token.Type.WHITESPACE)) {
      nextToken();
    }
    if (!token.equals(Token.Type.SYMBOL, DASH)) {
      //Save numeric element
      IteratorElement element = new IteratorElement(first);
      iteratorElementList.add(element);
      return true;
    }
    nextToken();
    if (token.is(Token.Type.WHITESPACE)) {
      nextToken();
    }
    if (!token.is(Token.Type.NUMERIC)) {
      reportError(Token.Type.NUMERIC.toString());
      return false;
    }
    //Save range element
    IteratorElement element = new IteratorElement(first, token.getValue());
    iteratorElementList.add(element);
    nextToken();
    return true;
  }

  private void reportError(String expected) {
    valid = false;
    StringBuffer buffer = new StringBuffer();
    int index = -1;
    if (prevPrevPrevValue != null) {
      buffer.append(prevPrevPrevValue);
      index = buffer.length();
    }
    if (prevPrevValue != null) {
      buffer.append(prevPrevValue);
      index = buffer.length();
    }
    if (prevValue != null) {
      buffer.append(prevValue);
      index = buffer.length();
    }
    if (token != null) {
      index++;
      buffer.append(token.getValue());
    }
    Formatter f;
    String caret = "";
    if (index > 0) {
      caret = String.format("%1$" + String.valueOf(index) + "s", new Object[] { "^" });
    }
    UIHarness.INSTANCE.openMessageDialog(manager, "In " + description
        + ", iterator syntax error.\n" + buffer.toString() + "\n" + caret
        + "\nExpected \"" + expected + "\".  Syntax example:  2,4-9,10", "Entry Error",
        axisID);
  }

  /**
   * Save the last two values for error messages.  Place the result of
   * tokenizer.next() into token.  Do not assume that tokenizer.next() is or is
   * not returning a new instance.
   */
  private void nextToken() {
    prevPrevPrevValue = prevPrevValue;
    prevPrevValue = prevValue;
    if (token != null) {
      prevValue = token.getValue();
    }
    else {
      prevValue = null;
    }
    if (tokenizer == null) {
      token = new Token();
      token.set(Token.Type.EOL);
    }
    try {
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      token = new Token();
      token.set(Token.Type.EOL);
    }/*
     System.out.println("\nprevPrevPrevValue="+prevPrevPrevValue);
     System.out.println("prevPrevValue="+prevPrevValue);
     System.out.println("prevValue="+prevValue);
     System.out.println("token="+token);*/
  }

  public boolean isValid() {
    return valid;
  }
}
