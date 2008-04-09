package etomo.type;

import java.io.IOException;

import etomo.ui.Token;
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

  abstract String getString(boolean parsable);

  /**
   * Returns true if the tokenizer passed to parse() contained a divider.
   * Otherwise returns false.
   * @return
   */
  abstract boolean wasDividerParsed();

  static Character getDividerSymbol(ParsedElementType type) {
    if (type == ParsedElementType.MATLAB) {
      return ParsedArrayDescriptor.DIVIDER_SYMBOL;
    }
    return ParsedIteratorDescriptor.DIVIDER_SYMBOL;
  }

  static ParsedDescriptor getInstance(ParsedElementType type,
      EtomoNumber.Type etomoNumberType) {
    if (type == ParsedElementType.MATLAB) {
      return new ParsedArrayDescriptor(etomoNumberType);
    }
    return new ParsedIteratorDescriptor();
  }

  public void setDebug(final boolean input) {
    super.setDebug(input);
    for (int i = 0; i < size(); i++) {
      getElement(i).setDebug(input);
    }
  }

  public Number getRawNumber() {
    return getElement(0).getRawNumber();
  }

  public String getRawString() {
    return getString(false);
  }

  public String getRawString(int index) {
    return getElement(index).getRawString();
  }

  /**
   * input is a collection, since it is not indexed, so it is a semi-raw string
   * - it has :'s
   */
  void setRawString(final String input) {
    clear();
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
   * No effect.  Descriptors have not required defaults so far.
   * @param numberIndex
   * @param defaultValueArray
   */
  void setDefaultValue(final int numberIndex, final Integer[] defaultValueArray) {
  }

  /**
   * No defaults in descriptors.
   */
  boolean isDefaultedEmpty() {
    return isEmpty();
  }

  boolean isCollection() {
    return true;
  }

  String getParsableString() {
    return getString(true);
  }

  /**
   * Returns true only when all numbers in the array are greater then or equal
   * to the number parameter.  Expands any array descriptors into the arrays
   * they represent.
   */
  boolean ge(final int number) {
    ParsedElementList expandedArray = getParsedNumberExpandedArray(null);
    boolean greaterOrEqual = true;
    for (int i = 0; i < expandedArray.size(); i++) {
      if (!expandedArray.get(i).ge(number)) {
        greaterOrEqual = false;
        break;
      }
    }
    return greaterOrEqual;
  }
}
