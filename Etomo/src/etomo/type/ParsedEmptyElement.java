package etomo.type;

import java.util.ArrayList;
import java.util.List;

import etomo.ui.Token;
import etomo.util.PrimativeTokenizer;

/**
 * Class that allows ParsedElementList to return null when getting an element.
 * EmptyParsedElement isn't available outside of ParsedElementList, but it
 * extends ParsedElement and behaves like an empty ParsedElement that is not a
 * collection.
 * @singleton
 * @immutable
 */
final class ParsedEmptyElement extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final ParsedEmptyElement NON_MATLAB_INSTANCE = new ParsedEmptyElement(
      ParsedElementType.NON_MATLAB);
  static final ParsedEmptyElement MATLAB_INSTANCE = new ParsedEmptyElement(
      ParsedElementType.MATLAB);
  static final ParsedEmptyElement STRING_INSTANCE = new ParsedEmptyElement(
      ParsedElementType.STRING);

  private final ParsedElementType type;

  private ParsedEmptyElement(ParsedElementType type) {
    this.type = type;
  }

  static ParsedEmptyElement getInstance(ParsedElementType type) {
    if (type == ParsedElementType.NON_MATLAB) {
      return NON_MATLAB_INSTANCE;
    }
    if (type == ParsedElementType.MATLAB) {
      return MATLAB_INSTANCE;
    }
    if (type == ParsedElementType.STRING) {
      return STRING_INSTANCE;
    }
    return NON_MATLAB_INSTANCE;
  }

  public String toString() {
    return "[empty]";
  }

  public String getRawString() {
    return "";
  }

  public String getRawString(int index) {
    return "";
  }

  public Number getRawNumber() {
    return new EtomoNumber().getNumber();
  }

  void clear() {
  }

  String validate() {
    return null;
  }

  void setRawString(int index, String string) {
  }

  void moveElement(int fromIndex, int toIndex) {
  }

  ParsedElement getElement(int index) {
    return this;
  }

  void setRawString(int index, float number) {
  }

  boolean ge(int number) {
    return false;
  }

  void setRawString(String input) {
  }

  List getParsedNumberExpandedArray(List parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ArrayList();
    }
    return parsedNumberExpandedArray;
  }

  void setDefaultValue(int numberIndex, Integer[] defaultValueArray) {
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    return null;
  }

  void removeElement(int index) {
  }

  int size() {
    return 0;
  }

  public boolean isEmpty() {
    return true;
  }

  boolean isDefaultedEmpty() {
    return true;
  }

  String getParsableString() {
    if (type == ParsedElementType.NON_MATLAB) {
      return "";
    }
    else if (type == ParsedElementType.MATLAB) {
      return "NaN";
    }
    else if (type == ParsedElementType.STRING) {
      return "''";
    }
    return "";
  }

  boolean isCollection() {
    return false;
  }
}