package etomo.type;

import etomo.storage.autodoc.ReadOnlyAttribute;
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
 * <p> Revision 1.1  2007/03/30 23:46:45  sueh
 * <p> bug# 964 Parses a Matlab number.
 * <p> </p>
 */
public final class ParsedNumber extends ParsedElement {
  public static final String rcsid = "$Id$";

  private final EtomoNumber number;

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;

  public ParsedNumber() {
    this(null, null);
  }

  ParsedNumber(EtomoNumber.Type etomoNumberType, Integer defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = defaultValue;
    number = new EtomoNumber(etomoNumberType);
    if (defaultValue != null) {
      number.setDefault(defaultValue.intValue());
    }
  }

  public void parse(ReadOnlyAttribute attribute) {
    //TODO bug# 964
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    //TODO bug# 964
    return null;
  }

  String getRawString() {
    if (defaultValue == null) {
      return number.toString();
    }
    return number.toDefaultedString();
  }
  
  boolean isCollection() {
    return false;
  }

  String getParsableString() {
    return getRawString();
  }

  void setRawNumber(String number) {
    this.number.set(number);
  }

  ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return null;
  }

  int size() {
    return 1;
  }
}
