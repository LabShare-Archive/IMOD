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
 * <p> $Log$ </p>
 */
public final class ParsedQuotedString extends ParsedElement{
  public static final String rcsid = "$Id$";

  private static final Character QUOTE = new Character('\'');
  
  private String string="";
  
  public ParsedQuotedString() {
  }
  
  public static ParsedQuotedString getInstance(ReadOnlyAttribute attribute) {
    ParsedQuotedString instance = new ParsedQuotedString();
    instance.parse(attribute);
    return instance;
  }

  /**
   * This is a quoted string only if starts with a quote (ignoring whitespace).
   * @param attribute
   * @return
   */
  public static boolean isQuotedString(ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return false;
    }
    String value = attribute.getValue();
    if (value == null) {
      return false;
    }
    if (value.trim().charAt(0) == QUOTE.charValue()) {
      return true;
    }
    return false;
  }
  
  public void parse(ReadOnlyAttribute attribute) {}
  
  public String getRawString() {
    return string;
  }
  
  public String getParsableString() {
    StringBuffer buffer = new StringBuffer(QUOTE.toString());
    buffer.append(getRawString());
    buffer.append(QUOTE.toString());
    return buffer.toString();
  }
  
  public void setElement(ParsedElement element) {
    string = element.getRawString();
  }
  
  public void setRawString(String string) {
    this.string=string;
  }
  
  Token parse(Token token,PrimativeTokenizer tokenizer) {return null;}

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
