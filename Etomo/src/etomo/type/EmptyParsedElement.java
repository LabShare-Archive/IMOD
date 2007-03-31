package etomo.type;

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
final class EmptyParsedElement extends ParsedElement {
  public static final String rcsid = "$Id$";

  String getRawString() {
    return "";
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    return null;
  }

  ParsedElement getElement(int index) {
    return this;
  }

  int size() {
    return 0;
  }

  String getParsableString() {
    return "";
  }

  boolean isCollection() {
    return false;
  }
}
