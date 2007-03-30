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
public abstract class ParsedElement {
  public static  final String  rcsid =  "$Id$";
  
  abstract String getRawString();
  abstract Token parse(Token token, PrimativeTokenizer tokenizer);
  abstract ParsedElement getElement(int index);
  abstract int size();
  abstract String getParsableString();
}
