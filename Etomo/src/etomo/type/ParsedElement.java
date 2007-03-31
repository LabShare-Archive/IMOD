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
* <p> $Log$
* <p> Revision 1.1  2007/03/30 23:42:20  sueh
* <p> bug# 964 Abstract class to act as an interface for elements stored in ParsedElementList.
* <p> </p>
*/
public abstract class ParsedElement {
  public static  final String  rcsid =  "$Id$";
  
  abstract String getRawString();
  abstract Token parse(Token token, PrimativeTokenizer tokenizer);
  abstract ParsedElement getElement(int index);
  abstract int size();
  abstract String getParsableString();
  abstract boolean isCollection();
}
