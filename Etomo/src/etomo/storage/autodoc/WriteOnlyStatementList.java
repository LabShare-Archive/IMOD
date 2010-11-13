package etomo.storage.autodoc;

import etomo.ui.swing.Token;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
abstract class WriteOnlyStatementList extends WriteOnlyAttributeList {
  public static  final String  rcsid =  "$Id$";
  
  abstract NameValuePair addNameValuePair();
  abstract Section addSection(Token type, Token name);
  abstract void addEmptyLine();
  abstract void addComment(Token comment);
  abstract void setCurrentDelimiter(Token newDelimiter);
  abstract String getCurrentDelimiter();
}
/**
* <p> $Log$
* <p> Revision 1.2  2007/04/11 22:14:49  sueh
* <p> bug# 964 Removed NameValuePair.getDelimiterChangeInstance and added
* <p> setDelimiterChange.
* <p>
* <p> Revision 1.1  2007/04/09 20:57:32  sueh
* <p> bug# 964 Changed NameValuePair to an abstract class called Statement and
* <p> child classes representing name/value pair, comment, empty line, and
* <p> subsection.  Made delimiter change an attribute of the name/value pair class.
* <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
* <p> Saving Attribute instance in name instead of strings so as not to create
* <p> duplications.
* <p>
* <p> Revision 1.5  2007/03/23 20:40:33  sueh
* <p> bug# 964 Adding a Type which represents the change in delimiter.
* <p>
* <p> Revision 1.4  2007/03/08 22:03:36  sueh
* <p> bug# 964 Save name/value pairs in the parser instead of saving them from the
* <p> Attribute.  This is necessary because the name/value pair must be placed in the
* <p> autodoc or section as soon as they are found to preserve the original order of the
* <p> autodoc file.
* <p>
* <p> Revision 1.3  2007/03/01 01:20:49  sueh
* <p> bug# 964 Added addComment and addEmptyLine.
* <p>
* <p> Revision 1.2  2006/06/22 22:08:30  sueh
* <p> bug# 852 Added addSection().
* <p>
* <p> Revision 1.1  2006/01/12 17:03:56  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
* <p> Revision 1.1  2006/01/11 23:22:02  sueh
* <p> bug# 675 A generic way to add name/value pairs to Autodoc's and Section's.
* <p> </p>
*/