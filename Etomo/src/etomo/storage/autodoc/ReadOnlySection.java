package etomo.storage.autodoc;
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
* <p> Revision 1.2  2007/04/09 20:46:02  sueh
* <p> bug# 964 Changed NameValuePair to an abstract class called Statement and
* <p> child classes representing name/value pair, comment, empty line, and
* <p> subsection.  Made delimiter change an attribute of the name/value pair class.
* <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
* <p> Saving Attribute instance in name instead of strings so as not to create
* <p> duplications.
* <p>
* <p> Revision 1.1  2007/03/21 19:41:12  sueh
* <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
* <p> </p>
*/

public interface ReadOnlySection extends ReadOnlyStatementList,ReadOnlySectionList{
  public static  final String  rcsid =  "$Id$";
  
  public ReadOnlyAttribute getAttribute(String name);
  
  public String getType();
}
