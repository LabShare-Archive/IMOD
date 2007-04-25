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
* <p> Revision 1.1  2007/03/21 19:41:01  sueh
* <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
* <p> </p>
*/
public interface ReadOnlyStatement {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * Get the Type of the instance.  This is a substitute for instanceof.
   * @return
   */
  public Statement.Type getType();
  
  /**
   * Get something equivalent to the original statement.  Not guaranteed to be
   * exactly the same.
   */
  public String getString();
  /**
   * NameValuePair:  returns the number of attributes that make up the name
   * Subsection:  returns 1 (refers to Section.type)
   * Everything else returns 0 (not a pair)
   * @return
   */
  public int sizeLeftSide();
  
  /**
   * NameValuePair:  returns name.get(index).getName()
   * Subsection:  if index is 0, return Section.type, otherwise returns null
   * Everything else return null
   * @param index
   * @return
   */
  public String getLeftSide(int index);
  
  /**
   * NameValuePair:  returns value or null if out of bounds
   * Subsection:  returns Section.name
   * Comment:  returns comment
   * EmptyLine:  return ""
   * @return
   */
  public String getRightSide();
}
