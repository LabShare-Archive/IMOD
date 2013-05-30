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
* <p> Revision 1.1  2007/04/09 20:49:11  sueh
* <p> bug# 964 Changed NameValuePair to an abstract class called Statement and
* <p> child classes representing name/value pair, comment, empty line, and
* <p> subsection.  Made delimiter change an attribute of the name/value pair class.
* <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
* <p> Saving Attribute instance in name instead of strings so as not to create
* <p> duplications.
* <p>
* <p> Revision 1.1  2007/03/21 19:41:01  sueh
* <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
* <p> </p>
*/
public interface ReadOnlyStatement {
  public static final String rcsid = "$Id$";

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
   * NameValuePair:  returns the entire left side of the statement (the name of the name/value pair)
   * Subsection:  returns Section.type
   * Everything else return null
   * @param index
   * @return
   */
  public String getLeftSide();

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

  /**
   * If this is a subsection, get the subsection as a ReadOnlySection
   * @return
   */
  public ReadOnlySection getSubsection();
}
