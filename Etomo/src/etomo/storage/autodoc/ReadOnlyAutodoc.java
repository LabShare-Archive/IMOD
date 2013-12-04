package etomo.storage.autodoc;

import java.io.IOException;
import java.util.HashMap;

import etomo.storage.LogFile;
import etomo.storage.autodoc.Autodoc.InternalTestType;

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
 * <p> Revision 1.11  2010/02/17 04:49:43  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.10  2010/01/11 23:57:43  sueh
 * <p> bug# 1299 Added exists.
 * <p>
 * <p> Revision 1.9  2009/06/05 02:02:12  sueh
 * <p> bug# 1219 Added getAutodocName.
 * <p>
 * <p> Revision 1.8  2009/02/04 23:30:00  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.7  2009/01/20 19:36:41  sueh
 * <p> bug# 1102 Extending ReadOnlySectionList.
 * <p>
 * <p> Revision 1.6  2008/10/27 18:35:50  sueh
 * <p> bug# 1141 Added isDebug and setDebug.
 * <p>
 * <p> Revision 1.5  2008/05/30 21:25:43  sueh
 * <p> bug# 1102 Formatted.
 * <p>
 * <p> Revision 1.4  2007/08/01 22:45:02  sueh
 * <p> bug# 985 Added runInternalTest to ReadOnlyAutodoc.
 * <p>
 * <p> Revision 1.3  2007/04/09 20:45:18  sueh
 * <p> bug# 964 Changed NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p> Saving Attribute instance in name instead of strings so as not to create
 * <p> duplications.
 * <p>
 * <p> Revision 1.2  2007/03/23 20:36:11  sueh
 * <p> bug# 964 Added getAttributeMultiLineValues, to get a hash map of value strings
 * <p> which retains EOL information.
 * <p>
 * <p> Revision 1.1  2007/03/21 19:40:37  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> </p>
 */
public interface ReadOnlyAutodoc extends ReadOnlyStatementList, ReadOnlySectionList {
  public static final String rcsid = "$Id$";

  public HashMap getAttributeValues(String sectionType, String attributeName);

  public HashMap getAttributeMultiLineValues(String sectionType, String attributeName);

  public boolean isError();

  public void printStoredData();

  public ReadOnlySection getSection(String type, String name);

  public boolean sectionExists(String type);

  public ReadOnlyAttribute getAttribute(String name);

  public SectionLocation getSectionLocation(String type);

  public SectionLocation getSectionLocation();

  public ReadOnlySection nextSection(SectionLocation location);

  void runInternalTest(InternalTestType type, boolean showTokens, boolean showDetails)
      throws IOException, LogFile.LockException;

  public boolean isDebug();

  public void setDebug(boolean input);

  public String getAutodocName();

  public boolean exists();
}
