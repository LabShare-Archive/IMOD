package etomo.storage.autodoc;

import java.util.HashMap;

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
 * <p> Revision 1.1  2007/03/21 19:40:37  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> </p>
 */
public interface ReadOnlyAutodoc extends ReadOnlyNameValuePairList {
  public static final String rcsid = "$Id$";

  public HashMap getAttributeValues(String sectionType, String attributeName);
  public HashMap getAttributeMultiLineValues(String sectionType, String attributeName);
  public boolean isError();
  public void printStoredData();
  public ReadOnlySection getSection(String type, String name);
  public boolean sectionExists(String type);
  public ReadOnlyAttribute getAttribute(String name);
  public SectionLocation getSectionLocation(String type);
  public ReadOnlySection nextSection(SectionLocation location);
}
