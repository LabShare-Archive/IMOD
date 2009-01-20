package etomo.storage.autodoc;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
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
public interface ReadOnlySectionList {
  public static  final String  rcsid =  "$Id$";
  
  public ReadOnlySection getSection(String type, String name);
  public SectionLocation getSectionLocation(String type);
  public ReadOnlySection nextSection(SectionLocation location);
  public String getString();
  public void setDebug();
  public String getName();
}
