package etomo.type;

import etomo.ui.Attribute;
import etomo.ui.Autodoc;
import etomo.ui.Section;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
*/
public class EtomoAutodoc {
  public static final String  rcsid =  "$Id$";
  
  public static final String FIELD_SECTION_NAME = "field";
  
  private static final String tooltipAttributeName = "tooltip";
  
  public static String getTooltip(Section section) {
    if (section == null) {
      return null;
    }
    String text = null;
    Attribute attribute = section.getAttribute(tooltipAttributeName);
    if (attribute != null) {
      text = attribute.getValue();
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute("usage");
    if (attribute != null) {
      text = attribute.getValue();
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute("manpage");
    if (attribute != null) {
      text = attribute.getValue();
      if (text != null) {
        return text;
      }
    }
    return null;
  }

  public static String getTooltip(Autodoc autodoc, String fieldName) {
    Section section = autodoc.getSection(FIELD_SECTION_NAME, fieldName);
    return getTooltip(section);
  }
  
  public static String getTooltip(Section section, String enumValueName) {
    try {
      return section.getAttribute("enum").getAttribute(enumValueName)
          .getAttribute(tooltipAttributeName).getValue();
    }
    catch (NullPointerException e) {
      return null;
    }
  }

}
/**
 * <p> $Log$ </p>
 */
