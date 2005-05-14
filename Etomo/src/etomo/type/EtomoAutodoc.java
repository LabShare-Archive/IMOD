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
      text = attribute.getUnformattedValue();
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute("usage");
    if (attribute != null) {
      text = attribute.getUnformattedValue();
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute("manpage");
    if (attribute != null) {
      return attribute.getUnformattedValue();
    }
    return null;
  }

  public static String getTooltip(Autodoc autodoc, String fieldName) {
    String tooltip = getTooltip(
        autodoc.getSection(FIELD_SECTION_NAME, fieldName));
    if (tooltip == null) {
      return null;
    }
    tooltip = tooltip.trim();
    if (tooltip.endsWith(".")) {
      return tooltip.substring(0, tooltip.length() -1) + " (" + fieldName + ").";
    }
    return tooltip + " (" + fieldName + ").";
  }
  
  public static String getTooltip(Section section, String enumValueName) {
    try {
      String enumTooltip = section.getAttribute("enum").getAttribute(enumValueName)
          .getAttribute(tooltipAttributeName).getUnformattedValue();
      if (enumTooltip == null) {
        return getTooltip(section);
      }
      return enumTooltip;
    }
    catch (NullPointerException e) {
      return getTooltip(section);
    }
  }
  
  public static String getTooltip(Section section, int enumValueName) {
    return getTooltip(section, Integer.toString(enumValueName));
  }
}

/**
 * <p> $Log$
 * <p> Revision 1.5  2005/05/12 01:29:18  sueh
 * <p> bug# 658 In getTooltip(Autodoc, String fieldName), added the fieldName
 * <p> to the end of tooltip.
 * <p>
 * <p> Revision 1.4  2005/02/21 23:02:55  sueh
 * <p> bug# 600 Return field-level tooltips when enum tooltip is not available.
 * <p>
 * <p> Revision 1.3  2005/02/18 01:28:47  sueh
 * <p> bug# 600 Adding getTooltip(Section, int) to get an enum with the value of
 * <p> the radio button choice.
 * <p>
 * <p> Revision 1.2  2005/02/15 19:29:24  sueh
 * <p> bug# 602 Getting unformatted value from attribute for tooltips.
 * <p>
 * <p> Revision 1.1  2005/02/11 16:44:17  sueh
 * <p> bug# 600 Adding class which knows about key words used in the autodocs
 * <p> and the rules for getting a tooltip.
 * <p> </p>
 */
