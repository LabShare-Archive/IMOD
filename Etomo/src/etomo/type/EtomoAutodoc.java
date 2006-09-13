package etomo.type;

import etomo.storage.autodoc.Attribute;
import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.Section;

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
  public static final String REQUIRED_ATTRIBUTE_NAME = "required";
  public static final int REQUIRED_TRUE_VALUE = 1;
  public static final char VAR_TAG = '%';
  
  private static final String TOOLTIP_ATTRIBUTE_NAME = "tooltip";

  public static String getTooltip(Section section) {
    if (section == null) {
      return null;
    }
    String text = null;
    Attribute attribute = section.getAttribute(TOOLTIP_ATTRIBUTE_NAME);
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
      return attribute.getValue();
    }
    return null;
  }

  public static String getTooltip(Autodoc autodoc, String fieldName) {
    if (autodoc == null) {
      return null;
    }
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
          .getAttribute(TOOLTIP_ATTRIBUTE_NAME).getValue();
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
 * <p> Revision 1.10  2006/04/25 18:57:00  sueh
 * <p> bug# 787 Added VAR_TAG, the variable character for autodoc variables.
 * <p>
 * <p> Revision 1.9  2006/01/12 17:04:10  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.8  2006/01/11 21:48:24  sueh
 * <p> bug# 675 Replaced attribute.getUnformattedValue() with attribute.getValue().
 * <p>
 * <p> Revision 1.7  2005/05/17 19:16:29  sueh
 * <p> bug# 663 Added static values for required attributes.
 * <p>
 * <p> Revision 1.6  2005/05/14 01:00:23  sueh
 * <p> bug# 658 Take out unnecessary trim (and prevent a null point exception).
 * <p>
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
