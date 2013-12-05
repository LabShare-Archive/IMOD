package etomo.type;

import java.io.IOException;
import java.util.ArrayList;

import etomo.storage.LogFile;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.ui.swing.Token;
import etomo.util.PrimativeTokenizer;

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
  public static final String rcsid = "$Id$";

  public static final String HEADER_SECTION_NAME = "SectionHeader";
  public static final String FIELD_SECTION_NAME = "Field";
  public static final String REQUIRED_ATTRIBUTE_NAME = "required";
  public static final String TYPE_ATTRIBUTE_NAME = "type";
  public static final String SHORT_ATTRIBUTE_NAME = "short";
  public static final String FORMAT_ATTRIBUTE_NAME = "format";
  public static final String USAGE_ATTRIBUTE_NAME = "usage";
  public static final String MANPAGE_ATTRIBUTE_NAME = "manpage";
  public static final String BOOLEAN_TYPE = "B";
  public static final String FLOAT_TYPE = "F";
  public static final String INTEGER_TYPE = "I";
  public static final String COMMENT_KEY = "comment";
  public static final int REQUIRED_TRUE_VALUE = 1;
  public static final char VAR_TAG = '%';
  public static final char NEW_LINE_CHAR = '^';
  private static final String TOOLTIP_ATTRIBUTE_NAME = "tooltip";
  public static final String DOUBLE_DASH_ATTRIBUTE_NAME="DoubleDashOptions";

  private static boolean debug = false;

  private EtomoAutodoc() {
  }

  public static String getTooltip(ReadOnlySection section) {
    if (section == null) {
      if (debug) {
        System.out.println("EtomoAutodoc.getTooltip:section is null");
      }
      return null;
    }
    String text = null;
    ReadOnlyAttribute attribute = section.getAttribute(TOOLTIP_ATTRIBUTE_NAME);
    if (attribute != null) {
      text = removeFormatting(attribute.getMultiLineValue());
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute(USAGE_ATTRIBUTE_NAME);
    if (attribute != null) {
      text = removeFormatting(attribute.getMultiLineValue());
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute(COMMENT_KEY);
    if (attribute != null) {
      text = removeFormatting(attribute.getMultiLineValue());
      if (text != null) {
        return text;
      }
    }
    attribute = section.getAttribute(MANPAGE_ATTRIBUTE_NAME);
    if (attribute != null) {
      return removeFormatting(attribute.getMultiLineValue());
    }
    return null;
  }

  public static String getTooltip(ReadOnlyAutodoc autodoc, String fieldName) {
    if (autodoc == null || fieldName == null) {
      return null;
    }
    boolean autodocDebug = autodoc.isDebug();
    if (debug && !autodocDebug) {
      autodoc.setDebug(true);
    }
    String tooltip = getTooltip(autodoc.getSection(FIELD_SECTION_NAME, fieldName));
    if (debug) {
      if (!autodocDebug) {
        autodoc.setDebug(false);
      }
    }
    if (tooltip == null) {
      return null;
    }
    tooltip = tooltip.trim();
    String source = "(" + autodoc.getAutodocName() + ":  " + fieldName + ")";
    if (tooltip.endsWith(".")) {
      return tooltip.substring(0, tooltip.length() - 1) + " " + source + ".";
    }
    return tooltip + " " + source + ".";
  }

  public static String getTooltip(ReadOnlySection section, String enumValueName) {
    try {
      String enumTooltip = section.getAttribute("enum").getAttribute(enumValueName)
          .getAttribute(TOOLTIP_ATTRIBUTE_NAME).getMultiLineValue();
      if (enumTooltip == null) {
        return getTooltip(section);
      }
      return removeFormatting(enumTooltip);
    }
    catch (NullPointerException e) {
      return getTooltip(section);
    }
  }

  /**
   * Removes the '^' formatting.
   * @param value
   * @return
   */
  public static String removeFormatting(String value) {
    if (value == null) {
      return null;
    }
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(value);
    StringBuffer tooltip = new StringBuffer();
    boolean removeIndentFormatting = false;
    boolean startOfLine = true;
    Token token = null;
    try {
      tokenizer.initialize();
      token = tokenizer.next();
    }
    catch (IOException e) {
      return value;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return value;
    }
    try {
      while (value != null && !token.is(Token.Type.EOF)) {
        // Remove indent formatting whitespace that comes after new-line formatting
        // character.
        if (removeIndentFormatting) {
          removeIndentFormatting = false;
          if (token.is(Token.Type.WHITESPACE)) {
            token = tokenizer.next();
            startOfLine = false;
            continue;
          }
        }
        // Remove the new-line formatting character ('^' at the beginning of the line).
        if (startOfLine && token.equals(Token.Type.SYMBOL, NEW_LINE_CHAR)) {
          // new-line may be followed by indent formatting that should be removed
          removeIndentFormatting = true;
          startOfLine = false;
        }
        // Convert end of line to a space.
        else if (token.is(Token.Type.EOL)) {
          tooltip.append(' ');
          // signal the start of the next line, so that new-line formatting
          // characters can be found
          startOfLine = true;
        }
        else {
          tooltip.append(token.getValue());
          startOfLine = false;
        }
        token = tokenizer.next();
      }
    }
    catch (IOException e) {
      return value;
    }
    return tooltip.toString();
  }

  /**
   * Uses the '^' formatting to format the value.
   * @param value
   * @return
   */
  public static String[] format(String value) {
    if (value == null) {
      return null;
    }
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(value);
    ArrayList list = new ArrayList();
    StringBuffer buffer = new StringBuffer(128);
    boolean startOfLine = true;
    boolean firstToken = true;
    Token token = null;
    try {
      tokenizer.initialize();
      token = tokenizer.next();
    }
    catch (IOException e) {
      return new String[] { value };
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return new String[] { value };
    }
    try {
      while (token != null && !token.is(Token.Type.EOF)) {
        if (startOfLine) {
          startOfLine = false;
          // Handle new-line character ('^' at the start of the line).
          if (token.equals(Token.Type.SYMBOL, NEW_LINE_CHAR)) {
            list.add(buffer.toString());
            buffer = new StringBuffer(128);
            startOfLine = false;
            token = tokenizer.next();
            continue;
          }
          else if (!firstToken) {
            // wasn't really the end of the line so convert EOL to a space.
            buffer.append(' ');
          }
          else {
            firstToken = false;
          }
          // still need to process the current token
        }
        if (token.is(Token.Type.EOL)) {
          // Wait to convert end-of-line to a space. If the next token is '^', then this
          // really is the end of a line.
          startOfLine = true;
        }
        else {
          buffer.append(token.getValue());
          startOfLine = false;
        }
        token = tokenizer.next();
      }
    }
    catch (IOException e) {
      return new String[] { value };
    }
    if (buffer.length() > 0) {
      list.add(buffer.toString());
    }
    if (list.size() == 0) {
      return new String[0];
    }
    if (list.size() == 1) {
      return new String[] { (String) list.get(0) };
    }
    return (String[]) list.toArray(new String[list.size()]);
  }

  public static String getTooltip(ReadOnlySection section, int enumValueName) {
    return getTooltip(section, Integer.toString(enumValueName));
  }

  public static void setDebug(boolean debug) {
    EtomoAutodoc.debug = debug;
  }
}

/**
 * <p> $Log$
 * <p> Revision 1.19  2009/06/05 02:04:12  sueh
 * <p> bug# 1219 Added the autodocName (the file name minus the extension).
 * <p>
 * <p> Revision 1.18  2009/02/04 23:30:30  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.17  2008/10/27 18:38:00  sueh
 * <p> bug# 1141 Added debug only print statements.
 * <p>
 * <p> Revision 1.16  2007/04/13 20:12:56  sueh
 * <p> bug# 964 Added debug member variable.
 * <p>
 * <p> Revision 1.15  2007/03/26 23:35:21  sueh
 * <p> bug# 964 Using getMultiLineValue so formatting can be removed.
 * <p>
 * <p> Revision 1.14  2007/03/23 20:42:00  sueh
 * <p> bug# 964 Added formt(String) and removingFormatting(String).
 * <p> RemoveFormatting is necessary since the BREAK character is no longer being
 * <p> handled by Autodoc and Token.
 * <p>
 * <p> Revision 1.13  2007/03/21 19:43:22  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.12  2007/03/15 21:47:14  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.11  2006/09/13 23:34:51  sueh
 * <p> bug# 921 Preventing null pointer exception in getTooltip.
 * <p>
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
