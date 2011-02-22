package etomo.ui.swing;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Electron Microscopy (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.8  2008/04/02 19:08:44  sueh
 * <p> bug# 1104 In convertToHtml.  Use one index instead of two to go backwards through the raw string.  Otherwise it gets confused and converts the ">" in the <html> to <html>&gt.
 * <p>
 * <p> Revision 3.7  2007/12/26 22:38:12  sueh
 * <p> bug# 1063 Added HtmlMask to prevent <b> and </b> from being converted into
 * <p> html.
 * <p>
 * <p> Revision 3.6  2007/04/13 20:40:48  sueh
 * <p> bug# 964 Fixed a bug by adding convertToHtml.  Html string don't display "<"
 * <p> and ">" reliably, so I replaced them with the html equivalent.
 * <p>
 * <p> Revision 3.5  2007/02/09 00:54:47  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton.
 * <p>
 * <p> Revision 3.4  2005/02/15 20:42:05  sueh
 * <p> bug# 602 Added convert(), which converts from a string formatted with '\n'
 * <p> to an html string.
 * <p>
 * <p> Revision 3.3  2005/02/11 16:46:40  sueh
 * <p> bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 * <p>
 * <p> Revision 3.2  2004/01/01 00:47:13  sueh
 * <p> bug# 372 Section returns null when Section is not found
 * <p>
 * <p> Revision 3.1  2003/12/31 01:32:57  sueh
 * <p> bug# 372 added getText() - finds the right tooltip in an
 * <p> autodoc
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.3  2003/08/21 22:16:56  rickg
 * <p> Fixed loss of last character.
 * <p>
 * <p> Revision 1.2  2003/07/30 21:54:13  rickg
 * <p> Initial revision
 * <p>
 * <p> Revision 1.1  2003/07/28 23:59:32  rickg
 * <p> In progress
 * <p> </p>
 */

public class TooltipFormatter {
  public static final String rcsid = "$Id$";

  public static final TooltipFormatter INSTANCE = new TooltipFormatter();
  private static final int N_COLUMNS = 60;

  private boolean debug = false;

  /**
   * Format the rawString into an HTML string appropriate for tooltips
   * @return the HTML formatted string
   */
  public String format(String rawString) {
    if (rawString == null) {
      return null;
    }
    StringBuffer htmlFormat = new StringBuffer("<html>");
    boolean splitting = true;
    int idxStart = 0;
    while (splitting) {
      int idxSearch = idxStart + N_COLUMNS;
      // Are we past the end of the string
      if (idxSearch >= rawString.length() - 1) {
        htmlFormat.append(convertToHtml(rawString.substring(idxStart)));
        splitting = false;
      }
      else {
        String subString = rawString.substring(idxStart, idxSearch);
        int idxStop = subString.lastIndexOf(' ');
        //  All one word!
        if (idxStop < 0) {
          idxStop = N_COLUMNS;
        }
        else {
          htmlFormat.append(convertToHtml(rawString.substring(idxStart, idxStart
              + idxStop))
              + "<br>");
          idxStart = idxStart + idxStop + 1;
        }
      }
    }
    return htmlFormat.toString();
  }

  /**
   * Convert ">" and "<" to the html versions, unless they are part of <b> or
   * </b>.  Go backwards through the string so that htmlMask is not invalidated.
   * @param rawString
   * @return
   */
  private String convertToHtml(String rawString) {
    if (debug) {
      System.out.println("TooltipFormatter.convertToHtml:rawString=" + rawString);
    }
    StringBuffer buffer = new StringBuffer(rawString);
    int index = rawString.length();
    HtmlMask htmlMask = new HtmlMask();
    htmlMask.mask(rawString);
    while (index >= 0) {
      index = Math.max(buffer.lastIndexOf("<", index - 1), buffer.lastIndexOf(">",
          index - 1));
      if (index != -1 && !htmlMask.isMasked(index)) {
        char c = buffer.charAt(index);
        buffer.deleteCharAt(index);
        if (c == '<') {
          buffer.insert(index, "<html>&lt");
        }
        else if (c == '>') {
          buffer.insert(index, "<html>&gt");
        }
      }
    }
    if (debug) {
      System.out.println("TooltipFormatter.convertToHtml:buffer.toString()="
          + buffer.toString());
    }
    return buffer.toString();
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  boolean isDebug() {
    return debug;
  }

  /**
   * converts from a string formatted with '\n' to an html string.  Doesn't
   * currently handle indents.  Wraps at N_COLUMNS.
   * @return
   */
  /*
   public String convert(String rawString) {
   if (rawString == null) {return null;}
   StringBuffer htmlFormat = new StringBuffer("<html>");
   int lineIndex = 0;
   for (int i = 0; i < rawString.length(); i++) {
   char currentChar = rawString.charAt(i);
   if (currentChar == '\n') {
   htmlFormat.append("<br>");
   lineIndex = 0;
   }
   else {
   htmlFormat.append(currentChar);
   lineIndex++;
   }
   if (lineIndex == N_COLUMNS) {
   htmlFormat.append("<br>");
   lineIndex = 0;
   }   
   }    
   return htmlFormat.toString();
   }*/

  private TooltipFormatter() {
  }

  private static final class HtmlMask {
    /**
     * Use only lower case in tagArray.
     */
    private final String[] tagArray = new String[] { "<b>", "</b>" };

    private boolean[] maskMap = null;

    private HtmlMask() {
    }

    /**
     * maskMap is constructed to be the same length as string.  Put a true at
     * each index which corresponds to a place in string which should be masked.
     * Not case sensitive.
     * @param string
     */
    private void mask(String string) {
      if (string == null) {
        maskMap = null;
        return;
      }
      string = string.toLowerCase();
      maskMap = new boolean[string.length()];
      for (int i = 0; i < maskMap.length; i++) {
        maskMap[i] = false;
      }
      if (tagArray == null || tagArray.length == 0) {
        return;
      }
      int iTag = 0;
      while (iTag < tagArray.length) {
        int iFind = 0;
        while (iFind != -1 && iFind < string.length()) {
          iFind = string.indexOf(tagArray[iTag], iFind);
          if (iFind != -1) {
            for (int i = iFind; i < iFind + tagArray[iTag].length(); i++) {
              maskMap[i] = true;
            }
            iFind += tagArray[iTag].length();
          }
        }
        iTag++;
      }
    }

    private boolean isMasked(int index) {
      if (maskMap == null || index < 0 || index >= maskMap.length) {
        return false;
      }
      return maskMap[index];
    }
  }
}
