package etomo.ui;

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
          htmlFormat.append(convertToHtml(rawString.substring(idxStart, idxStart + idxStop))
              + "<br>");
          idxStart = idxStart + idxStop + 1;
        }
      }
    }
    return htmlFormat.toString();
  }

  private String convertToHtml(String rawString) {
    StringBuffer buffer = new StringBuffer(rawString);
    int ltIndex = rawString.length();
    int gtIndex = rawString.length();
    while (ltIndex >= 0 || gtIndex >= 0) {
      ltIndex = buffer.lastIndexOf("<", ltIndex - 1);
      if (ltIndex != -1) {
        buffer.deleteCharAt(ltIndex);
        buffer.insert(ltIndex, "<html>&lt");
      }
      gtIndex = buffer.lastIndexOf(">", gtIndex - 1);
      if (gtIndex != -1) {
        buffer.deleteCharAt(gtIndex);
        buffer.insert(gtIndex, "<html>&gt");
      }
    }
    return buffer.toString();
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
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
}
