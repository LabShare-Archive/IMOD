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
 * <p> Revision 1.1  2003/07/28 23:59:32  rickg
 * <p> In progress
 * <p> </p>
 */

public class TooltipFormatter {
  public static final String rcsid =
    "$Id$";

  private int nColumns = 60;
  private String rawString = "";

  /**
   * Default constructor 
   *
   */
  public TooltipFormatter() {
  }

  /**
   * Raw string argument constructor 
   * @param str
   */
  public TooltipFormatter(String str) {
    rawString = str;
  }

  /**
   * Set the raw text string
   * @param str
   * @return
   */
  public TooltipFormatter setText(String str) {
    rawString = str;
    return this;
  }
  
  /**
   * Format the rawString into an HTML string appropriate for tooltips
   * @return the HTML formatted string
   */
  public String format() {
    StringBuffer htmlFormat = new StringBuffer("<html>");
    boolean splitting = true;
    int idxStart = 0;
    while (splitting) {
      int idxSearch = idxStart + nColumns;
      // Are we past the end of the string
      if (idxSearch >= rawString.length() - 1) {
        htmlFormat.append(
          rawString.substring(idxStart, rawString.length() - 1));
        splitting = false;
      }
      else {
        String subString = rawString.substring(idxStart, idxSearch);
        int idxStop = subString.lastIndexOf(' ');
        //  All one word!
        if (idxStop < 0) {
          idxStop = nColumns;
        }
        else {
          htmlFormat.append(
            rawString.substring(idxStart, idxStart + idxStop) + "<br>");
          idxStart = idxStart + idxStop + 1;
        }
      }
    }
    return htmlFormat.toString();
  }
}
