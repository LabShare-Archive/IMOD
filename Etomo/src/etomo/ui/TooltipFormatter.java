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
 * <p> $Log$ </p>
 */

public class TooltipFormatter {
  public static final String rcsid = "$Id$";

  private int nColumns = 60;
  private String rawString = "";

  public TooltipFormatter() {
  }
  
  public TooltipFormatter(String str) {
    rawString = str;
  }
  
  public String format() {
    StringBuffer htmlFormat = new StringBuffer("<html>");
    boolean splitting = false;
    int idxStart = 0;
    while(splitting)  {
      int idxSearch = idxStart + nColumns;
      if(idxSearch >= rawString.length())
      int idxStop = rawString.indexOf()
    }
  }
}
