package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;

/**
 * <p>Description: "Business logic" functions for the UI.</p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.1  2010/03/27 05:12:23  sueh
 * <p> bug# 1326 Class for SDK independent functionality needed by the front
 * <p> end.  Added isRotated.
 * <p> </p>
 */
public final class FrontEndLogic {
  public static final String rcsid = "$Id$";

  /**
   * Reads the MRC header.  Returns true if rows (Y) are greater or equal to
   * sections (Z).  Returns false if Y < Z.  Returns null when the header is
   * unreadable. 
   * @param manager
   * @param axisID
   * @param file
   * @return true if Y >= Z
   */
  public static EtomoBoolean2 isRotated(BaseManager manager, AxisID axisID, File file) {
    MRCHeader header = MRCHeader.getInstance(file.getParent(), file.getName(), axisID);
    try {
      header.read(manager);
      EtomoBoolean2 retval = new EtomoBoolean2();
      retval.set(header.getNRows() >= header.getNSections());
      return retval;
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    return null;
  }
}
