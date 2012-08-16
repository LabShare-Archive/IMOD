package etomo.logic;

import java.io.File;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.ui.swing.UIHarness;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class TransformsTool {
  public static final String rcsid = "$Id:$";

  private static final String TITLE = "Dataset Consistency Warning";

  /**
   * Pops up a warning and returns false if the edge function file (.xef) was not
   * successfully created after the .ecd file.  Ignores the Y edge function file (.yef).
   * @param invalidEdgeFunctions
   * @param manager
   * @param axisID
   * @param locationDescr
   * @param createEdgeFunctionsDescr
   * @return
   */
  public static boolean checkUpToDateEdgeFunctionsFile(
      final boolean invalidEdgeFunctions, final BaseManager manager, final AxisID axisID,
      final String locationDescr, final String createEdgeFunctionsDescr) {
    String instructions = "  Please open " + locationDescr + " and run "
        + createEdgeFunctionsDescr + ".";
    File edgeFunctionsFile = FileType.EDGE_FUNCTIONS_X.getFile(manager, axisID);
    if (!edgeFunctionsFile.exists()) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Warning: Edge functions do not exist." + instructions, TITLE, axisID);
      return false;
    }
    File pieceShiftsFile = FileType.PIECE_SHIFTS.getFile(manager, axisID);
    if (!pieceShiftsFile.exists()) {
      return true;
    }
    if (invalidEdgeFunctions) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Warning: Edge functions are invalid." + instructions, TITLE, axisID);
      return false;
    }
    if (pieceShiftsFile.lastModified() > edgeFunctionsFile.lastModified()) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Warning: Edge functions are out of date." + instructions, TITLE, axisID);
      return false;
    }
    return true;
  }
}
