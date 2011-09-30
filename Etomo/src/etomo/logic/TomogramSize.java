package etomo.logic;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.channels.FileChannel;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.TomogramState;
import etomo.util.DatasetFiles;
import etomo.util.MRCHeader;

/**
* <p>Description: For comparing tomogram size to TomogramState.tomogramSize fields.</p>
* 
* <p>Copyright: Copyright 2011</p>
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
public final class TomogramSize {
  public static final String rcsid = "$Id:$";

  /**
   * Returns true if any of the tomogram's dimensions listed in the header have changed.
   * @param manager
   * @param AxisIDFirst
   * @param popupAxisID
   * @return
   */
  public static boolean isTomogramSizeChanged(final ApplicationManager manager,
      final boolean AxisIDFirst, final AxisID popupAxisID) {
    AxisID axisID = AxisIDFirst ? AxisID.FIRST : AxisID.SECOND;
    TomogramState state = manager.getState();
    ConstEtomoNumber savedTomogramSizeColumns = state.getTomogramSizeColumns(axisID);
    if (savedTomogramSizeColumns.isNull()) {
      // Backward compatibility - check the size based only on deprecated tomogramSize.
      // This functionality should be removed eventually.
      ConstEtomoNumber savedTomogramSize = manager.getState().getTomogramSize(axisID);
      if (savedTomogramSize.isNull()) {
        return false;
      }
      return !manager.getState().getTomogramSize(axisID)
          .equals(getTomogramSize(manager, axisID));
    }
    MRCHeader header = MRCHeader.getInstanceFromFileName(manager, popupAxisID,
        DatasetFiles.getTomogram(manager, axisID).getName());
    return !savedTomogramSizeColumns.equals(header.getNColumns())
        || !state.getTomogramSizeRows(axisID).equals(header.getNRows())
        || !state.getTomogramSizeSections(axisID).equals(header.getNSections());
  }

  /**
   * Returns the physical size of the tomogram.
   * @param manager
   * @param axisID
   * @return
   */
  private static long getTomogramSize(final BaseManager manager, final AxisID axisID) {
    long size = 0;
    FileInputStream stream;
    try {
      stream = new FileInputStream(DatasetFiles.getTomogram(manager, axisID));
      FileChannel fileChannel = stream.getChannel();
      size = fileChannel.size();
    }
    catch (FileNotFoundException e) {
    }
    catch (IOException e) {
    }
    return size;
  }

  /**
   * Save the dimensions of the tomogram's dimensions listed in the header.
   * @param manager
   * @param axisID
   * @param popupAxisID
   */
  public static void saveTomogramSize(final ApplicationManager manager,
      final AxisID axisID, final AxisID popupAxisID) {
    TomogramState state = manager.getState();
    MRCHeader header = MRCHeader.getInstanceFromFileName(manager, popupAxisID,
        DatasetFiles.getTomogram(manager, axisID).getName());
    state.setTomogramSizeColumns(axisID, header.getNColumns());
    state.setTomogramSizeRows(axisID, header.getNRows());
    state.setTomogramSizeSections(axisID, header.getNSections());
  }
}
