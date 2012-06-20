package etomo.logic;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.channels.FileChannel;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.TomogramState;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

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
public final class TomogramTool {
  public final String rcsid = "$Id:$";

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
    try {
      if (header.read(manager)) {
        return !savedTomogramSizeColumns.equals(header.getNColumns())
            || !state.getTomogramSizeRows(axisID).equals(header.getNRows())
            || !state.getTomogramSizeSections(axisID).equals(header.getNSections());
      }
    }
    catch (IOException e) {
    }
    catch (InvalidParameterException e) {
    }
    return false;
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
    try {
      if (header.read(manager)) {
        state.setTomogramSizeColumns(axisID, header.getNColumns());
        state.setTomogramSizeRows(axisID, header.getNRows());
        state.setTomogramSizeSections(axisID, header.getNSections());
      }
    }
    catch (IOException e) {
    }
    catch (InvalidParameterException e) {
    }
  }

  /**
   * Calculates the starting slice for a subarea tomogram.  If height or yShift is
   * invalid, or the one of the results of the calculations is invalid, popups up an
   * errror message and returns null.
   * @param height - unbinned subarea tomogram height in Y
   * @param yShift - unbinned Y shift of the subarea tomogram
   * @return starting slice (long), empty etomoNumber (empty params or missing aligned stack), or null (invalid params)
   */
  public static ConstEtomoNumber getYStartingSlice(final BaseManager manager,
      final AxisID axisID, final String yHeight, final String yShift,
      final String yHeightLabel, final String yShiftLabel) {
    EtomoNumber enStartingSlice = new EtomoNumber(EtomoNumber.Type.LONG);
    if (yHeight == null || yHeight.matches("\\s*") || yShift == null
        || yShift.matches("\\s*")) {
      return enStartingSlice;
    }
    MRCHeader header = MRCHeader.getInstanceFromFileName(manager, axisID,
        FileType.ALIGNED_STACK.getFileName(manager, axisID));
    try {
      if (!header.read(manager)) {
        return enStartingSlice;
      }
    }
    catch (IOException e) {
      return enStartingSlice;
    }
    catch (InvalidParameterException e) {
      return enStartingSlice;
    }
    // Get the unbinned tomogram height in Y of the aligned stack
    long tomoHeight = header.getNRows()
        * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK);
    EtomoNumber enYHeight = new EtomoNumber();
    enYHeight.set(yHeight);
    if (!enYHeight.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid " + yHeightLabel + " - "
          + enYHeight.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    EtomoNumber enYShift = new EtomoNumber();
    enYShift.set(yShift);
    if (!enYShift.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid " + yShiftLabel + " - "
          + enYShift.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    enStartingSlice.set((tomoHeight - enYHeight.getInt()) / 2 - enYShift.getInt());
    if (!enStartingSlice.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid starting slice - "
          + enStartingSlice.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    if (enStartingSlice.lt(0)) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid starting slice: "
          + enStartingSlice + ".  " + yHeightLabel + " and/or " + yShiftLabel
          + " are incorrect.", "Entry Error", axisID);
      return null;
    }
    return enStartingSlice;
  }

  /**
   * Calculates the ending slice for a subarea tomogram.  If height is
   * invalid, or the one of the results of the calculations is invalid, popups up an
   * errror message and returns null.
   * @param startingSlice (long) - assumes this parameter is a valid number
   * @param height - unbinned subarea tomogram height in Y
   * @return ending slice (long), empty etomoNumber (empty params or missing aligned stack), or null (invalid params)
   */
  public static ConstEtomoNumber getYEndingSlice(final BaseManager manager,
      final AxisID axisID, final ConstEtomoNumber startingSlice, final String yHeight,
      final String yHeightLabel) {
    EtomoNumber enEndingSlice = new EtomoNumber(EtomoNumber.Type.LONG);
    if (startingSlice.isNull() || yHeight == null || yHeight.matches("\\s*")) {
      return enEndingSlice;
    }
    MRCHeader header = MRCHeader.getInstanceFromFileName(manager, axisID,
        FileType.ALIGNED_STACK.getFileName(manager, axisID));
    try {
      if (!header.read(manager)) {
        return enEndingSlice;
      }
    }
    catch (IOException e) {
      return enEndingSlice;
    }
    catch (InvalidParameterException e) {
      return enEndingSlice;
    }
    // Get the unbinned tomogram height in Y of the aligned stack
    long tomoHeight = header.getNRows()
        * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK);
    EtomoNumber enYHeight = new EtomoNumber();
    enYHeight.set(yHeight);
    if (!enYHeight.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid " + yHeightLabel + " - "
          + enYHeight.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    enEndingSlice.set(startingSlice.getLong() + enYHeight.getLong() - 1);
    if (!enEndingSlice.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid starting slice - "
          + enEndingSlice.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    if (enEndingSlice.ge(tomoHeight)) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid starting slice: "
          + enEndingSlice + ".  Check " + yHeightLabel + ".", "Entry Error", axisID);
      return null;
    }
    return enEndingSlice;
  }

  /**
   * Calculated the subarea y height and slice from the starting and ending slices
   * @param manager
   * @param axisID
   * @param yStartingSlice
   * @param yEndingSlice
   * @return pair of floats (height, shift) or null if aligned stack is missing.
   */
  public static long[] getYHeightAndShift(final BaseManager manager, final AxisID axisID,
      final long yStartingSlice, final long yEndingSlice) {
    long height = yEndingSlice + 1 - yStartingSlice;
    MRCHeader header = MRCHeader.getInstanceFromFileName(manager, axisID,
        FileType.ALIGNED_STACK.getFileName(manager, axisID));
    try {
      if (!header.read(manager)) {
        return null;
      }
    }
    catch (IOException e) {
      return null;
    }
    catch (InvalidParameterException e) {
      return null;
    }
    // Get the unbinned tomogram height in Y of the aligned stack
    long tomoHeight = header.getNRows()
        * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK);
    long shift = (tomoHeight - height) / 2 - yStartingSlice;
    return new long[] { height, shift };
  }
}
