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
    FileInputStream stream = null;
    try {
      stream = new FileInputStream(DatasetFiles.getTomogram(manager, axisID));
      FileChannel fileChannel = stream.getChannel();
      size = fileChannel.size();
    }
    catch (FileNotFoundException e) {
    }
    catch (IOException e) {
    }
    if (stream != null) {
      try {
        stream.close();
      }
      catch (IOException e1) {
        e1.printStackTrace();
      }
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
   * @return starting slice, empty etomoNumber (empty params or missing aligned stack), or null (invalid params)
   */
  public static ConstEtomoNumber getYStartingSlice(final BaseManager manager,
      final AxisID axisID, final String yHeight, String yShift,
      final String yHeightLabel, final String yShiftLabel) {
    EtomoNumber enStartingSlice = new EtomoNumber();
    if (yHeight == null || yHeight.matches("\\s*")) {
      return enStartingSlice;
    }
    if (yShift == null || yShift.matches("\\s*")) {
      yShift = "0.0";
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
    int tomoHeight = header.getNRows()
        * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK);
    EtomoNumber enYHeight = new EtomoNumber();
    enYHeight.set(yHeight);
    if (!enYHeight.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid " + yHeightLabel + " - "
          + enYHeight.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    EtomoNumber enYShift = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    enYShift.set(yShift);
    if (!enYShift.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid " + yShiftLabel + " - "
          + enYShift.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    enStartingSlice.set(((tomoHeight - enYHeight.getInt()) / 2 - enYShift.getDouble()));
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
   * startingx = (nx - sizex) / 2 - shiftx
   * endingx = (nx + sizex) / 2 - shiftx - 1
   * startingy = (ny - sizey) / 2 - shifty
   * endingy = (ny + sizey) / 2 - shifty - 1
   * @param fileType
   * @param sizeX
   * @param shiftX
   * @param manager
   * @param axisID
   * @param sizeXDescr
   * @param shiftXDescr
   * @param errorMsgTitle
   * @return
   */
  public static PairXAndY getStartingAndEndingXAndY(final FileType fileType,
      final String sizeX, final String shiftX, final String sizeY, final String shiftY,
      final BaseManager manager, final AxisID axisID, final String sizeXDescr,
      final String shiftXDescr, final String sizeYDescr, final String shiftYDescr,
      final String errorMsgTitle) {
    MRCHeader header = MRCHeader.getInstance(manager, axisID, fileType);
    try {
      header.read(manager);
    }
    catch (IOException e) {
      return null;
    }
    catch (InvalidParameterException e) {
      return null;
    }
    PairXAndY pairXAndY = new PairXAndY();
    // X
    int n = header.getNColumns();
    int[] pair = getStartingAndEnding(n, sizeX, shiftX, manager, axisID, sizeXDescr,
        shiftXDescr, errorMsgTitle);
    pairXAndY.setX(pair);
    // Y
    n = header.getNRows();
    pair = getStartingAndEnding(n, sizeY, shiftY, manager, axisID, sizeYDescr,
        shiftYDescr, errorMsgTitle);
    pairXAndY.setY(pair);
    return pairXAndY;
  }

  /**
   * starting = (n - size) / 2 - shift
   * ending = (n + size) / 2 - shift - 1
   * @param fileN
   * @param sizeString
   * @param shiftString
   * @param manager
   * @param axisID
   * @param sizeDescr
   * @param shiftDescr
   * @param errorMsgTitle
   * @return
   */
  private static int[] getStartingAndEnding(final int fileN, final String sizeString,
      final String shiftString, final BaseManager manager, final AxisID axisID,
      final String sizeDescr, final String shiftDescr, final String errorMsgTitle) {
    EtomoNumber size = new EtomoNumber();
    size.set(sizeString);
    String errorMsg = size.validate(sizeDescr);
    if (errorMsg != null) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMsg, errorMsgTitle, axisID);
      return null;
    }
    if (size.isNull()) {
      size.set(fileN);
    }
    EtomoNumber shift = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    shift.set(shiftString);
    errorMsg = shift.validate(shiftDescr);
    if (errorMsg != null) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMsg, errorMsgTitle, axisID);
      return null;
    }
    if (shift.isNull()) {
      shift.set(0);
    }
    int starting = (int) Math.round((fileN - size.getInt()) / 2 - shift.getDouble());
    int ending = (int) Math.round((fileN + size.getInt()) / 2 - shift.getDouble() - 1);
    return new int[] { starting, ending };
  }

  /**
   * Calculates the ending slice for a subarea tomogram.  If height is
   * invalid, or the one of the results of the calculations is invalid, popups up an
   * errror message and returns null.
   * @param startingSlice - assumes this parameter is a valid number
   * @param height - unbinned subarea tomogram height in Y
   * @return ending slice, empty etomoNumber (empty params or missing aligned stack), or null (invalid params)
   */
  public static ConstEtomoNumber getYEndingSlice(final BaseManager manager,
      final AxisID axisID, final ConstEtomoNumber startingSlice, final String yHeight,
      final String yHeightLabel) {
    EtomoNumber enEndingSlice = new EtomoNumber();
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
    int tomoHeight = header.getNRows()
        * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK);
    EtomoNumber enYHeight = new EtomoNumber();
    enYHeight.set(yHeight);
    if (!enYHeight.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Invalid " + yHeightLabel + " - "
          + enYHeight.getInvalidReason() + "", "Entry Error", axisID);
      return null;
    }
    enEndingSlice.set(startingSlice.getInt() + enYHeight.getInt() - 1);
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
   * @return pair of ints (height, shift) or null if aligned stack is missing.
   */
  public static int[] getYHeightAndShift(final BaseManager manager, final AxisID axisID,
      final int yStartingSlice, final int yEndingSlice) {
    int height = yEndingSlice + 1 - yStartingSlice;
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
    int tomoHeight = header.getNRows()
        * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK);
    int shift = (tomoHeight - height) / 2 - yStartingSlice;
    return new int[] { height, shift };
  }

  /**
   * Negates shiftX and shiftY.  When colornewst is true and only one value is set, the
   * other one is set to zero.
   * @param shiftX
   * @param shiftY
   * @param colornewst
   * @return
   */
  public static ConstEtomoNumber[] convertShiftsToOffsets(final String shiftX,
      final String shiftY, final boolean colornewst) {
    EtomoNumber[] offsets = new EtomoNumber[2];
    offsets[0] = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    offsets[1] = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    int index = 0;
    boolean xEmpty = false;
    offsets[index].set(shiftX);
    if (!offsets[index].isNull()) {
      if (offsets[index].isValid()) {
        offsets[index].set(offsets[index].getDouble() * -1);
      }
    }
    else {
      xEmpty = true;
    }
    index = 1;
    boolean yEmpty = false;
    offsets[index].set(shiftY);
    if (!offsets[index].isNull()) {
      if (offsets[index].isValid()) {
        offsets[index].set(offsets[index].getDouble() * -1);
      }
    }
    else {
      yEmpty = true;
    }
    if (colornewst) {
      if (xEmpty && !yEmpty) {
        offsets[0].set(0);
      }
      else if (yEmpty && !xEmpty) {
        offsets[1].set(0);
      }
    }
    return offsets;
  }

  /**
   * Negates offset
   * @param offset
   * @return
   */
  public static ConstEtomoNumber convertOffsetToShift(final String offset) {
    EtomoNumber shift = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    shift.set(offset);
    if (!shift.isNull() && shift.isValid()) {
      shift.set(shift.getDouble() * -1);
    }
    return shift;
  }

  /**
   * Handle a pair of X and a pair of Y integer values.  Drops extra values when setting.
   * Each pair will always contain either 2 null values or 2 non-null values.
   * @author sueh
   */
  public static final class PairXAndY {
    private final EtomoNumber[] pairX = new EtomoNumber[2];
    private final EtomoNumber[] pairY = new EtomoNumber[2];

    private PairXAndY() {
      int i = 0;
      pairX[i] = new EtomoNumber();
      pairY[i] = new EtomoNumber();
      i++;
      pairX[i] = new EtomoNumber();
      pairY[i] = new EtomoNumber();
    }

    /**
     * Calls set.
     * @param pair
     */
    private void setX(final int[] pair) {
      set(pair, pairX);
    }

    /**
     * Calls set.
     * @param pair
     */
    private void setY(final int[] pair) {
      set(pair, pairY);
    }

    /**
     * Sets toPair.  A null fromPair or a fromPair containing only one non-null number
     * causes a reset of toPair.
     * @param fromPair
     * @param toPair
     */
    private void set(final int[] fromPair, final EtomoNumber[] toPair) {
      int i = 0;
      if (fromPair != null && fromPair.length >= 2) {
        toPair[i].set(fromPair[i]);
        i++;
        toPair[i].set(fromPair[i]);
        if (toPair[0].isNull()) {
          toPair[1].reset();
        }
        else if (toPair[1].isNull()) {
          toPair[0].reset();
        }
      }
      else {
        toPair[i].reset();
        i++;
        toPair[i].reset();
      }
    }

    public boolean isXNull() {
      return pairX[0].isNull();
    }

    public boolean isYNull() {
      return pairY[0].isNull();
    }

    /**
     * Returns pairX[0].  May return a null value.
     * @return
     */
    public int getFirstX() {
      return pairX[0].getInt();
    }

    /**
     * Returns pairX[1].  May return a null value.
     * @return
     */
    public int getSecondX() {
      return pairX[1].getInt();
    }

    /**
     * Returns pairY[0].  May return a null value.
     * @return
     */
    public int getFirstY() {
      return pairY[0].getInt();
    }

    /**
     * Returns pairY[1].  May return a null value.
     * @return
     */
    public int getSecondY() {
      return pairY[1].getInt();
    }
  }
}
