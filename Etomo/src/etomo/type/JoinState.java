package etomo.type;

import java.util.Hashtable;
import java.util.Properties;

import etomo.BaseManager;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2005/12/16 18:26:06  sueh
 * <p> bug# 785 Added doneMode.
 * <p>
 * <p> Revision 1.3  2005/12/14 01:29:10  sueh
 * <p> bug# 782 Added toString().
 * <p>
 * <p> Revision 1.2  2005/07/29 19:47:24  sueh
 * <p> bug# 692 Added tests.  Changed ConstEtomoNumber.getInteger() to
 * <p> getInt.
 * <p>
 * <p> Revision 1.1  2004/12/14 21:47:35  sueh
 * <p> bug# 572 Represents the state of the join.  Contains items saved after
 * <p> processes are run.
 * <p> </p>
 */
public final class JoinState implements BaseState {
  public static final String rcsid = "$Id$";

  public static final String ROTATION_ANGLE_X = "RotationAngleX";
  public static final String ROTATION_ANGLE_Y = "RotationAngleY";
  public static final String ROTATION_ANGLE_Z = "RotationAngleZ";

  private static final String groupString = "JoinState";
  protected static final String sampleProducedString = "SampleProduced";
  protected static final boolean defaultSampleProduced = false;

  private final EtomoNumber doneMode = new EtomoNumber("DoneMode");

  //set on the successful completion of finishjoin
  private final EtomoNumber trialBinning = new EtomoNumber("TrialBinning");
  private final EtomoNumber trialSizeInX = new EtomoNumber("TrialSizeInX");
  private final EtomoNumber trialSizeInY = new EtomoNumber("TrialSizeInY");
  private final EtomoNumber trialShiftInX = new EtomoNumber("TrialShiftInX");
  private final EtomoNumber trialShiftInY = new EtomoNumber("TrialShiftInY");

  private final BaseManager manager;

  private Hashtable rotationAnglesList = null;
  private Hashtable revertRotationAnglesList = null;

  private final EtomoNumber totalRows = new EtomoNumber("TotalRows");
  private final EtomoNumber revertTotalRows = new EtomoNumber();

  //state variable for join setup tab
  protected boolean sampleProduced;

  public JoinState(BaseManager manager) {
    reset();
    this.manager = manager;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "trialBinning=" + trialBinning + ",trialSizeInX=" + trialSizeInX
        + ",\ntrialSizeInY=" + trialSizeInY + ",trialShiftInX=" + trialShiftInX
        + ",\ntrialShiftInY=" + trialShiftInY + ",sampleProduced="
        + sampleProduced + ",\ndoneMode=" + doneMode + "," + super.toString();
  }

  void reset() {
    doneMode.reset();
    trialBinning.reset();
    trialSizeInX.reset();
    trialSizeInY.reset();
    trialShiftInX.reset();
    trialShiftInY.reset();
    sampleProduced = defaultSampleProduced;
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    doneMode.store(props, prepend);
    trialBinning.store(props, prepend);
    trialSizeInX.store(props, prepend);
    trialSizeInY.store(props, prepend);
    trialShiftInX.store(props, prepend);
    trialShiftInY.store(props, prepend);
    props.setProperty(group + sampleProducedString, Boolean
        .toString(sampleProduced));
    totalRows.store(props, prepend);
    //store the rotation angles under their current row number
    if (rotationAnglesList != null) {
      for (int i = 0; i < totalRows.getInt(); i++) {
        SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList
            .get(new Integer(i));
        if (rotationAngles != null) {
          rotationAngles.store(props, SectionTableRowData.createPrepend(
              prepend, new EtomoNumber().set(i + 1)));
        }
      }
    }

  }

  public boolean equals(JoinState that) {
    return true;
  }

  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    doneMode.load(props, prepend);
    trialBinning.load(props, prepend);
    trialSizeInX.load(props, prepend);
    trialSizeInY.load(props, prepend);
    trialShiftInX.load(props, prepend);
    trialShiftInY.load(props, prepend);
    sampleProduced = Boolean.valueOf(
        props.getProperty(group + sampleProducedString, Boolean
            .toString(defaultSampleProduced))).booleanValue();
    totalRows.load(props, prepend);
    //retrieve the rotation angles by row number
    rotationAnglesList = null;
    if (!totalRows.isNull()) {
      for (int i = 0; i < totalRows.getInt(); i++) {
        SlicerAngles rotationAngles = new SlicerAngles();
        rotationAngles.load(props, SectionTableRowData.createPrepend(prepend,
            new EtomoNumber().set(i + 1)));
        if (!rotationAngles.isEmpty()) {
          if (rotationAnglesList == null) {
            rotationAnglesList = new Hashtable();
          }
          rotationAnglesList.put(new Integer(i), rotationAngles);
        }
      }
    }
  }

  public int getNewShiftInX(int min, int max) {
    return trialShiftInX.getInt() + (trialSizeInX.getInt() + 1) / 2
        - (max + min) / 2;
  }

  public SlicerAngles getRotationAngles(Integer index) {
    if (rotationAnglesList == null) {
      return null;
    }
    return (SlicerAngles) rotationAnglesList.get(index);
  }

  /**
   * calculate shift in y
   * @param min
   * @param max
   * @return
   */
  public int getNewShiftInY(int min, int max) {
    return trialShiftInY.getInt() + (trialSizeInY.getInt() + 1) / 2
        - (max + min) / 2;
  }

  public ConstEtomoNumber getTrialBinning() {
    return trialBinning;
  }

  public boolean isSampleProduced() {
    return sampleProduced;
  }
  
  public void setRevertState(boolean enableRevert) {
    if (enableRevert && rotationAnglesList != null) {
      revertRotationAnglesList = (Hashtable) rotationAnglesList.clone();
      revertTotalRows.set(totalRows);
    }
    else {
      revertRotationAnglesList = null;
      revertTotalRows.set("");
    }
  }
  
  public void revert() {
    rotationAnglesList = revertRotationAnglesList;
    totalRows.set(revertTotalRows);
  }

  public void deleteRow(int rowIndex) {
    if (rotationAnglesList == null) {
      return;
    }
    if (rowIndex >= totalRows.getInt()) {
      throw new IllegalStateException("rowIndex=" + rowIndex + ",totalRows="
          + totalRows);
    }
    //delete row
    Integer prevIndex = new Integer(rowIndex);
    rotationAnglesList.remove(prevIndex);
    //move the other rows up one row
    for (int i = rowIndex + 1; i < totalRows.getInt(); i++) {
      Integer curIndex = new Integer(i);
      SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList
          .remove(curIndex);
      if (rotationAngles != null) {
        rotationAnglesList.put(prevIndex, rotationAngles);
      }
      prevIndex = curIndex;
    }
  }
  
  public void printRotationAnglesList() {
    System.out.println("rotationAnglesList=" + rotationAnglesList);
  }

  public void moveRowUp(int rowIndex) {
    if (rotationAnglesList == null) {
      return;
    }
    if (rowIndex == 0 || rowIndex >= totalRows.getInt()) {
      throw new IllegalStateException("rowIndex=" + rowIndex + ",totalRows="
          + totalRows);
    }
    Integer curIndex = new Integer(rowIndex);
    Integer prevIndex = new Integer(rowIndex - 1);
    //swap the current row with the one above it
    SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList
        .remove(curIndex);
    SlicerAngles prevRotationAngles;
    if (rotationAngles != null) {
      prevRotationAngles = (SlicerAngles) rotationAnglesList.put(prevIndex,
          rotationAngles);
    }
    else {
      prevRotationAngles = (SlicerAngles) rotationAnglesList.remove(prevIndex);
    }
    if (prevRotationAngles != null) {
      rotationAnglesList.put(curIndex, prevRotationAngles);
    }
  }

  public void moveRowDown(int rowIndex) {
    if (rotationAnglesList == null) {
      return;
    }
    if (rowIndex >= totalRows.getInt() - 1) {
      throw new IllegalStateException("rowIndex=" + rowIndex + ",totalRows="
          + totalRows);
    }
    Integer curIndex = new Integer(rowIndex);
    Integer nextIndex = new Integer(rowIndex + 1);
    //swap the current row with the one below it
    SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList
        .remove(curIndex);
    SlicerAngles nextRotationAngles;
    if (rotationAngles != null) {
      nextRotationAngles = (SlicerAngles) rotationAnglesList.put(nextIndex,
          rotationAngles);
    }
    else {
      nextRotationAngles = (SlicerAngles) rotationAnglesList.remove(nextIndex);
    }
    if (nextRotationAngles != null) {
      rotationAnglesList.put(curIndex, nextRotationAngles);
    }
  }

  public void setRotationAnglesList(Hashtable rotationAnglesList) {
    this.rotationAnglesList = rotationAnglesList;
  }

  public void setTotalRows(int totalRows) {
    this.totalRows.set(totalRows);
  }

  public void setTrialBinning(int trialBinning) {
    this.trialBinning.set(trialBinning);
  }

  public void setTrialShiftInX(int trialShiftInX) {
    this.trialShiftInX.set(trialShiftInX);
  }

  public void setTrialShiftInY(int trialShiftInY) {
    this.trialShiftInY.set(trialShiftInY);
  }

  public void setTrialSizeInX(int trialSizeInX) {
    this.trialSizeInX.set(trialSizeInX);
  }

  public void setTrialSizeInY(int trialSizeInY) {
    this.trialSizeInY.set(trialSizeInY);
  }

  public void setSampleProduced(boolean sampleProduced) {
    this.sampleProduced = sampleProduced;
  }

  public final int getDoneMode() {
    return doneMode.getInt();
  }

  public final void setDoneMode(int doneMode) {
    this.doneMode.set(doneMode);
  }

  public final void clearDoneMode() {
    doneMode.reset();
  }
}