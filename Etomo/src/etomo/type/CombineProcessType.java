package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class CombineProcessType {
  public static final String rcsid = "$Id$";

  public static final int SOLVEMATCH_INDEX = 0;
  public static final int MATCHVOL1_INDEX = 1;
  public static final int PATCHCORR_INDEX = 2;
  public static final int MATCHORWARP_INDEX = 3;
  public static final int VOLCOMBINE_INDEX = 4;
  
  public static final int TOTAL = VOLCOMBINE_INDEX + 1;

  private final int index;
  
  public static CombineProcessType getInstance(String processName) {
    if (ProcessName.SOLVEMATCH.equals(processName)) {
      return SOLVEMATCH;
    }
    if (ProcessName.MATCHVOL1.equals(processName)) {
      return MATCHVOL1;
    }
    if (ProcessName.PATCHCORR.equals(processName)) {
      return PATCHCORR;
    }
    if (ProcessName.MATCHORWARP.equals(processName)) {
      return MATCHORWARP;
    }
    if (ProcessName.VOLCOMBINE.equals(processName)) {
      return VOLCOMBINE;
    }
    return null;
  }
  
  public static CombineProcessType getInstance(int processIndex) {
    if (processIndex == SOLVEMATCH_INDEX) {
      return SOLVEMATCH;
    }
    if (processIndex == MATCHVOL1_INDEX) {
      return MATCHVOL1;
    }
    if (processIndex == PATCHCORR_INDEX) {
      return PATCHCORR;
    }
    if (processIndex == MATCHORWARP_INDEX) {
      return MATCHORWARP;
    }
    if (processIndex == VOLCOMBINE_INDEX) {
      return VOLCOMBINE;
    }
    return null;
  }

  private CombineProcessType(int index) {
    this.index = index;
  }

  public static final CombineProcessType SOLVEMATCH = new CombineProcessType(
      SOLVEMATCH_INDEX);
  public static final CombineProcessType MATCHVOL1 = new CombineProcessType(
      MATCHVOL1_INDEX);
  public static final CombineProcessType PATCHCORR = new CombineProcessType(
      PATCHCORR_INDEX);
  public static final CombineProcessType MATCHORWARP = new CombineProcessType(
      MATCHORWARP_INDEX);
  public static final CombineProcessType VOLCOMBINE = new CombineProcessType(
      VOLCOMBINE_INDEX);
  
  public int getIndex() {
    return index;
  }

  public String toString() {
    if (this == SOLVEMATCH) {
      return ProcessName.SOLVEMATCH.toString();
    }
    if (this == MATCHVOL1) {
      return ProcessName.MATCHVOL1.toString();
    }
    if (this == PATCHCORR) {
      return ProcessName.PATCHCORR.toString();
    }
    if (this == MATCHORWARP) {
      return ProcessName.MATCHORWARP.toString();
    }
    if (this == VOLCOMBINE) {
      return ProcessName.VOLCOMBINE.toString();
    }
    return null;
  }
}
/**
 * <p> $Log$ </p>
 */