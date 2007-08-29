package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;

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
 * <p> Revision 1.1  2004/12/14 21:40:52  sueh
 * <p> bug# 572 Interface for TomogramState and JoinState.
 * <p> </p>
 */
public abstract class BaseState implements Storable {
  public static final String rcsid = "$Id$";

  private static final String KILLED_PROCESSCHUNKS_PROCESS_NAME = "KilledProcesschunksProcessName";
  private static final String KILLED_PROCESSCHUNKS_PROCESS_NAME_A = "A."
      + KILLED_PROCESSCHUNKS_PROCESS_NAME;
  private static final String KILLED_PROCESSCHUNKS_PROCESS_NAME_B = "B."
      + KILLED_PROCESSCHUNKS_PROCESS_NAME;

  private ProcessName killedProcesschunksProcessNameA = null;
  private ProcessName killedProcesschunksProcessNameB = null;

  abstract String createPrepend(String prepend);

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    if (killedProcesschunksProcessNameA == null) {
      props.remove(prepend + "." + KILLED_PROCESSCHUNKS_PROCESS_NAME_A);
    }
    else {
      props.setProperty(prepend + "." + KILLED_PROCESSCHUNKS_PROCESS_NAME_A,
          killedProcesschunksProcessNameA.toString());
    }
    if (killedProcesschunksProcessNameB == null) {
      props.remove(prepend + "." + KILLED_PROCESSCHUNKS_PROCESS_NAME_B);
    }
    else {
      props.setProperty(prepend + "." + KILLED_PROCESSCHUNKS_PROCESS_NAME_B,
          killedProcesschunksProcessNameB.toString());
    }
  }

  public void load(Properties props, String prepend) {
    //reset
    killedProcesschunksProcessNameA = null;
    killedProcesschunksProcessNameB = null;
    //load
    prepend = createPrepend(prepend);
    killedProcesschunksProcessNameA = ProcessName.getInstance(props
        .getProperty(prepend + "." + KILLED_PROCESSCHUNKS_PROCESS_NAME_A));
    killedProcesschunksProcessNameB = ProcessName.getInstance(props
        .getProperty(prepend + "." + KILLED_PROCESSCHUNKS_PROCESS_NAME_B));
  }

  public ProcessName getKilledProcesschunksProcessName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return killedProcesschunksProcessNameB;
    }
    return killedProcesschunksProcessNameA;
  }

  public void setKilledProcesschunksProcessName(AxisID axisID,
      String input) {
    if (axisID == AxisID.SECOND) {
      killedProcesschunksProcessNameB = ProcessName.getInstance(input,axisID);
    }
    else {
      killedProcesschunksProcessNameA = ProcessName.getInstance(input,axisID);
    }
  }

  public boolean equals(BaseState input) {
    if (killedProcesschunksProcessNameA != input.killedProcesschunksProcessNameA) {
      return false;
    }
    if (killedProcesschunksProcessNameB != input.killedProcesschunksProcessNameB) {
      return false;
    }
    return true;
  }
}
