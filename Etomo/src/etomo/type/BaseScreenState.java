package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;
import etomo.util.Utilities;
;
/**
* <p>Description: AxisID level storable object for the .edf and .ejf files.
* Should be used to store non-metadata.  Anything that is needed to run a
* process should not be stored in this object.</p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class BaseScreenState implements Storable {
  public static  final String  rcsid =  "$Id$";
  
  public static final String PARALLEL_HEADER_GROUP = "Parallel.Header";
  
  private final PanelHeaderState parallelHeaderState = new PanelHeaderState(PARALLEL_HEADER_GROUP);
  
  protected final AxisID axisID;
  private final AxisType axisType;
  private final String group;

  public BaseScreenState(AxisID axisID, AxisType axisType) {
    this.axisID = axisID;
    this.axisType = axisType;
    if (axisID == AxisID.ONLY && axisType == AxisType.DUAL_AXIS) {
      axisID = AxisID.FIRST;
    }
    else if (axisID == AxisID.FIRST && axisType == AxisType.SINGLE_AXIS) {
      axisID = AxisID.ONLY;
    }
    group =  "ScreenState" + axisID.getExtension().toUpperCase();
    selfTestInvariants();
  }
  
  void selfTestInvariants() {
    if (!Utilities.isSelfTest()) {
      return;
    }
    if (axisType == AxisType.NOT_SET) {
      throw new IllegalStateException("AxisType must be set.");
    }
    if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.SECOND) {
      throw new IllegalStateException("AxisID cannot be B in a single axis dataset.");
    }
    if (axisType == AxisType.SINGLE_AXIS && axisID != AxisID.ONLY) {
      throw new IllegalStateException("AxisID must be Only in a single axis dataset.");
    }
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      throw new IllegalStateException("AxisID must be A or B in a dual axis dataset.");
    }
    if (axisID == AxisID.FIRST && !group.endsWith("A")) {
      throw new IllegalStateException("Group must end with A for the first axis.");
    }
    if (axisID == AxisID.SECOND && !group.endsWith("B")) {
      throw new IllegalStateException("Group must end with B for the second axis.");
    }
  }
  
  protected final String getPrepend(String prepend) {
    if (prepend == "") {
      return group;
    }
    else {
      return prepend + "." + group;
    }
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    parallelHeaderState.store(props, prepend);
  }
  
  public void load(Properties props) {
    load(props, "");
  }
  
  public void load(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    parallelHeaderState.load(props, prepend);
  }
  
  public final PanelHeaderState getParallelHeaderState() {
    return parallelHeaderState;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/09/27 23:12:40  sueh
* <p> bug# 532 Separating the panel name (Parallel) from the element name
* <p> (Header) in the .edf file.
* <p>
* <p> Revision 1.1  2005/09/27 21:19:02  sueh
* <p> bug# 532 This is a top level Storable object that should be written to the
* <p> .edf and .ejf files.  One instance per axis is created.
* <p> </p>
*/