package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;;
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
  
  
  private final String group;
  private final PanelHeaderState parallelHeaderState = new PanelHeaderState(PARALLEL_HEADER_GROUP);
  
  public BaseScreenState(AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    group =  "ScreenState" + axisID.getExtension().toUpperCase();
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
* <p> Revision 1.1  2005/09/27 21:19:02  sueh
* <p> bug# 532 This is a top level Storable object that should be written to the
* <p> .edf and .ejf files.  One instance per axis is created.
* <p> </p>
*/