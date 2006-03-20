package etomo.type;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;

import etomo.storage.Storable;
import etomo.util.Utilities;

/**
 * <p>Description: AxisID level storable object for the .edf and .ejf files.
 * Should be used to store non-metadata.  Anything that is needed to run a
 * process should not be stored in this object.</p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
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
  public static final String rcsid = "$Id$";

  public static final String PARALLEL_HEADER_GROUP = "ParallelProcess.Header";

  private final PanelHeaderState parallelHeaderState = new PanelHeaderState(
      PARALLEL_HEADER_GROUP);

  protected final AxisID axisID;
  private final AxisType axisType;
  private final String group;
  private Properties loadedProperties = null;
  private String loadedPrepend = "";
  private HashSet keys = null;

  public BaseScreenState(AxisID axisID, AxisType axisType) {
    if (axisID == AxisID.ONLY && axisType == AxisType.DUAL_AXIS) {
      axisID = AxisID.FIRST;
    }
    else if (axisID == AxisID.FIRST && axisType == AxisType.SINGLE_AXIS) {
      axisID = AxisID.ONLY;
    }
    group = "ScreenState" + axisID.getExtension().toUpperCase();
    this.axisID = axisID;
    this.axisType = axisType;
    selfTestInvariants();
  }

  public final boolean getButtonState(String key) {
    if (key == null) {
      return false;
    }
    if (keys == null) {
      keys = new HashSet();
    }
    keys.add(key);
    if (loadedProperties == null) {
      return false;
    }
    EtomoBoolean2 buttonState = new EtomoBoolean2(key);
    buttonState.load(loadedProperties, loadedPrepend);
    return buttonState.is();
  }

  /**
   * set buttonState in the buttonStates hashtable
   * @param buttonState
   */
  public final void setButtonState(String key, boolean state) {
    if (key == null) {
      return;
    }
    if (loadedProperties == null) {
      loadedProperties = new Properties();
      loadedPrepend = getPrepend("");
    }
    loadedProperties.setProperty(loadedPrepend + '.' + key, String
        .valueOf(state));
  }

  void selfTestInvariants() {
    if (!Utilities.isSelfTest()) {
      return;
    }
    if (axisType == AxisType.NOT_SET) {
      throw new IllegalStateException("AxisType must be set; axisType="
          + axisType + ".");
    }
    if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.SECOND) {
      throw new IllegalStateException(
          "AxisID cannot be B in a single axis dataset; axisType=" + axisType
              + ",axisID=" + axisID + ".");
    }
    if (axisType == AxisType.SINGLE_AXIS && axisID != AxisID.ONLY) {
      throw new IllegalStateException(
          "AxisID must be Only in a single axis dataset; axisType=" + axisType
              + ",axisID=" + axisID + ".");
    }
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      throw new IllegalStateException(
          "AxisID must be A or B in a dual axis dataset; axisType=" + axisType
              + ",axisID=" + axisID + ".");
    }
    if (axisID == AxisID.FIRST && !group.endsWith("A")) {
      throw new IllegalStateException(
          "Group must end with A for the first axis; group=" + group
              + ",axisID=" + axisID + ".");
    }
    if (axisID == AxisID.SECOND && !group.endsWith("B")) {
      throw new IllegalStateException(
          "Group must end with B for the second axis; group=" + group
              + ",axisID=" + axisID + ".");
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

  protected void store(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    parallelHeaderState.store(props, prepend);
    if (keys == null || loadedProperties == null) {
      return;
    }
    synchronized (this) {
      Iterator i = keys.iterator();
      String key = null;
      while (i.hasNext()) {
        key = (String) i.next();
        //get the loaded property and store it in props
        String state = loadedProperties.getProperty(loadedPrepend + '.' + key);
        if (state != null) {
          props.setProperty(prepend + '.' + key, state);
        }
      }
    }
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    parallelHeaderState.load(props, prepend);
    loadedProperties = props;
    loadedPrepend = prepend;
  }

  public final PanelHeaderState getParallelHeaderState() {
    return parallelHeaderState;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2006/01/26 21:56:57  sueh
 * <p> bug# 401 For new datasets, create loadedProperties and loadedPrepend
 * <p>
 * <p> Revision 1.7  2006/01/20 21:08:18  sueh
 * <p> bug# 401 Fixed store(props, prepend):  it should be called by the child class
 * <p>
 * <p> Revision 1.6  2006/01/20 21:00:43  sueh
 * <p> bug# 401 Added generic functionality to store button states by keeping the
 * <p> reference to Properties prop from the load() function (loadedProperties)
 * <p> and keeping it up to date.  Keep a list of the keys used to update
 * <p> loadedProperties.  In store() copy the updated properties to the new
 * <p> instance of Properties.
 * <p>
 * <p> Revision 1.5  2005/11/14 21:27:04  sueh
 * <p> removed extra ;'s.
 * <p>
 * <p> Revision 1.4  2005/10/21 21:11:32  sueh
 * <p> bug# 743 fixed bug in BaseScreenState().
 * <p>
 * <p> Revision 1.3  2005/09/29 18:44:44  sueh
 * <p> bug# 532 Improved the handling of the different axis'.  Setting the axisID
 * <p> according to the axisType; so a single axis tomogram uses axis.ONLY
 * <p> and a dual axis used axis.FIRST.  Added member variables AxisID and
 * <p> AxisType.  Since axis.only and axis.first have different meanings, axisID
 * <p> can be used to decide if combine fields should be saved.  AxisType is
 * <p> used in selfTestInvarients() to make sure that the AxisID was set
 * <p> correctly.
 * <p>
 * <p> Revision 1.2  2005/09/27 23:12:40  sueh
 * <p> bug# 532 Separating the panel name (Parallel) from the element name
 * <p> (Header) in the .edf file.
 * <p>
 * <p> Revision 1.1  2005/09/27 21:19:02  sueh
 * <p> bug# 532 This is a top level Storable object that should be written to the
 * <p> .edf and .ejf files.  One instance per axis is created.
 * <p> </p>
 */