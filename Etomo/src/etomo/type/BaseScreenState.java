package etomo.type;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;

import etomo.storage.Storable;

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
  private Properties localProperties = null;
  private String localPrepend = null;//the prepend used to save to localProperties
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
  }

  /**
   * Get a button state out of the local Properties object and return it as a
   * boolean
   * @param key - key to button state
   * @return
   */
  public final boolean getButtonState(String key) {
    return getButtonState(key, false);
  }

  /**
   * Get a button state out of the local Properties object and return it as a
   * boolean.  If the state is not available, return defaultState.
   * @param key
   * @param defaultState
   * @return
   */
  public final boolean getButtonState(String key, boolean defaultState) {
    if (key == null) {
      return defaultState;
    }
    if (keys == null) {
      keys = new HashSet();
    }
    keys.add(key);
    if (localProperties == null) {
      return defaultState;
    }
    EtomoState buttonState = new EtomoState(key);
    buttonState.load(localProperties, localPrepend);
    if (buttonState.isNull()) {
      return defaultState;
    }
    return buttonState.is();
  }

  /**
   * Sets the button state in a local Properties object
   * @param buttonState
   */
  public final void setButtonState(String key, boolean state) {
    if (key == null) {
      return;
    }
    if (localProperties == null) {
      localProperties = new Properties();
      localPrepend = getPrepend("");
    }
    localProperties.setProperty(localPrepend + '.' + key, String.valueOf(state));
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

  /**
   * Store the values in localProperties in props.
   * @param props
   * @param prepend
   */
  protected void store(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    parallelHeaderState.store(props, prepend);
    if (keys == null || localProperties == null) {
      //nothing to store
      return;
    }
    synchronized (this) {
      Iterator i = keys.iterator();
      String key = null;
      while (i.hasNext()) {
        key = (String) i.next();
        //get the value from the local property using the local prepend
        String state = localProperties.getProperty(localPrepend + '.' + key);
        if (state != null) {
          //store the value in props using the modified prepend parameter
          props.setProperty(prepend + '.' + key, state);
        }
      }
    }
  }

  public void load(Properties props) {
    load(props, "");
  }

  /**
   * point localProperties to props and set localPrepend to prepend
   * @param props
   * @param prepend
   */
  public void load(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    parallelHeaderState.load(props, prepend);
    localProperties = props;
    localPrepend = prepend;
  }

  public final PanelHeaderState getParallelHeaderState() {
    return parallelHeaderState;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.11  2006/03/28 17:00:34  sueh
 * <p> bug# 437 Added getButtonState(String key, boolean defaultState) to
 * <p> return defaultState when unable to get the value.
 * <p>
 * <p> Revision 1.10  2006/03/27 20:59:37  sueh
 * <p> bug# 836 Changed loadedProperties to localProperties.  Added comments.
 * <p> Removed selfTestInvariants.
 * <p>
 * <p> Revision 1.9  2006/03/20 17:56:33  sueh
 * <p> bug# 835 Renamed the group for the parallel processing processor list
 * <p> panel to ParallelProcess.Header, to distinguish it from the ParallelDialog.
 * <p>
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
