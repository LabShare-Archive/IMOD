package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
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
public class ReconScreenState extends BaseScreenState {
  public static  final String  rcsid =  "$Id$";
  
  public static final String TOMO_GEN_NEWST_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + ".Newst.Header";
  public static final String TOMO_GEN_MTFFILTER_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + ".Mtffilter.Header";
  public static final String TOMO_GEN_TILT_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + ".Tilt.Header";
  public static final String TOMO_GEN_TRIAL_TILT_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + ".TrialTilt.Header";
  
  private final PanelHeaderState tomoGenNewstHeaderState = new PanelHeaderState(TOMO_GEN_NEWST_HEADER_GROUP);
  private final PanelHeaderState tomoGenMtffilterHeaderState = new PanelHeaderState(TOMO_GEN_MTFFILTER_HEADER_GROUP);
  private final PanelHeaderState tomoGenTiltHeaderState = new PanelHeaderState(TOMO_GEN_TILT_HEADER_GROUP);
  private final PanelHeaderState tomoGenTrialTiltHeaderState = new PanelHeaderState(TOMO_GEN_TRIAL_TILT_HEADER_GROUP);
  
  public ReconScreenState(AxisID axisID) {
    super(axisID);
  }
  
  public void store(Properties props) {
    super.store(props);
  }
  
  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    tomoGenNewstHeaderState.store(props, prepend);
    tomoGenMtffilterHeaderState.store(props, prepend);
    tomoGenTiltHeaderState.store(props, prepend);
    tomoGenTrialTiltHeaderState.store(props, prepend);
  }
  
  public void load(Properties props) {
    super.load(props);
  }
  
  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    tomoGenNewstHeaderState.load(props, prepend);
    tomoGenMtffilterHeaderState.load(props, prepend);
    tomoGenTiltHeaderState.load(props, prepend);
    tomoGenTrialTiltHeaderState.load(props, prepend);
  }
  
  private final static String getAxisExtension(AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    return axisID.getExtension().toUpperCase();
  }
  
  public final PanelHeaderState getTomoGenNewstHeaderState() {
    return tomoGenNewstHeaderState;
  }
  
  public final PanelHeaderState getTomoGenMtffilterHeaderState() {
    return tomoGenMtffilterHeaderState;
  }
  
  public final PanelHeaderState getTomoGenTiltHeaderState() {
    return tomoGenTiltHeaderState;
  }
  
  public final PanelHeaderState getTomoGenTrialTiltHeaderState() {
    return tomoGenTrialTiltHeaderState;
  }
}
/**
* <p> $Log$ </p>
*/