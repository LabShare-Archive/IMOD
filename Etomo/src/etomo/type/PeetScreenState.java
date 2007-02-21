package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
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
public final class PeetScreenState extends BaseScreenState implements ConstPeetScreenState {
  public static  final String  rcsid =  "$Id$";
  
  private final PanelHeaderState peetSetupHeaderState = new PanelHeaderState(
      DialogType.PEET.getStorableName()+".Setup."+PanelHeaderState.KEY);
  
  public PeetScreenState(AxisID axisID, AxisType axisType) {
    super(axisID,axisType);
  }
  
  public PanelHeaderState getPeetSetupHeaderState() {
    return peetSetupHeaderState;
  }
  
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props,prepend);
    prepend=getPrepend(prepend);
    peetSetupHeaderState.load(props,prepend);
  }
  
  public void store(Properties props) {
    store(props, "");
  }

  protected void store(Properties props, String prepend) {
    super.store(props,prepend);
    prepend=getPrepend(prepend);
    peetSetupHeaderState.store(props,prepend);
  }
}
