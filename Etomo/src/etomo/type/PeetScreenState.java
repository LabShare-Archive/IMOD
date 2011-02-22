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
* <p> $Log$
* <p> Revision 1.3  2007/05/01 00:43:02  sueh
* <p> bug# 964 Removed the run parameter panel.
* <p>
* <p> Revision 1.2  2007/04/19 21:58:24  sueh
* <p> bug# 964 Added petRunParametersHeaderState and
* <p> peetRunParametersHeaderState.
* <p>
* <p> Revision 1.1  2007/02/21 04:21:20  sueh
* <p> bug# 964 Screen state for PEET interface.
* <p> </p>
*/
public final class PeetScreenState extends BaseScreenState implements
    ConstPeetScreenState {
  public static final String rcsid = "$Id$";

  private final PanelHeaderState peetSetupHeaderState = new PanelHeaderState(
      DialogType.PEET.getStorableName() + ".Setup." + PanelHeaderState.KEY);
  private final PanelHeaderState peetRunHeaderState = new PanelHeaderState(
      DialogType.PEET.getStorableName() + ".Run." + PanelHeaderState.KEY);

  public PeetScreenState(AxisID axisID, AxisType axisType) {
    super(axisID, axisType);
  }

  public PanelHeaderState getPeetSetupHeaderState() {
    return peetSetupHeaderState;
  }

  public PanelHeaderState getPeetRunHeaderState() {
    return peetRunHeaderState;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    prepend = getPrepend(prepend);
    peetSetupHeaderState.load(props, prepend);
    peetRunHeaderState.load(props, prepend);
  }

  public void store(Properties props) {
    store(props, "");
  }

  protected void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = getPrepend(prepend);
    peetSetupHeaderState.store(props, prepend);
    peetRunHeaderState.store(props, prepend);
  }
}
