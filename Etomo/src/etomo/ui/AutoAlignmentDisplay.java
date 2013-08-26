package etomo.ui;

import etomo.comscript.MidasParam;
import etomo.comscript.XfalignParam;
import etomo.type.AxisID;
import etomo.type.DialogType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public interface AutoAlignmentDisplay {
  public static final String rcsid = "$Id:$";

  public void msgProcessEnded();

  public DialogType getDialogType();

  public AxisID getAxisID();

  public void getAutoAlignmentParameters(MidasParam param);

  public boolean getAutoAlignmentParameters(XfalignParam param, boolean doValidation);
}
