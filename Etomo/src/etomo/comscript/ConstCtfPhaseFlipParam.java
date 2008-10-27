package etomo.comscript;

import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
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
public interface ConstCtfPhaseFlipParam {
  public static  final String  rcsid =  "$Id$";
  
  public ConstEtomoNumber getVoltage();
  public ConstEtomoNumber getSphericalAberration();
  public ConstEtomoNumber getAmplitudeContrast();
  public ConstStringParameter getDefocusFile();
  public ConstEtomoNumber getInterpolationWidth();
  public ConstEtomoNumber getDefocusTol();
}
