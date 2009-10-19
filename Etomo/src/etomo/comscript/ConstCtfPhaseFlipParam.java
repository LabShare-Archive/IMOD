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
 * <p> $Log$
 * <p> Revision 1.1  2008/10/27 17:44:34  sueh
 * <p> bug# 1141 Constant interface to CtfPhaseFlipParam.
 * <p> </p>
 */
public interface ConstCtfPhaseFlipParam {
  public static final String rcsid = "$Id$";

  public ConstEtomoNumber getVoltage();

  public ConstEtomoNumber getSphericalAberration();

  public boolean getInvertTiltAngles();

  public ConstEtomoNumber getAmplitudeContrast();

  public ConstStringParameter getDefocusFile();

  public ConstEtomoNumber getInterpolationWidth();

  public ConstEtomoNumber getDefocusTol();
}
