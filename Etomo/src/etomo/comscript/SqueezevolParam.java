package etomo.comscript;

import java.util.Properties;

import etomo.type.ConstEtomoNumber;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.1  2004/12/01 03:46:14  sueh
* <p> bug# 557 Parameter for squeezevol.
* <p> </p>
*/
public class SqueezevolParam extends ConstSqueezevolParam {
  public static  final String  rcsid =  "$Id$";

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    
    reductionFactorXY.load(props, prepend);
    reductionFactorZ.load(props, prepend);
    linearInterpolation = Boolean.valueOf(props.getProperty(group
        + linearInterpolationString, Boolean.toString(defaultLinearInterpolation))).booleanValue();
  }

  public ConstEtomoNumber setReductionFactorXY(String reductionFactorXY) {
    return this.reductionFactorXY.set(reductionFactorXY);
  }
  
  public ConstEtomoNumber setReductionFactorZ(String reductionFactorZ) {
    return this.reductionFactorZ.set(reductionFactorZ);
  }
  
  public boolean setLinearInterpolation(boolean linearInterpolation) {
    return this.linearInterpolation = linearInterpolation;
  }
}
