package etomo.comscript;

import java.util.Properties;

import etomo.ApplicationManager;
import etomo.type.ConstEtomoNumber;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002 - 2006</p>
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
* <p> Revision 1.6  2006/01/20 20:48:06  sueh
* <p> updated copyright year
* <p>
* <p> Revision 1.5  2005/07/29 00:49:55  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.4  2004/12/16 02:12:44  sueh
* <p> bug# 564 Saved flipped status.
* <p>
* <p> Revision 1.3  2004/12/14 21:32:55  sueh
* <p> bug# 557 Made separate variables for x and y reduction factors to handle
* <p> an unflipped tomogram.
* <p>
* <p> Revision 1.2  2004/12/02 18:26:02  sueh
* <p> bug# 557 Moved everything except public functions that change
* <p> parameters the ConstSqueezevolParam.  Added load().
* <p>
* <p> Revision 1.1  2004/12/01 03:46:14  sueh
* <p> bug# 557 Parameter for squeezevol.
* <p> </p>
*/
public class SqueezevolParam extends ConstSqueezevolParam {
  public static  final String  rcsid =  "$Id$";

  public SqueezevolParam(ApplicationManager manager) {
    super(manager);
  }
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
    
    reductionFactorX.load(props, prepend);
    reductionFactorY.load(props, prepend);
    reductionFactorZ.load(props, prepend);
    linearInterpolation = Boolean.valueOf(props.getProperty(group
        + linearInterpolationString, Boolean.toString(defaultLinearInterpolation))).booleanValue();
  }

  public ConstEtomoNumber setReductionFactorX(String reductionFactorX) {
    return this.reductionFactorX.set(reductionFactorX);
  }
  public CommandDetails getSubcommandDetails() {
    return null;
  }
  public ConstEtomoNumber setReductionFactorY(String reductionFactorY) {
    return this.reductionFactorY.set(reductionFactorY);
  }
  
  public ConstEtomoNumber setReductionFactorZ(String reductionFactorZ) {
    return this.reductionFactorZ.set(reductionFactorZ);
  }
  
  public boolean setLinearInterpolation(boolean linearInterpolation) {
    return this.linearInterpolation = linearInterpolation;
  }
  
  public boolean setFlipped(boolean flipped) {
    return this.flipped = flipped;
  }
}
