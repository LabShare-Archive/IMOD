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
public interface ConstCtfPlotterParam {
  public static  final String  rcsid =  "$Id$";
  
  public ConstStringParameter getConfigFile();
  public ConstEtomoNumber getExpectedDefocus();
  public ConstEtomoNumber getOffsetToAdd();
}
