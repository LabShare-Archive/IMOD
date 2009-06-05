package etomo.comscript;
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
public interface ConstWarpVolParam {
  public static  final String  rcsid =  "$Id$";
  
  public String getTemporaryDirectory();
  public String getOutputSizeZ();
  public boolean isInterpolationOrderLinear();
}
