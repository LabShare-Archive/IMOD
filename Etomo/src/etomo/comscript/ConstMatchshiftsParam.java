/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$$</p>
 */
package etomo.comscript;

public class ConstMatchshiftsParam {
  public static final String rcsid = "$$Id$$";
  
  String rootName1;
  String rootName2;
  int xDim;
  int yDim;
  int zDim;
  
  private static final String command = "matchshifts";
  
  public ConstMatchshiftsParam() {
    reset();
  }
  
  protected void reset() {
    rootName1 = new String();
    rootName2 = new String();
    xDim = Integer.MIN_VALUE;
    yDim = Integer.MIN_VALUE;
    zDim = Integer.MIN_VALUE;
  }
  
  public String getCommand() {
    return command;
  }
}
