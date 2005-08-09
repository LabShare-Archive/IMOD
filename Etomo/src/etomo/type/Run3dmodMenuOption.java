package etomo.type;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class Run3dmodMenuOption {
  public static  final String  rcsid =  "$Id$";
  
  private static final int noneIndex = 0;
  private static final int startupWindowIndex = 1;
  private static final int binBy2Index = 2;
  private static final int binBy2XYIndex = 3;

  private final int index;
  
  private Run3dmodMenuOption(int index) {
    this.index = index;
  }
  
  public int toIndex() {
    return index;
  }
  
  public static final Run3dmodMenuOption NONE = new Run3dmodMenuOption(noneIndex);
  public static final Run3dmodMenuOption STARTUP_WINDOW = new Run3dmodMenuOption(startupWindowIndex);
  public static final Run3dmodMenuOption BIN_BY_2 = new Run3dmodMenuOption(binBy2Index);
  public static final Run3dmodMenuOption BIN_BY_2_XY = new Run3dmodMenuOption(binBy2XYIndex);
}
/**
* <p> $Log$ </p>
*/