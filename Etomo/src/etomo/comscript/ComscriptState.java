package etomo.comscript;

/**
* <p>Description: </p>
*
* <p>Copyright: Copyright 2004 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$$ </p>
*/
public interface ComscriptState {
  public static final String rcsid = "$$Id$$";
  
  public int getStartCommand();
  public int getEndCommand();
  public String getCommand(int commandIndex);
  public String getWatchedFile(int commandIndex);
}
