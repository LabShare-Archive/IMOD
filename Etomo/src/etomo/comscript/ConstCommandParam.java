package etomo.comscript;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$$ </p>
 */
public interface ConstCommandParam {
  public static final String rcsid = "$$Id$$";
  
  public boolean isParseComments();
  public String getProcessNameString();
  public String getCommand();
}
