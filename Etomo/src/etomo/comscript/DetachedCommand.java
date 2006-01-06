package etomo.comscript;
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
public interface DetachedCommand extends Command {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * returns the command in a string which works, even if it contains
   * directory paths with embedded spaces.
   * @return
   */
  public String getCommandString();
}
/**
* <p> $Log$ </p>
*/