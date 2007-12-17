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
  public boolean isValid();
  public boolean isSecondCommandLine();
  public String getSecondCommandLine();
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/07/20 23:11:03  sueh
* <p> bug#k 885 Added isValid().
* <p>
* <p> Revision 1.1  2006/01/06 02:36:34  sueh
* <p> bug# 792 Command interface for DetachedCommand.  Can create a safe
* <p> command string that can go into a run file.
* <p> </p>
*/