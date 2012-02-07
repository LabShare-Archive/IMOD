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
public interface DetachedCommandDetails extends CommandDetails {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * returns the command in a string which works, even if it contains
   * directory paths with embedded spaces.
   * @return
   */
  public String getCommandString();
  public String[] getCommandArray();
  public boolean isValid();
  public boolean isCommandNiced();
  public String getNiceCommand();
}
/**
* <p> $Log$
* <p> Revision 3.1  2009/09/01 03:17:47  sueh
* <p> bug# 1222
* <p>
* <p> Revision 1.3  2007/12/17 18:34:48  sueh
* <p> bug# 1061 Added isSecondCommandLine and getSecondCommandLine.
* <p>
* <p> Revision 1.2  2006/07/20 23:11:03  sueh
* <p> bug#k 885 Added isValid().
* <p>
* <p> Revision 1.1  2006/01/06 02:36:34  sueh
* <p> bug# 792 Command interface for DetachedCommand.  Can create a safe
* <p> command string that can go into a run file.
* <p> </p>
*/