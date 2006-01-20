package etomo.comscript;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005 - 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public interface Command {
  public static  final String  rcsid =  "$Id$";
  
  public String getCommandName();
  public String getCommandLine();
  public String[] getCommandArray();
}
/**
* <p> $Log$
* <p> Revision 1.7  2005/11/19 01:50:22  sueh
* <p> bug# 744 Moved functions only used by process manager post
* <p> processing and error processing from Commands to ProcessDetails.
* <p> This allows ProcesschunksParam to be passed to DetackedProcess
* <p> without having to add unnecessary functions to it.
* <p> </p>
*/