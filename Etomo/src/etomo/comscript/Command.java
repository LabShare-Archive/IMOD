package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;

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
  
  public CommandMode getCommandMode();
  public File getCommandOutputFile();
  public String getCommandName();
  public String getCommandLine();
  public String[] getCommandArray();
  public AxisID getAxisID();
  public String getCommand();//return the command or comscript file name
}
/**
* <p> $Log$
* <p> Revision 1.10  2006/05/22 22:35:33  sueh
* <p> bug# 577 Added getCommand().
* <p>
* <p> Revision 1.9  2006/05/11 19:38:30  sueh
* <p> bug# 838 Add CommandDetails, which extends Command and
* <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
* <p> functions.  Command contains all the command oriented functions.
* <p>
* <p> Revision 1.8  2006/01/20 20:45:32  sueh
* <p> updated copyright year
* <p>
* <p> Revision 1.7  2005/11/19 01:50:22  sueh
* <p> bug# 744 Moved functions only used by process manager post
* <p> processing and error processing from Commands to ProcessDetails.
* <p> This allows ProcesschunksParam to be passed to DetackedProcess
* <p> without having to add unnecessary functions to it.
* <p> </p>
*/