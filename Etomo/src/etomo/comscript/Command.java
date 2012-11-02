package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ProcessName;

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
  public static final String rcsid = "$Id$";

  public CommandMode getCommandMode();

  public File getCommandOutputFile();

  public String getCommandName();

  public String getCommandLine();

  public String[] getCommandArray();

  public AxisID getAxisID();

  public String getCommand();//return the command that will be run or passed to vmstopy

  public CommandDetails getSubcommandDetails();

  public String getSubcommandProcessName();

  public ProcessName getProcessName();

  public File getCommandInputFile();

  public boolean isMessageReporter();

  public FileType getOutputImageFileType();

  public FileType getOutputImageFileType2();
}
/**
 * <p> $Log$
 * <p> Revision 1.16  2010/04/28 15:46:37  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.15  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.14  2009/12/11 17:26:12  sueh
 * <p> bug# 1291 Added getCommandInputFile.
 * <p>
 * <p> Revision 1.13  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.12  2007/11/06 19:04:49  sueh
 * <p> bug# 1047 Added getSubcommandDetails to allow a Command to contain
 * <p> another command.  This is useful for processchunks.
 * <p>
 * <p> Revision 1.11  2007/02/05 21:34:29  sueh
 * <p> bug# 962 Changed getCommandMode to return an interface for inner Mode classes (CommandMode) instead of an int.
 * <p>
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
