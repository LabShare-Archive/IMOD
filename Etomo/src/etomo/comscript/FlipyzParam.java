package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.ManagerKey;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.19  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.18  2009/12/08 02:35:17  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.17  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.16  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.15  2007/12/13 01:05:12  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.14  2007/11/06 19:10:50  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.13  2007/05/11 15:27:19  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.12  2007/02/05 22:20:36  sueh
 * <p> bug# 962 Changed getCommandMode to return CommandMode.
 * <p>
 * <p> Revision 1.11  2006/05/22 22:39:05  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.10  2006/05/11 19:43:29  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.9  2006/04/06 18:59:14  sueh
 * <p> bug# 808 Implementing ProcessDetails.
 * <p>
 * <p> Revision 1.8  2006/01/20 20:47:11  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.7  2005/11/19 01:52:44  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.6  2005/04/25 20:39:53  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.5  2005/01/08 01:39:25  sueh
 * <p> bug# 578 Updated Command
 * <p> interface.
 * <p>
 * <p> Revision 1.4  2004/12/08 21:21:17  sueh
 * <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
 * <p>
 * <p> Revision 1.3  2004/12/04 00:34:30  sueh
 * <p> bug# 569 Handling directory paths with spaces:  converting from a
 * <p> command line to a command array to prevent the command line from
 * <p> being split on white space.
 * <p>
 * <p> Revision 1.2  2004/11/19 22:57:10  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.6  2004/11/12 22:48:27  sueh
 * <p> bug# 520 Added empty getIntegerValue and getBinning.
 * <p>
 * <p> Revision 1.1.2.5  2004/11/08 22:11:46  sueh
 * <p> bug# 520 Add getMode to conform to Command.  Returns 0, since there
 * <p> is no more for this Param.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/28 22:13:40  sueh
 * <p> bug# 520 Corrected clipflipyz's path.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/14 02:26:59  sueh
 * <p> bug# 520 Added TODO comment
 * <p>
 * <p> Revision 1.1.2.2  2004/10/08 15:47:41  sueh
 * <p> bug# 520 Get the checked-in version of clipflipyz.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/06 01:30:03  sueh
 * <p> bug# 520 Object that creates a command line to run clipflipyz.
 * <p> </p>
 */
public class FlipyzParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.CLIPFLIPYZ;
  public static final String COMMAND = "clipflipyz";
  private static final int commandSize = 3;
  private File flipFile;
  private String[] commandArray;

  public FlipyzParam(File tomogram, File workingDir) {
    //TODO use array for command string
    ArrayList options = genOptions(tomogram, workingDir);
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + COMMAND;
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  private ArrayList genOptions(File tomogram, File workingDir) {
    ArrayList options = new ArrayList(2);
    options.add(tomogram.getAbsolutePath());
    int index = tomogram.getName().lastIndexOf('.');
    StringBuffer flipFileName = new StringBuffer();
    if (index == -1) {
      flipFileName.append(tomogram.getName());
    }
    else {
      flipFileName.append(tomogram.getName().substring(0, index));
    }
    flipFile = new File(workingDir, flipFileName + ".flip");
    options.add(flipFile.getAbsolutePath());
    return options;
  }

  public File getFlipFile() {
    return flipFile;
  }

  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public String getCommandName() {
    return COMMAND;
  }

  public List getLogMessage(ManagerKey managerKey)
      throws LogFile.LockException, FileNotFoundException, IOException {
    return null;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getCommand() {
    return COMMAND;
  }

  public  String getName() {
    return COMMAND;
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  public File getCommandOutputFile() {
    return flipFile;
  }
  
  public File getCommandInputFile() {
    return null;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public ProcessName getSubcommandProcessName() {
    return null;
  }

  public int getIntValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public float getFloatValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstEtomoNumber getEtomoNumber(
      etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(
      etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public CommandMode getCommandMode() {
    return null;
  }
  public boolean isMessageReporter() {
    return false;
  }
}
