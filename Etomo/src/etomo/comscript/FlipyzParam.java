package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
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
public class FlipyzParam implements Command {
  public static  final String  rcsid =  "$Id$";
  
  public static final String command = "clipflipyz";
  private static final int commandSize = 3;
  private File flipFile;
  private String[] commandArray;
  
  public FlipyzParam(File tomogram, File workingDir) {
    //TODO use array for command string
    ArrayList options = genOptions(tomogram, workingDir);
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + command;          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
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
    return command;
  }
  
  public static String getName() {
    return command;
  }
  
  public String[] getCommandArray() {
    return commandArray;
  }
  
  public File getOutputFile() {
    return flipFile;
  }
  
  public int getIntegerValue(int name) {
    return Integer.MIN_VALUE;
  }
  
  public boolean getBooleanValue(int name) {
    return false;
  }
  
  public int getMode() {
    return 0;
  }
  
  public int getBinning() {
    return 1;
  }
}
