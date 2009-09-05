package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;

/**
 * <p>Description: Runs clip command.  Currently always uses the rotx option.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2009/09/01 03:17:47  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/04/01 20:04:46  sueh
 * <p> bug# 1208 Param class for running clip.
 * <p> </p>
 */
public final class ClipParam implements CommandDetails{
  public static final String rcsid = "$Id$";
  
  public static final ProcessName PROCESS_NAME  = ProcessName.CLIP;
  private static final int commandSize = 1;
  private File clipFile;
  private String[] commandArray;
  private boolean debug = true;
  
  public ClipParam(File tomogram, File workingDir) {
    //TODO use array for command string
    ArrayList options = genOptions(tomogram, workingDir);
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = BaseManager.getIMODBinPath() + PROCESS_NAME.toString();
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
    if (debug) {
      for (int i = 0; i < commandArray.length; i++) {
        System.err.print(commandArray[i]+" ");
      }
      System.err.println();
    }
  }
  
  public AxisID getAxisID() {
    return AxisID.ONLY;
  }
  
  private ArrayList genOptions(File tomogram, File workingDir) {
    ArrayList options = new ArrayList(3);
    options.add("rotx");
    options.add(tomogram.getAbsolutePath());
    int index = tomogram.getName().lastIndexOf('.');
    StringBuffer clipFileName = new StringBuffer();
    if (index == -1) {
      clipFileName.append(tomogram.getName());
    }
    else {
      clipFileName.append(tomogram.getName().substring(0, index));
    }
    //Still using .flip for the output name for clip rotx, since we used to flip
    //instead of rotate.
    clipFile = new File(workingDir, clipFileName + ".flip");
    options.add(clipFile.getAbsolutePath());
    return options;
  }

  public File getClipFile() {
    return clipFile;
  }
  
  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public String getCommandName() {
    return PROCESS_NAME.toString();
  }

  public String getCommand() {
    return PROCESS_NAME.toString();
  }

  public static String getName() {
    return PROCESS_NAME.toString();
  }
  
  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  public File getCommandOutputFile() {
    return clipFile;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }
  
  public ProcessName getSubcommandProcessName() {
    return null;
  }

  public int getIntValue( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }
  
  public IteratorElementList getIteratorElementList(
      final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList( final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public CommandMode getCommandMode() {
    return null;
  }
}
