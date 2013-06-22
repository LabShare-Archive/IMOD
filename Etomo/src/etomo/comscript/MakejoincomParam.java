package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.process.SystemProgram;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.JoinState;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.SectionTableRowData;
import etomo.type.SlicerAngles;
import etomo.util.DatasetFiles;

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
 * <p> Old log: Makejoincom.java,v
 * <p> Revision 1.1.2.1  2004/09/29 17:46:50  sueh
 * <p> bug# 520 Class to run the makejoincom script.  Gets its options from
 * <p> ConstJoinMetaData.
 * <p> </p>
 * 
 * <p> $Log$
 * <p> Revision 1.37  2011/06/18 04:22:20  sueh
 * <p> Bug# 1494 Makejoincom is now a python script.
 * <p>
 * <p> Revision 1.36  2011/05/10 16:49:36  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 1.35  2011/02/22 03:15:50  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.34  2010/04/28 15:58:49  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.33  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.32  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.31  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.30  2009/12/08 02:35:31  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.29  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.28  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.27  2007/12/13 01:05:20  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.26  2007/11/06 19:11:27  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.25  2007/05/11 15:27:55  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.24  2007/03/26 18:34:51  sueh
 * <p> bug# 964 Changed getDouble(boolean defaultIfNull) to getDefaultDouble() so that
 * <p> the functionality will be remembered and used.
 * <p>
 * <p> Revision 1.23  2007/03/07 21:01:35  sueh
 * <p> bug# 981 Changed ScriptParameter.isUseInScript to isNotNullAndNotDefault for
 * <p> clarity.
 * <p>
 * <p> Revision 1.22  2007/02/05 22:33:56  sueh
 * <p> bug# 962 Fixed problem where -already was not being used with -rot.
 * <p>
 * <p> Revision 1.21  2006/10/17 20:02:21  sueh
 * <p> bug# 939  Simplify genOptions() using ConstEtomoNumber.getDouble(boolean
 * <p> defaultIfNull).
 * <p>
 * <p> Revision 1.20  2006/05/22 22:39:16  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.19  2006/05/11 19:44:38  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.18  2006/04/06 19:33:36  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.  Added rotationAnglesList to save the rotation angles.
 * <p> Using -already when a .rot file has already been generated with the same
 * <p> angles.
 * <p>
 * <p> Revision 1.17  2006/01/20 20:47:21  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.16  2005/12/06 22:59:37  sueh
 * <p> bug# 757 Added maxxysize option
 * <p>
 * <p> Revision 1.15  2005/11/29 22:21:59  sueh
 * <p> bug# 757 Use setup section for make join.
 * <p>
 * <p> Revision 1.14  2005/11/19 01:52:56  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.13  2005/11/02 23:58:36  sueh
 * <p> bug# 738 Midas limit has a display value, so don't test for null.
 * <p>
 * <p> Revision 1.12  2005/11/02 23:57:01  sueh
 * <p> bug# 738 Added midas limit.
 * <p>
 * <p> Revision 1.11  2005/05/12 01:22:35  sueh
 * <p> bug# 520 Take rotation angle defaults from ConstEtomoNumber variables.
 * <p>
 * <p> Revision 1.10  2005/05/09 22:59:33  sueh
 * <p> bug# 658 Removed ScriptParameter.addToScript because is was only
 * <p> being use in one place.  In genOptions() replaced addToScript with code
 * <p> that does the same thing.
 * <p>
 * <p> Revision 1.9  2005/04/25 20:40:08  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.8  2005/01/25 22:47:02  sueh
 * <p> Adding boolean force parameter to ScriptParameter.addToScript() to tell
 * <p> the function to avoid checking isUseInScript()
 * <p>
 * <p> Revision 1.7  2005/01/25 21:42:02  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
 * <p> Revision 1.6  2005/01/21 22:42:19  sueh
 * <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
 * <p> isSetAndNotDefault() as a standard why to decide if a parameter should
 * <p> be placed in a comscript.
 * <p>
 * <p> Revision 1.5  2005/01/08 01:39:40  sueh
 * <p> bug# 578 Updated Command interface.
 * <p>
 * <p> Revision 1.4  2004/12/08 21:21:27  sueh
 * <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
 * <p>
 * <p> Revision 1.3  2004/12/01 03:45:47  sueh
 * <p> bug# 520 Removed unnecessary member variable SystemProgram
 * <p> program.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:04:18  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.13  2004/11/19 00:01:10  sueh
 * <p> bug# 520 Removed unnecessary pass-through functions from
 * <p> ConstSectionTableRowData.
 * <p>
 * <p> Revision 1.1.2.12  2004/11/16 23:27:56  sueh
 * <p> bug# 520 Bug fix:  In genOptions(), checking for sectionData == null
 * <p> before using.
 * <p>
 * <p> Revision 1.1.2.11  2004/11/16 02:20:46  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.10  2004/11/15 22:17:10  sueh
 * <p> bug# 520 Implementing Command.
 * <p>
 * <p> Revision 1.1.2.9  2004/10/30 01:31:40  sueh
 * <p> bug# 520 bug fix: A missing rotationAngleX caused the param to not use
 * <p> the -rot options.  Now the option will be used if any rotation angle is set and
 * <p> the unset ones will be defaulted.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/29 22:07:58  sueh
 * <p> bug# 520 Use -tmpext to set the temp file extension to .rot.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/29 01:17:19  sueh
 * <p> bug# 520 Removed working directory from meta data.  Getting working
 * <p> directory from propertyUserDir.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/22 20:57:50  sueh
 * <p> bug# 520 Simplifying ConstSectionTableRowData by passing
 * <p> EtomoSimpleType instead of String and int.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/22 03:20:52  sueh
 * <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
 * <p> passing EtomoInteger, EtomoFloat, etc and using its get() and getString()
 * <p> functions.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/21 02:34:59  sueh
 * <p> bug# 520 Removed unnecessary function run().
 * <p>
 * <p> Revision 1.1.2.3  2004/10/18 17:42:02  sueh
 * <p> bug# 520 Added -reference to the command string.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/14 02:27:53  sueh
 * <p> bug# 520 Setting working directory in SystemProgram.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/08 15:50:06  sueh
 * <p> bug# 520 Renamed Makejoincom to MakejoincomParam.  Switched from
 * <p> a command line to a command array because of the possibility of spaces
 * <p> within parameters.
 * <p> </p>
 */
public final class MakejoincomParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  public static final int MIDAS_LIMIT_DEFAULT = 1024;

  private static final int commandSize = 3;
  private static final ProcessName PROCESS_NAME = ProcessName.MAKEJOINCOM;
  public static final String commandName = "makejoincom";
  private static final boolean debug = true;

  private final ConstJoinMetaData metaData;
  private final JoinState state;
  private final BaseManager manager;

  private String[] commandArray;
  private SystemProgram program;

  private Hashtable rotationAnglesList = null;
  private boolean rotate = false;
  private int totalRows = 0;

  public MakejoincomParam(ConstJoinMetaData metaData, JoinState state, BaseManager manager) {
    this.metaData = metaData;
    this.state = state;
    this.manager = manager;
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "python";
    commandArray[1] = "-u";
    commandArray[2] = BaseManager.getIMODBinPath() + commandName;
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
    if (debug) {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < commandArray.length; i++) {
        buffer.append(commandArray[i]);
        if (i < commandArray.length - 1) {
          buffer.append(' ');
        }
      }
      System.err.println(buffer.toString());
    }
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    ArrayList sectionData = metaData.getSectionTableData();
    if (sectionData != null) {
      totalRows = sectionData.size();
      for (int i = 0; i < totalRows; i++) {
        ConstSectionTableRowData screen = (SectionTableRowData) sectionData.get(i);
        File section = screen.getSetupSection();
        if (i < totalRows - 1) {
          options.add("-top");
          //both numbers must exist
          options.add(screen.getSampleTopStart().toString() + ","
              + screen.getSampleTopEnd().toString());
        }
        if (i != 0) {
          options.add("-bot");
          //both numbers must exist
          options.add(screen.getSampleBottomStart().toString() + ","
              + screen.getSampleBottomEnd().toString());
        }
        //Is the volume rotated?
        if (screen.isRotated()) {
          rotate = true;
          //Get the rotation angles from the screen.  Use 0 in place of null.
          ConstEtomoNumber rotationAngleX = screen.getRotationAngleX();
          ConstEtomoNumber rotationAngleY = screen.getRotationAngleY();
          ConstEtomoNumber rotationAngleZ = screen.getRotationAngleZ();
          //Save the angles, so they can be saved in the state object.
          SlicerAngles rotationAngles = new SlicerAngles();
          rotationAngles.setX(rotationAngleX);
          rotationAngles.setY(rotationAngleY);
          rotationAngles.setZ(rotationAngleZ);
          if (rotationAnglesList == null) {
            rotationAnglesList = new Hashtable();
          }
          Integer hashKey = new Integer(i);
          rotationAnglesList.put(hashKey, rotationAngles);
          //The rotation file does not exist or the angles have changed.
          //Add the -rot option to run rotatevol and save the angles.
          options.add("-rot");
          StringBuffer buffer = new StringBuffer();
          buffer.append(rotationAngleX.getDefaultedDouble());
          buffer.append(",");
          buffer.append(rotationAngleY.getDefaultedDouble());
          buffer.append(",");
          buffer.append(rotationAngleZ.getDefaultedDouble());
          options.add(buffer.toString());
          options.add("-maxxysize");
          //Get the .rot file
          String rotFileName = section.getName();
          int extIndex = rotFileName.lastIndexOf('.');
          if (extIndex == -1) {
            rotFileName = rotFileName + DatasetFiles.ROTATED_TOMO_EXT;
          }
          else {
            rotFileName = rotFileName.substring(0, extIndex)
                + DatasetFiles.ROTATED_TOMO_EXT;
          }
          File rotFile = new File(manager.getPropertyUserDir(), rotFileName);
          //See if the rotation file exists and has the same angles
          SlicerAngles curRotationAngles = state.getRotationAngles(hashKey);
          if (rotFile.exists() && curRotationAngles != null
              && rotationAngleX.equals(curRotationAngles.getX())
              && rotationAngleY.equals(curRotationAngles.getY())
              && rotationAngleZ.equals(curRotationAngles.getZ())) {
            //Use the existing .rot file, since the angles haven't changed.
            options.add("-already");
          }
        }
        options.add(section.getAbsolutePath());
      }
    }
    options.add("-tmpext");
    options.add("rot");
    ScriptParameter densityRefSection = metaData.getDensityRefSectionParameter();
    if (densityRefSection.isNotNullAndNotDefault()) {
      options.add("-ref");
      options.add(densityRefSection.toString());
    }
    ConstEtomoNumber number = metaData.getMidasLimit();
    options.add("-midaslim");
    options.add(metaData.getMidasLimit().toString());
    options.add(metaData.getName());
    return options;
  }

  public String getCommand() {
    return commandName;
  }

  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public String getCommandName() {
    return commandName;
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public int getIntValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.TOTAL_ROWS) {
      return totalRows;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public boolean getBooleanValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.ROTATE) {
      return rotate;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.ROTATION_ANGLES_LIST) {
      return rotationAnglesList;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public String getName() {
    return commandName;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public static final class Fields implements etomo.comscript.FieldInterface {
    private Fields() {
    }

    public static final Fields ROTATE = new Fields();
    public static final Fields ROTATION_ANGLES_LIST = new Fields();
    public static final Fields TOTAL_ROWS = new Fields();
  }
}