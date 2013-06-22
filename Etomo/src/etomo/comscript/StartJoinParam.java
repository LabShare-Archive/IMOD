package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.List;

import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class StartJoinParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.STARTJOIN;
  private static final boolean debug = true;

  private final AxisID axisID;

  private Hashtable rotationAnglesList = null;
  private boolean rotate = false;
  private int totalRows = 0;

  public StartJoinParam(AxisID axisID) {
    this.axisID = axisID;
  }

  public void setRotate(boolean rotate) {
    this.rotate = rotate;
  }

  public void setRotationAnglesList(Hashtable rotationAnglesList) {
    this.rotationAnglesList = rotationAnglesList;
  }

  public void setTotalRows(int totalRows) {
    this.totalRows = totalRows;
  }

  public String getCommandLine() {
    return getCommand();
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public String getCommand() {
    String command = PROCESS_NAME.getComscript(axisID);
    if (debug) {
      System.err.println(command);
    }
    return command;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.ROTATION_ANGLES_LIST) {
      return rotationAnglesList;
    }
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

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
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

  public String[] getCommandArray() {
    return null;
  }

  public String getCommandName() {
    return null;
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public String getName() {
    return PROCESS_NAME.toString();
  }

  public FileType getOutputImageFileType() {
    return FileType.JOIN_SAMPLE;
  }

  public FileType getOutputImageFileType2() {
    return FileType.JOIN_SAMPLE_AVERAGES;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
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

  public static final class Fields implements etomo.comscript.FieldInterface {
    private Fields() {
    }

    public static final Fields ROTATION_ANGLES_LIST = new Fields();
    public static final Fields TOTAL_ROWS = new Fields();
    public static final Fields ROTATE = new Fields();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.16  2011/02/22 03:30:42  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.15  2010/04/28 16:06:49  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.14  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.13  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.12  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.11  2009/12/08 02:39:32  sueh
 * <p> bug# 1286 Changed command to COMMAND.
 * <p>
 * <p> Revision 1.10  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.9  2009/09/01 03:17:47  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.8  2007/12/13 01:06:18  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.7  2007/11/06 19:17:19  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.6  2007/05/11 15:33:07  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.5  2007/02/05 22:46:06  sueh
 * <p> bug# 962 Added getEtomoNumber, getIntKeyList, and getString.
 * <p>
 * <p> Revision 1.4  2006/06/05 16:16:50  sueh
 * <p> bug# 766 In ProcessName:  Changed getCommand() and getCommandArray() to
 * <p> getComscript... because the fuctions are specialized for comscripts.
 * <p>
 * <p> Revision 1.3  2006/05/22 22:40:37  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.2  2006/05/11 19:48:22  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.1  2006/04/06 19:38:23  sueh
 * <p> bug# 808 Added this param so that it could hold state data that is passed
 * <p> to it when makejoincom is successful.  The data is passed to JoinState
 * <p> when startjoin is successful.
 * <p> </p>
 */
