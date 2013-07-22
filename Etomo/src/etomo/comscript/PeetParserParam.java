package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.FileType;
import etomo.type.IteratorElementList;

import etomo.type.ProcessName;
import etomo.ui.swing.UIHarness;
import etomo.util.EnvironmentVariable;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.19  2011/04/09 06:26:11  sueh
 * <p> bug# 1416 In getCommandArray removed dead code.
 * <p>
 * <p> Revision 1.18  2011/02/22 03:22:24  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.17  2010/11/13 16:03:15  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.16  2010/04/28 16:05:12  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.15  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.14  2010/01/13 21:52:33  sueh
 * <p> bug# 1298 Removed LST_THRESHOLD_ARRAY.
 * <p>
 * <p> Revision 1.13  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.12  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.11  2009/12/08 02:37:49  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.10  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.9  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.8  2009/03/31 22:34:09  sueh
 * <p> bug# 1204 Porting from 3-13.
 * <p>
 * <p> Revision 1.7  2009/03/31 21:06:28  sueh
 * <p> bug# 1204 In getCommandArray, changed error message to give the typical location
 * <p> of the PEET software.
 * <p>
 * <p> Revision 1.6  2009/03/23 16:47:16  sueh
 * <p> bug# 1204 Added a usefull error message for PARTICLE_DIR not being set to
 * <p> getCommandArray.
 * <p>
 * <p> Revision 1.5  2009/03/17 00:32:30  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.4  2007/12/13 01:05:49  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.3  2007/11/06 19:14:58  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.2  2007/05/11 15:29:29  sueh
 * <p> bug# 964 Implemented CommandDetails.
 * <p>
 * <p> Revision 1.1  2007/04/27 23:38:09  sueh
 * <p> bug# 964 Changed prmParser() to peetParser.
 * <p>
 * <p> Revision 1.1  2007/04/26 02:43:17  sueh
 * <p> bug# 964 Represents the prmParser command.
 * <p> </p>
 */
public final class PeetParserParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.PEET_PARSER;
  private final File prmFile;
  private final BaseManager manager;

  private boolean debug = true;
  private int iterationListSize;
  private String[] lstThresholdsArray;
  private String szVol = null;
  private String lstFlagAllTom = null;

  public PeetParserParam(BaseManager manager, File prmFile) {
    this.prmFile = prmFile;
    this.manager = manager;
  }

  public String[] getCommandArray() {
    String[] command = new String[3];
    command[0] = "sh";
    String particleDir = EnvironmentVariable.INSTANCE.getValue(manager,
        manager.getPropertyUserDir(), EnvironmentVariable.PARTICLE_DIR, AxisID.ONLY);
    if (particleDir == null || particleDir.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "The environment variables PARTICLE_DIR has not been set.  Set it "
              + "to the location of the directory containing the PEET "
              + "software.  Make sure the PEET package is installed "
              + "(typically installed in /usr/local/Particle).  To download "
              + "PEET, go to ftp://bio3d.colorado.edu/PEET.", "Environment Error");
      return null;
    }
    File commandFile = new File(new File(EnvironmentVariable.INSTANCE.getValue(manager,
        manager.getPropertyUserDir(), EnvironmentVariable.PARTICLE_DIR, AxisID.ONLY),
        "bin"), PROCESS_NAME.toString());
    command[1] = commandFile.getAbsolutePath();
    command[2] = prmFile.getName();
    if (debug) {
      for (int i = 0; i < command.length; i++) {
        if (i > 0) {
          System.err.print(" ");
        }
        System.err.print(command[i]);
      }
      System.err.println();
    }
    return command;
  }

  public File getLogFile() {
    return new File(prmFile.getAbsolutePath() + ".log");
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    List message = new ArrayList();
    message.add(MatlabParam.SZ_VOL_KEY + " = " + szVol);
    message.add(MatlabParam.LST_FLAG_ALL_TOM_KEY + " = " + lstFlagAllTom);
    StringBuffer buffer = new StringBuffer();
    buffer.append(MatlabParam.LST_THRESHOLDS_KEY + " = ");
    if (lstThresholdsArray != null) {
      for (int i = 0; i < lstThresholdsArray.length; i++) {
        buffer.append(lstThresholdsArray[i]);
        if (i < lstThresholdsArray.length - 1) {
          buffer.append(", ");
        }
      }
    }
    message.add(buffer.toString());
    return message;
  }

  public void setParameters(MatlabParam matlabParam) {
    iterationListSize = matlabParam.getIterationListSize();
    lstThresholdsArray = matlabParam.getLstThresholdsExpandedArray();
    szVol = matlabParam.getSzVol();
    lstFlagAllTom = matlabParam.getLstFlagAllTom();
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

  public String getCommandName() {
    return PROCESS_NAME.toString();
  }

  public String getName() {
    return PROCESS_NAME.toString();
  }

  public FileType getOutputImageFileType() {
    return FileType.AVERAGED_VOLUMES;
  }

  public FileType getOutputImageFileType2() {
    return FileType.REFERENCE_VOLUMES;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
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

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String getCommand() {
    String command = PROCESS_NAME.toString();
    if (debug) {
      System.err.println(command);
    }
    return command;
  }

  public int getIntValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.ITERATION_LIST_SIZE) {
      return iterationListSize;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public boolean getBooleanValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public static final class Fields implements etomo.comscript.FieldInterface {
    private Fields() {
    }

    public static final Fields ITERATION_LIST_SIZE = new Fields();
  }
}
