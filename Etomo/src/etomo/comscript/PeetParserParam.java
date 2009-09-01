package etomo.comscript;

import java.io.File;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.storage.MatlabParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;

import etomo.type.ProcessName;
import etomo.ui.UIHarness;
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

  public PeetParserParam(BaseManager manager, File prmFile) {
    this.prmFile = prmFile;
    this.manager = manager;
  }

  public String[] getCommandArray() {
    String[] command = null;
    if (command != null) {
      return command;
    }
    command = new String[3];
    command[0] = "sh";
    String particleDir = EnvironmentVariable.INSTANCE.getValue(manager
        .getPropertyUserDir(), "PARTICLE_DIR", AxisID.ONLY, manager
        .getManagerKey());
    if (particleDir == null || particleDir.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog(
          "The environment variables PARTICLE_DIR has not been set.  Set it "
              + "to the location of the directory containing the PEET "
              + "software.  Make sure the PEET package is installed "
              + "(typically installed in /usr/local/Particle).  To download "
              + "PEET, go to ftp://bio3d.colorado.edu/PEET.",
          "Environment Error", manager.getManagerKey());
      return null;
    }
    File commandFile = new File(new File(EnvironmentVariable.INSTANCE.getValue(
        manager.getPropertyUserDir(), "PARTICLE_DIR", AxisID.ONLY, manager
            .getManagerKey()), "bin"), PROCESS_NAME.toString());
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

  public void getParameters(MatlabParam matlabParam) {
    iterationListSize = matlabParam.getIterationListSize();
    lstThresholdsArray = matlabParam.getLstThresholdsExpandedArray();
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public String getCommandName() {
    return null;
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
  
  public ProcessName getSubcommandProcessName() {
    return null;
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String getCommand() {
    String command = PROCESS_NAME.getComscript(AxisID.ONLY);
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

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.LST_THRESHOLDS_ARRAY) {
      return lstThresholdsArray;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public boolean getBooleanValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public float getFloatValue(etomo.comscript.FieldInterface fieldInterface) {
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
    public static final Fields LST_THRESHOLDS_ARRAY = new Fields();
  }
}
