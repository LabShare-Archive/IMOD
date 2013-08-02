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
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.ui.swing.UIHarness;
import etomo.util.EnvironmentVariable;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 * <p> Revision 1.7  2011/02/21 21:09:43  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.6  2010/11/13 16:03:15  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.5  2010/04/28 15:43:23  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.4  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.3  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.2  2009/12/11 17:25:27  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.1  2009/12/08 02:32:23  sueh
 * <p> bug# 1286 Param for averageAll process.
 * <p> </p>
 */
public final class AverageAllParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.AVERAGE_ALL;

  private final BaseManager manager;
  private final File prmFile;

  private int iterationListSize;
  private String[] lstThresholdsArray;
  private String szVol = null;
  private String lstFlagAllTom = null;

  private EtomoNumber iterationNumber = new EtomoNumber();

  public AverageAllParam(BaseManager manager, File prmFile) {
    this.manager = manager;
    this.prmFile = prmFile;
  }

  public static File getLogFile() {
    return new File(PROCESS_NAME.toString() + ".log");
  }

  public void setIterationNumber(int input) {
    iterationNumber.set(input);
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public String getCommandName() {
    return null;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public String[] getCommandArray() {
    List commandArray = new ArrayList();
    commandArray.add("sh");
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
    commandArray.add(new File(new File(EnvironmentVariable.INSTANCE.getValue(manager,
        manager.getPropertyUserDir(), EnvironmentVariable.PARTICLE_DIR, AxisID.ONLY),
        "bin"), PROCESS_NAME.toString()).getAbsolutePath());
    commandArray.add(prmFile.getName());
    if (!iterationNumber.isNull()) {
      commandArray.add(iterationNumber.toString());
    }
    commandArray.add("average");
    return (String[]) commandArray.toArray(new String[commandArray.size()]);
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

  public FileType getOutputImageFileType() {
    return FileType.AVERAGED_VOLUMES;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public void setParameters(MatlabParam matlabParam) {
    iterationListSize = matlabParam.getIterationListSize();
    lstThresholdsArray = matlabParam.getLstThresholdsExpandedArray();
    szVol = matlabParam.getSzVol();
    lstFlagAllTom = matlabParam.getLstFlagAllTom();
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String getCommand() {
    return PROCESS_NAME.toString();
  }

  public String getCommandLine() {
    return getCommand();
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getName() {
    return PROCESS_NAME.toString();
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
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
    if (fieldInterface == Fields.LST_THRESHOLDS_ARRAY) {
      return lstThresholdsArray;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public static final class Fields implements etomo.comscript.FieldInterface {
    private Fields() {
    }

    public static final Fields ITERATION_LIST_SIZE = new Fields();
    public static final Fields LST_THRESHOLDS_ARRAY = new Fields();
  }
}
