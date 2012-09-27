package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.ConstJoinState;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IntKeyList;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;

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
 * <p> Revision 1.18  2011/02/22 03:41:43  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.17  2010/11/13 16:03:15  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.16  2010/04/28 16:13:35  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.15  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.14  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.13  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.12  2009/12/08 02:40:03  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.11  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.10  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.9  2009/04/27 17:58:17  sueh
 * <p> In getJoinOptions and getReconOptions directed error messages to stderr
 * <p> instead of stdout.
 * <p>
 * <p> Revision 1.8  2009/03/17 00:33:36  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.7  2008/11/20 01:31:05  sueh
 * <p> bug# 1147 Using xfmodel in both Join and Reconstruction.  Changed
 * <p> genOptions to genJoinOptions.  Added genReconOptions.  Added a
 * <p> second constructor for BaseManager.
 * <p>
 * <p> Revision 1.6  2007/12/13 01:07:20  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.5  2007/11/06 19:18:13  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.4  2007/05/11 15:33:26  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.3  2007/03/01 01:13:30  sueh
 * <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
 * <p> in IntKeyList.
 * <p>
 * <p> Revision 1.2  2007/02/08 02:23:56  sueh
 * <p> bug# 962 Preventing xfmodel input file and output file from being the same.
 * <p>
 * <p> Revision 1.1  2007/02/05 22:50:01  sueh
 * <p> bug# 962 Xfmodel parameter object.
 * <p> </p>
 */
public final class XfmodelParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.XFMODEL;
  public static final String COMMAND_NAME = PROCESS_NAME.toString();

  private static final boolean debug = true;
  private static final int COMMAND_SIZE = 1;

  private final BaseManager manager;
  private final AxisID axisID;

  private String[] commandArray = null;
  private String inputFile = null;
  private String outputFile = null;
  private boolean join = true;

  /**
   * Does not create the command.  getCommandArray creates the command.  Need
   * lazy command creation because settings are coming from the screen.
   * @param manager
   */
  public XfmodelParam(JoinManager manager) {
    this.manager = manager;
    this.axisID = AxisID.ONLY;
  }

  public XfmodelParam(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    join = false;
  }

  private ArrayList genReconOptions() {
    if (join) {
      System.err.println("ERROR:  calling genReconOptions when join is true.");
      return null;
    }
    ArrayList options = new ArrayList();
    options.add("-XformsToApply");
    options.add(DatasetFiles.getTransformFileName(manager, axisID));
    options.add(DatasetFiles.getFiducialModelName(manager, axisID));
    options.add(FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(manager, axisID));
    return options;
  }

  private ArrayList genJoinOptions() {
    if (!join) {
      System.err.println("ERROR:  calling genJoinOptions when join is false.");
      return null;
    }
    ArrayList options = new ArrayList();
    options.add("-XformsToApply");
    options.add(DatasetFiles.getRefineXgFileName(manager));
    ConstJoinState state = ((JoinManager) manager).getState();
    boolean trial = state.getRefineTrial().is();
    ConstEtomoNumber binning = state.getJoinTrialBinning();
    if (trial && !binning.isNull() && binning.gt(1)) {
      options.add("-ScaleShifts");
      EtomoNumber scaleShifts = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      scaleShifts.set(1.0 / binning.getInt());
      options.add(scaleShifts.toString());
    }
    options.add("-ChunkSizes");
    IntKeyList.Walker startListWalker = state.getJoinStartListWalker(trial);
    IntKeyList.Walker endListWalker = state.getJoinEndListWalker(trial);
    //check for valid lists
    if (startListWalker.size() == endListWalker.size()) {
      StringBuffer buffer = new StringBuffer();
      while (startListWalker.hasNext()) {
        int start = startListWalker.nextEtomoNumber().getInt();
        int end = endListWalker.nextEtomoNumber().getInt();
        if (end >= start) {
          buffer.append(String.valueOf(end - start + 1));
        }
        else {
          buffer.append(String.valueOf(start - end + 1));
        }
        if (startListWalker.hasNext()) {
          buffer.append(',');
        }
      }
      options.add(buffer.toString());
    }
    if (inputFile == null) {
      options.add(DatasetFiles.getRefineModelFileName(manager));
    }
    else {
      options.add(inputFile);
    }
    if (outputFile == null) {
      options.add(DatasetFiles.getRefineAlignedModelFileName(manager));
    }
    else {
      options.add(outputFile);
    }
    return options;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public boolean isValid() {
    if (join) {
      File inFile = null;
      if (inputFile == null) {
        inFile = DatasetFiles.getRefineModelFile((JoinManager) manager);
      }
      else {
        inFile = new File(inputFile);
        if (!inFile.isAbsolute()) {
          inFile = new File(manager.getPropertyUserDir(), inputFile);
        }
      }
      File outFile = null;
      if (outputFile == null) {
        outFile = DatasetFiles.getRefineAlignedModelFile(manager);
      }
      else {
        outFile = new File(outputFile);
        if (!outFile.isAbsolute()) {
          outFile = new File(manager.getPropertyUserDir(), outputFile);
        }
      }
      if (inFile.equals(outFile)) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Cannot overwrite xfmodel input file, " + inFile + " with output file, "
                + outFile + ".", "XfmodelParam Error");
        return false;
      }
    }
    return true;
  }

  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String getCommand() {
    return COMMAND_NAME;
  }

  /**
   * creates the command, if it doesn't exist, and returns command array
   */
  public String[] getCommandArray() {
    if (commandArray == null) {
      ArrayList options;
      if (join) {
        options = genJoinOptions();
      }
      else {
        options = genReconOptions();
      }
      if (options == null) {
        return new String[] {};
      }
      commandArray = new String[options.size() + COMMAND_SIZE];
      commandArray[0] = BaseManager.getIMODBinPath() + COMMAND_NAME;
      for (int i = 0; i < options.size(); i++) {
        commandArray[i + COMMAND_SIZE] = (String) options.get(i);
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
    return commandArray;
  }

  public String getCommandLine() {
    getCommandArray();
    if (commandArray.length == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(commandArray[0]);
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(' ' + commandArray[i]);
    }
    return buffer.toString();
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public String getName() {
    return PROCESS_NAME.toString();
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public File getCommandOutputFile() {
    return DatasetFiles.getRefineAlignedModelFile(manager);
  }

  public File getCommandInputFile() {
    return null;
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {

    throw new IllegalArgumentException("field=" + fieldInterface);
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

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.OUTPUT_FILE) {
      return outputFile;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public static final class Fields implements etomo.comscript.FieldInterface {
    public static final Fields OUTPUT_FILE = new Fields();

    private Fields() {
    }
  }
}
