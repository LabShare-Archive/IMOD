package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.ConstJoinState;
import etomo.type.EtomoNumber;
import etomo.type.IntKeyList;
import etomo.type.ProcessName;
import etomo.ui.UIHarness;
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

  public static final String COMMAND_NAME = ProcessName.XFMODEL.toString();

  private static final boolean debug = true;
  private static final int COMMAND_SIZE = 1;
  private final JoinManager manager;
  private String[] commandArray = null;
  private String inputFile = null;
  private String outputFile = null;

  /**
   * Does not create the command.  getCommandArray creates the command.  Need
   * lazy command creation because setting are coming from the screen.
   * @param manager
   */
  public XfmodelParam(JoinManager manager) {
    this.manager = manager;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-XformsToApply");
    options.add(DatasetFiles.getRefineXgFileName(manager));
    ConstJoinState state = manager.getState();
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
        long start = startListWalker.nextEtomoNumber().getLong();
        long end = endListWalker.nextEtomoNumber().getLong();
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

  public boolean isValid() {
    File inFile = null;
    if (inputFile == null) {
      inFile = DatasetFiles.getRefineModelFile(manager);
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
      UIHarness.INSTANCE.openMessageDialog(
          "Cannot overwrite xfmodel input file, " + inFile
              + " with output file, " + outFile + ".", "XfmodelParam Error");
      return false;
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
      ArrayList options = genOptions();
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

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public File getCommandOutputFile() {
    return DatasetFiles.getRefineAlignedModelFile(manager);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Fields field) {

    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  
  public String[] getStringArray(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Fields field) {
    if (field == Fields.OUTPUT_FILE) {
      return outputFile;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Fields implements etomo.comscript.Fields {
    public static final Fields OUTPUT_FILE = new Fields();

    private Fields() {
    }
  }
}
