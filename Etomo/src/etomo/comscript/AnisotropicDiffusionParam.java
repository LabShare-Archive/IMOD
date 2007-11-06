package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.TestnadFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoNumber;
import etomo.type.ParsedArray;
import etomo.type.ParsedNumber;
import etomo.type.ProcessName;
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
 * <p> $Log$ </p>
 */
public final class AnisotropicDiffusionParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  private static final String K_VALUE_TAG = "-k";
  private static final String ITERATION_TAG = "-n";
  private static final String COMMAND_CHAR = "$";

  private final ParsedArray kValueList = ParsedArray
      .getInstance(EtomoNumber.Type.FLOAT);
  private final EtomoNumber iteration = new EtomoNumber();
  private final EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.FLOAT);
  /**
   * IterationList may contain array descriptors in the form start-end.
   * Example: "2,4 - 9,10".
   */
  private final ParsedArray iterationList = ParsedArray.getIteratorInstance();
  private final List command = new ArrayList();

  private final BaseManager manager;

  private String subdirName = "";
  private String inputFileName = "";
  private boolean debug = true;
  private CommandMode commandMode= null;

  public AnisotropicDiffusionParam(final BaseManager manager) {
    this.manager = manager;
  }

  public String setKValueList(final String input) {
    kValueList.setRawString(input);
    return kValueList.validate();
  }

  public void setKValue(final String input) {
    kValue.set(input);
  }

  public void setIteration(final Number input) {
    iteration.set(input);
  }

  public String setIterationList(final String input) {
    iterationList.setRawString(input);
    return iterationList.validate();
  }

  public void setSubdirName(final String input) {
    subdirName = input;
  }

  public void setInputFileName(final String input) {
    inputFileName = input;
  }

  public void deleteTestnadFiles() {
    File[] testnadFileList = new File(manager.getPropertyUserDir(), subdirName)
        .listFiles(new TestnadFileFilter());
    for (int i = 0; i < testnadFileList.length; i++) {
      testnadFileList[i].delete();
    }
  }

  /**
   * Creates fullnad.com.
   * @throws LogFile.FileException
   * @throws LogFile.WriteException
   */
  public void createFullnadFile() throws LogFile.FileException,
      LogFile.WriteException {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    LogFile fullnadFile = LogFile.getInstance(new File(subdir,
        getFullnadFileName()));
    fullnadFile.create();
    long writeId = fullnadFile.openWriter();
    fullnadFile.write(COMMAND_CHAR + ProcessName.ANISOTROPIC_DIFFUSION + " "
        + K_VALUE_TAG + " " + kValue + " " + ITERATION_TAG + " " + iteration 
        + " " + "INPUTFILE" + " " + "OUTPUTFILE", writeId);
    fullnadFile.newLine(writeId);
    fullnadFile.closeWriter(writeId);
  }
  
  public static String getFullnadFileName() {
    return ProcessName.ANISOTROPIC_DIFFUSION +DatasetFiles.COMSCRIPT_EXT;
  }

  public void createTestnadFiles() throws LogFile.WriteException,
      LogFile.FileException {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    EtomoNumber index = new EtomoNumber();
    EtomoNumber k = new EtomoNumber(EtomoNumber.Type.FLOAT);
    for (int i = 0; i < kValueList.size(); i++) {
      index.set(i + 1);
      k.set(kValueList.getRawString(i));
      LogFile testnadFile = LogFile.getInstance(new File(subdir,
          TestnadFileFilter.FILE_NAME_BODY + index.toStringWithLeadingZeros(3)
              + TestnadFileFilter.FILE_NAME_EXT));
      testnadFile.create();
      long writeId = testnadFile.openWriter();
      testnadFile.write(COMMAND_CHAR + ProcessName.ANISOTROPIC_DIFFUSION + " "
          + K_VALUE_TAG + " " + kValueList.getRawString(i) + " "
          + ITERATION_TAG + " " + iteration.toString() + " " + inputFileName
          + " " + getTestnadFileName(k, iteration), writeId);
      testnadFile.newLine(writeId);
      testnadFile.write(COMMAND_CHAR + "echo CHUNK DONE", writeId);
      testnadFile.newLine(writeId);
      testnadFile.closeWriter(writeId);
    }
  }

  private void buildCommand() {
    File subdir = new File(subdirName);
    command.add(BaseManager.getIMODBinPath()
        + ProcessName.ANISOTROPIC_DIFFUSION.toString());
    command.add(K_VALUE_TAG);
    command.add(kValue.toString());
    command.add("-i");
    command.add(iterationList.getRawString());
    command.add(new File(subdir, inputFileName).getPath());
    command.add(new File(subdir, getTestnadFileRoot(kValue)).getPath());
    if (debug) {
      for (int i=0;i<command.size();i++) {
        System.err.print(command.get(i)+" ");
      }
      System.err.println();
    }
  }

  public static List getTestnadFileNameList(BaseManager manager,
      ParsedArray kValueList, ConstEtomoNumber iteration) {
    EtomoNumber index = new EtomoNumber();
    EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.FLOAT);
    List list = new ArrayList();
    for (int i = 0; i < kValueList.size(); i++) {
      kValue.set(kValueList.getRawString(i));
      list.add(getTestnadFileName(kValue, iteration));
    }
    return list;
  }

  public static List getTestnadFileNameList(BaseManager manager,
      ConstEtomoNumber kValue, ParsedArray iterationList) {
    EtomoNumber index = new EtomoNumber();
    EtomoNumber iteration = new EtomoNumber();
    List list = new ArrayList();
    List expandedArray = iterationList.getParsedNumberExpandedArray(null);
    for (int i = 0; i < expandedArray.size(); i++) {
      iteration.set(((ParsedNumber) expandedArray.get(i)).getRawString());
      list.add(getTestnadFileName(kValue, iteration));
    }
    return list;
  }

  private static String getTestnadFileName(ConstEtomoNumber k,
      ConstEtomoNumber iteration) {
    return getTestnadFileRoot(k) + "-" + iteration.toStringWithLeadingZeros(3);
  }

  private static String getTestnadFileRoot(ConstEtomoNumber k) {
    return "test.K" + k.toStringWithLeadingZeros(1);
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }
  
  public void setCommandMode(CommandMode input) {
    commandMode = input;
  }

  public CommandMode getCommandMode() {
    return commandMode;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public String getCommandName() {
    return ProcessName.ANISOTROPIC_DIFFUSION.toString();
  }

  public String getCommandLine() {
    if (command.isEmpty()) {
      buildCommand();
    }
    StringBuffer commandLine = new StringBuffer();
    for (int i = 0; i < command.size(); i++) {
      commandLine.append((String) command.get(i) + " ");
    }
    return commandLine.toString();
  }

  public String[] getCommandArray() {
    if (command.isEmpty()) {
      buildCommand();
    }
    if (command.size() == 1) {
      return new String[] { (String) command.get(0) };
    }
    return (String[]) command.toArray(new String[command.size()]);
  }

  public String getCommand() {
    return ProcessName.ANISOTROPIC_DIFFUSION.toString();
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public boolean getBooleanValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Fields field) {
    if (field == Fields.K_VALUE_LIST) {
      return kValueList.getRawString();
    }
    if (field == Fields.ITERATION_LIST) {
      return iterationList.getRawString();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(etomo.comscript.Fields field) {
    if (field == Fields.ITERATION) {
      return iteration.getInt();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(etomo.comscript.Fields field) {
    if (field == Fields.K_VALUE) {
      return kValue.getFloat();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Fields implements etomo.comscript.Fields {
    private Fields() {
    }

    public static final Fields K_VALUE_LIST = new Fields();
    public static final Fields ITERATION = new Fields();
    public static final Fields K_VALUE = new Fields();
    public static final Fields ITERATION_LIST = new Fields();
  }

  public final static class Mode implements CommandMode {
    public static final Mode VARYING_K = new Mode();
  }
}
