package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.TestNADFileFilter;
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
 * <p> $Log$
 * <p> Revision 1.4  2007/12/13 01:02:30  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.Field.
 * <p>
 * <p> Revision 1.3  2007/11/09 17:43:25  sueh
 * <p> bug# 1047 In buildCommand, removed quotes from the iteration string.
 * <p>
 * <p> Revision 1.2  2007/11/07 14:54:25  sueh
 * <p> bug# 1047 In buildCommand put the iteration list in quotes because the
 * <p> command string doesn't work without quotes when run from the command line.
 * <p>
 * <p> Revision 1.1  2007/11/06 18:58:33  sueh
 * <p> bug# 1047 Represents the parameters of nad_eed_3d.
 * <p> </p>
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
  private final ParsedArray iterationList = ParsedArray.getInstance();
  private final List command = new ArrayList();

  private final BaseManager manager;

  private String subdirName = "";
  private String inputFileName = "";
  private boolean debug = true;
  private CommandMode commandMode = null;

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

  public void deleteTestFiles() {
    File[] testFileList = new File(manager.getPropertyUserDir(), subdirName)
        .listFiles(new TestNADFileFilter());
    for (int i = 0; i < testFileList.length; i++) {
      testFileList[i].delete();
    }
  }

  /**
   * Creates fullnad.com.
   * @throws LogFile.FileException
   * @throws LogFile.WriteException
   */
  public void createFilterFullFile() throws LogFile.FileException,
      LogFile.WriteException {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    LogFile filterFullFile = LogFile.getInstance(new File(subdir,
        getFilterFullFileName()));
    filterFullFile.create();
    long writeId = filterFullFile.openWriter();
    filterFullFile.write(COMMAND_CHAR + ProcessName.ANISOTROPIC_DIFFUSION + " "
        + K_VALUE_TAG + " " + kValue + " " + ITERATION_TAG + " " + iteration
        + " " + "INPUTFILE" + " " + "OUTPUTFILE", writeId);
    filterFullFile.newLine(writeId);
    filterFullFile.closeWriter(writeId);
  }

  public static String getFilterFullFileName() {
    return ProcessName.ANISOTROPIC_DIFFUSION + DatasetFiles.COMSCRIPT_EXT;
  }

  public void createTestFiles() throws LogFile.WriteException,
      LogFile.FileException {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    EtomoNumber index = new EtomoNumber();
    EtomoNumber k = new EtomoNumber(EtomoNumber.Type.FLOAT);
    for (int i = 0; i < kValueList.size(); i++) {
      index.set(i + 1);
      k.set(kValueList.getRawString(i));
      LogFile testFile = LogFile.getInstance(new File(subdir,
          TestNADFileFilter.FILE_NAME_BODY + index.toStringWithLeadingZeros(3)
              + TestNADFileFilter.FILE_NAME_EXT));
      testFile.create();
      long writeId = testFile.openWriter();
      testFile.write(COMMAND_CHAR + ProcessName.ANISOTROPIC_DIFFUSION + " "
          + K_VALUE_TAG + " " + kValueList.getRawString(i) + " "
          + ITERATION_TAG + " " + iteration.toString() + " " + inputFileName
          + " " + getTestFileName(k, iteration), writeId);
      testFile.newLine(writeId);
      testFile.write(COMMAND_CHAR + "echo CHUNK DONE", writeId);
      testFile.newLine(writeId);
      testFile.closeWriter(writeId);
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
    command.add("-P");
    command.add(new File(subdir, inputFileName).getPath());
    command.add(new File(subdir, getTestFileRoot(kValue)).getPath());
    if (debug) {
      for (int i = 0; i < command.size(); i++) {
        System.err.print(command.get(i) + " ");
      }
      System.err.println();
    }
  }

  /**
   * The test volume and the output volues
   * @param manager
   * @param kValueList
   * @param iteration
   * @return
   */
  public static List getTestFileNameList(BaseManager manager,
      ParsedArray kValueList, ConstEtomoNumber iteration, String testVolumeName) {
    EtomoNumber index = new EtomoNumber();
    EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.FLOAT);
    List list = new ArrayList();
    list.add(testVolumeName);
    for (int i = 0; i < kValueList.size(); i++) {
      kValue.set(kValueList.getRawString(i));
      list.add(getTestFileName(kValue, iteration));
    }
    return list;
  }

  public static List getTestFileNameList(BaseManager manager,
      ConstEtomoNumber kValue, ParsedArray iterationList, String testVolumeName) {
    EtomoNumber index = new EtomoNumber();
    EtomoNumber iteration = new EtomoNumber();
    List list = new ArrayList();
    list.add(testVolumeName);
    List expandedArray = iterationList.getParsedNumberExpandedArray(null);
    for (int i = 0; i < expandedArray.size(); i++) {
      iteration.set(((ParsedNumber) expandedArray.get(i)).getRawString());
      list.add(getTestFileName(kValue, iteration));
    }
    return list;
  }

  private static String getTestFileName(ConstEtomoNumber k,
      ConstEtomoNumber iteration) {
    return getTestFileRoot(k) + "-" + iteration.toStringWithLeadingZeros(3);
  }

  private static String getTestFileRoot(ConstEtomoNumber k) {
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

  public boolean getBooleanValue(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Field field) {
    if (field == Fields.K_VALUE_LIST) {
      return kValueList.getRawString();
    }
    if (field == Fields.ITERATION_LIST) {
      return iterationList.getRawString();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(etomo.comscript.Field field) {
    if (field == Fields.ITERATION) {
      return iteration.getInt();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(etomo.comscript.Field field) {
    if (field == Fields.K_VALUE) {
      return kValue.getFloat();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Field field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Fields implements etomo.comscript.Field {
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
