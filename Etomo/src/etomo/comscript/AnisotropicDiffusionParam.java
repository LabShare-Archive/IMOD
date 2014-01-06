package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.TestNADFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.DebugLevel;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ParsedArray;
import etomo.type.ProcessName;
import etomo.ui.swing.AnisotropicDiffusionDialog;
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
 * <p> Revision 1.20  2011/02/21 21:08:40  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.19  2010/11/13 16:03:15  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.18  2010/04/28 15:42:47  sueh
 * <p> bug# 1344 Added the mode to the constructor.  Completed the list of
 * <p> modes.  Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.17  2010/02/17 04:47:45  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.16  2010/01/11 23:48:24  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.15  2009/12/11 17:24:39  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.14  2009/12/08 02:31:14  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.13  2009/09/05 00:34:48  sueh
 * <p> bug# 1256 Removed ParsedIteratorDescriptor.  Using the IteratorParser
 * <p> and the IteratorElementList instead.
 * <p>
 * <p> Revision 1.12  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.11  2009/03/17 00:30:26  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.10  2009/03/06 23:36:43  sueh
 * <p> bug# 1196 Added getInputFileName and getSubdirName.
 * <p>
 * <p> Revision 1.9  2009/02/04 23:15:03  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.8  2008/09/10 20:50:32  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).
 * <p>
 * <p> Revision 1.7  2008/06/20 18:39:27  sueh
 * <p> bug# 1119 Changing debug to debugLevel.
 * <p>
 * <p> Revision 1.6  2008/04/08 23:52:48  sueh
 * <p> bug# 1105 Changed the array used in ParsedElement to a
 * <p> ParsedElementList because it always holds ParsedNumbers.
 * <p>
 * <p> Revision 1.5  2008/04/02 01:49:59  sueh
 * <p> bug# 1097 Made non-matlab syntax the default in the ParsedElements
 * <p> classes.  This is because matlab uses "NaN", which is unhealthy for
 * <p> Etomo and IMOD.
 * <p>
 * <p> Revision 1.4  2007/12/13 01:02:30  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
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
  private static final ProcessName PROCESS_NAME = ProcessName.ANISOTROPIC_DIFFUSION;

  private final ParsedArray kValueList = ParsedArray.getInstance(EtomoNumber.Type.DOUBLE,
      "K value");
  private final EtomoNumber iteration = new EtomoNumber();
  private final EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final List command = new ArrayList();

  private final BaseManager manager;
  /**
   * IterationList may contain array descriptors in the form start-end.
   * Example: "2,4 - 9,10".
   */
  private final IteratorElementList iterationList;

  private String subdirName = "";
  private String inputFileName = "";
  private DebugLevel debugLevel = DebugLevel.LOW;
  private final CommandMode mode;

  public AnisotropicDiffusionParam(final BaseManager manager, CommandMode mode) {
    this.manager = manager;
    this.mode = mode;
    iterationList = new IteratorElementList(manager, AxisID.ONLY,
        AnisotropicDiffusionDialog.ITERATION_LIST_LABEL);
  }

  /**
   * @param input
   * @return error message if invalid
   */
  public String setKValueList(final String input) {
    kValueList.setRawString(input);
    if (debugLevel == DebugLevel.HIGH) {
      System.out.println("AnisotropicDiffusionParam.setKValueList:kValueList="
          + kValueList);
    }
    return kValueList.validate();
  }

  public void setKValue(final String input) {
    kValue.set(input);
  }

  public void setIteration(final Number input) {
    iteration.set(input);
  }

  public boolean isMessageReporter() {
    return false;
  }

  public boolean setIterationList(final String input) {
    iterationList.setList(input);
    return iterationList.isValid();
  }

  public void setDebugLevel(DebugLevel input) {
    debugLevel = input;
  }

  public void setSubdirName(final String input) {
    subdirName = input;
  }

  public String getSubdirName() {
    return subdirName;
  }

  public void setInputFileName(final String input) {
    inputFileName = input;
  }

  public String getInputFileName() {
    return inputFileName;
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
  public void createFilterFullFile() throws LogFile.LockException, IOException {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    LogFile filterFullFile = LogFile
        .getInstance(new File(subdir, getFilterFullFileName()));
    filterFullFile.create();
    LogFile.WriterId writerId = filterFullFile.openWriter();
    filterFullFile.write(COMMAND_CHAR + PROCESS_NAME + " " + K_VALUE_TAG + " " + kValue
        + " " + ITERATION_TAG + " " + iteration + " " + "INPUTFILE" + " " + "OUTPUTFILE",
        writerId);
    filterFullFile.newLine(writerId);
    filterFullFile.closeWriter(writerId);
  }

  public static String getFilterFullFileName() {
    return PROCESS_NAME + DatasetFiles.COMSCRIPT_EXT;
  }

  public void createTestFiles() throws LogFile.LockException, IOException {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    EtomoNumber index = new EtomoNumber();
    EtomoNumber k = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    for (int i = 0; i < kValueList.size(); i++) {
      index.set(i + 1);
      k.set(kValueList.getRawString(i));
      LogFile testFile = LogFile.getInstance(new File(subdir,
          TestNADFileFilter.FILE_NAME_BODY + addLeadingZeros(index.toString(), 3)
              + TestNADFileFilter.FILE_NAME_EXT));
      testFile.create();
      LogFile.WriterId writerId = testFile.openWriter();
      testFile.write(
          COMMAND_CHAR + PROCESS_NAME + " " + K_VALUE_TAG + " "
              + kValueList.getRawString(i) + " " + ITERATION_TAG + " "
              + iteration.toString() + " " + inputFileName + " "
              + getTestFileName(k, iteration.toString()), writerId);
      testFile.newLine(writerId);
      testFile.write(COMMAND_CHAR + "echo CHUNK DONE", writerId);
      testFile.newLine(writerId);
      testFile.closeWriter(writerId);
    }
  }

  private void buildCommand() {
    File subdir = new File(subdirName);
    command.add(BaseManager.getIMODBinPath() + PROCESS_NAME.toString());
    command.add(K_VALUE_TAG);
    command.add(kValue.toString());
    command.add("-i");
    command.add(iterationList.toString());
    command.add("-PID");
    command.add(new File(subdir, inputFileName).getPath());
    command.add(new File(subdir, getTestFileRoot(kValue)).getPath());
    if (debugLevel.ge(DebugLevel.LOW)) {
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
  public static List getTestFileNameList(BaseManager manager, ParsedArray kValueList,
      ConstEtomoNumber iteration, String testVolumeName) {
    EtomoNumber index = new EtomoNumber();
    EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    List list = new ArrayList();
    list.add(testVolumeName);
    for (int i = 0; i < kValueList.size(); i++) {
      kValue.set(kValueList.getRawString(i));
      list.add(getTestFileName(kValue, iteration.toString()));
    }
    return list;
  }

  public static List getTestFileNameList(ConstEtomoNumber kValue,
      IteratorElementList iterationList, String testVolumeName) {
    List list = new ArrayList();
    list.add(testVolumeName);
    Iterator iterator = iterationList.getExpandedList().iterator();
    while (iterator.hasNext()) {
      list.add(getTestFileName(kValue, (String) iterator.next()));
    }
    return list;
  }

  private static String getTestFileName(ConstEtomoNumber k, String iteration) {
    return getTestFileRoot(k) + "-" + addLeadingZeros(iteration, 3);
  }

  private static String getTestFileRoot(ConstEtomoNumber k) {
    return "test.K" + addLeadingZeros(k.toString(), 1);
  }

  /**
   * Add leading zeros to the number up to maxZeros.  If the size of the number
   * (or the size of the number left of the decimal if the type is float or
   * double) is greater or equal to maxZeros then nothing is done.
   * Examples:
   * If the number is 1 and maxZeros is 3, returns 001
   * If the number is .08 and maxZeros is 1, return 0.08
   * @param maxZeros
   * @return
   */
  private static String addLeadingZeros(String number, int maxZeros) {
    String digits = number;
    int decimalPlace = digits.indexOf('.');
    if (decimalPlace != -1) {
      digits = digits.substring(0, decimalPlace);
    }
    int length = digits.length();
    if (length > maxZeros || length == maxZeros) {
      return number;
    }
    StringBuffer retval = new StringBuffer();
    for (int i = 0; i < maxZeros - length; i++) {
      retval.append("0");
    }
    retval.append(number);
    return retval.toString();
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public CommandMode getCommandMode() {
    return mode;
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

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getName() {
    return PROCESS_NAME.toString();
  }

  public FileType getOutputImageFileType() {
    System.out.println("getOutputImageFileType:commandMode=" + mode);
    if (mode == Mode.VARYING_K) {
      return FileType.NAD_TEST_VARYING_K;
    }
    if (mode == Mode.VARYING_ITERATIONS) {
      return FileType.NAD_TEST_VARYING_ITERATIONS;
    }
    if (mode == Mode.FULL) {
      return FileType.ANISOTROPIC_DIFFUSION_OUTPUT;
    }
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
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

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Field.K_VALUE_LIST) {
      return kValueList.getRawString();
    }
    if (fieldInterface == Field.ITERATION_LIST) {
      return iterationList.toString();
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public IteratorElementList getIteratorElementList(
      etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Field.ITERATION_LIST) {
      return iterationList;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Field.K_VALUE) {
      return kValue.getDouble();
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public int getIntValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Field.ITERATION) {
      return iteration.getInt();
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public static final class Field implements etomo.comscript.FieldInterface {
    private Field() {
    }

    public static final Field K_VALUE_LIST = new Field();
    public static final Field ITERATION = new Field();
    public static final Field K_VALUE = new Field();
    public static final Field ITERATION_LIST = new Field();
  }

  public final static class Mode implements CommandMode {
    public static final Mode VARYING_K = new Mode("VaryingK");
    public static final Mode VARYING_ITERATIONS = new Mode("VaryingIterations");
    public static final Mode FULL = new Mode("Full");

    private final String string;

    private Mode(String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
