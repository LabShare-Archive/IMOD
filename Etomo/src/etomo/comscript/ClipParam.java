package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.type.ViewType;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
 * <p>Description: Runs clip command.  Currently always uses the rotx option.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.10  2011/02/21 21:12:08  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.9  2010/04/28 15:45:10  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.8  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.7  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.6  2009/12/11 17:31:25  sueh
 * <p> bug# 1291 Corrected last checkin comment.
 * <p>
 * <p> Revision 1.5  2009/12/11 17:25:59  sueh
 * <p> bug# 1291 Made current functionality a mode called ROTX.  Added a new
 * <p> mode called STATS.
 * <p>
 * <p> Revision 1.4  2009/12/08 02:34:05  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.3  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.2  2009/09/01 03:17:47  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/04/01 20:04:46  sueh
 * <p> bug# 1208 Param class for running clip.
 * <p> </p>
 */
public final class ClipParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  public static final ProcessName PROCESS_NAME = ProcessName.CLIP;
  private static final int commandSize = 1;
  private File outputFile;
  private String[] commandArray;
  private boolean debug = true;

  private final ApplicationManager applicationManager;
  private final BaseManager manager;
  private final AxisID axisID;
  private final Mode mode;
  private final File inputFile;

  private ClipParam(final BaseManager manager,
      final ApplicationManager applicationManager, final AxisID axisID,
      final File inputFile, File workingDir, final Mode mode) {
    this.manager = manager;
    this.applicationManager = applicationManager;
    this.axisID = axisID;
    this.mode = mode;
    this.inputFile = inputFile;
    ArrayList options = genOptions(inputFile, workingDir);
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = BaseManager.getIMODBinPath() + PROCESS_NAME.toString();
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
    if (debug) {
      for (int i = 0; i < commandArray.length; i++) {
        System.err.print(commandArray[i] + " ");
      }
      System.err.println();
    }
  }

  public static ClipParam getRotxInstance(final BaseManager manager, final AxisID axisID,
      final File inputFile, final File workingDir) {
    return new ClipParam(manager, null, axisID, inputFile, workingDir, Mode.ROTX);
  }

  public static ClipParam getStatsInstance(final ApplicationManager manager,
      final AxisID axisID, final File inputFile, final File workingDir) {
    return new ClipParam(manager, manager, axisID, inputFile, workingDir, Mode.STATS);
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  private ArrayList genOptions(final File inputFile, final File workingDir) {
    ArrayList options = new ArrayList(3);
    // Add process.
    options.add(mode.toString());
    // Add options.
    if (mode == Mode.STATS) {
      if (applicationManager == null) {
        System.err
            .println("Warning: Unable to get the view type.  Coordinates may be incorrect "
                + "if this is a montage.");
      }
      else if (applicationManager.getConstMetaData().getViewType() == ViewType.MONTAGE) {
        options.add("-PID");
        options.add(manager.getBaseMetaData().getDatasetName() + axisID.getExtension()
            + ".pl");
        options.add("-O");
        options.add(Utilities.MONTAGE_SEPARATION + "," + Utilities.MONTAGE_SEPARATION);
      }
      // Put a * on the outliers.
      options.add("-n");
      options.add("2.5");
      // The length should be 1/4 of Z, but between 15 and 30.
      options.add("-l");
      int length;
      int min = 15;
      int max = 30;
      MRCHeader header = MRCHeader.getInstanceFromFileName(manager, axisID,
          inputFile.getName());
      try {
        header.read(manager);
        length = header.getNSections();
      }
      catch (InvalidParameterException e) {
        // Pick a midrange number if the header can't be read.
        length = max * 2;
      }
      catch (IOException e) {
        // Pick a midrange number if the header can't be read.
        length = max * 2;
      }
      length /= 4;
      if (length < min) {
        length = min;
      }
      if (length > max) {
        length = max;
      }
      options.add(String.valueOf(length));
      // Display the views starting from 1 instead of 0.
      options.add("-1");
    }
    // Add input files.
    options.add(inputFile.getAbsolutePath());
    // Add output files.
    if (mode == Mode.ROTX) {
      int index = inputFile.getName().lastIndexOf('.');
      StringBuffer clipFileName = new StringBuffer();
      if (index == -1) {
        clipFileName.append(inputFile.getName());
      }
      else {
        clipFileName.append(inputFile.getName().substring(0, index));
      }
      // Still using .flip for the output name for clip rotx, since we used to flip
      // instead of rotate.
      outputFile = new File(workingDir, clipFileName + ".flip");
      options.add(outputFile.getAbsolutePath());
    }
    return options;
  }

  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public String getCommandName() {
    return PROCESS_NAME.toString();
  }

  public String getCommand() {
    return PROCESS_NAME.toString();
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

  public String[] getCommandArray() {
    return commandArray;
  }

  public File getCommandOutputFile() {
    return outputFile;
  }

  public File getCommandInputFile() {
    return inputFile;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public int getIntValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public CommandMode getCommandMode() {
    return mode;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public static final class Mode implements CommandMode {
    private static final Mode ROTX = new Mode("rotx");
    public static final Mode STATS = new Mode("stats");

    private final String process;

    private Mode(String process) {
      this.process = process;
    }

    public String toString() {
      return process;
    }
  }
}
