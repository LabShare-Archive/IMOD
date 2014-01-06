package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.Transform;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.24  2011/05/10 16:49:36  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 1.23  2011/02/22 03:40:45  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.22  2010/04/28 16:12:55  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.21  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.20  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.19  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.18  2007/11/06 19:17:59  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.17  2007/03/07 21:04:08  sueh
 * <p> bug# 981 Changed ScriptParameter.isUseInScript to isNotNullAndNotDefault for
 * <p> clarity.
 * <p>
 * <p> Revision 1.16  2007/02/05 22:49:06  sueh
 * <p> bug# 962 Put comscript mode info into an inner class.
 * <p>
 * <p> Revision 1.15  2006/05/22 22:43:03  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.14  2006/05/11 19:50:53  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.13  2006/04/06 19:38:51  sueh
 * <p> bug# 808 Implementing ProcessDetails.
 * <p>
 * <p> Revision 1.12  2006/01/20 20:48:32  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.11  2005/11/19 01:55:10  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.10  2005/07/29 00:50:29  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.9  2005/04/25 20:41:52  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.8  2005/01/25 21:51:47  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
 * <p> Revision 1.7  2005/01/21 22:54:15  sueh
 * <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
 * <p> isSetAndNotDefault() as a standard why to decide if a parameter should
 * <p> be placed in a comscript.
 * <p>
 * <p> Revision 1.6  2005/01/08 01:46:38  sueh
 * <p> bug# 578 Updated Command interface.
 * <p>
 * <p> Revision 1.5  2004/12/08 21:22:43  sueh
 * <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
 * <p>
 * <p> Revision 1.4  2004/12/02 18:28:00  sueh
 * <p> bug 520 Remove unnecessary import.
 * <p>
 * <p> Revision 1.3  2004/12/01 03:46:22  sueh
 * <p> bug# 520 Removed unnecessary member variable SystemProgram
 * <p> program.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:15:33  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.10  2004/11/16 02:21:42  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.9  2004/11/12 22:49:00  sueh
 * <p> bug# 520 Added empty getIntegerValue and getBinning.
 * <p>
 * <p> Revision 1.1.2.8  2004/11/08 22:12:41  sueh
 * <p> bug# 520 Add getMode to conform to Command.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/30 01:32:09  sueh
 * <p> bug# 520 Added comments.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/29 01:17:46  sueh
 * <p> bug# 520 Removed working directory from meta data.  Getting working
 * <p> directory from propertyUserDir.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/28 16:57:04  sueh
 * <p> bug# 520 Specifying output file: -o rootname_auto.xf.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/22 20:59:25  sueh
 * <p> bug# 520 Using EtomoSimpleType where possible.  Changed offsetInX, Y
 * <p> to shiftInX, Y.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/22 03:21:16  sueh
 * <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
 * <p> passing EtomoInteger, EtomoFloat, etc and using their get() and
 * <p> getString() functions.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/21 02:37:22  sueh
 * <p> bug# 520 Adding modes (initial and refine) that can change how the
 * <p> options are set.  Removed unnecessary function run().  Implementing
 * <p> Command interface.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/18 17:45:04  sueh
 * <p> bug# 520 Added a param to create the xfalign command.
 * <p> </p>
 */
public class XfalignParam implements Command {
  public static final String rcsid = "$Id$";

  public static final String PRE_CROSS_CORRELATION_KEY = "PreCrossCorrelation";
  public static final String EDGE_TO_IGNORE_KEY = "EdgeToIgnore";
  public static final String REDUCE_BY_BINNING_KEY = "ReduceByBinning";
  public static final String SKIP_SECTIONS_KEY = "SkipSections";
  public static final String SECTIONS_NUMBERED_FROM_ONE_KEY = "SectionsNumberedFromOne";
  private static final int commandSize = 4;
  private static final String commandName = "xfalign";
  private static final String outputFileExtension = "_auto.xf";
  private static final AxisID AXIS_ID = AxisID.ONLY;

  private final EtomoNumber reduceByBinning = new EtomoNumber();
  private final EtomoNumber edgeToIgnore = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final FortranInputString warpPatchSize = new FortranInputString(2);
  private final FortranInputString shiftLimitsForWarp = new FortranInputString(2);

  private final AutoAlignmentMetaData autoAlignmentMetaData;
  private final String rootName;
  private final String outputFileName;
  private final File outputFile;
  private final boolean tomogramAverages;
  private final Mode mode;
  private final BaseManager manager;

  private String inputFileName = null;
  private String[] commandArray = null;
  private boolean preCrossCorrelation = false;
  private String skipSections = null;
  private boolean sectionsNumberedFromOne = false;
  private boolean boundaryModel = false;
  private boolean sobelFilter = false;

  public XfalignParam(final BaseManager manager,
      final AutoAlignmentMetaData autoAlignmentMetaData, final Mode mode,
      final boolean tomogramAverages) {
    this.autoAlignmentMetaData = autoAlignmentMetaData;
    this.mode = mode;
    this.tomogramAverages = tomogramAverages;
    this.rootName = manager.getName();
    this.manager = manager;
    outputFileName = rootName + outputFileExtension;
    outputFile = new File(manager.getPropertyUserDir(), outputFileName);
    warpPatchSize.setIntegerType(true);
    shiftLimitsForWarp.setIntegerType(true);
  }

  public AxisID getAxisID() {
    return AXIS_ID;
  }

  public String[] getCommandArray() {
    if (commandArray == null) {
      ArrayList options = genOptions();
      commandArray = new String[options.size() + commandSize];
      commandArray[0] = "python";
      commandArray[1] = "-u";
      commandArray[2] = BaseManager.getIMODBinPath() + commandName;
      commandArray[3] = "-PID";
      for (int i = 0; i < options.size(); i++) {
        commandArray[i + commandSize] = (String) options.get(i);
      }
    }
    return commandArray;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public String getCommandLine() {
    getCommandArray();
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public String getCommandName() {
    return commandName;
  }

  public ProcessName getProcessName() {
    return ProcessName.XFALIGN;
  }

  public String getCommand() {
    return commandName;
  }

  public static String getName() {
    return commandName;
  }

  public static String getOutputFileExtension() {
    return outputFileExtension;
  }

  public File getCommandOutputFile() {
    return outputFile;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public CommandMode getCommandMode() {
    return mode;
  }

  public boolean isMessageReporter() {
    return false;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    if (tomogramAverages) {
      options.add("-tomo");
    }
    if (mode == Mode.INITIAL) {
      options.add("-pre");
    }
    else if (mode == Mode.REFINE) {
      options.add("-ini");
      options.add(rootName + ".xf");
    }
    else {
      throw new IllegalArgumentException("Unknown mode " + mode + ".");
    }
    genFilterOptions(options);
    Transform transform = autoAlignmentMetaData.getAlignTransform();
    if (transform == null) {
      transform = Transform.DEFAULT;
    }
    if (sobelFilter) {
      options.add("-sobel");
    }
    options.add("-par");
    options.add(transform.getValue());
    if (!edgeToIgnore.isNull()) {
      options.add("-matt");
      options.add(edgeToIgnore.toString());
    }
    if (!reduceByBinning.isNull()) {
      options.add("-reduce");
      options.add(reduceByBinning.toString());
    }
    if (preCrossCorrelation && mode != Mode.REFINE) {
      options.add("-prexcorr");
    }
    if (skipSections != null) {
      options.add("-skip");
      options.add(skipSections);
    }
    if (sectionsNumberedFromOne) {
      options.add("-one");
    }
    if (!warpPatchSize.isNull()) {
      options.add("-warp");
      options.add(warpPatchSize.toString());
    }
    if (boundaryModel) {
      options.add("-boundary");
      options.add(FileType.AUTO_ALIGN_BOUNDARY_MODEL.getFileName(manager, AXIS_ID));
    }
    if (!shiftLimitsForWarp.isNull()) {
      options.add("-shift");
      options.add(shiftLimitsForWarp.toString());
    }
    options.add(inputFileName);
    options.add(outputFileName);
    return options;
  }

  private void genFilterOptions(ArrayList options) {
    ScriptParameter sigmaLowFrequency = autoAlignmentMetaData
        .getSigmaLowFrequencyParameter();
    ScriptParameter cutoffHighFrequency = autoAlignmentMetaData
        .getCutoffHighFrequencyParameter();
    ScriptParameter sigmaHighFrequency = autoAlignmentMetaData
        .getSigmaHighFrequencyParameter();
    // optional
    if ((autoAlignmentMetaData.isSigmaLowFrequencyEnabled() && sigmaLowFrequency
        .isNotNullAndNotDefault())
        || (autoAlignmentMetaData.isCutoffHighFrequencyEnabled() && cutoffHighFrequency
            .isNotNullAndNotDefault())
        || (autoAlignmentMetaData.isSigmaHighFrequencyEnabled() && sigmaHighFrequency
            .isNotNullAndNotDefault())) {
      options.add("-fil");
      // all three numbers must exist
      options.add(sigmaLowFrequency.toDefaultedString() + ","
          + sigmaHighFrequency.toDefaultedString() + ",0,"
          + cutoffHighFrequency.toDefaultedString());
    }
  }

  public void resetSobelFilter() {
    sobelFilter = false;
  }

  public void setSobelFilter(final boolean input) {
    sobelFilter = input;
  }

  public void setReduceByBinning(final Number input) {
    reduceByBinning.set(input);
  }

  public void resetReduceByBinning() {
    reduceByBinning.reset();
  }

  public void setBoundaryModel(final boolean input) {
    boundaryModel = input;
  }

  public void resetBoundaryModel() {
    boundaryModel = false;
  }

  public void setEdgeToIgnore(final String input) {
    edgeToIgnore.set(input);
  }

  public void resetEdgeToIgnore() {
    edgeToIgnore.reset();
  }

  public void setSkipSectionsFrom1(final String input) {
    if (input == null || input.matches("\\s*")) {
      sectionsNumberedFromOne = false;
      skipSections = null;
    }
    else {
      sectionsNumberedFromOne = true;
      skipSections = input;
    }
  }

  public void resetSkipSectionsFrom1() {
    sectionsNumberedFromOne = false;
    skipSections = null;
  }

  public void setPreCrossCorrelation(final boolean input) {
    preCrossCorrelation = input;
  }

  public void setWarpPatchSize(final String x, final String y) {
    warpPatchSize.set(0, x);
    warpPatchSize.set(1, y);
  }

  public void resetWarpPatchSize() {
    warpPatchSize.reset();
  }

  public void setShiftLimitsForWarp(final String x, final String y) {
    shiftLimitsForWarp.set(0, x);
    shiftLimitsForWarp.set(1, y);
  }

  public void resetShiftLimitsForWarp() {
    shiftLimitsForWarp.reset();
  }

  public void setInputFileName(final String input) {
    inputFileName = input;
  }

  public final static class Mode implements CommandMode {
    public static final Mode INITIAL = new Mode("Initial");
    public static final Mode REFINE = new Mode("Refine");

    private final String string;

    private Mode(String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
