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
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstJoinState;
import etomo.type.ConstSectionTableRowData;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IntKeyList;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.SectionTableRowData;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;

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
 * <p> Revision 1.46  2011/06/18 04:20:47  sueh
 * <p> Bug# 1494 finishjoin is now a python script.
 * <p>
 * <p> Revision 1.45  2011/05/10 16:49:36  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 1.44  2011/02/22 03:13:18  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.43  2010/11/13 16:03:15  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.42  2010/04/28 15:54:48  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.41  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.40  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.39  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.38  2009/12/08 02:35:04  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.37  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.36  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.35  2009/03/17 00:31:52  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.34  2009/02/05 23:42:12  sueh
 * <p> bug# 1148 Changed IntKeyList.put to add.
 * <p>
 * <p> Revision 1.33  2008/08/18 22:33:29  sueh
 * <p> bug# 1130 Added "-l" to GenOptions:  from dialog, sets localFits.  Added
 * <p> "-l" to genRejoinOptions:  from state.
 * <p>
 * <p> Revision 1.32  2008/05/22 00:13:33  sueh
 * <p> bug# 1110 In genRejoinOptions handling this bug by telling the user how
 * <p> fix the damage done by this bug (rerunning Finish Join).
 * <p>
 * <p> Revision 1.31  2008/03/21 17:08:34  sueh
 * <p> bug# 1101 Put "tcsh -f" back.
 * <p>
 * <p> Revision 1.30  2007/12/13 01:05:03  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.29  2007/11/06 19:10:36  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.28  2007/06/08 23:55:59  sueh
 * <p> bug# 995 Saving the value of useEveryNSlices when is REJOIN_TRIAL mode.
 * <p>
 * <p> Revision 1.27  2007/05/11 15:27:01  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.26  2007/04/23 23:19:01  sueh
 * <p> bug# 991 getRejoinOptions:  get the last end list value.
 * <p>
 * <p> Revision 1.25  2007/03/07 21:01:24  sueh
 * <p> bug# 981 Changed ScriptParameter.isUseInScript to isNotNullAndNotDefault for
 * <p> clarity.
 * <p>
 * <p> Revision 1.24  2007/03/01 01:12:14  sueh
 * <p> bug# 964 Using getInstance functions in IntKeyList.
 * <p>
 * <p> Revision 1.23  2007/02/08 02:01:13  sueh
 * <p> bug# 962 Added TRIAL_REJOIN functionality.
 * <p>
 * <p> Revision 1.22  2007/02/05 21:52:53  sueh
 * <p> bug# 962 Added genRejoinOptions.
 * <p>
 * <p> Revision 1.21  2006/10/25 14:59:08  sueh
 * <p> bug# 949 Temporarily printing the finishjoin command.
 * <p>
 * <p> Revision 1.20  2006/05/22 22:38:52  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.19  2006/05/11 19:43:18  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.18  2006/04/06 18:58:57  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 1.17  2006/01/20 20:47:02  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.16  2005/11/29 22:20:46  sueh
 * <p> bug# 757 Use join final start and end for finish join.
 * <p>
 * <p> Revision 1.15  2005/11/19 01:52:32  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.14  2005/07/29 19:45:31  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.13  2005/07/29 00:47:49  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.12  2005/07/21 21:32:26  sueh
 * <p> bug# 532 ConstEtomoNumber.getInvalidReason() is no longer returning
 * <p> the description.
 * <p>
 * <p> Revision 1.11  2005/04/25 20:39:41  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.10  2005/01/25 21:40:41  sueh
 * <p> Converting EtomoNumbers to ScriptParameters.
 * <p>
 * <p> Revision 1.9  2005/01/21 22:41:57  sueh
 * <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
 * <p> isSetAndNotDefault() as a standard why to decide if a parameter should
 * <p> be placed in a comscript.
 * <p>
 * <p> Revision 1.8  2005/01/08 01:39:09  sueh
 * <p> bug# 578 Changed the names of the statics used to make variables
 * <p> available in the Command interface.  Add GET_.  Updated Command
 * <p> interface.
 * <p>
 * <p> Revision 1.7  2004/12/08 21:21:06  sueh
 * <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
 * <p> Changed statics SHIFT_IN_X_VALUE_NAME, etc to SHIFT_IN_X.
 * <p>
 * <p> Revision 1.6  2004/12/02 18:24:48  sueh
 * <p> bug 520 Remove unnecessary import.
 * <p>
 * <p> Revision 1.5  2004/12/01 03:45:15  sueh
 * <p> bug# 520 Removed unnecessary member variable SystemProgram
 * <p> program.
 * <p>
 * <p> Revision 1.4  2004/11/24 23:03:47  sueh
 * <p> bug# 520 Add -P option to get the PID from finishjoin.
 * <p>
 * <p> Revision 1.3  2004/11/23 22:29:29  sueh
 * <p> bug# 520 Converted finalStart and end to EtomoNumbers.
 * <p>
 * <p> Revision 1.2  2004/11/19 22:56:59  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.10  2004/11/16 02:20:21  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.9  2004/11/12 22:47:53  sueh
 * <p> bug# 520 Storing binning, size, and shift information.  Added getBinning().
 * <p> Added getIntegerValue().
 * <p>
 * <p> Revision 1.1.2.8  2004/11/11 01:35:32  sueh
 * <p> bug# 520 Adding trial mode:  using -t with sampling rate in Z and binning.
 * <p>
 * <p> Revision 1.1.2.7  2004/11/08 22:10:50  sueh
 * <p> bug# 520 Add modes, implement MAX_SIZE_MODE.  Implement
 * <p> Command.  Add function to query current mode.  This way the information
 * <p> return by the -m option (max size) can be retrieved with generic code.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/30 01:28:16  sueh
 * <p> bug# 520 Added comments.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/29 01:17:06  sueh
 * <p> bug# 520 Removed working directory from meta data.  Getting working
 * <p> directory from propertyUserDir.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/25 22:58:24  sueh
 * <p> bug# 520 Use the negative of shift in X, Y when passing to finish join.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/22 20:55:34  sueh
 * <p> bug# 520 Using EtomoSimpleType where possible.  Changed offsetInX, Y
 * <p> to shiftInX, Y.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/22 03:20:38  sueh
 * <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
 * <p> passing EtomoInteger, EtomoFloat, etc and using its get() and getString()
 * <p> functions.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/21 02:32:20  sueh
 * <p> bug# 520 Param for finishjoin.
 * <p> </p>
 */
public final class FinishjoinParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  public static final String SIZE_TAG = "Maximum size required:";
  public static final String OFFSET_TAG = "Offset needed to center:";
  public static final int SIZE_IN_X_INDEX = 3;
  public static final int SIZE_IN_Y_INDEX = 4;
  public static final int OFFSET_IN_X_INDEX = 4;
  public static final int OFFSET_IN_Y_INDEX = 5;

  private static final ProcessName PROCESS_NAME = ProcessName.FINISHJOIN;
  public static final String COMMAND_NAME = "finishjoin";
  private final int debug;
  private String[] commandArray;
  private String rootName;
  private File outputFile;
  private Mode mode;
  private final JoinManager manager;
  // set in genOptions
  private EtomoNumber alignmentRefSection = null;
  private ScriptParameter sizeInX = null;
  private ScriptParameter sizeInY = null;
  private ScriptParameter shiftInX = null;
  private ScriptParameter shiftInY = null;
  private IntKeyList joinStartList = null;
  private IntKeyList joinEndList = null;
  private ScriptParameter binning = null;
  private EtomoNumber useEveryNSlices = null;
  // set in getRejoinOptions
  private IntKeyList refineStartList = null;
  private IntKeyList refineEndList = null;
  private EtomoBoolean2 localFits = null;

  public FinishjoinParam(JoinManager manager, Mode mode) {
    this(manager, mode, 1);
  }

  public FinishjoinParam(JoinManager manager, Mode mode, int debug) {
    this.debug = debug;
    this.manager = manager;
    this.mode = mode;
    rootName = manager.getBaseMetaData().getName();
    outputFile = new File(manager.getPropertyUserDir(), rootName + DatasetFiles.JOIN_EXT);
    ArrayList options;
    if (mode == Mode.REJOIN || mode == Mode.SUPPRESS_EXECUTION
        || mode == Mode.TRIAL_REJOIN) {
      options = genRejoinOptions();
    }
    else {
      options = genOptions();
    }
    if (mode == Mode.SUPPRESS_EXECUTION) {
      if (debug >= 1) {
        System.err.print("SUPPRESS_EXECUTION:");
      }
    }
    int commandSize = 4;
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "python";
    commandArray[1] = "-u";
    commandArray[2] = BaseManager.getIMODBinPath() + COMMAND_NAME;
    commandArray[3] = "-PID";
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
    if (debug >= 1) {
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

  public static String getOutputName(JoinManager manager) {
    return manager.getBaseMetaData().getName() + ".join";
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.ALIGNMENT_REF_SECTION) {
      return alignmentRefSection;
    }
    if (fieldInterface == Fields.SIZE_IN_X) {
      return sizeInX;
    }
    if (fieldInterface == Fields.SIZE_IN_Y) {
      return sizeInY;
    }
    if (fieldInterface == Fields.SHIFT_IN_X) {
      return shiftInX;
    }
    if (fieldInterface == Fields.SHIFT_IN_Y) {
      return shiftInY;
    }
    if (fieldInterface == Fields.BINNING) {
      return binning;
    }
    if (fieldInterface == Fields.USE_EVERY_N_SLICES) {
      return useEveryNSlices;
    }
    if (fieldInterface == Fields.LOCAL_FITS) {
      return localFits;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.JOIN_START_LIST) {
      return joinStartList;
    }
    if (fieldInterface == Fields.JOIN_END_LIST) {
      return joinEndList;
    }
    if (fieldInterface == Fields.REFINE_START_LIST) {
      return refineStartList;
    }
    if (fieldInterface == Fields.REFINE_END_LIST) {
      return refineEndList;
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
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

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getCommand() {
    return COMMAND_NAME;
  }

  public String getName() {
    return COMMAND_NAME;
  }

  public static int getShift(String offset) {
    EtomoNumber offsetNumber = new EtomoNumber(EtomoNumber.Type.INTEGER);
    if (offsetNumber.set(offset).isValid()) {
      return offsetNumber.getInt() * -1;
    }
    throw new IllegalArgumentException(offsetNumber.getDescription() + ": "
        + offsetNumber.getInvalidReason());
  }

  public File getCommandOutputFile() {
    return outputFile;
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

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public FileType getOutputImageFileType() {
    if (mode == Mode.FINISH_JOIN) {
      return FileType.JOIN;
    }
    if (mode == Mode.MAX_SIZE) {
      return null;
    }
    if (mode == Mode.TRIAL) {
      return FileType.TRIAL_JOIN;
    }
    if (mode == Mode.REJOIN) {
      return FileType.JOIN;
    }
    if (mode == Mode.TRIAL_REJOIN) {
      return FileType.TRIAL_JOIN;
    }
    if (mode == Mode.SUPPRESS_EXECUTION) {
      return null;
    }
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  /**
   * For calls from the Join tab.  Generates options and sets the member
   * variables (except refine start and end lists).  For rejoin see
   * genRejoinOptions.
   * @return
   */
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    // options.add("-P");
    ConstJoinMetaData metaData = manager.getConstMetaData();
    if (metaData.isUseAlignmentRefSection()) {
      alignmentRefSection = new EtomoNumber(metaData.getAlignmentRefSection());
      options.add("-r");
      options.add(alignmentRefSection.toString());
    }
    // Add optional size
    sizeInX = new ScriptParameter(metaData.getSizeInXParameter());
    sizeInY = new ScriptParameter(metaData.getSizeInYParameter());
    if (sizeInX.isNotNullAndNotDefault() || sizeInY.isNotNullAndNotDefault()) {
      options.add("-s");
      // both numbers must exist
      options.add(sizeInX.toString() + "," + sizeInY.toString());
    }
    // Add optional offset
    shiftInX = new ScriptParameter(metaData.getShiftInXParameter());
    shiftInY = new ScriptParameter(metaData.getShiftInYParameter());
    if (shiftInX.isNotNullAndNotDefault() || shiftInY.isNotNullAndNotDefault()) {
      options.add("-o");
      // both numbers must exist
      // offset is a negative shift
      options.add(Integer.toString(shiftInX.getInt() * -1) + ","
          + Integer.toString(shiftInY.getInt() * -1));
    }
    localFits = new EtomoBoolean2();
    if (metaData.isLocalFits()) {
      localFits.set(true);
      options.add("-l");
    }
    if (mode == Mode.MAX_SIZE) {
      options.add("-m");
    }
    if (mode == Mode.TRIAL) {
      options.add("-t");
      useEveryNSlices = new EtomoNumber(metaData.getUseEveryNSlices());
      options.add(useEveryNSlices.toString());
      binning = new ScriptParameter(metaData.getTrialBinningParameter());
      if (binning.isNotNullAndNotDefault()) {
        options.add("-b");
        options.add(binning.toString());
      }
    }
    options.add(rootName);
    ArrayList sectionData = metaData.getSectionTableData();
    int sectionDataSize = sectionData.size();
    joinStartList = IntKeyList.getStringInstance();
    joinEndList = IntKeyList.getStringInstance();
    for (int i = 0; i < sectionDataSize; i++) {
      ConstSectionTableRowData data = (SectionTableRowData) sectionData.get(i);
      joinStartList.put(i, data.getJoinFinalStart());
      joinEndList.put(i, data.getJoinFinalEnd());
      // both numbers must exist
      options.add(data.getJoinFinalStart() + "," + data.getJoinFinalEnd());
    }
    return options;
  }

  /**
   * For calls from the Model and Rejoin tabs.  Generates the options using the
   * state.  Saves refine start and end lists.
   * @return
   */
  private ArrayList genRejoinOptions() {
    ArrayList options = new ArrayList();
    // options.add("-P");
    ConstJoinState state = manager.getState();
    boolean trial = state.getRefineTrial().is();
    if (!state.getJoinAlignmentRefSection(trial).isNull()) {
      options.add("-r");
      options.add(state.getJoinAlignmentRefSection(trial).toString());
    }
    // Add optional size
    if (state.getJoinSizeInXParameter(trial).isNotNullAndNotDefault()
        || state.getJoinSizeInYParameter(trial).isNotNullAndNotDefault()) {
      options.add("-s");
      // both numbers must exist
      options.add(state.getJoinSizeInX(trial).toString() + ","
          + state.getJoinSizeInY(trial).toString());
    }
    // Add optional offset
    if (state.getJoinShiftInXParameter(trial).isNotNullAndNotDefault()
        || state.getJoinShiftInYParameter(trial).isNotNullAndNotDefault()) {
      options.add("-o");
      // both numbers must exist
      // offset is a negative shift
      options.add(Integer.toString(state.getJoinShiftInX(trial).getInt() * -1) + ","
          + Integer.toString(state.getJoinShiftInY(trial).getInt() * -1));
    }
    if (state.isJoinLocalFits(trial)) {
      options.add("-l");
    }
    if (mode == Mode.TRIAL_REJOIN) {
      ConstJoinMetaData metaData = manager.getConstMetaData();
      options.add("-t");
      useEveryNSlices = new EtomoNumber(metaData.getRejoinUseEveryNSlices());
      options.add(useEveryNSlices.toString());
      ScriptParameter rejoinBinning = new ScriptParameter(
          metaData.getRejoinTrialBinningParameter());
      if (rejoinBinning.isNotNullAndNotDefault()) {
        options.add("-b");
        options.add(rejoinBinning.toString());
      }
    }
    options.add("-gaps");
    options.add("-xform");
    options.add(DatasetFiles.getRefineJoinXgFileName(manager));
    options.add(rootName);
    ConstJoinMetaData metaData = manager.getConstMetaData();
    ArrayList sectionData = metaData.getSectionTableData();
    // The first start and end: start comes from join final start and end comes
    // from the boundary table
    IntKeyList.Walker startListWalker = state.getJoinStartListWalker(trial);
    IntKeyList.Walker endListWalker = state.getJoinEndListWalker(trial);
    IntKeyList.Walker gapStartListWalker = metaData.getBoundaryRowStartListWalker();
    IntKeyList.Walker gapEndListWalker = metaData.getBoundaryRowEndListWalker();
    // Make sure that lists are valid
    int numRows = startListWalker.size();
    // Build the refine start and end list, so it can be sent to the join state
    refineStartList = IntKeyList.getNumberInstance();
    refineEndList = IntKeyList.getNumberInstance();
    if (numRows >= 2 && numRows == endListWalker.size()
        && numRows == gapStartListWalker.size() + 1
        && numRows == gapEndListWalker.size() + 1) {
      // Add the first first and end pair. Start comes from join final start and
      // end comes from the boundary table
      ConstEtomoNumber start = startListWalker.nextEtomoNumber();
      ConstEtomoNumber end = gapEndListWalker.nextEtomoNumber();
      options.add(start + "," + end);
      // set the first key (first row index) from the join start list. Then add
      // the rows in order, incrementing the key by 1 each time
      refineStartList.reset(1);
      refineEndList.reset(1);
      refineStartList.add(start);
      refineEndList.add(end);
      // The middle start and end pairs come from the boundary table
      // while look should end when gapEndListWalker runs out of values (its ahead
      // of gapStartListWalker by 1).
      while (gapEndListWalker.hasNext()) {
        start = gapStartListWalker.nextEtomoNumber();
        end = gapEndListWalker.nextEtomoNumber();
        options.add(start + "," + end);
        refineStartList.add(start);
        refineEndList.add(end);
      }
      // The last start and end: start comes from the boundary table and end comes from
      // the last join final end.
      start = gapStartListWalker.nextEtomoNumber();
      end = endListWalker.getLastEtomoNumber();
      options.add(start + "," + end);
      refineStartList.add(start);
      refineEndList.add(end);
    }
    else if (numRows >= 2 && numRows == endListWalker.size()
        && numRows > gapStartListWalker.size() + 1
        && numRows > gapEndListWalker.size() + 1) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "The dataset file may be corrupted.  If this process fails, exit "
              + "and rerun Etomo, then go to the Join tab and run Finish "
              + "Join to fix the problem.", "Etomo Warning");
    }
    return options;
  }

  public static final class Fields implements etomo.comscript.FieldInterface {
    public static final Fields ALIGNMENT_REF_SECTION = new Fields();
    public static final Fields SIZE_IN_X = new Fields();
    public static final Fields SIZE_IN_Y = new Fields();
    public static final Fields SHIFT_IN_X = new Fields();
    public static final Fields SHIFT_IN_Y = new Fields();
    public static final Fields BINNING = new Fields();
    public static final Fields JOIN_START_LIST = new Fields();
    public static final Fields JOIN_END_LIST = new Fields();
    public static final Fields REFINE_START_LIST = new Fields();
    public static final Fields REFINE_END_LIST = new Fields();
    public static final Fields USE_EVERY_N_SLICES = new Fields();
    public static final Fields LOCAL_FITS = new Fields();

    private Fields() {
    }
  }

  public final static class Mode implements etomo.comscript.CommandMode {
    public static final Mode FINISH_JOIN = new Mode("FinishJoin");
    public static final Mode MAX_SIZE = new Mode("MaxSize");
    public static final Mode TRIAL = new Mode("Trial");
    public static final Mode REJOIN = new Mode("Rejoin");
    public static final Mode TRIAL_REJOIN = new Mode("TrialRejoin");
    public static final Mode SUPPRESS_EXECUTION = new Mode("SuppressExecution");

    private final String key;

    public String toString() {
      return key;
    }

    private Mode(String key) {
      this.key = key;
    }
  }
}
