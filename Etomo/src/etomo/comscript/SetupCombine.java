/**
 * <p>
 * Description:
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 3.24  2011/02/21 21:32:38  sueh
 * bug# 1437 Reformatting.
 *
 * Revision 3.23  2010/12/03 19:35:52  sueh
 * bug# 1419 In SetupCombine, running command with python.
 *
 * Revision 3.22  2010/02/17 04:47:54  sueh
 * bug# 1301 Using the manager instead of the manager key do pop up
 * messages.
 *
 * Revision 3.21  2009/12/28 20:37:40  sueh
 * bug# 1300 Added -v option to tcsh when debug is on.
 *
 * Revision 3.20  2009/12/28 20:09:16  sueh
 * bug# 1300 Added -v option tcsh.
 *
 * Revision 3.19  2009/03/17 00:32:57  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.18  2007/12/26 22:11:58  sueh
 * bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 *
 * Revision 3.17  2007/12/10 21:59:49  sueh
 * bug# 1041 Formatted.
 *
 * Revision 3.16  2007/09/07 00:18:12  sueh
 * bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * instead of getInstance and createInstance.
 *
 * Revision 3.15  2006/07/19 15:17:00  sueh
 * bug# 903 genOptions:  throw an exception when patchZMin or Max is null.
 *
 * Revision 3.14  2006/06/07 17:46:31  sueh
 * bug# 766 Don't use uselist unless it has data
 *
 * Revision 3.13  2006/05/16 21:27:58  sueh
 * bug# 856 Added transfer and useList.
 *
 * Revision 3.12  2006/05/12 00:08:13  sueh
 * bug# 857 Running setupcombine with command line arguments.
 *
 * Revision 3.11  2006/03/16 01:50:37  sueh
 * bug# 828 Added matchMode.
 * CombineParams.DialogMatchMode reflects the state of the dialog.
 * CombineParams.MatchMode reflects the state of the script.
 * MatchMode is set equal to dialogMatchMode when the script is updated.
 *
 * Revision 3.10  2005/11/02 21:38:29  sueh
 * bug# 754 Parsing errors and warnings inside ProcessMessages.
 * Replaced warningMessage with processMessages.  Removed functions
 * getWarningMessage, getWarnings, and parseWarnings.  Added
 * getProcessMessages().
 *
 * Revision 3.9  2005/10/28 18:49:33  sueh
 * bug# 725 standardizing message parsing in SystemProgram.  Passing
 * multilineError to SystemProgram constructor.
 *
 * Revision 3.8  2005/09/09 21:21:41  sueh
 * bug# 532 Handling null from stderr and stdout.
 *
 * Revision 3.7  2005/07/29 00:49:34  sueh
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * because the current manager changes when the user changes the tab.
 * Passing the manager where its needed.
 *
 * Revision 3.6  2005/05/10 17:31:32  sueh
 * bug# 660 Added getWarnings() to get an array of warnings from standard
 * error.
 *
 * Revision 3.5  2005/04/25 20:40:46  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.
 *
 * Revision 3.4  2005/01/14 02:59:05  sueh
 * Prevented non-error messages from showing up in the err.log  file unless
 * debug is on.
 *
 * Revision 3.3  2004/06/17 19:26:39  rickg
 * Bug #477 added USE_MODEL_ONLY option
 *
 * Revision 3.2  2004/04/22 23:28:28  rickg
 * *** empty log message ***
 *
 * Revision 3.1  2004/04/21 20:26:55  sueh
 * bug# 61 added warningMessage and parseWarning(), which is called
 * from run()
 *
 * Revision 3.0  2003/11/07 23:19:00  rickg
 * Version 1.0.0
 *
 * Revision 2.11  2003/11/06 16:50:27  rickg
 * Removed -e flag for tcsh execution for all but the com scripts
 *
 * Revision 2.10  2003/11/04 20:56:11  rickg
 * Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 *
 * Revision 2.9  2003/11/04 00:53:50  rickg
 * Bug #345 Explicitly set path to script using IMOD_DIR
 * remove -c from tcsh invokation
 *
 * <p>
 * Revision 2.8 2003/05/21 21:23:34 rickg
 * <p>
 * Added e flag to tcsh execution
 * <p>
 * <p>
 * Revision 2.7 2003/05/13 20:01:13 rickg
 * <p>
 * Added -f option, removed -e to tcsh call
 * <p>
 * <p>
 * Revision 2.6 2003/05/12 23:23:58 rickg
 * <p>
 * Explicitly call tcsh -ec for windows
 * <p>
 * <p>
 * Revision 2.5 2003/05/08 23:19:03 rickg
 * <p>
 * Standardized debug setting
 * <p>
 * <p>
 * Revision 2.4 2003/05/07 22:30:06 rickg
 * <p>
 * Don't need to set working directory since it defaults to user.dir
 * <p>
 * <p>
 * Revision 2.3 2003/04/24 17:46:54 rickg
 * <p>
 * Changed fileset name to dataset name
 * <p>
 * <p>
 * Revision 2.2 2003/03/20 17:23:58 rickg
 * <p>
 * Comment update
 * <p>
 * <p>
 * Revision 2.1 2003/03/18 23:49:02 rickg
 * <p>
 * Changed method name to get CombineParams reference
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.7 2003/01/06 05:51:23 rickg
 * <p>
 * Corrected fiducial list order, it was backwards
 * <p>
 * <p>
 * Revision 1.6 2003/01/04 00:06:08 rickg
 * <p>
 * Swapped order for to/from fiducial mapping when
 * <p>
 * matching from b to a.
 * <p>
 * <p>
 * Revision 1.5 2002/10/24 23:49:53 rickg
 * <p>
 * Added getStdout method
 * <p>
 * <p>
 * Revision 1.4 2002/10/10 18:55:38 rickg
 * <p>
 * Output blank line when there is no match lists
 * <p>
 * Enabled SystemProgram debugging and remove local
 * <p>
 * writing to stdout.
 * <p>
 * <p>
 * Revision 1.3 2002/10/08 23:57:11 rickg
 * <p>
 * Added remaining parameters for stdin sequence to script
 * <p>
 * <p>
 * Revision 1.2 2002/09/30 23:45:21 rickg
 * <p>
 * Reformatted after emacs trashed it
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */
package etomo.comscript;

import java.io.IOException;
import java.util.ArrayList;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.ProcessMessages;
import etomo.process.SystemProcessException;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.CombinePatchSize;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.FiducialMatch;
import etomo.type.MatchMode;
import etomo.util.DatasetFiles;

public class SetupCombine {
  public static final String rcsid = "$Id$";

  private final SystemProgram setupcombine;

  private final ArrayList command = new ArrayList();
  int exitValue;
  ConstMetaData metaData;
  boolean debug;
  private final BaseManager manager;
  private MatchMode matchMode = null;
  private boolean transfer = false;

  public SetupCombine(ApplicationManager manager) throws SystemProcessException {
    this.manager = manager;
    metaData = manager.getConstMetaData();
    debug = EtomoDirector.INSTANCE.getArguments().isDebug();

    //  Create a new SystemProgram object for setupcombine, set the
    //  working directory and stdin array.
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06 
    command.add("python");
    command.add("-u");
    command.add(ApplicationManager.getIMODBinPath() + "setupcombine");
    genOptions();
    String[] commandArray = new String[command.size()];
    for (int i = 0; i < commandArray.length; i++) {
      commandArray[i] = (String) command.get(i);
    }
    setupcombine = new SystemProgram(manager, manager.getPropertyUserDir(), commandArray,
        AxisID.ONLY);
    //genStdInputSequence();
  }

  public MatchMode getMatchMode() {
    return matchMode;
  }

  public boolean isTransfer() {
    return transfer;
  }

  private void genOptions() throws SystemProcessException {
    CombineParams combineParams = metaData.getCombineParams();
    matchMode = combineParams.getMatchMode();
    String matchListTo;
    String matchListFrom;
    FiducialMatch fiducialMatch = combineParams.getFiducialMatch();
    CombinePatchSize patchSize = combineParams.getPatchSize();
    //dataset name
    command.add("-name");
    command.add(metaData.getDatasetName());
    //transfer fid coord file and point list
    if (combineParams.isTransfer()) {
      transfer = true;
      command.add("-transfer");
      command.add(DatasetFiles.getTransferFidCoordFileName());
      String useList = combineParams.getUseList();
      if (useList != null && !useList.matches("\\s*")) {
        command.add("-uselist");
        command.add(combineParams.getUseList());
      }
    }
    //matching relationship
    if (matchMode == MatchMode.A_TO_B) {
      command.add("-atob");
    }
    //corresponding lists
    if (!transfer) {
      if (matchMode == MatchMode.A_TO_B) {
        matchListTo = combineParams.getFiducialMatchListB();
        matchListFrom = combineParams.getFiducialMatchListA();
      }
      else {
        matchListTo = combineParams.getFiducialMatchListA();
        matchListFrom = combineParams.getFiducialMatchListB();
      }
      //points lists
      if (!matchListTo.equals("")) {
        command.add("-tolist");
        command.add(matchListTo);
        command.add("-fromlist");
        command.add(matchListFrom);
      }
    }
    //fiducial surfaces / use model
    if (fiducialMatch != null && fiducialMatch != FiducialMatch.NOT_SET) {
      command.add("-surfaces");
      command.add(fiducialMatch.getOption());
    }
    //patch sizes
    if (patchSize != null) {
      command.add("-patchsize");
      command.add(patchSize.getOption());
    }
    int min = combineParams.getPatchXMin();
    int max = combineParams.getPatchXMax();
    if (min != 0 || max != 0) {
      command.add("-xlimits");
      command.add(String.valueOf(min) + "," + String.valueOf(max));
    }
    min = combineParams.getPatchYMin();
    max = combineParams.getPatchYMax();
    if (min != 0 || max != 0) {
      command.add("-ylimits");
      command.add(String.valueOf(min) + "," + String.valueOf(max));
    }
    ConstEtomoNumber number = combineParams.getPatchZMin();
    if (number.isNull()) {
      throw new SystemProcessException(ConstCombineParams.PATCH_Z_MIN_LABEL
          + " is required.");
    }
    min = combineParams.getPatchZMin().getInt();
    number = combineParams.getPatchZMax();
    if (number.isNull()) {
      throw new SystemProcessException(ConstCombineParams.PATCH_Z_MAX_LABEL
          + " is required.");
    }
    max = combineParams.getPatchZMax().getInt();
    if (min != 0 || max != 0) {
      command.add("-zlimits");
      command.add(String.valueOf(min) + "," + String.valueOf(max));
    }
    //patch region model
    if (combineParams.usePatchRegionModel()) {
      command.add("-regionmod");
      command.add(combineParams.patchRegionModel);
    }
    if (!combineParams.tempDirectory.equals("")) {
      command.add("-tempdir");
      command.add(combineParams.tempDirectory);
      if (combineParams.getManualCleanup()) {
        command.add("-noclean");
      }
    }
  }

  /**
   * @deprecated
   * Generate the standard input sequence
   */
  private void genStdInputSequence() {

    CombineParams combineParams = metaData.getCombineParams();
    matchMode = combineParams.getMatchMode();
    //writing the script, so set the script match mode to be the same as the
    //screen match mode
    combineParams.setMatchMode(matchMode);

    String[] tempStdInput = new String[15];

    //  compile the input sequence to setupcombine
    int lineCount = 0;

    //  Dataset name
    tempStdInput[lineCount++] = metaData.getDatasetName();

    //  Matching relationship
    if (matchMode == null || matchMode == MatchMode.B_TO_A) {
      tempStdInput[lineCount++] = "a";
      if (combineParams.getFiducialMatchListA() != "") {
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListA();
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListB();
      }
      else {
        tempStdInput[lineCount++] = "";
      }
    }
    else {
      tempStdInput[lineCount++] = "b";
      if (combineParams.getFiducialMatchListB() != "") {
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListB();
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListA();
      }
      else {
        tempStdInput[lineCount++] = "";
      }
    }

    //  Fiducial surfaces / use model
    if (combineParams.getFiducialMatch() == FiducialMatch.BOTH_SIDES) {
      tempStdInput[lineCount++] = "2";
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE) {
      tempStdInput[lineCount++] = "1";
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE_INVERTED) {
      tempStdInput[lineCount++] = "-1";
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL) {
      tempStdInput[lineCount++] = "0";
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL_ONLY) {
      tempStdInput[lineCount++] = "-2";
    }

    //  Patch sizes
    if (combineParams.getPatchSize() == CombinePatchSize.LARGE) {
      tempStdInput[lineCount++] = "l";
    }

    if (combineParams.getPatchSize() == CombinePatchSize.MEDIUM) {
      tempStdInput[lineCount++] = "m";
    }

    if (combineParams.getPatchSize() == CombinePatchSize.SMALL) {
      tempStdInput[lineCount++] = "s";
    }

    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchXMin());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchXMax());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchYMin());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchYMax());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchZMin());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchZMax());

    tempStdInput[lineCount++] = combineParams.patchRegionModel;

    tempStdInput[lineCount++] = combineParams.tempDirectory;
    if (!combineParams.tempDirectory.equals("")) {
      if (combineParams.getManualCleanup()) {
        tempStdInput[lineCount++] = "y";
      }
      else {
        tempStdInput[lineCount++] = "n";
      }
    }

    //
    //  Copy the temporary stdInput to the real stdInput to get the number
    //  of array elements correct
    String[] stdInput = new String[lineCount];
    for (int i = 0; i < lineCount; i++) {
      stdInput[i] = tempStdInput[i];
    }
    setupcombine.setStdInput(stdInput);

  }

  public int run() throws IOException {
    int exitValue;
    //  Execute the script
    setupcombine.run();
    exitValue = setupcombine.getExitValue();
    return exitValue;
  }

  public String[] getStdError() {
    return setupcombine.getStdError();
  }

  public final ProcessMessages getProcessMessages() {
    return setupcombine.getProcessMessages();
  }
}