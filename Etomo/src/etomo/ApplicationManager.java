package etomo;

import java.io.*;
import etomo.util.InvalidParameterException;
import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.FontUIResource;

import etomo.comscript.*;
import etomo.process.*;
import etomo.type.*;
import etomo.storage.*;
import etomo.ui.*;

/**
 * <p>Description: Provides the main entry point, handles high level message 
 *  processing, management of other high-level</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.4  2003/01/29 15:22:58  rickg
 * <p> Updated logic for combine step
 * <p>
 * <p> Revision 2.3  2003/01/28 20:42:53  rickg
 * <p> Bug fix: save current dialog state when running align.com
 * <p>
 * <p> Revision 2.2  2003/01/28 00:15:29  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.1  2003/01/27 18:12:41  rickg
 * <p> Fixed bug from single window transition in positioning dialog
 * <p> opening function
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.36.2.1  2003/01/24 18:27:46  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.36  2003/01/10 20:46:34  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.35  2003/01/10 18:39:58  rickg
 * <p> Using existing com scripts now gives the correct
 * <p> process state
 * <p>
 * <p> Revision 1.34  2003/01/10 18:33:16  rickg
 * <p> Added test parameter filename to command line args
 * <p>
 * <p> Revision 1.33  2003/01/08 21:04:38  rickg
 * <p> More descriptive error dialog when the are not
 * <p> available for combining.
 * <p>
 * <p> Revision 1.32  2003/01/07 00:30:16  rickg
 * <p> Added imodViewResidual method
 * <p>
 * <p> Revision 1.31  2003/01/06 04:53:16  rickg
 * <p> Set default parameters for transferfid panel and handle
 * <p> new backwards flag for b to a
 * <p>
 * <p> Revision 1.30  2003/01/04 00:41:00  rickg
 * <p> Implemented transferfid method
 * <p>
 * <p> Revision 1.29  2002/12/19 00:35:20  rickg
 * <p> Implemented persitent advanced state handling
 * <p>
 * <p> Revision 1.27  2002/12/11 21:26:31  rickg
 * <p> Added font setting into user prefs setting
 * <p>
 * <p> Revision 1.26  2002/12/11 05:39:00  rickg
 * <p> Added basic font change method
 * <p>
 * <p> Revision 1.25  2002/12/11 00:39:48  rickg
 * <p> Basic handling of settings dialog
 * <p> added setUserPreferences method
 * <p>
 * <p> Revision 1.24  2002/12/09 04:18:50  rickg
 * <p> Better handling of current working directory, user.dir and
 * <p> metaData always agree now.
 * <p>
 * <p> Revision 1.23  2002/12/05 01:21:02  rickg
 * <p> Added isAdvanced stub
 * <p>
 * <p> Revision 1.22  2002/11/21 19:24:38  rickg
 * <p> Set user.dir to current working directory
 * <p>
 * <p> Revision 1.21  2002/10/29 18:22:04  rickg
 * <p> Simplified rawstack open checking
 * <p>
 * <p> Revision 1.20  2002/10/25 19:30:43  rickg
 * <p> Modifies several catches to explicilty specify exception
 * <p>
 * <p> Revision 1.19  2002/10/24 19:52:55  rickg
 * <p> Added command line --demo argument
 * <p>
 * <p> Revision 1.18  2002/10/22 21:38:24  rickg
 * <p> ApplicationManager now controls both demo and debug
 * <p> modes
 * <p>
 * <p> Revision 1.17  2002/10/17 22:47:35  rickg
 * <p> process dialogs are now managed attributes
 * <p> setVisible calls changed to show
 * <p> unused variable dialogFinshed removed
 * <p>
 * <p> Revision 1.16  2002/10/17 16:23:04  rickg
 * <p> Added private method to update the dependent tilt parameters when the
 * <p> align.com parameters are changed
 * <p>
 * <p> Revision 1.15  2002/10/16 17:37:18  rickg
 * <p> Construct a imodManager when a new data set is opened
 * <p>
 * <p> Revision 1.14  2002/10/14 22:44:27  rickg
 * <p> Added combine to execute section of doneCombine
 * <p>
 * <p> Revision 1.13  2002/10/14 19:04:18  rickg
 * <p> openMessageDialog made public
 * <p>
 * <p> Revision 1.12  2002/10/10 23:40:33  rickg
 * <p> refactored createCombineScripts to setupCombineScripts
 * <p>
 * <p> Revision 1.11  2002/10/10 19:16:19  rickg
 * <p> Get HOME and IMOD_DIR environement variables during
 * <p> initialization instead of each time they requested.  Also
 * <p> exit if they are not available.
 * <p>
 * <p> Revision 1.10  2002/10/09 04:29:17  rickg
 * <p> Implemented calls to updateCombineCom
 * <p>
 * <p> Revision 1.9  2002/10/09 00:04:37  rickg
 * <p> Added default patch boundary logic
 * <p> still needs work on getting combine parameters at the correct times
 * <p>
 * <p> Revision 1.8  2002/10/07 22:20:21  rickg
 * <p> removed unused imports
 * <p>
 * <p> Revision 1.7  2002/09/30 23:44:43  rickg
 * <p> Started implementing updateCombineCom
 * <p>
 * <p> Revision 1.6  2002/09/30 22:01:29  rickg
 * <p> Added check to verify dual axis for combination
 * <p>
 * <p> Revision 1.5  2002/09/20 18:56:09  rickg
 * <p> Added private message and yes/no dialog methods
 * <p> Check to see if the raw stack and coarsely aligned stacks should be
 * <p> closed by the user
 * <p>
 * <p> Revision 1.4  2002/09/19 22:57:56  rickg
 * <p> Imod mangement is now handled through the ImodManager
 * <p>
 * <p> Revision 1.3  2002/09/17 23:44:56  rickg
 * <p> Adding ImodManager, in progress
 * <p>
 * <p> Revision 1.2  2002/09/13 21:29:57  rickg
 * <p> Started updating for ImodManager
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ApplicationManager {
  public static final String rcsid =
    "$Id$";

  private boolean debug = true;
  private boolean demo = false;

  private boolean isDataParamDirty = false;
  private String homeDirectory;
  private String IMODDirectory;

  private UserConfiguration userConfig = new UserConfiguration();
  private MetaData metaData = new MetaData();
  private File paramFile = null;
  // advanced dialog state for this instance, this gets set upon startup from
  // the user configuration and can be modified for this instance by either
  // the option or advanced menu items
  private boolean isAdvanced = false;

  //  Process dialog references
  private SetupDialog setupDialog = null;
  private PreProcessingDialog preProcDialogA = null;
  private PreProcessingDialog preProcDialogB = null;
  private CoarseAlignDialog coarseAlignDialogA = null;
  private CoarseAlignDialog coarseAlignDialogB = null;
  private FiducialModelDialog fiducialModelDialogA = null;
  private FiducialModelDialog fiducialModelDialogB = null;
  private AlignmentEstimationDialog fineAlignmentDialogA = null;
  private AlignmentEstimationDialog fineAlignmentDialogB = null;
  private TomogramPositioningDialog tomogramPositioningDialogA = null;
  private TomogramPositioningDialog tomogramPositioningDialogB = null;
  private TomogramGenerationDialog tomogramGenerationDialogA = null;
  private TomogramGenerationDialog tomogramGenerationDialogB = null;
  private TomogramCombinationDialog tomogramCombinationDialog = null;

  private SettingsDialog settingsDialog = null;

  //  This object controls the reading and writing of David's com scripts
  private ComScriptManager comScriptMgr = new ComScriptManager(this);

  //  The ProcessManager manages the execution of com scripts
  private ProcessManager processMgr = new ProcessManager(this);
  private ProcessTrack processTrack = new ProcessTrack();
  private String threadNameA = "none";
  private String threadNameB = "none";

  // imodManager manages the opening and closing closing of imod(s), message 
  // passing for loading model
  private ImodManager imodManager;

  private MainFrame mainFrame;
  private Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

  /**
   * 
   */
  public ApplicationManager(String[] args) {
    //  Initialize the program settings
    String testParamFilename = initProgram(args);

    //  Initialize the static application manager reference for the
    //  context popup
    ContextPopup initContextPopup = new ContextPopup(this);

    //  Create a new main window and wait for an event from the user
    mainFrame = new MainFrame(this);
    mainFrame.setMRUFileLabels(userConfig.getMRUFileList());

    // Open the etomo data file if one was found on the command line
    if (!testParamFilename.equals("")) {
      File etomoDataFile = new File(testParamFilename);
      if (openTestParamFile(etomoDataFile)) {
        openProcessingPanel();
      }
      else {
        openSetupDialog();
      }
    }
    else {
      openSetupDialog();
    }
    mainFrame.pack();
    mainFrame.show();
  }

  /**
   * 
   */
  public static void main(String[] args) {
    new ApplicationManager(args);
  }

  /**
  * Open the setup dialog
  */
  public void openSetupDialog() {

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (setupDialog == null) {
      setupDialog = new SetupDialog(this);
      setupDialog.initializeFields(metaData);
    }
    mainFrame.openSetupPanel(setupDialog);
    Dimension frameSize = mainFrame.getSize();
    mainFrame.setLocation(
      (screenSize.width - frameSize.width) / 2,
      (screenSize.height - frameSize.height) / 2);
  }

  /**
   * Close message from the setup dialog window
   */
  public void doneSetupDialog() {
    if (setupDialog == null) {
      openMessageDialog(
        "Can not update metadata parameters without an active setup dialog",
        "Program logic error");
    }

    //  Get the selected exit button
    DialogExitState exitState = setupDialog.getExitState();

    if (exitState != DialogExitState.CANCEL) {
      //  FIXME?  should this totally write over the metaData object?
      metaData = setupDialog.getFields();

      if (metaData.isValid()) {
        mainFrame.updateDataParameters(metaData);
        processTrack.setSetupState(ProcessState.INPROGRESS);

        //  Set the current working directory for the application
        if (metaData.getWorkingDirectory().length() > 0) {
          System.setProperty("user.dir", getWorkingDirectory());
        }
        isDataParamDirty = true;

        //  Initialize a new IMOD manager
        imodManager = new ImodManager(metaData);
      }
      else {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Setup Parameter Error";
        errorMessage[1] = metaData.getInvalidReason();
        openMessageDialog(errorMessage, "Setup Parameter Error");
        return;
      }

      if (exitState == DialogExitState.POSTPONE) {
        metaData.setComScriptCreated(true);
        processTrack.setSetupState(ProcessState.COMPLETE);
      }
      else {
        try {
          processMgr.setupComScripts(metaData);
          processTrack.setSetupState(ProcessState.COMPLETE);
          metaData.setComScriptCreated(true);
        }
        catch (BadComScriptException except) {
          except.printStackTrace();
          openMessageDialog(except.getMessage(), "Can't run copytomocoms");
        }
        catch (IOException except) {
          openMessageDialog(
            "Can't run copytomocoms\n" + except.getMessage(),
            "Copytomocoms IOException");
        }
      }
    }

    //  Switch the main window to the procesing panel
    openProcessingPanel();

    //  Free the dialog
    setupDialog = null;

  }

  /**
   * Open the main window in processing mode
   */
  public void openProcessingPanel() {
    mainFrame.setLocation(0, 0);
    //  FIXME should set main window size to user default
    mainFrame.setSize(
      new Dimension(
        userConfig.getMainWindowWidth(),
        userConfig.getMainWindowHeight()));
    mainFrame.showProcessingPanel(metaData.getAxisType());
    mainFrame.updateAllProcessingStates(processTrack);
    mainFrame.validate();
  }

  /**
   * Open the pre-processing dialog
   */
  public void openPreProcDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    // FIXME: When a panel is overwriten by another should it be nulled and
    // closed or left and and reshown when needed?
    // Problem with stale data for align and tilt info since they are on
    // multiple panels
    // Check to see if the dialog panel is already open
    if (showIfExists(preProcDialogA, preProcDialogB, axisID)) {
      return;
    }

    PreProcessingDialog preProcDialog = new PreProcessingDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      preProcDialogB = preProcDialog;
    }
    else {
      preProcDialogA = preProcDialog;
    }

    // Load the required ccderaser{|a|b}.com files
    // Fill in the parameters and set it to the appropriate state
    comScriptMgr.loadEraserCom(axisID);
    preProcDialog.setCCDEraserParams(comScriptMgr.getCCDEraserParam(axisID));

    mainFrame.showProcess(preProcDialog.getContainer(), axisID);
  }

  /**
   * 
   */
  public void donePreProcDialog(AxisID axisID) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }

    if (preProcDialog == null) {
      openMessageDialog(
        "Can not update preprocessing parameters without an active "
          + "preprocessing dialog",
        "Program logic error");
      return;
    }

    //  Keep dialog box open until we get good info or it is cancelled
    DialogExitState exitState = preProcDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      updateEraserCom(axisID);

      // If there are raw stack imod processes open ask the user if they
      // should be closed.
      try {
        if (imodManager.isRawStackOpen(AxisID.FIRST)
          || imodManager.isRawStackOpen(AxisID.SECOND)) {
          String[] message = new String[2];
          message[0] = "There are raw stack imod processes open";
          message[1] = "Should they be closed?";
          boolean answer = openYesNoDialog(message);
          if (answer) {
            if (isDualAxis()) {
              imodManager.quitRawStack(AxisID.FIRST);
              imodManager.quitRawStack(AxisID.SECOND);
            }
            else {
              imodManager.quitRawStack(AxisID.ONLY);
            }
          }
        }
      }
      catch (AxisTypeException except) {
        except.printStackTrace();
        openMessageDialog(except.getMessage(), "AxisType problem");
      }
      catch (SystemProcessException except) {
        except.printStackTrace();
        openMessageDialog(except.getMessage(), "Problem closing raw stack");
      }

      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setPreProcessingState(ProcessState.COMPLETE, axisID);
        mainFrame.setPreProcessingState(ProcessState.COMPLETE, axisID);
        //  Go to the coarse align dialog by default
        openCoarseAlignDialog(axisID);
      }
      else {
        processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        //  Go to the coarse align dialog by default
        mainFrame.showBlankProcess(axisID);
      }
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      preProcDialogB = null;
    }
    else {
      preProcDialogA = null;
    }
    preProcDialog = null;
  }

  /**
   * Open imod to create the erase model
   */
  public void imodErase(AxisID axisID) {
    String eraseModelName =
      metaData.getFilesetName() + axisID.getExtension() + ".erase";
    try {
      imodManager.modelRawStack(eraseModelName, axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "Can't open imod on raw stack");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "Axis type problem in imod erase");
    }
  }

  /**
   * Get the eraser{|a|b}.com script
   */
  private void updateEraserCom(AxisID axisID) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }

    //  Get the user input data from the dialog box.  The CCDEraserParam
    //  is first initialized from the currently loaded com script to
    //  provide deafault values for those not handled by the dialog box
    //  get function needs some error checking
    CCDEraserParam ccdEraserParam = new CCDEraserParam();
    ccdEraserParam = comScriptMgr.getCCDEraserParam(axisID);
    preProcDialog.getCCDEraserParams(ccdEraserParam);
    comScriptMgr.saveEraserCom(ccdEraserParam, axisID);

  }

  /**
   * Run the eraser script
   */
  public void eraser(AxisID axisID) {
    updateEraserCom(axisID);
    String threadName = processMgr.eraser(axisID);
    setThreadName(threadName, axisID);
    mainFrame.startProgressBar("Erasing pixels", axisID);
  }

  /**
   * Open the coarse alignment dialog
   */
  public void openCoarseAlignDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    if (showIfExists(coarseAlignDialogA, coarseAlignDialogB, axisID)) {
      return;
    }

    CoarseAlignDialog coarseAlignDialog = new CoarseAlignDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = coarseAlignDialog;
    }
    else {
      coarseAlignDialogA = coarseAlignDialog;
    }

    //  Create the dialog box
    comScriptMgr.loadXcorrCom(axisID);
    coarseAlignDialog.setCrossCorrelationParams(
      comScriptMgr.getTiltxcorrParam(axisID));
    mainFrame.showProcess(coarseAlignDialog.getContainer(), axisID);
  }

  /**
   *  Get the parameters from the coarse align process dialog box
   */
  public void doneCoarseAlignDialog(AxisID axisID) {
    //  Set a reference to the correct object
    CoarseAlignDialog coarseAlignDialog;
    if (axisID == AxisID.SECOND) {
      coarseAlignDialog = coarseAlignDialogB;
    }
    else {
      coarseAlignDialog = coarseAlignDialogA;
    }

    if (coarseAlignDialog == null) {
      openMessageDialog(
        "Can not update coarse align without an active coarse align dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = coarseAlignDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateXcorrCom(axisID)) {
        return;
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setCoarseAlignmentState(ProcessState.COMPLETE, axisID);
        mainFrame.setCoarseAlignState(ProcessState.COMPLETE, axisID);
        //  Go to the fiducial model dialog by default
        openFiducialModelDialog(axisID);
      }
      else {
        processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
        mainFrame.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = null;
    }
    else {
      coarseAlignDialogA = null;
    }
    coarseAlignDialog = null;
  }

  /**
   * Get the parameters from dialog box and run the cross correlation script
   */
  public void crossCorrelate(AxisID axisID) {

    // Get the parameters from the dialog box
    if (updateXcorrCom(axisID)) {
      String threadName = processMgr.crossCorrelate(axisID);
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Cross-correlating raw stack", axisID);
    }
  }

  /**
   * Run the coarse alignment script
   */
  public void coarseAlign(AxisID axisID) {
    String threadName = processMgr.coarseAlign(axisID);
    setThreadName(threadName, axisID);
    mainFrame.startProgressBar("Creating coarse stack", axisID);
  }

  /**
   * Open imod to view the coarsely aligned stack
   */
  public void imodAlign(AxisID axisID) {
    try {
      imodManager.openCoarseAligned(axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "Problem opening coarse stack");
    }
  }

  /**
   * Run midas on the raw stack
   */
  public void midasRawStack(AxisID axisID) {
    processMgr.midasRawStack(axisID);
  }

  /**
   * Get the required parameters from the dialog box and update the xcorr.com
   * script
   * @return true if successful in getting the parameters and saving the com
   * script
   */
  private boolean updateXcorrCom(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog;
    if (axisID == AxisID.SECOND) {
      coarseAlignDialog = coarseAlignDialogB;
    }
    else {
      coarseAlignDialog = coarseAlignDialogA;
    }

    try {
      TiltxcorrParam tiltXcorrParam = comScriptMgr.getTiltxcorrParam(axisID);
      coarseAlignDialog.getCrossCorrelationParams(tiltXcorrParam);
      comScriptMgr.saveXcorrCom(tiltXcorrParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Align Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Open the fiducial model generation dialog
   */
  public void openFiducialModelDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    if (showIfExists(fiducialModelDialogA, fiducialModelDialogB, axisID)) {
      return;
    }

    // Create a new dialog panel and map it the generic reference
    FiducialModelDialog fiducialModelDialog =
      new FiducialModelDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = fiducialModelDialog;
    }
    else {
      fiducialModelDialogA = fiducialModelDialog;
    }

    //  Load the required track{|a|b}.com files, fill in the dialog box params
    //  and set it to the appropriate state
    comScriptMgr.loadTrackCom(axisID);
    fiducialModelDialog.setBeadtrackParams(
      comScriptMgr.getBeadtrackParam(axisID));
    mainFrame.showProcess(fiducialModelDialog.getContainer(), axisID);
  }

  /**
   * 
   */
  public void doneFiducialModelDialog(AxisID axisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }

    if (fiducialModelDialog == null) {
      openMessageDialog(
        "Can not update fiducial model without an active fiducial model dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = fiducialModelDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateTrackCom(axisID)) {
        return;
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setFiducialModelState(ProcessState.COMPLETE, axisID);
        mainFrame.setFiducialModelState(ProcessState.COMPLETE, axisID);
        openFineAlignmentDialog(axisID);
      }
      else {
        processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
        mainFrame.setFiducialModelState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = null;
    }
    else {
      fiducialModelDialogA = null;
    }
    fiducialModelDialog = null;
  }

  /**
   * Open imod with the seed model
   */
  public void imodSeedFiducials(AxisID axisID) {
    String seedModel =
      metaData.getFilesetName() + axisID.getExtension() + ".seed";
    try {
      imodManager.modelCoarseAligned(seedModel, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(
        except.getMessage(),
        "Can't open imod on coarse aligned stack with model: " + seedModel);
    }
  }

  /**
   * Get the beadtrack parameters from the fiducial model dialog and run the
   * track com script
   */
  public void fiducialModelTrack(AxisID axisID) {
    if (updateTrackCom(axisID)) {
      String threadName = processMgr.fiducialModelTrack(axisID);
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Tracking fiducials", axisID);
    }
  }

  /**
   * Open imod with the new fidcuial model
   */
  public void imodFixFiducials(AxisID axisID) {
    String fiducialModel =
      metaData.getFilesetName() + axisID.getExtension() + ".fid";
    try {
      imodManager.modelCoarseAligned(fiducialModel, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(
        except.getMessage(),
        "Can't open imod on coarse aligned stack with model: " + fiducialModel);
    }
  }

  /**
   * Update the specified track com script
   */
  private boolean updateTrackCom(AxisID axisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }

    if (fiducialModelDialog == null) {
      openMessageDialog(
        "Can not update track?.com without an active fiducial model dialog",
        "Program logic error");
      return false;
    }

    try {
      BeadtrackParam beadtrackParam = comScriptMgr.getBeadtrackParam(axisID);
      fiducialModelDialog.getBeadtrackParams(beadtrackParam);
      comScriptMgr.saveTrackCom(beadtrackParam, axisID);

    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      openMessageDialog(errorMessage, "Beadtrack Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Beadtrack Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Open the alignment estimation dialog
   */
  public void openFineAlignmentDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    if (showIfExists(fineAlignmentDialogA, fineAlignmentDialogB, axisID)) {
      return;
    }

    // Create a new dialog panel and map it the generic reference
    AlignmentEstimationDialog fineAlignmentDialog =
      new AlignmentEstimationDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = fineAlignmentDialog;
    }
    else {
      fineAlignmentDialogA = fineAlignmentDialog;
    }

    //  Load the required align{|a|b}.com files, fill in the dialog box params
    //  and set it to the appropriate state
    comScriptMgr.loadAlignCom(axisID);
    fineAlignmentDialog.setTiltalignParams(
      comScriptMgr.getTiltalignParam(axisID));

    //  Create a default transferfid object to populate the alignment dialog
    fineAlignmentDialog.setTransferFidParams(new TransferfidParam());
    mainFrame.showProcess(fineAlignmentDialog.getContainer(), axisID);
  }

  /**
   *
   */
  public void doneAlignmentEstimationDialog(AxisID axisID) {
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }

    if (fineAlignmentDialog == null) {
      openMessageDialog(
        "Can not update align?.com without an active alignment dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = fineAlignmentDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateAlignCom(axisID)) {
        return;
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainFrame.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
      else {
        processTrack.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        mainFrame.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        openTomogramPositioningDialog(axisID);

        // Check to see if the user wants to keep any coarse aligned imods
        // open
        try {
          if (imodManager.isCoarseAlignedOpen(AxisID.FIRST)
            || imodManager.isCoarseAlignedOpen(AxisID.SECOND)) {
            String[] message = new String[2];
            message[0] = "There are coarsely aligned stack imod processes open";
            message[1] = "Should they be closed?";
            boolean answer = openYesNoDialog(message);
            if (answer) {
              if (isDualAxis()) {
                imodManager.quitCoarseAligned(AxisID.FIRST);
                imodManager.quitCoarseAligned(AxisID.SECOND);
              }
              else {
                imodManager.quitCoarseAligned(AxisID.ONLY);
              }
            }
          }
        }
        catch (AxisTypeException except) {
          except.printStackTrace();
          openMessageDialog(except.getMessage(), "AxisType problem");
        }
        catch (SystemProcessException except) {
          except.printStackTrace();
          openMessageDialog(
            except.getMessage(),
            "Problem closing coarse stack");
        }
      }
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = null;
    }
    else {
      fineAlignmentDialogA = null;
    }
    fineAlignmentDialog = null;
  }

  /**
   * Execute the fine alignment script (align.com) for the appropriate axis
   * @param the AxisID identifying the axis to align.
   */
  public void fineAlignment(AxisID axisID) {
    if (!updateAlignCom(axisID)) {
      return;
    }
    String threadName = processMgr.fineAlignment(axisID);
    setThreadName(threadName, axisID);
    mainFrame.startProgressBar("Aligning stack", axisID);
  }

  /**
   * Open imod with the new fidcuial model
   */
  public void imodViewResiduals(AxisID axisID) {
    String fiducialModel =
      metaData.getFilesetName() + axisID.getExtension() + ".resmod";
    try {
      imodManager.modelCoarseAligned(fiducialModel, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(
        except.getMessage(),
        "Can't open imod on coarse aligned stack with model: " + fiducialModel);
    }
  }

  /**
   * Open imodv with the new fidcuial model
   */
  public void imodView3DModel(AxisID axisID) {
    String fiducialModel =
      metaData.getFilesetName() + axisID.getExtension() + ".3dmod";
    imodManager.openFiducialModel(fiducialModel, axisID);
  }

  /**
   * Open imod to view the coarsely aligned stack
   * @param axisID the AxisID to coarse align.
   */
  public void imodFineAlign(AxisID axisID) {
    try {
      imodManager.openFineAligned(axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(
        except.getMessage(),
        "Can't open imod on fine aligned stack");
    }

  }

  /**
   * Run transferfid
   */
  public void transferfid(AxisID sourceAxisID) {
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (sourceAxisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }

    if (fineAlignmentDialog != null) {
      TransferfidParam transferfidParam = new TransferfidParam();
      // Setup the default parameters depending upon the axis to transfer the
      // fiducials from
      String fileSetName = metaData.getFilesetName();
      transferfidParam.setFileSetName(fileSetName);
      if (sourceAxisID == AxisID.SECOND) {
        transferfidParam.setBToA(true);
      }

      // Get any user specified changes
      fineAlignmentDialog.getTransferFidParams(transferfidParam);
      String threadName = processMgr.transferFiducials(transferfidParam);
      setThreadName(threadName, sourceAxisID);
      mainFrame.startProgressBar("Transfering fiducuals", sourceAxisID);
    }
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters from
   * the alignment estimation dialog.  This also updates the local alignment
   * state of the appropriate tilt files.
   */
  private boolean updateAlignCom(AxisID axisID) {
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }

    if (fineAlignmentDialog == null) {
      openMessageDialog(
        "Can not update align?.com without an active alignment dialog",
        "Program logic error");
      return false;
    }

    TiltalignParam tiltalignParam;

    try {
      tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      fineAlignmentDialog.getTiltalignParams(tiltalignParam);
      comScriptMgr.saveAlignCom(tiltalignParam, axisID);
      //  Update the tilt.com script with the dependent parameters
      updateTiltDependsOnAlign(tiltalignParam, axisID);

      mainFrame.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getNewString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tiltalign Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      openMessageDialog(errorMessage, "Tiltalign Parameter Syntax Error");
      return false;
    }

    return true;
  }

  /**
   * Update the tilt parameters dependent on the align script
   * - local alignments file
   * - the exclude list
   */
  private void updateTiltDependsOnAlign(
    ConstTiltalignParam tiltalignParam,
    AxisID currentAxis) {

    comScriptMgr.loadTiltCom(currentAxis);
    TiltParam tiltParam = comScriptMgr.getTiltParam(currentAxis);

    String alignFileExtension = currentAxis.getExtension() + "local.xf";

    if (tiltalignParam.getLocalAlignments()) {
      tiltParam.setLocalAlignFile(getFilesetName() + alignFileExtension);
    }
    else {
      tiltParam.setLocalAlignFile("");
    }

    tiltParam.setExcludeList(tiltalignParam.getIncludeExcludeList());
    comScriptMgr.saveTiltCom(tiltParam, currentAxis);
  }

  /**
   * Open the tomogram positioning dialog
   */
  public void openTomogramPositioningDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    if (showIfExists(tomogramPositioningDialogA,
      tomogramPositioningDialogB,
      axisID)) {
      return;
    }

    // Create a new dialog panel and map it the generic reference
    TomogramPositioningDialog tomogramPositioningDialog =
      new TomogramPositioningDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialogB = tomogramPositioningDialog;
    }
    else {
      tomogramPositioningDialogA = tomogramPositioningDialog;
    }

    // Get the tilt{|a|b}.com and align{|a|b}.com parameters
    comScriptMgr.loadTiltCom(axisID);
    tomogramPositioningDialog.setTiltParams(comScriptMgr.getTiltParam(axisID));

    comScriptMgr.loadAlignCom(axisID);
    tomogramPositioningDialog.setAlignParams(
      comScriptMgr.getTiltalignParam(axisID));

    // Open the dialog panel
    mainFrame.showProcess(tomogramPositioningDialog.getContainer(), axisID);
  }

  /**
   * 
   */
  public void doneTomogramPositioningDialog(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog;
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialog = tomogramPositioningDialogB;
    }
    else {
      tomogramPositioningDialog = tomogramPositioningDialogA;
    }

    if (tomogramPositioningDialog == null) {
      openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramPositioningDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      boolean tiltFinished = updateSampleTiltCom(axisID);
      boolean alignFinished = updateAlignCom(tomogramPositioningDialog, axisID);
      if (!(tiltFinished & alignFinished)) {
        return;
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramPositioningState(
          ProcessState.INPROGRESS,
          axisID);
        mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
      else {
        processTrack.setTomogramPositioningState(ProcessState.COMPLETE, axisID);
        mainFrame.setTomogramPositioningState(ProcessState.COMPLETE, axisID);
        openTomogramGenerationDialog(axisID);

        try {
          if (imodManager.isSampleOpen(AxisID.FIRST)
            || imodManager.isSampleOpen(AxisID.SECOND)) {
            String[] message = new String[2];
            message[0] = "There are sample reconstruction imod processes open";
            message[1] = "Should they be closed?";
            boolean answer = openYesNoDialog(message);
            if (answer) {
              if (isDualAxis()) {
                imodManager.quitSample(AxisID.FIRST);
                imodManager.quitSample(AxisID.SECOND);
              }
              else {
                imodManager.quitSample(AxisID.FIRST);
              }
            }
          }
        }
        catch (AxisTypeException except) {
          except.printStackTrace();
          openMessageDialog(except.getMessage(), "AxisType problem");
        }
        catch (SystemProcessException except) {
          except.printStackTrace();
          openMessageDialog(
            except.getMessage(),
            "Problem closing sample reconstruction");
        }
      }
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialogB = null;
    }
    else {
      tomogramPositioningDialogA = null;
    }
    tomogramPositioningDialog = null;
  }

  /**
   * Run the sample com script
   */
  public void createSample(AxisID axisID) {
    //  Get the user input data from the dialog box
    if (updateSampleTiltCom(axisID)) {
      String threadName = processMgr.createSample(axisID);
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Creating sample tomogram", axisID);
    }
  }

  /**
   * 
   */
  public void imodSample(AxisID axisID) {
    try {
      imodManager.openSample(axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(
        except.getMessage(),
        "Problem opening sample reconstruction");
    }

  }

  /**
   * 
   */
  public void tomopitch(AxisID axisID) {
    String threadName = processMgr.tomopitch(axisID);
    setThreadName(threadName, axisID);
    mainFrame.startProgressBar("Finding sample position", axisID);
  }

  /**
   * 
   */
  public void finalAlign(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog;
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialog = tomogramPositioningDialogB;
    }
    else {
      tomogramPositioningDialog = tomogramPositioningDialogA;
    }

    if (updateAlignCom(tomogramPositioningDialog, axisID)) {
      String threadName = processMgr.fineAlignment(axisID);
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Calculating final alignment", axisID);
    }
  }

  /**
   * Update the tilt{|a|b}.com file with sample parameters for the specified
   * axis
   */
  private boolean updateSampleTiltCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog;
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialog = tomogramPositioningDialogB;
    }
    else {
      tomogramPositioningDialog = tomogramPositioningDialogA;
    }

    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return false;
    }

    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    try {
      TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
      tomogramPositioningDialog.getTiltParams(tiltParam);
      comScriptMgr.saveTiltCom(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tilt Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters from
   * the tomogram positioning dialog.
   */
  private boolean updateAlignCom(
    TomogramPositioningDialog tomogramPositioningDialog,
    AxisID axisID) {

    TiltalignParam tiltalignParam;
    try {
      tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      tomogramPositioningDialog.getAlignParams(tiltalignParam);
      comScriptMgr.saveAlignCom(tiltalignParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tiltalign Parameter Syntax Error");
      return false;
    }

    return true;
  }

  /**
   * Open the tomogram generation dialog
   */
  public void openTomogramGenerationDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    if (showIfExists(tomogramGenerationDialogA,
      tomogramGenerationDialogB,
      axisID)) {
      return;
    }

    // Create a new dialog panel and map it the generic reference
    TomogramGenerationDialog tomogramGenerationDialog =
      new TomogramGenerationDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialogB = tomogramGenerationDialog;
    }
    else {
      tomogramGenerationDialogA = tomogramGenerationDialog;
    }

    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTiltCom(axisID);
    tomogramGenerationDialog.setTiltParams(comScriptMgr.getTiltParam(axisID));

    mainFrame.showProcess(tomogramGenerationDialog.getContainer(), axisID);
  }

  /**
   * 
   */
  public void doneTomogramGenerationDialog(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog;
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialog = tomogramGenerationDialogB;
    }
    else {
      tomogramGenerationDialog = tomogramGenerationDialogA;
    }

    if (tomogramGenerationDialog == null) {
      openMessageDialog(
        "Can not update tilt?.com without an active tomogram generation dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramGenerationDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateTiltCom(axisID)) {
        return;
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramGenerationState(
          ProcessState.INPROGRESS,
          axisID);
        mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
      else {
        processTrack.setTomogramGenerationState(ProcessState.COMPLETE, axisID);
        mainFrame.setTomogramGenerationState(ProcessState.COMPLETE, axisID);
        if (isDualAxis()) {
          openTomogramCombinationDialog();
          if(axisID == AxisID.SECOND) {
            mainFrame.showBlankProcess(axisID);
          }
        }
        else {
          openPostProcessingDialog();
        }
      }
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialogB = null;
    }
    else {
      tomogramGenerationDialogA = null;
    }
    tomogramGenerationDialog = null;
  }

  /**
   *
   */
  private boolean updateTiltCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog;
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialog = tomogramGenerationDialogB;
    }
    else {
      tomogramGenerationDialog = tomogramGenerationDialogA;
    }

    if (tomogramGenerationDialog == null) {
      openMessageDialog(
        "Can not update tilt?.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }

    try {
      TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
      tomogramGenerationDialog.getTiltParams(tiltParam);
      comScriptMgr.saveTiltCom(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tilt Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   *
   */
  public void newst(AxisID axisID) {
    // FIXME  how should Generation be modified

    try {
      NewstParam newstParam;
      comScriptMgr.loadNewstCom(axisID);
      newstParam = comScriptMgr.getNewstComNewstParam(axisID);
      newstParam.setSize(",,");
      comScriptMgr.saveNewstCom(newstParam, axisID);

      String threadName = processMgr.newst(axisID);
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Creating final stack", axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
    }
  }

  /**
   * 
   */
  public void tilt(AxisID axisID) {
    if (updateTiltCom(axisID)) {
      String threadName = processMgr.tilt(axisID);
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Calculating tomogram", axisID);
    }
  }

  /**
   * Open imod to view the coarsely aligned stack
   * @param axisID the AxisID of the tomogram to open.
   */
  public void imodTomogram(AxisID axisID) {
    try {
      imodManager.openTomogram(axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      openMessageDialog(
        except.getMessage(),
        "Can't open imod with the tomogram");
    }

  }

  /**
   * Open the tomogram combination dialog
   */
  public void openTomogramCombinationDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    // Verify that this process is applicable
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      openMessageDialog(
        "This step is valid only for a dual axis tomogram",
        "Invalid tomogram combination selection");
      return;
    }

    if (tomogramCombinationDialog == null) {
      tomogramCombinationDialog = new TomogramCombinationDialog(this);
    }

   CombineParams combineParams =
      new CombineParams(metaData.getCombineParams());

   if (!combineParams.isPatchBoundarySet()) {
      String recFileName;
      if (combineParams.getMatchBtoA()) {
        recFileName = metaData.getFilesetName() + "a.rec";
      }
      else {
        recFileName = metaData.getFilesetName() + "b.rec";
        }
        try {
          combineParams.setDefaultPatchBoundaries(recFileName);
        }
        catch (InvalidParameterException except) {
          String[] detailedMessage = new String[4];
          detailedMessage[0] = "Unable to set default patch boundaries";
          detailedMessage[1] = "Are both tomograms computed and available?";
          detailedMessage[2] = "";
          detailedMessage[3] = except.getMessage();

          openMessageDialog(
            detailedMessage,
            "Invalid parameter: " + recFileName);
          //Close the dialog
          return;
        }
        catch (IOException except) {
          except.printStackTrace();
          openMessageDialog(except.getMessage(), "IO Error: " + recFileName);
          //Close the dialog
          return;
        }
      }

      // Fill in the dialog box params and set it to the appropriate state
      tomogramCombinationDialog.setCombineParams(combineParams);

    mainFrame.showProcess(
      tomogramCombinationDialog.getContainer(),
      AxisID.FIRST);
  }

  /**
   * 
   */
  public void doneTomogramCombinationDialog() {
    if (tomogramCombinationDialog == null) {
      openMessageDialog(
        "Can not update combine.com without an active tomogram combination dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramCombinationDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(AxisID.ONLY);
    }
    else {
      //  Get the user input data from the dialog box
      if (updateCombineCom()) {
        if (exitState == DialogExitState.POSTPONE) {
          processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
          mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
          mainFrame.showBlankProcess(AxisID.ONLY);
        }
        else {
          combine();
          processTrack.setTomogramCombinationState(ProcessState.COMPLETE);
          mainFrame.setTomogramCombinationState(ProcessState.COMPLETE);
          //  FIXME open up combine control panel
          openPostProcessingDialog();
        }
      }
    }
  }

  /**
   * Combine the two tomograms
   */
  public void combine() {
      String threadName = processMgr.combine();   
      setThreadName(threadName, AxisID.FIRST);
      mainFrame.startProgressBar("Combining tomograms", AxisID.FIRST);
  }
  
  /**
   * Update the combine parameters from the calling dialog
   * @param tomogramCombinationDialog the calling dialog.
   * @return true if the combine parameters are valid false otherwise.  If the
   * combine parameters are invalid a message dialog describing the invalid
   * parameters is presented to the user.
   */
  private boolean updateCombineCom() {
    if (tomogramCombinationDialog == null) {
      openMessageDialog(
        "Can not update combine.com without an active tomogram combination dialog",
        "Program logic error");
      return false;
    }
    CombineParams combineParams = new CombineParams();
    try {
      tomogramCombinationDialog.getCombineParams(combineParams);
      if (!combineParams.isValid()) {
        openMessageDialog(
          combineParams.getInvalidReasons(),
          "Invlaid combine parameters");
        return false;
      }

    }
    catch (NumberFormatException except) {
      openMessageDialog(except.getMessage(), "Number format error");
      return false;
    }

    metaData.setCombineParams(combineParams);
    return true;
  }

  /**
   * Run the setupcombine script with the current combine parameters stored in
   * metaData object.  updateCombineCom is called first to get the currect
   * parameters from the dialog.
   * @param tomogramCombinationDialog the calling dialog.
   */
  public void createCombineScripts() {
    if (!updateCombineCom()) {
      return;
    }

    try {
      processMgr.setupCombineScripts(metaData);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      openMessageDialog(except.getMessage(), "Can't run setupcombine");
    }

    catch (IOException except) {
      except.printStackTrace();
      openMessageDialog(
        "Can't run setupcombine\n" + except.getMessage(),
        "Setupcombine IOException");
    }
  }

  /**
   * Open the post processing dialog
   */
  public void openPostProcessingDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    PostProcessingDialog postProcessingDialog = new PostProcessingDialog(this);

    DialogExitState exitState = postProcessingDialog.getExitState();

    if (exitState == DialogExitState.POSTPONE
      || exitState == DialogExitState.EXECUTE) {
      //
      //  Get the user input data from the dialog box
      //
      //FIXME      mainFrame.setPostProcessingState(ProcessState.INPROGRESS);
    }
    if (exitState == DialogExitState.EXECUTE) {
      //FIXME      mainFrame.setPostProcessingState(ProcessState.COMPLETE);
    }

  }

  //
  //  Utility functions
  //

  private boolean showIfExists(
    ProcessDialog panelA,
    ProcessDialog panelB,
    AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (panelB == null) {
        return false;
      }
      else {
        mainFrame.showProcess(panelB.getContainer(), axisID);
        return true;
      }
    }
    else {
      if (panelA == null) {
        return false;
      }
      else {
        mainFrame.showProcess(panelA.getContainer(), axisID);
        return true;
      }
    }
  }

  /**
   * Check if the current data set is a dual axis data set
   * @return true if the data set is a dual axis data set
   */
  public boolean isDualAxis() {
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      return false;
    }
    else {
      return true;
    }
  }

  /**
   * Get the current advanced state
   */

  public boolean getAdvanced() {
    return isAdvanced;
  }

  /**
   * 
   */
  public void setAdvanced(boolean state) {
    isAdvanced = state;
  }

  /**
   * 
   */
  public void packMainWindow() {
    mainFrame.repaint();
  }

  /**
   * Return the fileset name.  This is the basename of the raw image stack and
   * the name used for the base of all intermediate and final data files.
   * @return a string containing the fileset name.
   */
  public String getFilesetName() {
    return metaData.getFilesetName();
  }

  /**
   * Return the current working directory.
   * @return a string containing the current working directory.
   */
  public String getWorkingDirectory() {
    return metaData.getWorkingDirectory();
  }

  /**
   * A message asking the ApplicationManager to load in the information from the
   * test parameter file.
   * @param paramFile the File object specifiying the data parameter file.
   */
  public boolean openTestParamFile(File paramFile) {
    FileInputStream processDataStream;

    try {
      // Read in the test parameter data file
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[2];
      storable[0] = metaData;
      storable[1] = processTrack;
      paramStore.load(storable);
      setTestParamFile(paramFile);

      //  Update the MRU test data filename list
      userConfig.putDataFile(paramFile.getAbsolutePath());
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());

      //  Set the current working directory for the application
      if (metaData.getWorkingDirectory().length() > 0) {
        System.setProperty("user.dir", getWorkingDirectory());
      }

      //  Initialize a new IMOD manager
      imodManager = new ImodManager(metaData);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not find the test parameter data file:";
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "File not found error");
      return false;
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not read the test parameter data from file:";
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Test parameter file read error");
      return false;
    }
    return true;
  }

  /**
   * A message asking the ApplicationManager to save the test parameter
   * information to a file.
   */
  public void saveTestParamFile() {
    try {
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[2];
      storable[0] = metaData;
      storable[1] = processTrack;
      paramStore.save(storable);

      //  Update the MRU test data filename list
      userConfig.putDataFile(paramFile.getAbsolutePath());
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());

      // Reset the process track flag
      processTrack.resetModified();
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file save error";
      errorMessage[1] = "Could not save test parameter data to file:";
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Test parameter file save error");
    }
  }

  /**
   * Return the test parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  public File getTestParamFile() {
    return paramFile;
  }

  /**
   * Set the data set parameter file.  This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setTestParamFile(File paramFile) {
    this.paramFile = paramFile;

    //  Update main window information and status bar
    mainFrame.updateDataParameters(metaData);
  }

  /**
   * Open up the settings dialog box
   */
  public void openSettingsDialog() {

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (settingsDialog == null) {
      settingsDialog = new SettingsDialog(this);
      settingsDialog.setParameters(userConfig);
      Dimension frmSize = mainFrame.getSize();
      Point loc = mainFrame.getLocation();
      settingsDialog.setLocation(loc.x, loc.y + frmSize.height);
      settingsDialog.setModal(false);
    }
    settingsDialog.show();
  }

  /**
   * 
   */
  public void getSettingsParameters() {
    if (settingsDialog != null) {
      settingsDialog.getParameters(userConfig);
      setUserPreferences();

      mainFrame.repaintWindow();

    }
  }

  /**
   * 
   */
  public void closeSettingsDialog() {
    if (settingsDialog != null) {
      settingsDialog.dispose();
    }
  }

  /**
   * 
   */
  private void setupRequestDialog() {
    String[] message = new String[2];
    message[0] = "The setup process has not been completed";
    message[1] =
      "Complete the Setup process before opening other process dialogs";
    openMessageDialog(message, "Program Operation Error");
    return;
  }

  /**
   * 
   */
  private String initProgram(String[] args) {
    //  Parse the command line
    String testParamFilename = parseCommandLine(args);

    // Get the HOME directory environment variable to find the program
    // configuration file
    homeDirectory = getEnvironmentVariable("HOME");
    if (homeDirectory == "") {
      String[] message = new String[2];
      message[0] =
        "Can not find home directory! Unable to load user preferences";
      message[1] =
        "Set HOME environment variable and restart program to fix this problem";
      openMessageDialog(message, "Program Initialization Error");
      System.exit(1);
    }

    //  Set the user.dir system property to the current working dirctory
    String workingDirectory = getEnvironmentVariable("PWD");
    if (workingDirectory == "") {
      String[] message = new String[2];
      message[0] = "Can not find current working directory!";
      message[1] = "Home directory will be the starting point for file opens.";
      openMessageDialog(message, "Program Initialization Error");
    }
    else {
      // Set both the metadata object for persistence and the user.dir
      // property for objects that can't directly access this object
      metaData.setWorkingDirectory(workingDirectory);
      System.setProperty("user.dir", workingDirectory);
    }

    // Get the IMOD directory so we know which program to run
    IMODDirectory = getEnvironmentVariable("IMOD_DIR");
    if (homeDirectory == "") {
      String[] message = new String[2];
      message[0] = "Can not find IMOD directory! Unable to run programs";
      message[1] =
        "Set IMOD_DIR environment variable and restart program to fix this problem";
      openMessageDialog(message, "Program Initialization Error");
      System.exit(1);
    }

    //  Create a File object specifying the user configuration file
    File userConfigFile = new File(homeDirectory, ".etomo");

    //  Make sure the config file exists, create it if it doesn't
    try {
      userConfigFile.createNewFile();
    }
    catch (IOException except) {
      System.err.println(
        "Could not create file:" + userConfigFile.getAbsolutePath());
      System.err.println(except.getMessage());
      return "";
    }

    // Load in the user configuration
    ParameterStore userParams = new ParameterStore(userConfigFile);
    Storable storable[] = new Storable[1];
    storable[0] = userConfig;
    try {
      userParams.load(storable);
    }
    catch (IOException except) {
      openMessageDialog(
        except.getMessage(),
        "IO Exception: Can't load user configuration"
          + userConfigFile.getAbsolutePath());
    }

    //  Set the user preferences
    setUserPreferences();

    return testParamFilename;
  }

  /**
   * Set the user preferences
   */
  private void setUserPreferences() {
    ToolTipManager.sharedInstance().setInitialDelay(
      userConfig.getToolTipsInitialDelay());
    ToolTipManager.sharedInstance().setDismissDelay(
      userConfig.getToolTipsDismissDelay());
    setUIFont(userConfig.getFontFamily(), userConfig.getFontSize());
    setLookAndFeel(userConfig.getNativeLookAndFeel());
    isAdvanced = userConfig.getAdvancedDialogs();
  }

  /**
   * Parse the command line.  This method will return a non-empty string if
   * there is a etomo data .
   * @param The command line arguments
   * @return A string that will be set to the etomo data filename if one is
   * found on the command line otherwise it is "".
   */
  private String parseCommandLine(String[] args) {
    String testParamFilename = "";

    //  Parse the command line arguments
    for (int i = 0; i < args.length; i++) {

      // Filename argument should be the only one not beginning with at least
      // one dash
      if (!args[i].startsWith("-")) {
        testParamFilename = args[i];
      }

      if (args[i].equals("--demo")) {
        demo = true;
      }
    }
    return testParamFilename;
  }

  /**
   * Exit the program
   */
  public boolean exitProgram() {

    if (isDataParamDirty || processTrack.isModified()) {
      int returnValue =
        JOptionPane.showConfirmDialog(
          mainFrame,
          "Save the current data file ?",
          "Save data file?",
          JOptionPane.YES_NO_CANCEL_OPTION);

      if (returnValue == JOptionPane.CANCEL_OPTION) {
        return false;
      }
      if (returnValue == JOptionPane.YES_OPTION) {
        if (paramFile == null) {
          if (!mainFrame.getTestParamFilename()) {
            return false;
          }
        }
        saveTestParamFile();
      }
    }

    //  Get the current window size
    Dimension size = mainFrame.getSize();
    userConfig.setMainWindowWidth(size.width);
    userConfig.setMainWindowHeight(size.height);
    
    //  Write out the user configuration data
    File userConfigFile = new File(homeDirectory, ".etomo");

    //  Make sure the config file exists, create it if it doesn't
    try {
      userConfigFile.createNewFile();
    }
    catch (IOException except) {
      System.err.println(
        "IOException: Could not create file:"
          + userConfigFile.getAbsolutePath()
          + "\n"
          + except.getMessage());
      System.err.println(except.getMessage());
      return true;
    }

    ParameterStore userParams = new ParameterStore(userConfigFile);
    Storable storable[] = new Storable[1];
    storable[0] = userConfig;
    if (!userConfigFile.canWrite()) {
      openMessageDialog(
        "Change permissions of $HOME/.etomo to allow writing",
        "Unable to save user configuration file");
    }

    if (userConfigFile.canWrite()) {
      try {
        userParams.save(storable);
      }
      catch (IOException excep) {
        excep.printStackTrace();
        openMessageDialog(
          "IOException: unable to save user parameters\n" + excep.getMessage(),
          "Unable to save user parameters");
      }
    }
    return true;
  }

  /**
   * Sets the look and feel for the program.
   * @param nativeLookAndFeel set to true to use the host os look and feel,
   * false will use the Metal look and feel.
   */
  private static void setLookAndFeel(boolean nativeLookAndFeel) {
    String lookAndFeelClassName;

    //UIManager.LookAndFeelInfo plaf[] = UIManager.getInstalledLookAndFeels();
    //for(int i = 0; i < plaf.length; i++) {
    //  System.err.println(plaf[i].getClassName());
    //}
    if (nativeLookAndFeel) {
      lookAndFeelClassName = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
    }
    else {
      lookAndFeelClassName = UIManager.getCrossPlatformLookAndFeelClassName();
    }

    try {
      UIManager.setLookAndFeel(lookAndFeelClassName);
    }
    catch (Exception excep) {
      System.err.println(
        "Could not set " + lookAndFeelClassName + " look and feel");
    }
  }

  /**
   * 
   */
  public static void setUIFont(String fontFamily, int fontSize) {

    // sets the default font for all Swing components.
    // ex. 
    //  setUIFont (new javax.swing.plaf.FontUIResource("Serif",Font.ITALIC,12));
    // Taken from: http://www.rgagnon.com/javadetails/java-0335.html
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      if (value instanceof FontUIResource) {
        FontUIResource currentFont = (FontUIResource) value;
        FontUIResource newFont =
          new FontUIResource(fontFamily, currentFont.getStyle(), fontSize);
        UIManager.put(key, newFont);
      }
    }
  }

  /**
   * Return the IMOD directory
   */
  public String getIMODDirectory() {
    return IMODDirectory;
  }

  /**
   * Return the users home directory environment variable HOME or an empty
   * string if it doesn't exist.
   */
  private String getHomeDirectory() {
    return homeDirectory;
  }

  /**
   * 
   */
  private String getEnvironmentVariable(String varName) {
    //  There is not a real good way to access the system environment variables
    //  since the primary method was deprecated
    SystemProgram echoHome = new SystemProgram("env");
    try {
      echoHome.enableDebug(debug);
      echoHome.run();
    }
    catch (Exception excep) {
      excep.printStackTrace();
      System.err.println(excep.getMessage());
      System.err.println(
        "Unable to run env command to find "
          + varName
          + " environment variable");

      return "";
    }
    String[] stderr = echoHome.getStdError();
    if (stderr.length > 0) {
      System.err.println("Error running 'env' command");
      for (int i = 0; i < stderr.length; i++) {
        System.err.println(stderr[i]);
      }
    }

    // Search through the evironment string array to find the request
    // environment variable
    String searchString = varName + "=";
    int nChar = searchString.length();
    String[] stdout = echoHome.getStdOutput();
    for (int i = 0; i < stdout.length; i++) {
      if (stdout[i].indexOf(searchString) == 0) {
        return stdout[i].substring(nChar);
      }
    }
    return "";
  }

  /**
   * 
   */
  private boolean openYesNoDialog(String[] message) {
    try {
      int answer =
        JOptionPane.showConfirmDialog(
          mainFrame,
          message,
          "Etomo question",
          JOptionPane.YES_NO_OPTION);

      if (answer == JOptionPane.YES_OPTION) {
        return true;
      }
      else {
        return false;
      }
    }
    catch (HeadlessException except) {
      except.printStackTrace();
      return false;
    }
  }

  /**
   * 
   */
  public void openMessageDialog(Object message, String title) {
    JOptionPane.showMessageDialog(
      mainFrame,
      message,
      title,
      JOptionPane.ERROR_MESSAGE);
  }

  /**
   * Returns the debug state.
   * @return boolean
   */
  public boolean isDebug() {
    return debug;
  }

  /**
   * Returns the demo state.
   * @return boolean
   */
  public boolean isDemo() {
    return demo;
  }

  /**
   * Run the specified command as a background process with a indeterminate
   * progress bar.
   */
  public void backgroundProcess(String commandLine) {
    processMgr.test(commandLine);
  }

  /**
   * Notification message that a background process is done.
   */
  public void processProgress(String threadName, double percent) {

  }

  public void processDone(String threadName) {
    if (threadName.equals(threadNameA)) {
      mainFrame.stopProgressBar(AxisID.FIRST);
      threadNameA = "none";
    }
    else if (threadName.equals(threadNameB)) {
      mainFrame.stopProgressBar(AxisID.SECOND);
      threadNameB = "none";
    }
    else {
      openMessageDialog(
        "Unknown thread finished!!!",
        "Thread name: " + threadName);
    }
  }

  private void setThreadName(String name, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      threadNameB = name;
    }
    else {
      threadNameA = name;
    }
  }

}
