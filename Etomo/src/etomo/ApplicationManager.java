package etomo;

import java.io.*;
import etomo.util.InvalidParameterException;
import java.awt.*;
import javax.swing.*;

import etomo.comscript.*;
import etomo.process.*;
import etomo.type.*;
import etomo.storage.*;
import etomo.ui.*;

// FIXME check that the dispose method is called in all appropriate places
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

  //  Process dialog references
  private SetupDialog setupDialog = null;
  private PreProcessingDialog preProcDialog = null;
  private CoarseAlignDialog coarseAlignDialog = null;
  private FiducialModelDialog fiducialModelDialog = null;
  private AlignmentEstimationDialog alignmentEstimationDialog = null;
  private TomogramPositioningDialog tomogramPositioningDialog = null;
  private TomogramGenerationDialog tomogramGenerationDialog = null;
  private TomogramCombinationDialog tomogramCombinationDialog = null;

  //  This object controls the reading and writing of David's com scripts
  private ComScriptManager comScriptMgr = new ComScriptManager(this);

  //  The ProcessManager manages the execution of com scripts
  private ProcessManager processMgr = new ProcessManager(this);
  private ProcessTrack processTrack = new ProcessTrack();

  // imodManager manages the opening and closing closing of imod(s), message 
  // passing for loading model
  private ImodManager imodManager;

  private MainFrame mainFrame;
  private Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

  /**Construct the application*/
  public ApplicationManager(String[] args) {

    parseCommandLine(args);

    //  Initialize the program settings
    initProgram();

    //  Initialize the static application manager reference for the
    //  context popup
    ContextPopup initContextPopup = new ContextPopup(this);

    //  Create a new main window and wait for an event from the user
    //
    mainFrame = new MainFrame(this);
    mainFrame.setMRUFileLabels(userConfig.getMRUFileList());
    mainFrame.pack();
    Dimension frameSize = mainFrame.getSize();
    mainFrame.setLocation(
      (screenSize.width - frameSize.width) / 2,
      (screenSize.height - frameSize.height) / 2);

    mainFrame.show();
  }

  /**Main method*/
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

      Dimension size = setupDialog.getSize();
      setupDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      setupDialog.setModal(false);
    }
    setupDialog.show();
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
        mainFrame.setSetupState(ProcessState.INPROGRESS);

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
        setupDialog.show();
        return;
      }

      if (exitState == DialogExitState.POSTPONE) {
        metaData.setComScriptCreated(true);
      }
      else {
        try {
          processMgr.setupComScripts(metaData);
          processTrack.setSetupState(ProcessState.COMPLETE);
          mainFrame.setSetupState(ProcessState.COMPLETE);
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

    //  Free the dialog
    setupDialog.dispose();
    setupDialog = null;
  }

  /**
   * Open the pre-processing dialog
   */
  public void openPreProcDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (preProcDialog == null) {
      preProcDialog = new PreProcessingDialog(this);

      Dimension size = preProcDialog.getSize();
      preProcDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      preProcDialog.setModal(false);

      // Load the required ccderaser{|a|b}.com files
      // Fill in the parameters and set it to the appropriate state
      if (isDualAxis()) {
        comScriptMgr.loadEraserCom(AxisID.FIRST);
        preProcDialog.setCCDEraserParams(
          comScriptMgr.getCCDEraserParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadEraserCom(AxisID.SECOND);
        preProcDialog.setCCDEraserParams(
          comScriptMgr.getCCDEraserParam(AxisID.SECOND),
          AxisID.SECOND);
        preProcDialog.setEnabledB(true);
      }
      else {
        comScriptMgr.loadEraserCom(AxisID.ONLY);
        preProcDialog.setCCDEraserParams(
          comScriptMgr.getCCDEraserParam(AxisID.ONLY),
          AxisID.ONLY);
        preProcDialog.setEnabledB(false);
      }
    }
    preProcDialog.show();
  }

  public void donePreProcDialog() {
    if (preProcDialog == null) {
      openMessageDialog(
        "Can not update preprocessing parameters without an active "
          + "preprocessing dialog",
        "Program logic error");
      return;
    }
    //  Keep dialog box open until we get good info or it is cancelled
    DialogExitState exitState = preProcDialog.getExitState();

    if (exitState != DialogExitState.CANCEL) {

      //  Get the user input data from the dialog box.  The CCDEraserParam
      //  is first initialized from the currently loaded com script to
      //  provide deafault values for those not handled by the dialog box
      //  get function needs some error checking
      CCDEraserParam ccdEraserParam = new CCDEraserParam();
      if (isDualAxis()) {
        ccdEraserParam = comScriptMgr.getCCDEraserParam(AxisID.FIRST);
        preProcDialog.getCCDEraserParams(ccdEraserParam, AxisID.FIRST);
        comScriptMgr.saveEraserCom(ccdEraserParam, AxisID.FIRST);

        ccdEraserParam = comScriptMgr.getCCDEraserParam(AxisID.SECOND);
        preProcDialog.getCCDEraserParams(ccdEraserParam, AxisID.SECOND);
        comScriptMgr.saveEraserCom(ccdEraserParam, AxisID.SECOND);

      }
      else {
        ccdEraserParam = comScriptMgr.getCCDEraserParam(AxisID.FIRST);
        preProcDialog.getCCDEraserParams(ccdEraserParam, AxisID.ONLY);
        comScriptMgr.saveEraserCom(ccdEraserParam, AxisID.ONLY);
      }

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
        processTrack.setPreProcessingState(ProcessState.COMPLETE);
        mainFrame.setPreProcState(ProcessState.COMPLETE);
      }
      else {
        processTrack.setPreProcessingState(ProcessState.INPROGRESS);
        mainFrame.setPreProcState(ProcessState.INPROGRESS);
      }
    }

    //  Close and delete the dialog box
    preProcDialog.dispose();
    preProcDialog = null;
  }

  /**
   * Open imod to create the erase model
   */
  public void imodErase(AxisID axisID) {
    String extension = "";

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
   * Run the eraser script
   */
  public void eraser(AxisID axisID) {
    processMgr.eraser(axisID);
  }

  /**
   * Open the coarse alignment dialog
   */
  public void openCoarseAlignDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    //  Create the dialog box
    if (coarseAlignDialog == null) {
      coarseAlignDialog = new CoarseAlignDialog(this);
      Dimension size = coarseAlignDialog.getSize();
      coarseAlignDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      coarseAlignDialog.setModal(false);

      // Load the required xcorr{|a|b}.com files
      // Fill in the parameters and set it to the appropriate state
      if (isDualAxis()) {
        comScriptMgr.loadXcorrCom(AxisID.FIRST);
        coarseAlignDialog.setCrossCorrelationParams(
          comScriptMgr.getTiltxcorrParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadXcorrCom(AxisID.SECOND);
        coarseAlignDialog.setCrossCorrelationParams(
          comScriptMgr.getTiltxcorrParam(AxisID.SECOND),
          AxisID.SECOND);

        coarseAlignDialog.setEnabledB(true);
      }
      else {
        comScriptMgr.loadXcorrCom(AxisID.ONLY);
        coarseAlignDialog.setCrossCorrelationParams(
          comScriptMgr.getTiltxcorrParam(AxisID.ONLY),
          AxisID.ONLY);

        coarseAlignDialog.setEnabledB(false);
      }
    }
    coarseAlignDialog.show();
  }

  /**
   *  Get the parameters from the coarse align process dialog box
   */
  public void doneCoarseAlignDialog() {
    if (coarseAlignDialog == null) {
      openMessageDialog(
        "Can not update coarse align without an active coarse align dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = coarseAlignDialog.getExitState();

    if (exitState != DialogExitState.CANCEL) {
      //  Get the user input data from the dialog box
      if (updateXcorrCom()) {
        processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setCoarseAlignmentState(ProcessState.COMPLETE);
        }

        mainFrame.setCoarseAlignState(processTrack.getCoarseAlignmentState());
      }

      else {
        coarseAlignDialog.show();
        return;
      }
    }
    coarseAlignDialog.dispose();
    coarseAlignDialog = null;
  }

  /**
   * Get the parameters from dialog box and run the cross correlation script
   */
  public void crossCorrelate(AxisID axisID) {

    // Get the parameters from the dialog box
    if (updateXcorrCom()) {
      processMgr.crossCorrelate(axisID);
    }
  }

  /**
   * Run the coarse alignement script
   */
  public void coarseAlign(AxisID axisID) {
    processMgr.coarseAlign(axisID);
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
  private boolean updateXcorrCom() {
    if (coarseAlignDialog == null) {
      openMessageDialog(
        "Can not update xcorr?.com without an active coarse align dialog",
        "Program logic error");
      return false;
    }

    TiltxcorrParam tiltXcorrParam;
    AxisID currentAxis = AxisID.ONLY;
    try {
      if (isDualAxis()) {
        currentAxis = AxisID.FIRST;
        tiltXcorrParam = comScriptMgr.getTiltxcorrParam(currentAxis);
        coarseAlignDialog.getCrossCorrelationParams(
          tiltXcorrParam,
          currentAxis);
        comScriptMgr.saveXcorrCom(tiltXcorrParam, currentAxis);

        currentAxis = AxisID.SECOND;
        tiltXcorrParam = comScriptMgr.getTiltxcorrParam(currentAxis);
        coarseAlignDialog.getCrossCorrelationParams(
          tiltXcorrParam,
          currentAxis);
        comScriptMgr.saveXcorrCom(tiltXcorrParam, currentAxis);
      }
      else {
        currentAxis = AxisID.ONLY;
        tiltXcorrParam = comScriptMgr.getTiltxcorrParam(currentAxis);
        coarseAlignDialog.getCrossCorrelationParams(
          tiltXcorrParam,
          currentAxis);
        comScriptMgr.saveXcorrCom(tiltXcorrParam, currentAxis);
      }
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Align Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Open the fiducial model generation dialog
   */
  public void openFiducialModelDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (fiducialModelDialog == null) {
      fiducialModelDialog = new FiducialModelDialog(this);
      Dimension size = fiducialModelDialog.getSize();
      fiducialModelDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      fiducialModelDialog.setModal(false);

      //  Load the required track{|a|b}.com files, fill in the dialog box params
      //  and set it to the appropriate state
      if (isDualAxis()) {
        comScriptMgr.loadTrackCom(AxisID.FIRST);
        fiducialModelDialog.setBeadtrackParams(
          comScriptMgr.getBeadtrackParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadTrackCom(AxisID.SECOND);
        fiducialModelDialog.setBeadtrackParams(
          comScriptMgr.getBeadtrackParam(AxisID.SECOND),
          AxisID.SECOND);

        fiducialModelDialog.setEnabledB(true);
      }
      else {
        comScriptMgr.loadTrackCom(AxisID.ONLY);
        fiducialModelDialog.setBeadtrackParams(
          comScriptMgr.getBeadtrackParam(AxisID.ONLY),
          AxisID.ONLY);

        fiducialModelDialog.setEnabledB(false);
      }
    }
    fiducialModelDialog.show();
  }

  public void doneFiducialModelDialog() {
    if (fiducialModelDialog == null) {
      openMessageDialog(
        "Can not update fiducial model without an active fiducial model dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = fiducialModelDialog.getExitState();
    if (exitState != DialogExitState.CANCEL) {
      //  Get the user input data from the dialog box
      if (updateTrackCom()) {
        processTrack.setFiducialModelState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setFiducialModelState(ProcessState.COMPLETE);
        }

        mainFrame.setFiducialModelState(processTrack.getFiducialModelState());
      }
      else {
        fiducialModelDialog.show();
        return;
      }
    }
    fiducialModelDialog.dispose();
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
    if (updateTrackCom()) {
      processMgr.fiducialModelTrack(axisID);
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
  private boolean updateTrackCom() {
    if (fiducialModelDialog == null) {
      openMessageDialog(
        "Can not update track?.com without an active fiducial model dialog",
        "Program logic error");
      return false;
    }

    AxisID currentAxis = AxisID.ONLY;
    try {
      BeadtrackParam beadtrackParam;
      if (isDualAxis()) {
        currentAxis = AxisID.FIRST;
        beadtrackParam = comScriptMgr.getBeadtrackParam(currentAxis);
        fiducialModelDialog.getBeadtrackParams(beadtrackParam, currentAxis);
        comScriptMgr.saveTrackCom(beadtrackParam, currentAxis);

        currentAxis = AxisID.SECOND;
        beadtrackParam = comScriptMgr.getBeadtrackParam(currentAxis);
        fiducialModelDialog.getBeadtrackParams(beadtrackParam, currentAxis);
        comScriptMgr.saveTrackCom(beadtrackParam, currentAxis);
      }
      else {
        currentAxis = AxisID.ONLY;
        beadtrackParam = comScriptMgr.getBeadtrackParam(currentAxis);
        fiducialModelDialog.getBeadtrackParams(beadtrackParam, currentAxis);
        comScriptMgr.saveTrackCom(beadtrackParam, currentAxis);
      }
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Beadtrack Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Beadtrack Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Open the alignment estimation dialog
   */
  public void openAlignmentEstimationDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    if (alignmentEstimationDialog == null) {
      //  Open the dialog in the appropriate mode for the current state of
      //  processing
      alignmentEstimationDialog = new AlignmentEstimationDialog(this);

      Dimension size = alignmentEstimationDialog.getSize();
      alignmentEstimationDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      alignmentEstimationDialog.setModal(false);

      //  Load the required align{|a|b}.com files, fill in the dialog box params
      //  and set it to the appropriate state
      if (isDualAxis()) {
        comScriptMgr.loadAlignCom(AxisID.FIRST);
        alignmentEstimationDialog.setTiltalignParams(
          comScriptMgr.getTiltalignParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadAlignCom(AxisID.SECOND);
        alignmentEstimationDialog.setTiltalignParams(
          comScriptMgr.getTiltalignParam(AxisID.SECOND),
          AxisID.SECOND);

        alignmentEstimationDialog.setEnabledB(true);
      }
      else {
        comScriptMgr.loadAlignCom(AxisID.ONLY);
        alignmentEstimationDialog.setTiltalignParams(
          comScriptMgr.getTiltalignParam(AxisID.ONLY),
          AxisID.ONLY);

        alignmentEstimationDialog.setEnabledB(false);
      }
    }
    alignmentEstimationDialog.show();
  }

  /**
   *
   *
   */
  public void doneAlignmentEstimationDialog() {
    if (alignmentEstimationDialog == null) {
      openMessageDialog(
        "Can not update align?.com without an active alignment dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = alignmentEstimationDialog.getExitState();

    if (exitState != DialogExitState.CANCEL) {

      //  Get the user input data from the dialog box
      if (updateAlignCom()) {

        processTrack.setFineAlignmentState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setFineAlignmentState(ProcessState.COMPLETE);
        }

        mainFrame.setAlignmentEstState(processTrack.getFineAlignmentState());
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
      else {
        alignmentEstimationDialog.show();
        return;
      }
    }
    alignmentEstimationDialog.dispose();
    alignmentEstimationDialog = null;
  }

  /**
   * Execute the fine alignment script (align.com) for the appropriate axis
   * @param the AxisID identifying the axis to align.
   * @param the AlignmentEstimationDialog that contains the parameters for the
   * alignment script.
   */
  public void fineAlignment(AxisID axisID) {
    if (updateAlignCom()) {
      processMgr.fineAlignment(axisID);
    }
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
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters from
   * the alignment estimation dialog.  This also updates the local alignment
   * state of the appropriate tilt files.
   */
  private boolean updateAlignCom() {
    if (alignmentEstimationDialog == null) {
      openMessageDialog(
        "Can not update align?.com without an active alignment dialog",
        "Program logic error");
      return false;
    }

    TiltalignParam tiltalignParam;

    AxisID currentAxis = AxisID.ONLY;
    try {
      if (isDualAxis()) {
        //  First axis
        currentAxis = AxisID.FIRST;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);

        alignmentEstimationDialog.getTiltalignParams(
          tiltalignParam,
          currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);
        //  Update the tilt.com script with the dependent parameters
        updateTiltDependsOnAlign(tiltalignParam, currentAxis);

        // Second axis
        currentAxis = AxisID.SECOND;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);
        alignmentEstimationDialog.getTiltalignParams(
          tiltalignParam,
          currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);
        //  Update the tilt.com script with the dependent parameters
        updateTiltDependsOnAlign(tiltalignParam, currentAxis);
      }
      else {
        //  Single axis
        currentAxis = AxisID.ONLY;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);
        alignmentEstimationDialog.getTiltalignParams(
          tiltalignParam,
          currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);
        //  Update the tilt.com script with the dependent parameters
        updateTiltDependsOnAlign(tiltalignParam, currentAxis);
      }

      mainFrame.setAlignmentEstState(ProcessState.INPROGRESS);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tiltalign Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
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
  public void openTomogramPositioningDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    if (tomogramPositioningDialog == null) {
      //  Open the dialog in the appropriate mode for the current state of
      //  processing
      tomogramPositioningDialog = new TomogramPositioningDialog(this);

      Dimension size = tomogramPositioningDialog.getSize();
      tomogramPositioningDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      tomogramPositioningDialog.setModal(false);

      //  Load the required align{|a|b}.com and tilt{|a|b}.com files, fill in the
      //  dialog box params and set it to the appropriate state
      if (isDualAxis()) {
        comScriptMgr.loadTiltCom(AxisID.FIRST);

        tomogramPositioningDialog.setTiltParams(
          comScriptMgr.getTiltParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadAlignCom(AxisID.FIRST);
        tomogramPositioningDialog.setAlignParams(
          comScriptMgr.getTiltalignParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadTiltCom(AxisID.SECOND);
        tomogramPositioningDialog.setTiltParams(
          comScriptMgr.getTiltParam(AxisID.SECOND),
          AxisID.SECOND);

        comScriptMgr.loadAlignCom(AxisID.SECOND);
        tomogramPositioningDialog.setAlignParams(
          comScriptMgr.getTiltalignParam(AxisID.SECOND),
          AxisID.SECOND);

        tomogramPositioningDialog.setEnabledB(true);
      }
      else {
        comScriptMgr.loadTiltCom(AxisID.ONLY);
        tomogramPositioningDialog.setTiltParams(
          comScriptMgr.getTiltParam(AxisID.ONLY),
          AxisID.ONLY);

        comScriptMgr.loadAlignCom(AxisID.ONLY);
        tomogramPositioningDialog.setAlignParams(
          comScriptMgr.getTiltalignParam(AxisID.ONLY),
          AxisID.ONLY);

        tomogramPositioningDialog.setEnabledB(false);
      }
    }
    tomogramPositioningDialog.show();
  }

  public void doneTomogramPositioningDialog() {
    if (tomogramPositioningDialog == null) {
      openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramPositioningDialog.getExitState();

    if (exitState != DialogExitState.CANCEL) {
      //  Get the user input data from the dialog box
      boolean tiltFinished = updateSampleTiltCom();
      boolean alignFinished = updateAlignCom(tomogramPositioningDialog);
      if (tiltFinished & alignFinished) {
        processTrack.setTomogramPositioningState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setTomogramPositioningState(ProcessState.COMPLETE);
        }

        mainFrame.setTomogramPositioningState(
          processTrack.getTomogramPositioningState());

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
      else {
        tomogramPositioningDialog.show();
        return;
      }
    }
    tomogramPositioningDialog.dispose();
    tomogramPositioningDialog = null;
  }

  /**
   * Run the sample com script
   */
  public void createSample(AxisID axisID) {
    //  Get the user input data from the dialog box
    if (updateSampleTiltCom()) {
      processMgr.createSample(axisID);
    }
  }

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

  public void tomopitch(AxisID axisID) {
    processMgr.tomopitch(axisID);
  }

  public void finalAlign(AxisID axisID) {

    if (updateAlignCom(tomogramPositioningDialog)) {
      processMgr.fineAlignment(axisID);
    }
  }

  private boolean updateSampleTiltCom() {
    if (tomogramPositioningDialog == null) {
      openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return false;
    }

    AxisID currentAxis = AxisID.ONLY;
    try {
      TiltParam tiltParam;
      if (isDualAxis()) {
        currentAxis = AxisID.FIRST;
        tiltParam = comScriptMgr.getTiltParam(currentAxis);

        tomogramPositioningDialog.getTiltParams(tiltParam, currentAxis);
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);

        currentAxis = AxisID.SECOND;
        tiltParam = comScriptMgr.getTiltParam(currentAxis);
        tomogramPositioningDialog.getTiltParams(tiltParam, currentAxis);
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);
      }
      else {
        currentAxis = AxisID.ONLY;
        tiltParam = comScriptMgr.getTiltParam(currentAxis);
        tomogramPositioningDialog.getTiltParams(tiltParam, currentAxis);
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);
      }

      mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
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
  private boolean updateAlignCom(TomogramPositioningDialog tomogramPositioningDialog) {

    TiltalignParam tiltalignParam;
    AxisID currentAxis = AxisID.ONLY;
    try {
      if (isDualAxis()) {
        currentAxis = AxisID.FIRST;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);

        tomogramPositioningDialog.getAlignParams(tiltalignParam, currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);

        currentAxis = AxisID.SECOND;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);
        tomogramPositioningDialog.getAlignParams(tiltalignParam, currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);
      }
      else {
        currentAxis = AxisID.ONLY;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);
        tomogramPositioningDialog.getAlignParams(tiltalignParam, currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);
      }

    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tiltalign Parameter Syntax Error");
      return false;
    }

    return true;
  }

  /**
   * Open the tomogram generation dialog
   */
  public void openTomogramGenerationDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }

    if (tomogramGenerationDialog == null) {
      //  Open the dialog in the appropriate mode for the current state of
      //  processing
      tomogramGenerationDialog = new TomogramGenerationDialog(this);
      Dimension size = tomogramGenerationDialog.getSize();
      tomogramGenerationDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      tomogramGenerationDialog.setModal(false);

      //  Load the required align{|a|b}.com files, fill in the dialog box params
      //  and set it to the appropriate state
      if (isDualAxis()) {
        comScriptMgr.loadTiltCom(AxisID.FIRST);
        tomogramGenerationDialog.setTiltParams(
          comScriptMgr.getTiltParam(AxisID.FIRST),
          AxisID.FIRST);

        comScriptMgr.loadTiltCom(AxisID.SECOND);
        tomogramGenerationDialog.setTiltParams(
          comScriptMgr.getTiltParam(AxisID.SECOND),
          AxisID.SECOND);

        tomogramGenerationDialog.setEnabledB(true);
      }
      else {
        comScriptMgr.loadTiltCom(AxisID.ONLY);
        tomogramGenerationDialog.setTiltParams(
          comScriptMgr.getTiltParam(AxisID.ONLY),
          AxisID.ONLY);

        tomogramGenerationDialog.setEnabledB(false);
      }
    }
    tomogramGenerationDialog.show();
  }

  public void doneTomogramGenerationDialog() {
    if (tomogramGenerationDialog == null) {
      openMessageDialog(
        "Can not update tilt?.com without an active tomogram generation dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramGenerationDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      tomogramGenerationDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      if (updateTiltCom()) {
        processTrack.setTomogramGenerationState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setTomogramGenerationState(ProcessState.COMPLETE);
        }

        mainFrame.setTomogramGenerationState(
          processTrack.getTomogramGenerationState());
        tomogramGenerationDialog.dispose();
      }
      else {
        tomogramGenerationDialog.show();
        return;
      }
    }
    tomogramGenerationDialog.dispose();
    tomogramGenerationDialog = null;
  }

  /**
   *
   *
   */
  private boolean updateTiltCom() {
    if (tomogramGenerationDialog == null) {
      openMessageDialog(
        "Can not update tilt?.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }

    AxisID currentAxis = AxisID.ONLY;
    try {
      TiltParam tiltParam;
      if (isDualAxis()) {
        currentAxis = AxisID.FIRST;
        tiltParam = comScriptMgr.getTiltParam(currentAxis);

        tomogramGenerationDialog.getTiltParams(tiltParam, currentAxis);
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);

        currentAxis = AxisID.SECOND;
        tiltParam = comScriptMgr.getTiltParam(currentAxis);

        tomogramGenerationDialog.getTiltParams(tiltParam, currentAxis);
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);
      }
      else {
        currentAxis = AxisID.ONLY;
        tiltParam = comScriptMgr.getTiltParam(currentAxis);
        tomogramGenerationDialog.getTiltParams(tiltParam, currentAxis);
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);
      }

      mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Tilt Parameter Syntax Error");
      return false;
    }

    return true;

  }

  /**
   *
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
      processMgr.newst(axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
    }
  }

  /**
   * 
   *
   */
  public void tilt(AxisID axisID) {
    if (updateTiltCom()) {
      processMgr.tilt(axisID);
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
      //  Open the dialog in the appropriate mode for the current state of
      //  processing
      tomogramCombinationDialog = new TomogramCombinationDialog(this);
      Dimension size = tomogramCombinationDialog.getSize();
      tomogramCombinationDialog.setLocation(
        (screenSize.width - size.width) / 2,
        (screenSize.height - size.height) / 2);
      tomogramCombinationDialog.setModal(false);

      // If the patch boundaries have not been previously set, then set them
      // to the default
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
          except.printStackTrace();
          openMessageDialog(
            except.getMessage(),
            "Invalid parameter: " + recFileName);
        }
        catch (IOException except) {
          except.printStackTrace();
          openMessageDialog(except.getMessage(), "IO Error: " + recFileName);
        }
      }

      // Fill in the dialog box params and set it to the appropriate state
      tomogramCombinationDialog.setCombineParams(combineParams);
    }
    tomogramCombinationDialog.show();
  }

  public void doneTomogramCombinationDialog() {
    if (tomogramCombinationDialog == null) {
      openMessageDialog(
        "Can not update combine.com without an active tomogram combination dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramCombinationDialog.getExitState();

    if (exitState != DialogExitState.CANCEL) {

      //  Get the user input data from the dialog box
      if (updateCombineCom()) {
        processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processMgr.combine();
          processTrack.setTomogramCombinationState(ProcessState.COMPLETE);
        }

        mainFrame.setTomogramCombinationState(
          processTrack.getTomogramCombinationState());
        tomogramCombinationDialog.dispose();
      }
      else {
        tomogramCombinationDialog.show();
        return;
      }
    }

    tomogramCombinationDialog.dispose();
    tomogramCombinationDialog = null;
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
   * 
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
    Dimension frmSize = mainFrame.getSize();
    Point loc = mainFrame.getLocation();
    postProcessingDialog.setLocation(loc.x, loc.y + frmSize.height);
    postProcessingDialog.setModal(false);
    postProcessingDialog.show();

    DialogExitState exitState = postProcessingDialog.getExitState();

    if (exitState == DialogExitState.POSTPONE
      || exitState == DialogExitState.EXECUTE) {
      //
      //  Get the user input data from the dialog box
      //
      mainFrame.setPostProcessingState(ProcessState.INPROGRESS);
    }
    if (exitState == DialogExitState.EXECUTE) {
      mainFrame.setPostProcessingState(ProcessState.COMPLETE);
    }

  }

  //
  //  Utility functions
  //

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
  public void openTestParamFile(File paramFile) {
    FileInputStream processDataStream;

    try {

      // Read in the test parameter data file
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[2];
      storable[0] = metaData;
      storable[1] = processTrack;
      paramStore.load(storable);
      setTestParamFile(paramFile);

      //  Update the ProcessState for each process panel
      mainFrame.setSetupState(processTrack.getSetupState());
      mainFrame.setPreProcState(processTrack.getPreProcessingState());
      mainFrame.setCoarseAlignState(processTrack.getCoarseAlignmentState());
      mainFrame.setFiducialModelState(processTrack.getFiducialModelState());
      mainFrame.setAlignmentEstState(processTrack.getFineAlignmentState());
      mainFrame.setTomogramPositioningState(
        processTrack.getTomogramPositioningState());
      mainFrame.setTomogramGenerationState(
        processTrack.getTomogramGenerationState());
      mainFrame.setTomogramCombinationState(
        processTrack.getTomogramCombinationState());
      mainFrame.setPostProcessingState(processTrack.getPostProcessingState());

      //  Update the MRU test data filename list
      userConfig.putDataFile(paramFile.getAbsolutePath());
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());

      //  Set the current working directory for the application
      if (metaData.getWorkingDirectory().length() > 0) {
        System.setProperty("user.dir", metaData.getWorkingDirectory());
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
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not read the test parameter data from file:";
      errorMessage[2] = except.getMessage();
      openMessageDialog(errorMessage, "Test parameter file read error");
    }
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
   * Set the data set parameter file
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setTestParamFile(File paramFile) {
    this.paramFile = paramFile;
    //
    //  Update main window information and status bar
    //
    mainFrame.updateDataParameters(metaData);
    mainFrame.setStatusBar(paramFile.getAbsolutePath());
  }

  private void setupRequestDialog() {
    String[] message = new String[2];
    message[0] = "The setup process has not been completed";
    message[1] =
      "Complete the Setup process before opening other process dialogs";
    openMessageDialog(message, "Program Operation Error");
    return;
  }

  private void initProgram() {

      
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
      message[0] =
        "Can not find current working directory!";
      message[1] =
        "Home directory will be the starting point for file opens.";
      openMessageDialog(message, "Program Initialization Error");
    }
    else {
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

    //
    //  Create a File object specifying the user configuration file
    //
    File userConfigFile = new File(homeDirectory, ".etomo");
    //
    //  Make sure the config file exists, create it if it doesn't
    //
    try {
      userConfigFile.createNewFile();
    }
    catch (IOException except) {
      System.out.println(
        "Could not create file:" + userConfigFile.getAbsolutePath());
      System.out.println(except.getMessage());
      return;
    }

    //
    // Load in the user configuration
    //
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

    //
    //  Attempt to load the configuration file and set the user preferences
    //
    ToolTipManager.sharedInstance().setInitialDelay(
      userConfig.getToolTipsInitialDelay());
    ToolTipManager.sharedInstance().setDismissDelay(
      userConfig.getToolTipsDismissDelay());
    setLookAndFeel(userConfig.getNativeLookAndFeel());
  }

  private void parseCommandLine(String[] args) {

    //  Parse the command line arguments
    if (args.length > 0) {
      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("--demo")) {
          demo = true;
          break;
        }
      }
    }
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

    //  Write out the user configuration data
    File userConfigFile = new File(homeDirectory, ".etomo");

    //  Make sure the config file exists, create it if it doesn't
    try {
      userConfigFile.createNewFile();
    }
    catch (IOException except) {
      System.out.println(
        "IOException: Could not create file:"
          + userConfigFile.getAbsolutePath()
          + "\n"
          + except.getMessage());
      System.out.println(except.getMessage());
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
    //  System.out.println(plaf[i].getClassName());
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
      System.out.println(
        "Could not set " + lookAndFeelClassName + " look and feel");
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

  private String getEnvironmentVariable(String varName) {
    //  There is not a real good way to access the system environment variables
    //  since the primary method was deprecated
    SystemProgram echoHome = new SystemProgram("env");
    try {
      echoHome.enableDebug(true);
      echoHome.run();
    }
    catch (Exception excep) {
      excep.printStackTrace();
      System.out.println(excep.getMessage());
      System.out.println(
        "Unable to run env command to find "
          + varName
          + " environment variable");

      return "";
    }
    String[] stderr = echoHome.getStdError();
    if (stderr.length > 0) {
      System.out.println("Error running 'env' command");
      for (int i = 0; i < stderr.length; i++) {
        System.out.println(stderr[i]);
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

  public void openMessageDialog(Object message, String title) {
    JOptionPane.showMessageDialog(
      mainFrame,
      message,
      title,
      JOptionPane.ERROR_MESSAGE);
  }
  /**
   * Returns the debug.
   * @return boolean
   */
  public boolean isDebug() {
    return debug;
  }

  /**
   * Returns the demo.
   * @return boolean
   */
  public boolean isDemo() {
    return demo;
  }

}
