package etomo;

import java.io.*;
import java.util.*;
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

  private boolean isDataParamDirty = false;
  private String homeDirectory;
  private UserConfiguration userConfig = new UserConfiguration();
  private MetaData metaData = new MetaData();
  private File paramFile = null;

  //  This object controls the reading and writing of David's com scripts
  private ComScriptManager comScriptMgr = new ComScriptManager(this);

  //  This object manages the execution of com scripts and the running of imod
  private ProcessManager processMgr = new ProcessManager(this);
  private ProcessTrack processTrack = new ProcessTrack();

  // This object manages the opening and closing closing of imod(s), message 
  // passing for loading model
  private ImodManager imodManager = new ImodManager(this);

  private MainFrame mainFrame;
  private Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

  /**Construct the application*/
  public ApplicationManager() {

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

    mainFrame.setVisible(true);
  }

  /**Main method*/
  public static void main(String[] args) {
    new ApplicationManager();

    //  Parse the command line arguments
    if (args.length > 0) {
    }
  }

  /**
  * Open the setup dialog
  */
  public void openSetupDialog() {

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    SetupDialog setupDialog = new SetupDialog(this);
    setupDialog.initializeFields(metaData);

    Dimension size = setupDialog.getSize();
    setupDialog.setLocation(
      (screenSize.width - size.width) / 2,
      (screenSize.height - size.height) / 2);
    setupDialog.setModal(false);
    setupDialog.setVisible(true);
  }

  /**
   * Close message from the setup dialog window
   */
  public void doneSetupDialog(SetupDialog setupDialog) {

    //  Keep dialog box open until we get good info or it is cancelled
    boolean dialogFinished = false;

    //  Get the selected exit button
    DialogExitState exitState = setupDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      setupDialog.dispose();
    }
    else {
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
        dialogFinished = true;
        isDataParamDirty = true;
      }
      else {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Setup Parameter Error";
        errorMessage[1] = metaData.getInvalidReason();
        JOptionPane.showMessageDialog(
          mainFrame,
          errorMessage,
          "Setup Parameter Error",
          JOptionPane.ERROR_MESSAGE);
        setupDialog.setVisible(true);
        return;
      }
    }

    //  This allows the user to use existing com scripts by syncing the setup
    //  parameters with the existing scripts
    if (exitState == DialogExitState.POSTPONE) {
      metaData.setComScriptCreated(true);
    }

    //  Create the com scripts
    if (exitState == DialogExitState.EXECUTE) {
      try {
        processMgr.setupComScripts(metaData);
        processTrack.setSetupState(ProcessState.COMPLETE);
        mainFrame.setSetupState(ProcessState.COMPLETE);
        metaData.setComScriptCreated(true);
        setupDialog.dispose();
        dialogFinished = true;
      }
      catch (BadComScriptException except) {
        except.printStackTrace();
        JOptionPane.showMessageDialog(
          mainFrame,
          except.getMessage(),
          "Can't run copytomocoms",
          JOptionPane.ERROR_MESSAGE);
        dialogFinished = false;
      }

      catch (IOException except) {
        JOptionPane.showMessageDialog(
          mainFrame,
          "Can't run copytomocoms\n" + except.getMessage(),
          "Copytomocoms IOException",
          JOptionPane.ERROR_MESSAGE);
        dialogFinished = false;
      }
    }
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
    PreProcessingDialog preProcDialog = new PreProcessingDialog(this);

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
    preProcDialog.setVisible(true);
  }

  public void donePreProcDialog(PreProcessingDialog preProcDialog) {

    CCDEraserParam ccdEraserParam = new CCDEraserParam();

    //  Keep dialog box open until we get good info or it is cancelled
    boolean dialogFinished = false;
    DialogExitState exitState = preProcDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      dialogFinished = true;
    }

    else {
      //  Get the user input data from the dialog box.  The CCDEraserParam
      //  is first initialized from the currently loaded com script to
      //  provide deafault values for those not handled by the dialog box
      //   get function needs some error checking
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
      processTrack.setPreProcessingState(ProcessState.INPROGRESS);
      mainFrame.setPreProcState(ProcessState.INPROGRESS);
    }

    if (exitState == DialogExitState.EXECUTE) {
      processTrack.setPreProcessingState(ProcessState.COMPLETE);
      mainFrame.setPreProcState(ProcessState.COMPLETE);
    }

    //  Close the dialog box
    preProcDialog.dispose();
  }

  /**
   * Open imod to create the erase model
   */
  public void imodErase(AxisID axisID) {
    processMgr.imodErase(axisID);
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
    CoarseAlignDialog coarseAlignDialog = new CoarseAlignDialog(this);
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

    coarseAlignDialog.setVisible(true);
  }

  /**
   *
   */
  public void doneCoarseAlignDialog(CoarseAlignDialog coarseAlignDialog) {

    DialogExitState exitState = coarseAlignDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      coarseAlignDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      boolean dialogFinished = updateXcorrCom(coarseAlignDialog);
      if (dialogFinished) {
        processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setCoarseAlignmentState(ProcessState.COMPLETE);
        }

        mainFrame.setCoarseAlignState(processTrack.getCoarseAlignmentState());
        coarseAlignDialog.dispose();
      }
      else {
        coarseAlignDialog.setVisible(true);
      }
    }
  }

  /**
   * Get the parameters from dialog box and run the cross correlation script
   */
  public void crossCorrelate(
    AxisID axisID,
    CoarseAlignDialog coarseAlignDialog) {
    // Get the parameters from the dialog box
    if (updateXcorrCom(coarseAlignDialog)) {
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
    processMgr.imodAlign(axisID);
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
  private boolean updateXcorrCom(CoarseAlignDialog coarseAlignDialog) {
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
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Xcorr Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Align Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Xcorr Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
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
    FiducialModelDialog fiducialModelDialog = new FiducialModelDialog(this);
    //Dimension frmSize = mainFrame.getSize();
    //Point loc = mainFrame.getLocation();
    //fiducialModelDialog.setLocation(loc.x, loc.y + frmSize.height);
    Dimension size = fiducialModelDialog.getSize();
    fiducialModelDialog.setLocation(
      (screenSize.width - size.width) / 2,
      (screenSize.height - size.height) / 2);
    fiducialModelDialog.setModal(false);
    //fiducialModelDialog.setAdvanced(advancedState.fiducialModel);

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
    fiducialModelDialog.setVisible(true);
  }

  public void doneFiducialModelDialog(FiducialModelDialog fiducialModelDialog) {

    DialogExitState exitState = fiducialModelDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      fiducialModelDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      boolean dialogFinished = updateTrackCom(fiducialModelDialog);
      if (dialogFinished) {
        processTrack.setFiducialModelState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setFiducialModelState(ProcessState.COMPLETE);
        }

        mainFrame.setFiducialModelState(processTrack.getFiducialModelState());
        fiducialModelDialog.dispose();
      }
      else {
        fiducialModelDialog.setVisible(true);
      }
    }
  }

  /**
   * Open imod with the seed model
   */
  public void imodSeedFiducials(AxisID axisID) {
    processMgr.imodSeedFiducials(axisID);
  }

  /**
   * Get the beadtrack parameters from the fiducial model dialog and run the
   * track com script
   */
  public void fiducialModelTrack(
    AxisID axisID,
    FiducialModelDialog fiducialModelDialog) {
    if (updateTrackCom(fiducialModelDialog)) {
      processMgr.fiducialModelTrack(axisID);
    }
  }

  /**
   * Open imod with the new fidcuial model
   */
  public void imodFixFiducials(AxisID axisID) {
    processMgr.imodFixFiducials(axisID);
  }

  /**
   * Update the specified track com script
   */
  private boolean updateTrackCom(FiducialModelDialog fiducialModelDialog) {
    BeadtrackParam beadtrackParam;
    AxisID currentAxis = AxisID.ONLY;
    try {
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
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Beadtrack Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Beadtrack Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
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

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    AlignmentEstimationDialog alignmentEstimationDialog =
      new AlignmentEstimationDialog(this);

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

    alignmentEstimationDialog.setVisible(true);
  }

  /**
   *
   *
   */
  public void doneAlignmentEstimationDialog(AlignmentEstimationDialog alignmentEstimationDialog) {
    DialogExitState exitState = alignmentEstimationDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      alignmentEstimationDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      boolean dialogFinished = updateAlignCom(alignmentEstimationDialog);

      if (dialogFinished) {
        processTrack.setFineAlignmentState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setFineAlignmentState(ProcessState.COMPLETE);
        }

        mainFrame.setAlignmentEstState(processTrack.getFineAlignmentState());
        alignmentEstimationDialog.dispose();
      }
      else {
        alignmentEstimationDialog.setVisible(true);
      }
    }
  }

  /**
   *
   *
   */
  public void fineAlignment(
    AxisID axisID,
    AlignmentEstimationDialog alignmentEstimationDialog) {
    if (updateAlignCom(alignmentEstimationDialog)) {
      processMgr.fineAlignment(axisID);
    }
  }

  /**
   * Open imod to view the coarsely aligned stack
   */
  public void imodFineAlign(AxisID axisID) {
    processMgr.imodFineAlign(axisID);
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters from
   * the alignment estimation dialog.  This also updates the local alignment
   * state of the appropriate tilt files.
   */
  private boolean updateAlignCom(AlignmentEstimationDialog alignmentEstimationDialog) {
    TiltalignParam tiltalignParam;
    TiltParam tiltParam;

    AxisID currentAxis = AxisID.ONLY;
    try {
      if (isDualAxis()) {
        currentAxis = AxisID.FIRST;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);

        alignmentEstimationDialog.getTiltalignParams(
          tiltalignParam,
          currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);

        comScriptMgr.loadTiltCom(currentAxis);
        tiltParam = comScriptMgr.getTiltParam(currentAxis);
        if (tiltalignParam.getLocalAlignments()) {
          tiltParam.setLocalAlignFile(getFilesetName() + "alocal.xf");
        }
        else {
          tiltParam.setLocalAlignFile("");
        }
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);

        currentAxis = AxisID.SECOND;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);
        alignmentEstimationDialog.getTiltalignParams(
          tiltalignParam,
          currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);

        comScriptMgr.loadTiltCom(currentAxis);
        tiltParam = comScriptMgr.getTiltParam(currentAxis);
        if (tiltalignParam.getLocalAlignments()) {
          tiltParam.setLocalAlignFile(getFilesetName() + "blocal.xf");
        }
        else {
          tiltParam.setLocalAlignFile("");
        }
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);

      }
      else {
        currentAxis = AxisID.ONLY;
        tiltalignParam = comScriptMgr.getTiltalignParam(currentAxis);
        alignmentEstimationDialog.getTiltalignParams(
          tiltalignParam,
          currentAxis);
        comScriptMgr.saveAlignCom(tiltalignParam, currentAxis);

        comScriptMgr.loadTiltCom(currentAxis);
        tiltParam = comScriptMgr.getTiltParam(currentAxis);
        if (tiltalignParam.getLocalAlignments()) {
          tiltParam.setLocalAlignFile(getFilesetName() + "local.xf");
        }
        else {
          tiltParam.setLocalAlignFile("");
        }
        comScriptMgr.saveTiltCom(tiltParam, currentAxis);

      }

      mainFrame.setAlignmentEstState(ProcessState.INPROGRESS);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Tiltalign Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = currentAxis.toString();
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Tiltalign Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
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

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    TomogramPositioningDialog tomogramPositioningDialog =
      new TomogramPositioningDialog(this);

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

    tomogramPositioningDialog.setVisible(true);
  }

  public void doneTomogramPositioningDialog(TomogramPositioningDialog tomogramPositioningDialog) {
    DialogExitState exitState = tomogramPositioningDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      tomogramPositioningDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      boolean tiltFinished = updateSampleTiltCom(tomogramPositioningDialog);
      boolean alignFinished = updateAlignCom(tomogramPositioningDialog);
      if (tiltFinished & alignFinished) {
        processTrack.setTomogramPositioningState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setTomogramPositioningState(ProcessState.COMPLETE);
        }

        mainFrame.setTomogramPositioningState(
          processTrack.getTomogramPositioningState());
        tomogramPositioningDialog.dispose();
      }
      else {
        tomogramPositioningDialog.setVisible(true);
      }
    }
  }

  /**
   * Run the sample com script
   */
  public void createSample(
    AxisID axisID,
    TomogramPositioningDialog tomogramPositioningDialog) {
    //  Get the user input data from the dialog box
    if (updateSampleTiltCom(tomogramPositioningDialog)) {
      processMgr.createSample(axisID);
    }
  }

  public void imodSample(AxisID axisID) {
    processMgr.imodSample(axisID);
  }

  public void tomopitch(AxisID axisID) {
    processMgr.tomopitch(axisID);
  }

  public void finalAlign(
    TomogramPositioningDialog tomogramPositioningDialog,
    AxisID axisID) {
    if (updateAlignCom(tomogramPositioningDialog)) {
      processMgr.fineAlignment(axisID);
    }
  }

  private boolean updateSampleTiltCom(TomogramPositioningDialog tomogramPositioningDialog) {
    TiltParam tiltParam;
    AxisID currentAxis = AxisID.ONLY;
    try {
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
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Tilt Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
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
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Tiltalign Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
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

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    TomogramGenerationDialog tomogramGenerationDialog =
      new TomogramGenerationDialog(this);
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

    tomogramGenerationDialog.setVisible(true);
  }

  public void doneTomogramGenerationDialog(TomogramGenerationDialog tomogramGenerationDialog) {
    DialogExitState exitState = tomogramGenerationDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      tomogramGenerationDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      boolean dialogFinished = updateTiltCom(tomogramGenerationDialog);
      if (dialogFinished) {
        processTrack.setTomogramGenerationState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setTomogramGenerationState(ProcessState.COMPLETE);
        }

        mainFrame.setTomogramGenerationState(
          processTrack.getTomogramGenerationState());
        tomogramGenerationDialog.dispose();
      }
      else {
        tomogramGenerationDialog.setVisible(true);
      }
    }
  }

  /**
   *
   *
   */
  private boolean updateTiltCom(TomogramGenerationDialog tomogramGenerationDialog) {

    TiltParam tiltParam;
    AxisID currentAxis = AxisID.ONLY;
    try {
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
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Tilt Parameter Syntax Error",
        JOptionPane.ERROR_MESSAGE);
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
  public void tilt(
    AxisID axisID,
    TomogramGenerationDialog tomogramGenerationDialog) {
    if (updateTiltCom(tomogramGenerationDialog)) {
      processMgr.tilt(axisID);
    }
  }

  /**
   * Open imod to view the coarsely aligned stack
   */
  public void imodTomogram(AxisID axisID) {
    processMgr.imodTomogram(axisID);
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

    // FIXME need to check if this is a dual axis tomogram, if not open a dialog

    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    TomogramCombinationDialog tomogramCombinationDialog =
      new TomogramCombinationDialog(this);
    Dimension size = tomogramCombinationDialog.getSize();
    tomogramCombinationDialog.setLocation(
      (screenSize.width - size.width) / 2,
      (screenSize.height - size.height) / 2);
    tomogramCombinationDialog.setModal(false);
    tomogramCombinationDialog.setVisible(true);
    //  Fill in the dialog box params and set it to the appropriate state

    tomogramCombinationDialog.setCombineParams(metaData.getCombineParams());

  }

  public void doneTomogramCombinationDialog(TomogramCombinationDialog tomogramCombinationDialog) {

    DialogExitState exitState = tomogramCombinationDialog.getExitState();

    if (exitState == DialogExitState.CANCEL) {
      tomogramCombinationDialog.dispose();
    }
    else {
      //  Get the user input data from the dialog box
      boolean dialogFinished = updateCombineCom(tomogramCombinationDialog);
      if (dialogFinished) {
        tomogramCombinationDialog.getCombineParams(metaData.getCombineParams());

        processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);

        if (exitState == DialogExitState.EXECUTE) {
          processTrack.setTomogramCombinationState(ProcessState.COMPLETE);
        }

        mainFrame.setTomogramCombinationState(
          processTrack.getTomogramCombinationState());
        tomogramCombinationDialog.dispose();
      }
      else {
        tomogramCombinationDialog.setVisible(true);
      }
    }

    if (exitState == DialogExitState.POSTPONE
      || exitState == DialogExitState.EXECUTE) {
      //
      //  Get the user input data from the dialog box
      //
      mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);

    }
    if (exitState == DialogExitState.EXECUTE) {
      mainFrame.setTomogramCombinationState(ProcessState.COMPLETE);
    }

  }

  //FIXME get the combine parameters from the dialog box
  private boolean updateCombineCom(TomogramCombinationDialog tomogramCombinationDialog) {

    return true;
  }

  /**
   * Run the setupcombine script with the current combine parameters stored in
   * metaData object
   */
  public void createCombineScripts() {

    try {
      processMgr.createCombineScripts(metaData);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        mainFrame,
        except.getMessage(),
        "Can't run setupcombine",
        JOptionPane.ERROR_MESSAGE);
    }

    catch (IOException except) {
      JOptionPane.showMessageDialog(
        mainFrame,
        "Can't run setupcombine\n" + except.getMessage(),
        "Setupcombine IOException",
        JOptionPane.ERROR_MESSAGE);
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
    PostProcessingDialog postProcessingDialog = new PostProcessingDialog();
    Dimension frmSize = mainFrame.getSize();
    Point loc = mainFrame.getLocation();
    postProcessingDialog.setLocation(loc.x, loc.y + frmSize.height);
    postProcessingDialog.setModal(false);
    postProcessingDialog.setVisible(true);

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
   * @param the File object specifiying the data parameter file.
   */
  public void openTestParamFile(File paramFile) {
    FileInputStream processDataStream;
    try {

      //
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
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not find the test parameter data file:";
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "File not found error",
        JOptionPane.ERROR_MESSAGE);
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not read the test parameter data from file:";
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Test parameter file read error",
        JOptionPane.ERROR_MESSAGE);
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
      JOptionPane.showMessageDialog(
        mainFrame,
        errorMessage,
        "Test parameter file save error",
        JOptionPane.ERROR_MESSAGE);
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
    JOptionPane.showMessageDialog(
      mainFrame,
      message,
      "Program Operation Error",
      JOptionPane.ERROR_MESSAGE);
    return;
  }

  private void initProgram() {
    //
    // Get the HOME directory environment variable to find the program
    // configuration file
    //
    homeDirectory = getHomeDirectory();
    if (homeDirectory == "") {
      String[] message = new String[2];
      message[0] =
        "Can not find home directory! Unable to load user preferences";
      message[1] =
        "Set HOME environment variable and restart program to fix this problem";
      JOptionPane.showMessageDialog(
        mainFrame,
        message,
        "Program Initialization Error",
        JOptionPane.ERROR_MESSAGE);
      return;
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
    catch (Exception except) {
      JOptionPane.showMessageDialog(
        mainFrame,
        except.getMessage(),
        "Can't load user configuration" + userConfigFile.getAbsolutePath(),
        JOptionPane.ERROR_MESSAGE);
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
        "Could not create file:" + userConfigFile.getAbsolutePath());
      System.out.println(except.getMessage());
      return true;
    }

    ParameterStore userParams = new ParameterStore(userConfigFile);
    Storable storable[] = new Storable[1];
    storable[0] = userConfig;
    if (!userConfigFile.canWrite()) {
      JOptionPane.showMessageDialog(
        mainFrame,
        "Change permissions of $HOME/.etomo to allow writing",
        "Unable to save user configuration file",
        JOptionPane.ERROR_MESSAGE);
    }

    if (userConfigFile.canWrite()) {
      try {
        userParams.save(storable);
      }
      catch (Exception excep) {
        excep.printStackTrace();
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
    return getEnvironmentVariable("IMOD_DIR");
  }

  /**
   * Return the users home directory environment variable HOME or an empty
   * string if it doesn't exist.
   */
  private String getHomeDirectory() {
    return getEnvironmentVariable("HOME");
  }

  private String getEnvironmentVariable(String varName) {
    //  There is not a real good way to access the system environment variables
    //  since the primary method was deprecated
    SystemProgram echoHome = new SystemProgram("env");
    try {
      echoHome.run();
    }
    catch (Exception excep) {
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
}
