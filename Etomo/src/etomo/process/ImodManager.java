package etomo.process;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.ConstMetaData;

/*p
 * <p>Description: This class manages the opening, closing and sending of 
 * messages to the appropriate imod processes. This class is state based in the
 * sense that is initialized with MetaData information and uses that information
 * to know which data sets to work with.  Thus if the </p>
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
 * <p> Revision 3.1  2003/11/11 00:20:43  sueh
 * <p> Bug349 fiducialModelA and B are now ImodProcesses.
 * <p> Initialize them. OpenFiducialModel(): changed it to work with
 * <p> ImodProcess.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.25  2003/11/05 20:29:52  rickg
 * <p> Bug #292 Added preserve contrast functionality to coarse align open
 * <p>
 * <p> Revision 2.24  2003/11/05 18:02:45  sueh
 * <p> bug298
 * <p>
 * <p> Revision 2.23  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 2.22  2003/11/04 17:53:31  rickg
 * <p> Bug #345 Explicitly set path to 3dmodusing IMOD_DIR
 * <p>
 * <p> Revision 2.21  2003/10/30 23:36:02  rickg
 * <p> Bug# 343 Open patch vector model in model mode
 * <p>
 * <p> Revision 2.20  2003/10/14 23:44:04  rickg
 * <p> Bug# 285 Fixed select mapping for fine aligned tomogram
 * <p>
 * <p> Revision 2.19  2003/09/08 05:45:39  rickg
 * <p> Rename single axis full volume dataset
 * <p>
 * <p> Revision 2.18  2003/09/02 21:58:08  rickg
 * <p> Changed naming structure to match trial tomogram structure
 * <p>
 * <p> Revision 2.17  2003/08/25 22:18:50  rickg
 * <p> Removed errant model opening for the tomogram where a matching
 * <p> or patch region model had been previously opened
 * <p>
 * <p> Revision 2.16  2003/08/05 21:20:17  rickg
 * <p> Implemented model and movie modes where appropriate
 * <p>
 * <p> Revision 2.15  2003/07/25 22:58:23  rickg
 * <p> Model mode management changes
 * <p>
 * <p> Revision 2.14  2003/07/22 22:15:19  rickg
 * <p> Add erased stack management
 * <p>
 * <p> Revision 2.13  2003/06/05 21:15:51  rickg
 * <p> Open sample in model mode
 * <p>
 * <p> Revision 2.12  2003/05/12 23:25:34  rickg
 * <p> imodv -> 3dmod -view
 * <p>
 * <p> Revision 2.11  2003/05/09 17:50:58  rickg
 * <p> Set appmgr on construction
 * <p>
 * <p> Revision 2.10  2003/05/08 23:19:03  rickg
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.9  2003/05/07 22:29:14  rickg
 * <p> set fill cache for matchCheck
 * <p>
 * <p> Revision 2.8  2003/04/30 18:48:34  rickg
 * <p> Changed matchcheck* to a single imod instance
 * <p>
 * <p> Revision 2.7  2003/04/28 23:25:26  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.6  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.5  2003/04/16 22:18:59  rickg
 * <p> Added imod of full and trimmed volume
 * <p>
 * <p> Revision 2.4  2003/03/20 21:18:40  rickg
 * <p> Added matchshift results button/access
 * <p>
 * <p> Revision 2.3  2003/03/19 00:23:43  rickg
 * <p> Added patch vector model management
 * <p>
 * <p> Revision 2.2  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.1  2003/03/07 07:22:50  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.9.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.9  2003/01/10 20:46:20  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.8  2003/01/07 00:30:46  rickg
 * <p> Fixed javadoc text
 * <p>
 * <p> Revision 1.7  2002/10/29 18:21:00  rickg
 * <p> Check to see if the ImodProcess is non-null before calling open isRunning
 * <P> in is*Open() functions.  Return false if the object is null so that they
 * <p> can be called without exceptions.
 * <p>
 * <p> Revision 1.6  2002/10/07 22:25:03  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.5  2002/09/20 18:33:04  rickg
 * <p> Added rest of quit methods
 * <p>
 * <p> Revision 1.4  2002/09/20 17:16:04  rickg
 * <p> Added typed exceptions
 * <p> Added methods to check if a particular process is open
 * <p> Added quit methods for processes
 * <p>
 * <p> Revision 1.3  2002/09/19 23:11:26  rickg
 * <p> Completed initial vesion to work with ImodProcess
 * <p>
 * <p> Revision 1.2  2002/09/17 23:39:38  rickg
 * <p> ImodProcess based, in progress
 * <p>
 * <p> Revision 1.1  2002/09/13 21:28:31  rickg
 * <p> initial entry
 * <p>
 * <p> </p>
 */
public class ImodManager {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;
  private AxisType axisType;
  private String datasetName;

  private ImodAssistant rawStackA;
  private ImodAssistant rawStackB;
  private ImodAssistant erasedStackA;
  private ImodAssistant erasedStackB;

  private ImodAssistant coarseAlignedA;
  private ImodAssistant coarseAlignedB;
  private ImodAssistant fineAlignedA;
  private ImodAssistant fineAlignedB;
  private ImodAssistant sampleA;
  private ImodAssistant sampleB;
  private ImodAssistant fullVolumeA;
  private ImodAssistant fullVolumeB;
  private ImodAssistant combinedTomogram;
  private ImodAssistant patchVectorModel;
  private ImodAssistant matchCheck;
  private ImodAssistant trimmedVolume;
  private ImodAssistant fiducialModelA;
  private ImodAssistant fiducialModelB;

  /**
   * Default constructor
   * @param metaData this class is used to initialize the
   * dataset name and axisType of the data to used in imod.
   */
  public ImodManager(ApplicationManager appMgr, ConstMetaData metaData) {
    applicationManager = appMgr;

    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();

    //  Initialize the necessary ImodProcesses
    if (axisType == AxisType.SINGLE_AXIS) {
      rawStackA = new ImodAssistant(datasetName + ".st");
      erasedStackA = new ImodAssistant(datasetName + "_fixed.st");
      coarseAlignedA = new ImodAssistant(datasetName + ".preali");
      fineAlignedA = new ImodAssistant(datasetName + ".ali");
      //ImodProcess("top.rec mid.rec bot.rec", "tomopitch.mod")
      sampleA = new ImodAssistant(AxisID.ONLY, "top", "mid", "bot" ,".rec", "tomopitch" ,".mod");
      //setSwapYZ(true)
      fullVolumeA = new ImodAssistant(datasetName + "_full.rec");
      fullVolumeA.setup(true, false, false);
      combinedTomogram = fullVolumeA;
      fiducialModelA = new ImodAssistant();
    }
    else {
      rawStackA = new ImodAssistant(datasetName + "a.st");
      rawStackB = new ImodAssistant(datasetName + "b.st");
      erasedStackA = new ImodAssistant(datasetName + "a_fixed.st");
      erasedStackB = new ImodAssistant(datasetName + "b_fixed.st");
      coarseAlignedA = new ImodAssistant(datasetName + "a.preali");
      coarseAlignedB = new ImodAssistant(datasetName + "b.preali");
      fineAlignedA = new ImodAssistant(datasetName + "a.ali");
      fineAlignedB = new ImodAssistant(datasetName + "b.ali");
      //sampleA = new ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod")
      sampleA = new ImodAssistant(AxisID.FIRST, "top", "mid", "bot" ,".rec", "tomopitch" ,".mod");
      // sampleB = new ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod)
      sampleB = new ImodAssistant(AxisID.SECOND, "top", "mid", "bot" ,".rec", "tomopitch" ,".mod");
      fullVolumeA = new ImodAssistant(datasetName + "a.rec");
      //fullVolumeA.setSwapYZ(true)
      fullVolumeA.setup(true, false, false);
      fullVolumeB = new ImodAssistant(datasetName + "b.rec");
      //fullVolumeB.setSwapYZ(true)
      fullVolumeB.setup(true, false, false);
      combinedTomogram = new ImodAssistant("sum.rec");
      //combinedTomogram.setSwapYZ(true)
      //combinedTomogram.setModelView(true)
      combinedTomogram.setup(true, false, false);
      patchVectorModel = new ImodAssistant("patch_vector.mod");
      patchVectorModel.setup(false, true, false);
      matchCheck = new ImodAssistant("matchcheck.mat matchcheck.rec");
      //matchCheck.setSwapYZ(true)
      //matchCheck.setFillCache(true)
      matchCheck.setup(true, false, true);
      fiducialModelA = new ImodAssistant();
      fiducialModelB = new ImodAssistant();
    }
    trimmedVolume = new ImodAssistant(datasetName + ".rec");
  }

  /**
   * Open the specified raw data stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openRawStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant rawStack = selectRawStack(axisID);
    rawStack.open();
  }

  /**
   * Open the specified model with on raw stack 3dmod
   */
  public void modelRawStack(String modelName, AxisID axisID, boolean modelMode)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an imod with right course aligned data set that
    // is already open
    openRawStack(axisID);
    ImodAssistant rawStack = selectRawStack(axisID);
    //rawStack.openModel(modelName);
    //if (modelMode) {
    //  rawStack.modelMode();
    //}
    //else {
    //  rawStack.movieMode();
    //}
    rawStack.model(modelName, modelMode);

  }

  /**
   * Check to see if the specified raw stack is open
   */
  public boolean isRawStackOpen(AxisID axisID) {
    ImodAssistant rawStack = selectRawStack(axisID);
    if (rawStack == null) {
      return false;
    }
    //return rawStack.isRunning();
    return rawStack.isOpen();
  }

  /**
   * Close the specified raw stack
   */
  public void quitRawStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant rawStack = selectRawStack(axisID);
    rawStack.quit();
  }

  /**
   * Open the specified erased data stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openErasedStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant erasedStack = selectErasedStack(axisID);
    erasedStack.open();
  }

  /**
   * Open the specified model with the erased stack 3dmod
   */
  public void modelErasedStack(String modelName, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an imod with right course aligned data set that
    // is already open
    openErasedStack(axisID);
    ImodAssistant erasedStack = selectErasedStack(axisID);
    //erasedStack.openModel(modelName);
    erasedStack.model(modelName);
  }

  /**
   * Check to see if the specified erased stack is open
   */
  public boolean isErasedStackOpen(AxisID axisID) {
    ImodAssistant erasedStack = selectErasedStack(axisID);
    if (erasedStack == null) {
      return false;
    }
    //return erasedStack.isRunning();
    return erasedStack.isOpen();
  }

  /**
   * Close the specified erased stack
   */
  public void quitErasedStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant erasedStack = selectErasedStack(axisID);
    erasedStack.quit();
  }

  /**
   * Open the specified coarse aligned stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openCoarseAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.open();
  }

  /**
   * Open the specified model with the course aligned imod
   */
  public void modelCoarseAligned(
    String modelName,
    AxisID axisID,
    boolean modelMode,
    boolean preserveConstrast)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an 3dmod with right coarse aligned data set that
    // is already open
    openCoarseAligned(axisID);
    ImodAssistant coarseAligned = selectCoarseAligned(axisID);
    //if (preserveConstrast) {
    //  coarseAligned.openModelPreserveContrast(modelName);
    //}
    //else {
    //  coarseAligned.openModel(modelName);
    //}
    //coarseAligned.modelMode();
    //if (modelMode) {
    //  coarseAligned.modelMode();
    //}
    //else {
    //  coarseAligned.movieMode();
    //}
    coarseAligned.setPreserveContrast(preserveConstrast);
    coarseAligned.model(modelName, modelMode);
  }

  public void openBeadFixer(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.openBeadFixer();
  }

  /**
   * Check to see if the specified coarsely aligned stack is open
   */
  public boolean isCoarseAlignedOpen(AxisID axisID) {
    ImodAssistant coarseAligned = selectCoarseAligned(axisID);
    if (coarseAligned == null) {
      return false;
    }
    return coarseAligned.isOpen();
  }

  /**
   * Close the specified coarsely aligned stack
   */
  public void quitCoarseAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.quit();
  }

  /**
   * Open the specified fine aligned stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   * 
   * This was originally run using SystemProgram and this command line
   *    String imodBinPath =
      ApplicationManager.getIMODDirectory().getAbsolutePath()
        + File.separator
        + "bin"
        + File.separator;
    String commandLine = imodBinPath + "3dmod -view " + model;
   */
  public void openFiducialModel(String model, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant fiducialModel = selectFiducialModel(axisID);
    //fiducialModel.setModelName(model);
    //fiducialModel.setUseModv(true);
    //fiducialModel.setOutputWindowID(false);
    //fiducialModel.open();
    fiducialModel.setUseModv(true);
    fiducialModel.setOutputWindowID(false);
    fiducialModel.openWithModel(model);



  }

  /**
   * Open the specified fine aligned stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openFineAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant fineAligned = selectFineAligned(axisID);
    fineAligned.open();
  }

  /**
   * Check to see if the specified finely aligned stack is open
   */
  public boolean isFineAlignedOpen(AxisID axisID) {
    ImodAssistant fineAligned = selectCoarseAligned(axisID);
    if (fineAligned == null) {
      return false;
    }
    return fineAligned.isOpen();
  }

  /**
   * Close the specified finely aligned stack
   */
  public void quitFinelyAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant fineAligned = selectCoarseAligned(axisID);
    fineAlignedB.quit();
  }

  /**
   * Open the specified tomograph samples in 3dmod if they are not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openSample(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant sample = selectSample(axisID);
    //sample.open();
    //sample.modelMode();
    sample.openInModelMode();
  }

  /**
   * Check to see if the specified sample reconstruction is open
   * @param axisID the AxisID of the desired axis.
   */
  public boolean isSampleOpen(AxisID axisID) {
    ImodAssistant sample = selectSample(axisID);
    if (sample == null) {
      return false;
    }
    return sample.isOpen();
  }

  /**
   * Close the specifed sample reconstruction
   */
  public void quitSample(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant sample = selectSample(axisID);
    sample.quit();
  }

  /**
   * Open the specified tomogram in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openFullVolume(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant fullVolume = selectFullVolume(axisID);
    // Remove any model references that may be left over from earlier matching
    // or patch region model instances
    //fullVolume.setModelName("");
    //fullVolume.open();
    fullVolume.openWithModel("");
  }

  /**
   * Open both tomograms and their matching models
   * @param datasetName
   */
  public void matchingModel(String datasetName)
    throws AxisTypeException, SystemProcessException {
    //fullVolumeA.open();
    //fullVolumeA.openModel(datasetName + "a.matmod");
    //fullVolumeA.modelMode();
    fullVolumeA.openWithModel(datasetName + "a.matmod", true);
    
    //fullVolumeB.open();
    //fullVolumeB.openModel(datasetName + "b.matmod");
    //fullVolumeB.modelMode();
    fullVolumeB.openWithModel(datasetName + "b.matmod", true);
  }

  /**
   * Open the patch region model and the volume being matched to if it is not
   * already open
   * @param axisID
   */
  public void patchRegionModel(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (axisID == AxisID.SECOND) {
      //fullVolumeB.open();
      //fullVolumeB.openModel("patch_region.mod");
      //fullVolumeB.modelMode();
      fullVolumeB.openWithModel("patch_region.mod", true);
    }
    else {
      //fullVolumeA.open();
      //fullVolumeA.openModel("patch_region.mod");
      //fullVolumeA.modelMode(); 
      fullVolumeA.openWithModel("patch_region.mod", true);
    }
  }

  /**
   * Check to see if the specified tomogram is open
   * @param axisID the AxisID of the desired axis.
   */
  public boolean isFullVolumeOpen(AxisID axisID) {
    ImodAssistant tomogram = selectFullVolume(axisID);
    if (tomogram == null) {
      return false;
    }
    return tomogram.isOpen();
  }

  /**
   * Close the specified tomogram
   */
  public void quitFullVolume(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodAssistant tomogram = selectFullVolume(axisID);
    tomogram.quit();
  }

  /**
   * Open the patch vector in 3dmod if it is not already open
   */
  public void openPatchVectorModel() throws SystemProcessException {
    //patchVectorModel.open();
    //patchVectorModel.modelMode();
    patchVectorModel.openInModelMode();
  }

  /**
   * Check to see if the patch vector model is open
   */
  public boolean ispatchVectorModelOpen() {
    return patchVectorModel.isOpen();
  }

  /**
   * Close the patch vector model
   */
  public void quitPatchVectorModel() throws SystemProcessException {
    patchVectorModel.quit();
  }

  /**
   * Open the combined tomogram in 3dmod if it is not already open
   */
  public void openCombinedTomogram() throws SystemProcessException {
    combinedTomogram.open();
  }

  /**
   * Check to see if the combined tomogram is open
   */
  public boolean isCombinedTomogramOpen() {
    return combinedTomogram.isOpen();
  }

  /**
   * Close the combined tomogram
   */
  public void quitCombinedTomogram() throws SystemProcessException {
    combinedTomogram.quit();
  }

  /**
   * Open the matchcheck.mat in 3dmod if it is not already open
   */
  public void openMatchCheck() throws SystemProcessException {
    matchCheck.open();
  }

  /**
   * Check to see if the matchcheck.mat is open
   */
  public boolean isMatchCheck() {
    return matchCheck.isOpen();
  }

  /**
   * Close the matchcheck.mat tomogram
   */
  public void quitMatchCheck() throws SystemProcessException {
    matchCheck.quit();
  }

  /**
   * Open the trimmed volume in 3dmod if it is not already open
   */
  public void openTrimmedVolume() throws SystemProcessException {
    trimmedVolume.open();
  }

  /**
   * Check to see if the trimmed volume is open
   */
  public boolean isTrimmedVolume() {
    return trimmedVolume.isOpen();
  }

  /**
   * Close thetrimmed volume tomogram
   */
  public void quitTrimmedVolume() throws SystemProcessException {
    trimmedVolume.quit();
  }

  /**
   * Check the axisID argument to see if it is valid given the axisType of the
   * object.
   */
  private void checkAxisID(AxisID axisID) throws AxisTypeException {
    if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.SECOND) {
      throw new AxisTypeException("Second axis requested in a single axis data set");
    }
  }

  /**
   * Select the ImodProcess object indicated by the axisID
   */
  private ImodAssistant selectRawStack(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return rawStackB;
    }
    return rawStackA;
  }

  private ImodAssistant selectErasedStack(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return erasedStackB;
    }
    return erasedStackA;
  }

  private ImodAssistant selectCoarseAligned(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return coarseAlignedB;
    }
    return coarseAlignedA;
  }

  private ImodAssistant selectFineAligned(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fineAlignedB;
    }
    return fineAlignedA;
  }

  private ImodAssistant selectSample(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleB;
    }
    return sampleA;
  }

  private ImodAssistant selectFullVolume(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fullVolumeB;
    }
    return fullVolumeA;
  }

  private ImodAssistant selectFiducialModel(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fiducialModelB;
    }
    return fiducialModelA;
  }

}
