package etomo.process;

import java.util.HashMap;
import java.util.Vector;
import java.util.Set;
import java.util.Iterator;
import java.io.File;

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
 * <p> Revision 3.18  2004/04/28 00:39:43  sueh
 * <p> bug# 320 changed warnStaleFile() message
 * <p>
 * <p> Revision 3.17  2004/04/27 23:17:03  sueh
 * <p> bug# 320 added warnStaleFile() to tell user and a file that has
 * <p> changed on disk and ask to close it
 * <p>
 * <p> Revision 3.16  2004/03/29 20:54:09  sueh
 * <p> bug# 409 add MTF Filter
 * <p>
 * <p> Revision 3.15  2004/03/07 22:35:04  sueh
 * <p> bug# 399 removed deprecated code
 * <p>
 * <p> Revision 3.14  2004/02/25 22:44:42  sueh
 * <p> bug# 403 comments - clarified setMetaData
 * <p>
 * <p> Revision 3.13  2004/02/16 18:49:13  sueh
 * <p> bug# 276 added getModelName()
 * <p>
 * <p> Revision 3.12  2004/02/07 02:58:29  sueh
 * <p> bug# 169 Added preview key, deprecated out-of-date
 * <p> functions, changed the metadata load, created an open()
 * <p> function which uses the vector index, fixed a problem in
 * <p> create(String,AxisID,String).
 * <p>
 * <p> Revision 3.11  2004/02/05 18:03:19  sueh
 * <p> bug# 306 added setSwapYZ - used to set swapYZ before
 * <p> opening 3dmod
 * <p>
 * <p> Revision 3.10  2004/02/04 18:09:44  sueh
 * <p> bug# 171 added isOpen() to find out if any 3dmod is open,
 * <p> added quit() to quit all 3dmods
 * <p>
 * <p> Revision 3.9  2003/12/04 22:01:27  sueh
 * <p> bug242 Added create() functions, to create an ImodState
 * <p> without opening it.  Deprecating old interface.  fixing
 * <p> openFullVolume() - should come up in movie mode.
 * <p>
 * <p> Revision 3.8  2003/12/03 16:39:06  sueh
 * <p> bug242 put the ImodStates into Vectors.  Allow multiple
 * <p> ImodStates on each key
 * <p>
 * <p> Revision 3.7  2003/12/02 23:16:40  sueh
 * <p> bug242 changed from ImodAssistant to ImodState, added
 * <p> reset() function to handle FullVolume.
 * <p>
 * <p> Revision 3.6  2003/12/01 17:09:54  sueh
 * <p> bug242 Allowing ImodAssistants to be created on the fly.
 * <p>
 * <p> Revision 3.5  2003/11/25 22:53:46  sueh
 * <p> bug242 removed the last dependency on non-map
 * <p> ImodAssistants, moved constant 3dmod settings to the
 * <p> constructor
 * <p>
 * <p> Revision 3.4  2003/11/22 00:08:13  sueh
 * <p> bug242 quitFinelyAligned ignoring axisID and only quitting B
 * <p> axis - fixed
 * <p>
 * <p> Revision 3.3  2003/11/21 23:53:11  sueh
 * <p> bug242 ImodManager -  incorporated new ImodAssistant
 * <p> interface changes, created generic functions, created the
 * <p> map of ImodAssistants, allow existing functions to use the
 * <p> map
 * <p>
 * <p> Revision 3.2  2003/11/15 01:42:05  sueh
 * <p> bug242 switched from ImodProcess to ImodAssistant
 * <p> without generalizing the code
 * <p>
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
  private AxisType axisType = AxisType.SINGLE_AXIS;
  private String datasetName = "";
  private HashMap imodMap;

  protected ImodState rawStackA;
  protected ImodState rawStackB;
  protected ImodState erasedStackA;
  protected ImodState erasedStackB;

  protected ImodState coarseAlignedA;
  protected ImodState coarseAlignedB;
  protected ImodState fineAlignedA;
  protected ImodState fineAlignedB;
  protected ImodState sampleA;
  protected ImodState sampleB;
  protected ImodState fullVolumeA;
  protected ImodState fullVolumeB;
  protected ImodState combinedTomogram;
  protected ImodState patchVectorModel;
  protected ImodState matchCheck;
  protected ImodState trimmedVolume;
  protected ImodState fiducialModelA;
  protected ImodState fiducialModelB;

  private boolean metaDataSet = false;

  //public keys

  public static final String RAW_STACK_KEY = new String("raw stack");
  public static final String ERASED_STACK_KEY = new String("erased stack");
  public static final String COARSE_ALIGNED_KEY = new String("coarse aligned");
  public static final String FINE_ALIGNED_KEY = new String("fine aligned");
  public static final String SAMPLE_KEY = new String("sample");
  public static final String FULL_VOLUME_KEY = new String("full volume");
  public static final String COMBINED_TOMOGRAM_KEY =
    new String("combined tomogram");
  public static final String FIDUCIAL_MODEL_KEY = new String("fiducial model");
  public static final String TRIMMED_VOLUME_KEY = new String("trimmed volume");
  public static final String PATCH_VECTOR_MODEL_KEY =
    new String("patch vector model");
  public static final String MATCH_CHECK_KEY = new String("match check");
  public static final String TRIAL_TOMOGRAM_KEY = new String("trial tomogram");
  public static final String MTF_FILTER_KEY = new String("mtf filter");
  public static final String PREVIEW_KEY = new String("preview");

  //private keys - used with imodMap
  private static final String rawStackKey = RAW_STACK_KEY;
  private static final String erasedStackKey = ERASED_STACK_KEY;
  private static final String coarseAlignedKey = COARSE_ALIGNED_KEY;
  private static final String fineAlignedKey = FINE_ALIGNED_KEY;
  private static final String sampleKey = SAMPLE_KEY;
  private static final String fullVolumeKey = FULL_VOLUME_KEY;
  private String combinedTomogramKey;
  private static final String fiducialModelKey = FIDUCIAL_MODEL_KEY;
  private static final String trimmedVolumeKey = TRIMMED_VOLUME_KEY;
  private static final String patchVectorModelKey = PATCH_VECTOR_MODEL_KEY;
  private static final String matchCheckKey = MATCH_CHECK_KEY;
  private static final String trialTomogramKey = TRIAL_TOMOGRAM_KEY;
  private static final String mtfFilterKey = MTF_FILTER_KEY;
  private static final String previewKey = PREVIEW_KEY;

  private boolean useMap = true;

  //constructors

  /**
   * @param appMgr
   */
  public ImodManager(ApplicationManager appMgr) {
    applicationManager = appMgr;
    imodMap = new HashMap();
  }

  //Interface

  /**
   * for running 3dmod from the SetupDialog
   */
  public void setPreviewMetaData(ConstMetaData metaData) {
    if (metaDataSet) {
      return;
    }
    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();
    AxisID axisID;
    createPrivateKeys();
  }

  public void setMetaData(ConstMetaData metaData) {
    //if metaDataSet is true and the axisType is changing from dual to single,
    //combinedTomograms will not be retrievable.  However the global isOpen()
    //and quit() functions will work on them.
    metaDataSet = true;
    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();
    createPrivateKeys();
    if (axisType == AxisType.SINGLE_AXIS) {
      loadSingleAxisMap();
    }
    else {
      loadDualAxisMap();
    }
  }

  public int create(String key)
    throws AxisTypeException, SystemProcessException {
    return create(key, null, null);
  }

  public int create(String key, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    return create(key, axisID, null);
  }

  public int create(String key, AxisID axisID, String datasetName)
    throws AxisTypeException, SystemProcessException {
    Vector vector;
    ImodState imodState;
    key = getPrivateKey(key);
    vector = getVector(key, axisID);
    if (vector == null) {
      vector = newVector(key, axisID, datasetName);
      if (axisID == null) {
        imodMap.put(key, vector);
      }
      else {
        imodMap.put(key + axisID.getExtension(), vector);
      }
      return 0;
    }
    imodState = newImodState(key, axisID, datasetName);
    vector.add(imodState);
    return vector.lastIndexOf(imodState);
  }

  public void open(String key)
    throws AxisTypeException, SystemProcessException {
    open(key, null, null);
    //used for:
    //openCombinedTomogram
  }

  public void open(String key, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    open(key, axisID, null);
    //used for:
    //openCoarseAligned
    //openErasedStack
    //openFineAligned
  }

  public void open(String key, AxisID axisID, String model)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      create(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      if (model == null) {
        imodState.open();
      }
      else {
        imodState.open(model);
      }
    }
    //used for:
    //openFiducialModel
  }

  public void open(String key, AxisID axisID, int vectorIndex)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(
        key
          + " was not created in "
          + axisType.toString()
          + " with axisID="
          + axisID.getExtension()
          + " at index "
          + vectorIndex);
    }
    imodState.open();
  }

  public boolean isOpen(String key) throws AxisTypeException {
    return isOpen(key, null);
  }

  public boolean isOpen(String key, AxisID axisID) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return false;
    }
    return imodState.isOpen();
  }

  public boolean isOpen() throws AxisTypeException {
    if (imodMap.size() == 0) {
      return false;
    }
    Set set = imodMap.keySet();
    Iterator iterator = set.iterator();
    while (iterator.hasNext()) {
      ImodState imodState = get((String) iterator.next(), true);
      if (imodState != null && imodState.isOpen()) {
        return true;
      }
    }
    return false;
  }

  public void model(String key, AxisID axisID, String modelName)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      create(key, axisID);
      imodState = get(key, axisID);
    }
    imodState.open();
    if (imodState != null) {
      imodState.model(modelName);
    }
    // erasedStack.model(modelName);
  }

  public void model(
    String key,
    AxisID axisID,
    String modelName,
    boolean modelMode)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      create(key, axisID);
      imodState = get(key, axisID);
    }
    imodState.open();
    if (imodState != null) {
      imodState.model(modelName, modelMode);
    }
    //    rawStack.model(modelName, modelMode);
  }

  public void model(
    String key,
    AxisID axisID,
    String modelName,
    boolean modelMode,
    boolean preserveContrast)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      create(key, axisID);
      imodState = get(key, axisID);
    }
    imodState.open();
    if (imodState != null) {
      imodState.setPreserveContrast(preserveContrast);
      imodState.model(modelName, modelMode);
    }
    //coarseAligned.setPreserveContrast(preserveConstrast);
    //coarseAligned.model(modelName, modelMode);
  }
  
  public String getModelName(String key, AxisID axisID) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return "";
    }
    return imodState.getModelName();
  }

  public void openBeadFixer(String key, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState.isUseModv()) {
      throw new UnsupportedOperationException("The Bead Fixer cannot be opened in 3dmodv");
    }
    imodState.openBeadFixer();
  }

  public void quit(String key)
    throws AxisTypeException, SystemProcessException {
    quit(key, null);
    //combinedTomogram.quit();
  }

  public void quit(String key, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState != null) {
      imodState.quit();
    }
    //coarseAligned.quit();
  }

  public void quit() throws AxisTypeException, SystemProcessException {
    if (imodMap.size() == 0) {
      return;
    }
    Set set = imodMap.keySet();
    Iterator iterator = set.iterator();
    while (iterator.hasNext()) {
      Vector vector = getVector((String) iterator.next(), true);
      int size = vector.size();
      for (int i = 0; i < size; i++) {
        ImodState imodState = (ImodState) vector.get(i);
        if (imodState != null) {
          imodState.quit();
        }
      }
    }
  }
  
  public void reset(String key, AxisID axisID) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState != null) {
      imodState.reset();
    }
  }

  public void setSwapYZ(String key, boolean swapYZ) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState != null) {
      imodState.setSwapYZ(swapYZ);
    }
  }

  public void setWorkingDirectory(
    String key,
    AxisID axisID,
    int vectorIndex,
    File workingDirectory)
    throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID, vectorIndex);
    if (imodState != null) {
      imodState.setWorkingDirectory(workingDirectory);
    }
  }
  
  public boolean warnStaleFile(String key, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (!imodState.isWarnedStaleFile() && imodState.isOpen()) {
      imodState.setWarnedStaleFile(true);
      return true;
    }
    return false;
  }

  //protected methods

  protected Vector newVector(ImodState imodState) {
    Vector vector = new Vector(1);
    vector.add(imodState);
    return vector;
  }

  protected Vector newVector(String key) {
    return newVector(newImodState(key));
  }

  protected Vector newVector(String key, AxisID axisID) {
    return newVector(newImodState(key, axisID));
  }

  protected Vector newVector(String key, AxisID axisID, String datasetName) {
    return newVector(newImodState(key, axisID, datasetName));
  }

  protected ImodState newImodState(String key) {
    return newImodState(key, null, null);
  }

  protected ImodState newImodState(String key, AxisID axisID) {
    return newImodState(key, axisID, null);
  }

  protected ImodState newImodState(
    String key,
    AxisID axisID,
    String datasetName) {
    if (key.equals(RAW_STACK_KEY) && axisID != null) {
      return newRawStack(axisID);
    }
    if (key.equals(ERASED_STACK_KEY) && axisID != null) {
      return newErasedStack(axisID);
    }
    if (key.equals(COARSE_ALIGNED_KEY) && axisID != null) {
      return newCoarseAligned(axisID);
    }
    if (key.equals(FINE_ALIGNED_KEY) && axisID != null) {
      return newFineAligned(axisID);
    }
    if (key.equals(SAMPLE_KEY) && axisID != null) {
      return newSample(axisID);
    }
    if (key.equals(FULL_VOLUME_KEY) && axisID != null) {
      return newFullVolume(axisID);
    }
    if (key.equals(COMBINED_TOMOGRAM_KEY) && axisType == AxisType.DUAL_AXIS) {
      return newCombinedTomogram();
    }
    if (key.equals(PATCH_VECTOR_MODEL_KEY) && axisType == AxisType.DUAL_AXIS) {
      return newPatchVectorModel();
    }
    if (key.equals(MATCH_CHECK_KEY) && axisType == AxisType.DUAL_AXIS) {
      return newMatchCheck();
    }
    if (key.equals(FIDUCIAL_MODEL_KEY)) {
      return newFiducialModel();
    }
    if (key.equals(TRIMMED_VOLUME_KEY)) {
      return newTrimmedVolume();
    }
    if (key.equals(TRIAL_TOMOGRAM_KEY)
      && axisID != null
      && datasetName != null) {
      return newTrialTomogram(axisID, datasetName);
    }
    if (key.equals(MTF_FILTER_KEY) && axisID != null) {
      return newMtfFilter(axisID);
    }
    if (key.equals(PREVIEW_KEY) && axisID != null) {
      return newPreview(axisID);
    }
    throw new IllegalArgumentException(
      key
        + " cannot be created in "
        + axisType.toString()
        + " with axisID="
        + axisID.getExtension());
  }

  protected void createPrivateKeys() {
    if (axisType == AxisType.SINGLE_AXIS) {
      combinedTomogramKey = FULL_VOLUME_KEY;
    }
    else {
      combinedTomogramKey = COMBINED_TOMOGRAM_KEY;
    }
  }

  protected String getPrivateKey(String publicKey) {
    if (publicKey.equals(COMBINED_TOMOGRAM_KEY)) {
      return combinedTomogramKey;
    }
    else
      return publicKey;
  }

  protected void loadSingleAxisMap() {
    ImodState imodState;
    imodMap.put(rawStackKey, newVector(newRawStack(AxisID.ONLY)));
    imodMap.put(erasedStackKey, newVector(newErasedStack(AxisID.ONLY)));
    imodMap.put(coarseAlignedKey, newVector(newCoarseAligned(AxisID.ONLY)));
    imodMap.put(fineAlignedKey, newVector(newFineAligned(AxisID.ONLY)));
    imodMap.put(sampleKey, newVector(newSample(AxisID.ONLY)));
    imodMap.put(fullVolumeKey, newVector(newFullVolume(AxisID.ONLY)));
    imodMap.put(fiducialModelKey, newVector(newFiducialModel()));
    imodMap.put(trimmedVolumeKey, newVector(newTrimmedVolume()));
    imodMap.put(mtfFilterKey, newVector(newMtfFilter(AxisID.ONLY)));
  }

  protected void loadDualAxisMap() {
    ImodState imodState;
    imodMap.put(
      rawStackKey + AxisID.FIRST.getExtension(),
      newVector(newRawStack(AxisID.FIRST)));
    imodMap.put(
      rawStackKey + AxisID.SECOND.getExtension(),
      newVector(newRawStack(AxisID.SECOND)));
    imodMap.put(
      erasedStackKey + AxisID.FIRST.getExtension(),
      newVector(newErasedStack(AxisID.FIRST)));
    imodMap.put(
      erasedStackKey + AxisID.SECOND.getExtension(),
      newVector(newErasedStack(AxisID.SECOND)));
    imodMap.put(
      coarseAlignedKey + AxisID.FIRST.getExtension(),
      newVector(newCoarseAligned(AxisID.FIRST)));
    imodMap.put(
      coarseAlignedKey + AxisID.SECOND.getExtension(),
      newVector(newCoarseAligned(AxisID.SECOND)));
    imodMap.put(
      fineAlignedKey + AxisID.FIRST.getExtension(),
      newVector(newFineAligned(AxisID.FIRST)));
    imodMap.put(
      fineAlignedKey + AxisID.SECOND.getExtension(),
      newVector(newFineAligned(AxisID.SECOND)));
    imodMap.put(
      sampleKey + AxisID.FIRST.getExtension(),
      newVector(newSample(AxisID.FIRST)));
    imodMap.put(
      sampleKey + AxisID.SECOND.getExtension(),
      newVector(newSample(AxisID.SECOND)));
    imodMap.put(
      fullVolumeKey + AxisID.FIRST.getExtension(),
      newVector(newFullVolume(AxisID.FIRST)));
    imodMap.put(
      fullVolumeKey + AxisID.SECOND.getExtension(),
      newVector(newFullVolume(AxisID.SECOND)));
    imodMap.put(combinedTomogramKey, newVector(newCombinedTomogram()));
    imodMap.put(patchVectorModelKey, newVector(newPatchVectorModel()));
    imodMap.put(matchCheckKey, newVector(newMatchCheck()));
    imodMap.put(
      fiducialModelKey + AxisID.FIRST.getExtension(),
      newVector(newFiducialModel()));
    imodMap.put(
      fiducialModelKey + AxisID.SECOND.getExtension(),
      newVector(newFiducialModel()));
    imodMap.put(trimmedVolumeKey, newVector(newTrimmedVolume()));
    imodMap.put(
      mtfFilterKey + AxisID.FIRST.getExtension(),
      newVector(newMtfFilter(AxisID.FIRST)));
    imodMap.put(
    mtfFilterKey + AxisID.SECOND.getExtension(),
      newVector(newMtfFilter(AxisID.SECOND)));
  }

  protected ImodState newRawStack(AxisID axisID) {
    ImodState imodState = new ImodState(axisID, datasetName, ".st");
    return imodState;
  }
  protected ImodState newErasedStack(AxisID axisID) {
    ImodState imodState = new ImodState(axisID, datasetName, "_fixed.st");
    return imodState;
  }
  protected ImodState newCoarseAligned(AxisID axisID) {
    ImodState imodState = new ImodState(axisID, datasetName, ".preali");
    return imodState;
  }
  protected ImodState newFineAligned(AxisID axisID) {
    ImodState imodState = new ImodState(axisID, datasetName, ".ali");
    return imodState;
  }
  protected ImodState newSample(AxisID axisID) {
    ImodState imodState =
      new ImodState(axisID, "top", "mid", "bot", ".rec", "tomopitch", ".mod");
    imodState.setUseMode(true);
    imodState.setMode(ImodState.MODEL);
    return imodState;
  }
  protected ImodState newFullVolume(AxisID axisID) {
    ImodState imodState;
    if (axisType == AxisType.SINGLE_AXIS) {
      imodState = new ImodState(datasetName + "_full.rec");
    }
    else {
      imodState = new ImodState(axisID, datasetName, ".rec");
    }
    imodState.initialize(true, false, false);
    return imodState;
  }
  protected ImodState newCombinedTomogram() {
    ImodState imodState = new ImodState("sum.rec");
    imodState.initialize(true, false, false);
    return imodState;
  }
  protected ImodState newPatchVectorModel() {
    ImodState imodState = new ImodState("patch_vector.mod");
    imodState.initialize(false, false, true);
    imodState.setUseMode(true);
    imodState.setMode(ImodState.MODEL);
    return imodState;
  }
  protected ImodState newMatchCheck() {
    ImodState imodState = new ImodState("matchcheck.mat matchcheck.rec");
    imodState.initialize(true, true, false);
    return imodState;
  }
  protected ImodState newFiducialModel() {
    ImodState imodState = new ImodState();
    imodState.setUseModv(true);
    return imodState;
  }
  protected ImodState newTrimmedVolume() {
    ImodState imodState = new ImodState(datasetName + ".rec");
    return imodState;
  }
  protected ImodState newTrialTomogram(AxisID axisID, String datasetName) {
    ImodState imodState = new ImodState(datasetName);
    imodState.initialize(true, false, false);
    return imodState;
  }
  protected ImodState newMtfFilter(AxisID axisID) {
    ImodState imodState = new ImodState(axisID, datasetName, "_filt.ali");
    return imodState;
  }
  protected ImodState newPreview(AxisID axisID) {
    ImodState imodState = new ImodState(axisID, datasetName, ".st");
    return imodState;
  }
  protected boolean isPerAxis(String key) {
    if (key.equals(COMBINED_TOMOGRAM_KEY)
      || key.equals(PATCH_VECTOR_MODEL_KEY)
      || key.equals(MATCH_CHECK_KEY)
      || key.equals(TRIMMED_VOLUME_KEY)) {
      return false;
    }
    return true;
  }

  protected boolean isDualAxisOnly(String key) {
    if (key.equals(COMBINED_TOMOGRAM_KEY)
      || key.equals(PATCH_VECTOR_MODEL_KEY)
      || key.equals(MATCH_CHECK_KEY)) {
      return true;
    }
    return false;
  }

  protected ImodState get(String key) throws AxisTypeException {
    Vector vector = getVector(key);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.lastElement();
  }

  protected ImodState get(String key, boolean axisIdInKey)
    throws AxisTypeException {
    Vector vector = getVector(key, axisIdInKey);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.lastElement();
  }

  protected ImodState get(String key, AxisID axisID) throws AxisTypeException {
    Vector vector = getVector(key, axisID);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.lastElement();
  }

  protected ImodState get(String key, AxisID axisID, int vectorIndex)
    throws AxisTypeException {
    Vector vector = getVector(key, axisID);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.get(vectorIndex);
  }

  protected Vector getVector(String key) throws AxisTypeException {
    Vector vector;
    if (!useMap) {
      throw new UnsupportedOperationException("This operation is not supported when useMap is false");
    }
    if (axisType == AxisType.SINGLE_AXIS && isDualAxisOnly(key)) {
      throw new AxisTypeException(
        key + " cannot be found in " + axisType.toString());
    }
    if (isDualAxisOnly(key) && isPerAxis(key)) {
      throw new UnsupportedOperationException(
        key + " cannot be found without axisID information");
    }
    if (isPerAxis(key)) {
      vector = (Vector) imodMap.get(key + AxisID.ONLY.getExtension());
    }
    else {
      vector = (Vector) imodMap.get(key);
    }
    if (vector == null) {
      return null;
    }
    return (Vector) vector;
  }

  protected Vector getVector(String key, boolean axisIdInKey)
    throws AxisTypeException {
    if (!axisIdInKey) {
      return getVector(key);
    }
    return (Vector) imodMap.get(key);
  }

  protected Vector getVector(String key, AxisID axisID)
    throws AxisTypeException {
    Vector vector;
    if (axisID == null) {
      return getVector(key);
    }
    if (!useMap) {
      throw new UnsupportedOperationException("This operation is not supported when useMap is false");
    }
    if (axisType == AxisType.SINGLE_AXIS) {
      if (isDualAxisOnly(key)) {
        throw new AxisTypeException(
          key + " cannot be found in " + axisType.toString());
      }
      if (axisID != AxisID.ONLY) {
        axisID = AxisID.ONLY;
      }
    }
    if (!isPerAxis(key)) {
      vector = (Vector) imodMap.get(key);
    }
    else {
      vector = (Vector) imodMap.get(key + axisID.getExtension());
    }
    if (vector == null) {
      return null;
    }
    return (Vector) vector;
  }
}
