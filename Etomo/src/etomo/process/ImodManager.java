package etomo.process;

import java.lang.IllegalArgumentException;
import java.lang.UnsupportedOperationException;
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

  public static final String RAW_STACK_KEY = new String("rawStack");
  public static final String ERASED_STACK_KEY = new String("erasedStack");
  public static final String COARSE_ALIGNED_KEY = new String("coarseAligned");
  public static final String FINE_ALIGNED_KEY = new String("fineAligned");
  public static final String SAMPLE_KEY = new String("sample");
  public static final String FULL_VOLUME_KEY = new String("fullVolume");
  public static final String COMBINED_TOMOGRAM_KEY =
    new String("combinedTomogram");
  public static final String FIDUCIAL_MODEL_KEY = new String("fiducialModel");
  public static final String TRIMMED_VOLUME_KEY = new String("trimmedVolume");
  public static final String PATCH_VECTOR_MODEL_KEY =
    new String("patchVectorModel");
  public static final String MATCH_CHECK_KEY = new String("matchCheck");
  public static final String TRIAL_TOMOGRAM_KEY = new String("trialTomogram");
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
  private static final String previewKey = PREVIEW_KEY;

  private boolean useMap = true;

  //constructors

  /**
   * Default constructor
   * @param metaData this class is used to initialize the
   * dataset name and axisType of the data to used in imod.
   * @deprecated
   */
  public ImodManager(
    ApplicationManager appMgr,
    ConstMetaData metaData,
    boolean useMap) {
    applicationManager = appMgr;

    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();
    this.useMap = useMap;

    if (!useMap) {
      //  Initialize the necessary ImodProcesses
      if (axisType == AxisType.SINGLE_AXIS) {
        rawStackA = new ImodState(datasetName + ".st");
        erasedStackA = new ImodState(datasetName + "_fixed.st");
        coarseAlignedA = new ImodState(datasetName + ".preali");
        fineAlignedA = new ImodState(datasetName + ".ali");
        //sampleA = new ImodProcess("top.rec mid.rec bot.rec", "tomopitch.mod");
        sampleA =
          new ImodState(
            AxisID.ONLY,
            "top",
            "mid",
            "bot",
            ".rec",
            "tomopitch",
            ".mod");
        fullVolumeA = new ImodState(datasetName + "_full.rec");
        //fullVolumeA.setSwapYZ(true);
        fullVolumeA.initialize(true, false, false);
        combinedTomogram = fullVolumeA;
        fiducialModelA = new ImodState();
      }
      else {
        rawStackA = new ImodState(datasetName + "a.st");
        rawStackB = new ImodState(datasetName + "b.st");
        erasedStackA = new ImodState(datasetName + "a_fixed.st");
        erasedStackB = new ImodState(datasetName + "b_fixed.st");
        coarseAlignedA = new ImodState(datasetName + "a.preali");
        coarseAlignedB = new ImodState(datasetName + "b.preali");
        fineAlignedA = new ImodState(datasetName + "a.ali");
        fineAlignedB = new ImodState(datasetName + "b.ali");
        //sampleA = new ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod")
        sampleA =
          new ImodState(
            AxisID.FIRST,
            "top",
            "mid",
            "bot",
            ".rec",
            "tomopitch",
            ".mod");
        // sampleB = new ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod")
        sampleB =
          new ImodState(
            AxisID.SECOND,
            "top",
            "mid",
            "bot",
            ".rec",
            "tomopitch",
            ".mod");
        fullVolumeA = new ImodState(datasetName + "a.rec");
        //fullVolumeA.setSwapYZ(true)
        fullVolumeA.initialize(true, false, false);
        fullVolumeB = new ImodState(datasetName + "b.rec");
        //fullVolumeB.setSwapYZ(true)
        fullVolumeB.initialize(true, false, false);
        combinedTomogram = new ImodState("sum.rec");
        //combinedTomogram.setSwapYZ(true)
        //combinedTomogram.setModelView(true)
        combinedTomogram.initialize(true, false, false);
        patchVectorModel = new ImodState("patch_vector.mod");
        //patchVectorModel.setModelView(true);
        patchVectorModel.initialize(false, false, true);
        matchCheck = new ImodState("matchcheck.mat matchcheck.rec");
        //matchCheck.setSwapYZ(true)
        //matchCheck.setFillCache(true)
        matchCheck.initialize(true, true, false);
        fiducialModelA = new ImodState();
        fiducialModelB = new ImodState();
      }
      trimmedVolume = new ImodState(datasetName + ".rec");
    }
    else {
      createPrivateKeys();
      if (axisType == AxisType.SINGLE_AXIS) {
        loadSingleAxisMap();
      }
      else {
        loadDualAxisMap();
      }
    }
  }

  /**
   * @deprecated
   * @param appMgr
   * @param metaData
   */
  public ImodManager(ApplicationManager appMgr, ConstMetaData metaData) {
    applicationManager = appMgr;
    imodMap = new HashMap();

    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();
    AxisID axisID;
    createPrivateKeys();
    if (axisType == AxisType.SINGLE_AXIS) {
      loadSingleAxisMap();
    }
    else {
      loadDualAxisMap();
    }
  }

  /**
   * @param appMgr
   */
  public ImodManager(ApplicationManager appMgr) {
    applicationManager = appMgr;
    imodMap = new HashMap();
  }

  //Interface

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

  //old interface
  /**
   * Open both tomograms and their matching models
   * Ignores ImodAssistant configuration
   * @param datasetName
   * @deprecated
   */

  public void matchingModel(String datasetName)
    throws AxisTypeException, SystemProcessException {
    //fullVolumeA.open();
    //fullVolumeA.openModel(datasetName + "a.matmod");
    //fullVolumeA.modelMode();
    ImodState fullVolume = selectFullVolume(AxisID.FIRST);
    fullVolume.open(datasetName + "a.matmod", true);

    //fullVolumeB.open();
    //fullVolumeB.openModel(datasetName + "b.matmod");
    //fullVolumeB.modelMode();
    fullVolume = selectFullVolume(AxisID.SECOND);
    fullVolume.open(datasetName + "b.matmod", true);
  }

  /**
   * Open the patch region model and the volume being matched to if it is not
   * already open
   * Ignores ImodAssistant configuration
   * @param axisID
   * @deprecated
   */
  public void patchRegionModel(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    //if (axisID == AxisID.SECOND) {
    //fullVolumeB.open();
    //fullVolumeB.openModel("patch_region.mod");
    //fullVolumeB.modelMode();
    //}
    //else {
    //fullVolumeA.open();
    //fullVolumeA.openModel("patch_region.mod");
    //fullVolumeA.modelMode(); 

    //}
    ImodState fullVolume = selectFullVolume(axisID);
    fullVolume.open("patch_region.mod", true);
  }

  /**
   * Open the specified raw data stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public void openRawStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(rawStackKey, axisID);
      return;
    }
    checkAxisID(axisID);
    ImodState rawStack = selectRawStack(axisID);
    rawStack.open();
  }

  /**
   * Open the specified model with on raw stack 3dmod
   * @deprecated
   */
  public void modelRawStack(String modelName, AxisID axisID, boolean modelMode)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an imod with right course aligned data set that
    // is already open
    openRawStack(axisID);
    ImodState rawStack = selectRawStack(axisID);
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
   * @deprecated
   */
  public boolean isRawStackOpen(AxisID axisID) throws AxisTypeException {
    ImodState rawStack = selectRawStack(axisID);
    if (rawStack == null) {
      return false;
    }
    //return rawStack.isRunning();
    return rawStack.isOpen();
  }

  /**
   * Close the specified raw stack
   * @deprecated
   */
  public void quitRawStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState rawStack = selectRawStack(axisID);
    rawStack.quit();
  }

  /**
   * Open the specified erased data stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public void openErasedStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(erasedStackKey, axisID);
      return;
    }
    checkAxisID(axisID);
    ImodState erasedStack = selectErasedStack(axisID);
    erasedStack.open();
  }

  /**
   * Open the specified model with the erased stack 3dmod
   * @deprecated
   */
  public void modelErasedStack(String modelName, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an imod with right course aligned data set that
    // is already open
    openErasedStack(axisID);
    ImodState erasedStack = selectErasedStack(axisID);
    //erasedStack.openModel(modelName);
    erasedStack.model(modelName);
  }

  /**
   * Check to see if the specified erased stack is open
   * @deprecated
   */
  public boolean isErasedStackOpen(AxisID axisID) throws AxisTypeException {
    ImodState erasedStack = selectErasedStack(axisID);
    if (erasedStack == null) {
      return false;
    }
    //return erasedStack.isRunning();
    return erasedStack.isOpen();
  }

  /**
   * Close the specified erased stack
   * @deprecated
   */
  public void quitErasedStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState erasedStack = selectErasedStack(axisID);
    erasedStack.quit();
  }

  /**
   * Open the specified coarse aligned stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public void openCoarseAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(coarseAlignedKey, axisID);
      return;
    }
    checkAxisID(axisID);
    ImodState coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.open();
  }

  /**
   * Open the specified model with the course aligned imod
   * @deprecated
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
    ImodState coarseAligned = selectCoarseAligned(axisID);
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

  /**
   * 
   * @param axisID
   * @throws AxisTypeException
   * @throws SystemProcessException
   * @deprecated
   */
  public void openBeadFixer(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.openBeadFixer();
  }

  /**
   * Check to see if the specified coarsely aligned stack is open
   * @deprecated
   */
  public boolean isCoarseAlignedOpen(AxisID axisID) throws AxisTypeException {
    ImodState coarseAligned = selectCoarseAligned(axisID);
    if (coarseAligned == null) {
      return false;
    }
    return coarseAligned.isOpen();
  }

  /**
   * Close the specified coarsely aligned stack
   * @deprecated
   */
  public void quitCoarseAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState coarseAligned = selectCoarseAligned(axisID);
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
    @deprecated
   */
  public void openFiducialModel(String model, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(fiducialModelKey, axisID, model);
      return;
    }
    checkAxisID(axisID);
    ImodState fiducialModel = selectFiducialModel(axisID);
    //fiducialModel.setModelName(model);
    //fiducialModel.setUseModv(true);
    //fiducialModel.setOutputWindowID(false);
    //fiducialModel.open();
    fiducialModel.setUseModv(true);
    fiducialModel.open(model);
  }

  /**
   * Open the specified fine aligned stack in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public void openFineAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(fineAlignedKey, axisID);
      return;
    }
    checkAxisID(axisID);
    ImodState fineAligned = selectFineAligned(axisID);
    fineAligned.open();
  }

  /**
   * Check to see if the specified finely aligned stack is open
   * @deprecated
   */
  public boolean isFineAlignedOpen(AxisID axisID) throws AxisTypeException {
    ImodState fineAligned = selectCoarseAligned(axisID);
    if (fineAligned == null) {
      return false;
    }
    return fineAligned.isOpen();
  }

  /**
   * Close the specified finely aligned stack
   * @deprecated
   */
  public void quitFinelyAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState fineAligned = selectCoarseAligned(axisID);
    fineAligned.quit();
  }

  /**
   * Open the specified tomograph samples in 3dmod if they are not already open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public void openSample(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(sampleKey, axisID);
      return;
    }
    checkAxisID(axisID);
    ImodState sample = selectSample(axisID);
    //sample.open();
    //sample.modelMode();
    sample.setUseMode(true);
    sample.setMode(ImodState.MODEL);
    sample.open();
  }

  /**
   * Check to see if the specified sample reconstruction is open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public boolean isSampleOpen(AxisID axisID) throws AxisTypeException {
    ImodState sample = selectSample(axisID);
    if (sample == null) {
      return false;
    }
    return sample.isOpen();
  }

  /**
   * Close the specifed sample reconstruction
   * @deprecated
   */
  public void quitSample(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState sample = selectSample(axisID);
    sample.quit();
  }

  /**
   * Open the specified tomogram in 3dmod if it is not already open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public void openFullVolume(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      reset(fullVolumeKey, axisID);
      open(fullVolumeKey, axisID);
      return;
    }
    checkAxisID(axisID);
    ImodState fullVolume = selectFullVolume(axisID);
    // Remove any model references that may be left over from earlier matching
    // or patch region model instances
    //fullVolume.setModelName("");
    //fullVolume.open();
    fullVolume.open("");
  }

  /**
   * Check to see if the specified tomogram is open
   * @param axisID the AxisID of the desired axis.
   * @deprecated
   */
  public boolean isFullVolumeOpen(AxisID axisID) throws AxisTypeException {
    ImodState tomogram = selectFullVolume(axisID);
    if (tomogram == null) {
      return false;
    }
    return tomogram.isOpen();
  }

  /**
   * Close the specified tomogram
   * @deprecated
   */
  public void quitFullVolume(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodState tomogram = selectFullVolume(axisID);
    tomogram.quit();
  }

  /**
   * Open the patch vector in 3dmod if it is not already open
   * @deprecated
   */
  public void openPatchVectorModel()
    throws AxisTypeException, SystemProcessException {
    //patchVectorModel.open();
    //patchVectorModel.modelMode();
    patchVectorModel = get(patchVectorModelKey);
    patchVectorModel.setUseMode(true);
    patchVectorModel.setMode(ImodState.MODEL);
    patchVectorModel.open();
  }

  /**
   * Check to see if the patch vector model is open
   * @deprecated
   */
  public boolean ispatchVectorModelOpen() throws AxisTypeException {
    if (useMap) {
      return isOpen(patchVectorModelKey);
    }
    return patchVectorModel.isOpen();
  }

  /**
   * Close the patch vector model
   * @deprecated
   */
  public void quitPatchVectorModel()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      quit(patchVectorModelKey);
      return;
    }
    patchVectorModel.quit();
  }

  /**
   * Open the combined tomogram in 3dmod if it is not already open
   * @deprecated
   */
  public void openCombinedTomogram()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(combinedTomogramKey);
      return;
    }
    combinedTomogram.open();
  }

  /**
   * Check to see if the combined tomogram is open
   * @deprecated
   */
  public boolean isCombinedTomogramOpen() throws AxisTypeException {
    if (useMap) {
      return isOpen(combinedTomogramKey);
    }
    return combinedTomogram.isOpen();
  }

  /**
   * Close the combined tomogram
   * @deprecated
   */
  public void quitCombinedTomogram()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      quit(combinedTomogramKey);
      return;
    }
    combinedTomogram.quit();
  }

  /**
   * Open the matchcheck.mat in 3dmod if it is not already open
   * @deprecated
   */
  public void openMatchCheck()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(matchCheckKey);
      return;
    }
    matchCheck.open();
  }

  /**
   * Check to see if the matchcheck.mat is open
   * @deprecated
   */
  public boolean isMatchCheck() throws AxisTypeException {
    if (useMap) {
      return isOpen(matchCheckKey);
    }
    return matchCheck.isOpen();
  }

  /**
   * Close the matchcheck.mat tomogram
   * @deprecated
   */
  public void quitMatchCheck()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      quit(matchCheckKey);
      return;
    }
    matchCheck.quit();
  }

  /**
   * Open the trimmed volume in 3dmod if it is not already open
   * @deprecated
   */
  public void openTrimmedVolume()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      open(trimmedVolumeKey);
      return;
    }
    trimmedVolume.open();
  }

  /**
   * Check to see if the trimmed volume is open
   * @deprecated
   */
  public boolean isTrimmedVolume() throws AxisTypeException {
    if (useMap) {
      return isOpen(trimmedVolumeKey);
    }
    return trimmedVolume.isOpen();
  }

  /**
   * Close thetrimmed volume tomogram
   * @deprecated
   */
  public void quitTrimmedVolume()
    throws AxisTypeException, SystemProcessException {
    if (useMap) {
      quit(trimmedVolumeKey);
      return;
    }
    trimmedVolume.quit();
  }

  /**
   * Check the axisID argument to see if it is valid given the axisType of the
   * object.
   * @deprecated
   */
  private void checkAxisID(AxisID axisID) throws AxisTypeException {
    if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.SECOND) {
      throw new AxisTypeException("Second axis requested in a single axis data set");
    }
  }

  /**
   * Select the ImodProcess object indicated by the axisID
   * @deprecated
   */
  private ImodState selectRawStack(AxisID axisID) throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return rawStackB;
      }
      return rawStackA;
    }
    else
      return get(rawStackKey, axisID);

  }

  /**
   * @deprecated
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  private ImodState selectErasedStack(AxisID axisID) throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return erasedStackB;
      }
      return erasedStackA;
    }
    else
      return get(erasedStackKey, axisID);
  }

  /**
   * @deprecated
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  private ImodState selectCoarseAligned(AxisID axisID)
    throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return coarseAlignedB;
      }
      return coarseAlignedA;
    }
    else
      return get(coarseAlignedKey, axisID);
  }

  /**
   * @deprecated
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  private ImodState selectFineAligned(AxisID axisID) throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return fineAlignedB;
      }
      return fineAlignedA;
    }
    else
      return get(fineAlignedKey, axisID);
  }

  /**
   * @deprecated
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  private ImodState selectSample(AxisID axisID) throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return sampleB;
      }
      return sampleA;
    }
    else
      return get(sampleKey, axisID);
  }

  /**
   * @deprecated
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  private ImodState selectFullVolume(AxisID axisID) throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return fullVolumeB;
      }
      return fullVolumeA;
    }
    else
      return get(fullVolumeKey, axisID);
  }

  /**
   * @deprecated
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  private ImodState selectFiducialModel(AxisID axisID)
    throws AxisTypeException {
    if (!useMap) {
      if (axisID == AxisID.SECOND) {
        return fiducialModelB;
      }
      return fiducialModelA;
    }
    else
      return get(fiducialModelKey, axisID);
  }

}
