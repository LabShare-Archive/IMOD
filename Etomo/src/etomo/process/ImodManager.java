package etomo.process;
import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.ConstMetaData;

/*
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

  private ApplicationManager appManager;
  private AxisType axisType;
  private String filesetName;

  private ImodProcess rawStackA;
  private ImodProcess rawStackB;
  private ImodProcess coarseAlignedA;
  private ImodProcess coarseAlignedB;
  private ImodProcess fineAlignedA;
  private ImodProcess fineAlignedB;
  private ImodProcess sampleA;
  private ImodProcess sampleB;
  private ImodProcess tomogramA;
  private ImodProcess tomogramB;
  private ImodProcess combinedTomogram;

  /**
   * Default constructor
   * @param metaData this class is used to initialize the
   * fileset name and axisType of the data to used in imod.
   */
  public ImodManager(ConstMetaData metaData) {
    axisType = metaData.getAxisType();
    filesetName = metaData.getFilesetName();

    //  Initialize the necessary ImodProcesses
    if (axisType == AxisType.SINGLE_AXIS) {
      rawStackA = new ImodProcess(filesetName + ".st");
      coarseAlignedA = new ImodProcess(filesetName + ".preali");
      fineAlignedA = new ImodProcess(filesetName + ".ali");
      sampleA = new ImodProcess("top.rec mid.rec bot.rec", "tomopitch.mod");
      tomogramA = new ImodProcess(filesetName + ".rec");
      tomogramA.setSwapYZ(true);
      
    }
    else {
      rawStackA = new ImodProcess(filesetName + "a.st");
      rawStackB = new ImodProcess(filesetName + "b.st");
      coarseAlignedA = new ImodProcess(filesetName + "a.preali");
      coarseAlignedB = new ImodProcess(filesetName + "b.preali");
      fineAlignedA = new ImodProcess(filesetName + "a.ali");
      fineAlignedB = new ImodProcess(filesetName + "b.ali");
      sampleA = new ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod");
      sampleB = new ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod");
      tomogramA = new ImodProcess(filesetName + "a.rec");
      tomogramA.setSwapYZ(true);
      tomogramB = new ImodProcess(filesetName + "b.rec");
      tomogramB.setSwapYZ(true);
    }

  }

  /**
   * Open the specified raw data stack in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openRawStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess rawStack = selectRawStack(axisID);
    rawStack.open();
  }

  /**
   * Open the specified model with the course aligned imod
   */
  public void modelRawStack(String modelName, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an imod with right course aligned data set that
    // is already open
    openRawStack(axisID);
    ImodProcess rawStack = selectRawStack(axisID);
    rawStack.openModel(modelName);
  }

  /**
   * Check to see if the specified raw stack is open
   */
  public boolean isRawStackOpen(AxisID axisID) {
    ImodProcess rawStack = selectRawStack(axisID);
    if (rawStack == null) {
      return false;
    }
    return rawStack.isRunning();
  }

  /**
   * Close the specified raw stack
   */
  public void quitRawStack(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess rawStack = selectRawStack(axisID);
    rawStack.quit();
  }

  /**
   * Open the specified coarse aligned stack in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openCoarseAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.open();
  }

  /**
   * Open the specified model with the course aligned imod
   */
  public void modelCoarseAligned(String modelName, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    // Make sure there is an imod with right coarse aligned data set that
    // is already open
    openCoarseAligned(axisID);
    ImodProcess coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.openModel(modelName);
  }

  /**
   * Check to see if the specified coarsely aligned stack is open
   */
  public boolean isCoarseAlignedOpen(AxisID axisID) {
    ImodProcess coarseAligned = selectCoarseAligned(axisID);
    if (coarseAligned == null) {
      return false;
    }
    return coarseAligned.isRunning();
  }

  /**
   * Close the specified coarsely aligned stack
   */
  public void quitCoarseAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess coarseAligned = selectCoarseAligned(axisID);
    coarseAligned.quit();
  }

  /**
   * Open the specified fine aligned stack in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openFineAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess fineAligned = selectCoarseAligned(axisID);
    fineAligned.open();
  }

  /**
   * Check to see if the specified finely aligned stack is open
   */
  public boolean isFineAlignedOpen(AxisID axisID) {
    ImodProcess fineAligned = selectCoarseAligned(axisID);
    if (fineAligned == null) {
      return false;
    }
    return fineAligned.isRunning();
  }

  /**
   * Close the specified finely aligned stack
   */
  public void quitFinelyAligned(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess fineAligned = selectCoarseAligned(axisID);
    fineAlignedB.quit();
  }

  /**
   * Open the specified tomograph samples in imod if they are not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openSample(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess sample = selectSample(axisID);
    sample.open();
  }

  /**
   * Check to see if the specified sample reconstruction is open
   * @param axisID the AxisID of the desired axis.
   */
  public boolean isSampleOpen(AxisID axisID) {
    ImodProcess sample = selectSample(axisID);
    if (sample == null) {
      return false;
    }
    return sample.isRunning();
  }

  /**
   * Close the specifed sample reconstruction
   */
  public void quitSample(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess sample = selectSample(axisID);
    sample.quit();
  }

  /**
   * Open the specified tomogram in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openTomogram(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess tomogram = selectTomogram(axisID);
    tomogram.open();
  }

  /**
   * Check to see if the specified tomogram is open
   * @param axisID the AxisID of the desired axis.
   */
  public boolean isTomogramOpen(AxisID axisID){
    ImodProcess tomogram = selectTomogram(axisID);
    if (tomogram == null) {
      return false;
    }
    return tomogram.isRunning();
  }

  /**
   * Close the specified tomogram
   */
  public void quitTomogram(AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    checkAxisID(axisID);
    ImodProcess tomogram = selectTomogram(axisID);
    tomogram.quit();
  }

  /**
   * Open the combined tomogram in imod if it is not already open
   */
  public void openCombinedTomogram() throws SystemProcessException {
    combinedTomogram.open();
  }

  /**
   * Check to see if the combined tomogram is open
   */
  public boolean isCombinedTomogramOpen() {
    return combinedTomogram.isRunning();
  }

  /**
   * Close the combined tomogram
   */
  public void quitCombinedTomogram(AxisID axisID)
    throws SystemProcessException {
    combinedTomogram.quit();
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
  private ImodProcess selectRawStack(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return rawStackB;
    }
    return rawStackA;
  }

  private ImodProcess selectCoarseAligned(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return coarseAlignedB;
    }
    return coarseAlignedA;
  }

  private ImodProcess selectFineAligned(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fineAlignedB;
    }
    return fineAlignedA;
  }

  private ImodProcess selectSample(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleB;
    }
    return sampleA;
  }

  private ImodProcess selectTomogram(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramB;
    }
    return tomogramA;
  }

}
