/**
 * <p>Description: </p>
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
 * <p> Revision 3.16  2011/09/08 05:38:34  sueh
 * <p> Bug# 1545 running with runpyscript.
 * <p>
 * <p> Revision 3.15  2011/02/22 03:37:49  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.14  2007/03/07 21:03:58  sueh
 * <p> bug# 981 Changed ScriptParameter.isUseInScript to isNotNullAndNotDefault for
 * <p> clarity.
 * <p>
 * <p> Revision 3.13  2007/02/05 22:48:20  sueh
 * <p> bug# 962 Made EtomoNumber type info an inner class.
 * <p>
 * <p> Revision 3.12  2006/06/14 21:18:34  sueh
 * <p> bug# 873 Added mirrorInX.
 * <p>
 * <p> Revision 3.11  2006/05/22 22:42:40  sueh
 * <p> bug# 577 Placed the command in an ArrayList rather then a String.
 * <p>
 * <p> Revision 3.10  2006/05/12 18:24:34  sueh
 * <p> bug# 856 Added the "-c" option.
 * <p>
 * <p> Revision 3.9  2005/07/29 00:50:14  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.8  2005/07/18 17:53:10  sueh
 * <p> bug# 692 Removed selftest function in axis id because there are too many
 * <p> situations where it is valid for it to fail.  Remove
 * <p> AxisID.getStorageExtension() because it is the same as getExtension
 * <p> without the call to the selftest function.
 * <p>
 * <p> Revision 3.7  2005/06/03 20:12:48  sueh
 * <p> bug# 671 the groupString for the first axis should have an "a" on the end,
 * <p> even when axis type is single.  To avoid triggering a self test exception,
 * <p> call AxisID.getStorageExtension() instead of getExtension().
 * <p>
 * <p> Revision 3.6  2005/06/01 21:26:51  sueh
 * <p> bug# 667 Removing the Controller classes.  Trying make meta data and
 * <p> app manager equals didn't work very well.  Meta data is created by and
 * <p> managed by app mgr and the class structure should reflect that.   Getting
 * <p> meta data from the manager instead of EtomoDirector.
 * <p>
 * <p> Revision 3.5  2005/05/12 01:25:20  sueh
 * <p> bug# 591 Removed defaults so that fields would always appear in
 * <p> etomo_err.log.
 * <p>
 * <p> Revision 3.4  2005/01/25 21:51:31  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
 * <p> Revision 3.3  2005/01/22 04:04:06  sueh
 * <p> bug# 509 bug# 591  Setting numberViews with a string to avoid
 * <p> NumberFormatException if it is deleted.
 * <p>
 * <p> Revision 3.2  2005/01/21 22:53:55  sueh
 * <p> bug# 509 bug# 591  Implemented Storable.  Changed storable fields to
 * <p> EtomoNumbers.  Stopped using 0 as null for SearchDirection.  Added
 * <p> setCenterViewResetValue() to set the center view to use if the user is not
 * <p> relying on a .rawtlt file.
 * <p>
 * <p> Revision 3.1  2004/04/22 23:27:47  rickg
 * <p> Switched getIMODBinPath method
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.10  2003/11/06 16:50:27  rickg
 * <p> Removed -e flag for tcsh execution for all but the com scripts
 * <p>
 * <p> Revision 2.9  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from
 * <p> ApplicationManager
 * <p>
 * <p> Revision 2.8  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.7  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.6  2003/09/25 16:45:24  sueh
 * <p> bug223 Added Number of views in the search to
 * <p> Fine Alignment button, Transfer Parameters panel.
 * <p>
 * <p> Revision 2.5  2003/05/23 22:03:47  rickg
 * <p> Added -P to command string to get shell PID output
 * <p>
 * <p> Revision 2.4  2003/05/21 21:24:03  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.3  2003/05/13 19:57:43  rickg
 * <p> Explicit call to full path for transferfid
 * <p> don't use -c for tcsh
 * <p>
 * <p> Revision 2.2  2003/05/13 16:54:40  rickg
 * <p> Modified command to run on windows/cygwin
 * <p>
 * <p> Revision 2.1  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.4  2003/01/08 04:00:21  rickg
 * <p> Mods in progress
 * <p>
 * <p> Revision 1.3  2003/01/06 04:50:54  rickg
 * <p> Added command line arguments for b to a and search
 * <p> rotation directions as well as getter and setters for those
 * <p> parameters
 * <p>
 * <p> Revision 1.2  2003/01/04 00:37:27  rickg
 * <p> Added datasetName, bToA, searchDirection, centerView* members
 * <p> Added centerView command line args
 * <p> Added necessary spaces after command line args
 * <p>
 * <p> Revision 1.1  2002/12/31 00:57:08  rickg
 * <p> Initial revision
 * <p>
 * <p> </p>
 */

package etomo.comscript;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.util.DatasetFiles;

public final class TransferfidParam implements Storable {
  public static final String rcsid = "$Id$";

  protected static final String group = "Transferfid";

  private String inputImageFile;
  private String outputImageFile;
  private String inputModelFile;
  private String outputModelFile;
  private String datasetName;
  private final EtomoBoolean2 bToA = new EtomoBoolean2("BToA");
  private final EtomoBoolean2 runMidas = new EtomoBoolean2("RunMidas");
  // null => both, -1 => -90, 1=> +90
  private final EtomoNumber searchDirection = new EtomoNumber(EtomoNumber.Type.INTEGER,
      "SearchDirection");
  private final EtomoNumber centerViewA = new EtomoNumber("CenterViewA");
  private final EtomoNumber centerViewB = new EtomoNumber("CenterViewB");
  private final ScriptParameter numberViews = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "NumberViews");
  private final EtomoBoolean2 mirrorInX = new EtomoBoolean2("MirrorInX");

  private ConstMetaData metaData = null;
  private boolean createLog = false;
  private String groupString;
  private final ApplicationManager manager;

  public TransferfidParam(ApplicationManager manager, AxisID axisID) {
    this.manager = manager;
    // MetaData always uses FIRST and SECOND to store, so create groupString with
    // FIRST or SECOND
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    groupString = group + axisID.getExtension();
    searchDirection.setValidValues(new int[] { -1, 1 });
    numberViews.setDisplayValue(5);
    reset();
  }

  private void reset() {
    inputImageFile = "";
    outputImageFile = "";
    inputModelFile = "";
    outputModelFile = "";
    datasetName = "";
    bToA.reset();
    resetStorableFields();
  }

  /**
   * reset fields that are loaded and stored
   *
   */
  private void resetStorableFields() {
    runMidas.reset();
    searchDirection.reset();
    centerViewA.reset();
    centerViewB.reset();
    numberViews.reset();
    mirrorInX.reset();
  }

  public void initialize() {
    setCenterViewAResetValue();
    setCenterViewBResetValue();
    centerViewA.reset();
    centerViewB.reset();
  }

  public void setMirrorInX(boolean mirrorInX) {
    this.mirrorInX.set(mirrorInX);
  }

  public ConstEtomoNumber getMirrorInX() {
    return mirrorInX;
  }

  private void setCenterViewAResetValue() {
    if (metaData == null) {
      metaData = manager.getConstMetaData();
    }
    setCenterViewResetValue(centerViewA, metaData.getTiltAngleSpecA());
  }

  private void setCenterViewBResetValue() {
    if (metaData == null) {
      metaData = manager.getConstMetaData();
    }
    setCenterViewResetValue(centerViewB, metaData.getTiltAngleSpecB());
  }

  private void setCenterViewResetValue(EtomoNumber centerView, TiltAngleSpec tiltAngleSpec) {
    if (tiltAngleSpec.getType() != TiltAngleType.RANGE) {
      return;
    }
    centerView.setDisplayValue(Math.round(1 - tiltAngleSpec.getRangeMin()
        / tiltAngleSpec.getRangeStep()));
  }

  /**
   * get deep copies of fields that are loaded and stored
   * @param that
   */
  public void getStorableFields(TransferfidParam that) {
    that.runMidas.set(runMidas);
    that.searchDirection.set(searchDirection);
    that.centerViewA.set(centerViewA);
    that.centerViewB.set(centerViewB);
    that.numberViews.set(numberViews);
    that.mirrorInX.set(mirrorInX);
  }

  /**
   * set deep copies of fields that are loaded and stored
   * @param that
   */
  public void setStorableFields(TransferfidParam that) {
    runMidas.set(that.runMidas);
    searchDirection.set(that.searchDirection);
    centerViewA.set(that.centerViewA);
    centerViewB.set(that.centerViewB);
    numberViews.set(that.numberViews);
    mirrorInX.set(that.mirrorInX);
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    runMidas.store(props, prepend);
    searchDirection.store(props, prepend);
    centerViewA.store(props, prepend);
    centerViewB.store(props, prepend);
    numberViews.store(props, prepend);
    mirrorInX.store(props, prepend);
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    resetStorableFields();
    prepend = createPrepend(prepend);

    runMidas.load(props, prepend);
    searchDirection.load(props, prepend);
    centerViewA.load(props, prepend);
    centerViewB.load(props, prepend);
    numberViews.load(props, prepend);
    mirrorInX.load(props, prepend);
  }

  protected String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  /**
   * Get the command string specified by the current state
   */
  public List<String> getCommand() {
    // Do not use the -e flag for tcsh since David's scripts handle the failure
    // of commands and then report appropriately. The exception to this is the
    // com scripts which require the -e flag. RJG: 2003-11-06
    List<String> command = new ArrayList<String>();
    command.add("python");
    command.add("-u");
    command.add(ApplicationManager.getIMODBinPath() + "transferfid");
    command.add("-PID");

    if (bToA.is()) {
      command.add("-b");
    }
    if (!inputImageFile.equals("")) {
      command.add("-ia");
      command.add(inputImageFile);
    }

    if (!outputImageFile.equals("")) {
      command.add("-ib");
      command.add(outputImageFile);
    }

    if (!inputModelFile.equals("")) {
      command.add("-f");
      command.add(inputModelFile);
    }

    if (!outputModelFile.equals("")) {
      command.add("-o");
      command.add(outputModelFile);
    }

    if (!centerViewA.isNull()) {
      command.add("-za");
      command.add(centerViewA.toString());
    }

    if (!centerViewB.isNull()) {
      command.add("-zb");
      command.add(centerViewB.toString());
    }

    if (numberViews.isNotNullAndNotDefault()) {
      command.add("-n");
      command.add(numberViews.toString());
    }

    if (searchDirection.isPositive()) {
      command.add("-a");
      command.add("90");
    }

    if (searchDirection.isNegative()) {
      command.add("-a");
      command.add("-90");
    }

    if (mirrorInX.is()) {
      command.add("-x");
    }

    if (runMidas.is()) {
      command.add("-m");
    }
    command.add("-c");
    command.add(DatasetFiles.getTransferFidCoordFileName());

    command.add(datasetName);
    return command;
  }

  /**
   * Returns the inputImageFile.
   * @return String
   */
  public String getInputImageFile() {
    return inputImageFile;
  }

  /**
   * Returns the inputModelFile.
   * @return String
   */
  public String getInputModelFile() {
    return inputModelFile;
  }

  /**
   * Returns the outputImageFile.
   * @return String
   */
  public String getOutputImageFile() {
    return outputImageFile;
  }

  /**
   * Returns the outputModelFile.
   * @return String
   */
  public String getOutputModelFile() {
    return outputModelFile;
  }

  /**
   * Returns the runMidas.
   * @return boolean
   */
  public ConstEtomoNumber getRunMidas() {
    return runMidas;
  }

  /**
   * Returns numberViews 
   * @return int
   */
  public ConstEtomoNumber getNumberViews() {
    return numberViews;
  }

  /**
   * @param numberViews
   */
  public void setNumberViews(String numberViews) {
    this.numberViews.set(numberViews);
  }

  /**
   * Sets the inputImageFile.
   * @param inputImageFile The inputImageFile to set
   */
  public void setInputImageFile(String inputImageFile) {
    this.inputImageFile = inputImageFile;
  }

  /**
   * Sets the inputModelFile.
   * @param inputModelFile The inputModelFile to set
   */
  public void setInputModelFile(String inputModelFile) {
    this.inputModelFile = inputModelFile;
  }

  /**
   * Sets the outputImageFile.
   * @param outputImageFile The outputImageFile to set
   */
  public void setOutputImageFile(String outputImageFile) {
    this.outputImageFile = outputImageFile;
  }

  /**
   * Sets the outputModelFile.
   * @param outputModelFile The outputModelFile to set
   */
  public void setOutputModelFile(String outputModelFile) {
    this.outputModelFile = outputModelFile;
  }

  /**
   * Sets the runMidas.
   * @param runMidas The runMidas to set
   */
  public void setRunMidas(boolean runMidas) {
    this.runMidas.set(runMidas);
  }

  /**
   * Returns the setName.
   * @return String
   */
  public String getDatasetName() {
    return datasetName;
  }

  /**
   * Sets the setName.
   * @param setName The setName to set
   */
  public void setDatasetName(String datasetName) {
    this.datasetName = datasetName;
  }

  /**
   * Returns the bToA.
   * @return boolean
   */
  public ConstEtomoNumber getBToA() {
    return bToA;
  }

  /**
   * Sets the bToA.
   * @param bToA The bToA to set
   */
  public void setBToA(boolean bToA) {
    this.bToA.set(bToA);
  }

  /**
   * Returns the centerViewA.
   * @return int
   */
  public EtomoNumber getCenterViewA() {
    return centerViewA;
  }

  /**
   * Returns the centerViewB.
   * @return int
   */
  public EtomoNumber getCenterViewB() {
    return centerViewB;
  }

  /**
   * Returns the searchDirection.
   * @return int
   */
  public EtomoNumber getSearchDirection() {
    return searchDirection;
  }

  /**
   * Sets the centerViewA.
   * @param centerViewA The centerViewA to set
   */
  public void setCenterViewA(String centerViewA) {
    this.centerViewA.set(centerViewA);
  }

  /**
   * Sets the centerViewB.
   * @param centerViewB The centerViewB to set
   */
  public void setCenterViewB(String centerViewB) {
    this.centerViewB.set(centerViewB);
  }

  /**
   * Sets the searchDirection.
   * @param searchDirection The searchDirection to set
   */
  public void setSearchDirection(int searchDirection) {
    this.searchDirection.set(searchDirection);
  }

  /**
   * Returns the createLog.
   * @return boolean
   */
  public boolean isCreateLog() {
    return createLog;
  }

  /**
   * Sets the createLog.
   * @param createLog The createLog to set
   */
  public void setCreateLog(boolean createLog) {
    this.createLog = createLog;
  }

}