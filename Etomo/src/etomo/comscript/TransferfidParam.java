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

import java.util.Properties;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.MetaData;
import etomo.type.ScriptParameter;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;

public class TransferfidParam implements Storable {
  public static final String rcsid = "$Id$";
  
  protected static final String group = "Transferfid";
  
  String inputImageFile;
  String outputImageFile;
  String inputModelFile;
  String outputModelFile;
  String datasetName;
  EtomoBoolean2 bToA = new EtomoBoolean2("BToA");
  EtomoBoolean2 runMidas = new EtomoBoolean2("RunMidas");
  //null => both, -1 => -90, 1=> +90  
  EtomoNumber searchDirection = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SearchDirection"); 
  EtomoNumber centerViewA = new EtomoNumber(EtomoNumber.LONG_TYPE, "CenterViewA");
  EtomoNumber centerViewB = new EtomoNumber(EtomoNumber.LONG_TYPE, "CenterViewB");
  ScriptParameter numberViews = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "NumberViews");

  private MetaData metaData = null;
  boolean createLog = false;
  private String groupString;

  public TransferfidParam(AxisID axisID) {
    //MetaData always uses FIRST and SECOND to store, so create groupString with
    //FIRST or SECOND
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    groupString = group + axisID.getExtension();
    searchDirection.setValidValues(new int[] {-1,1});
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
  }
  
  public void initialize() {
    setCenterViewAResetValue();
    setCenterViewBResetValue();
    centerViewA.reset();
    centerViewB.reset();
  }
  
  private void setCenterViewAResetValue() {
    if (metaData == null) {
      metaData = EtomoDirector.getInstance().getCurrentReconstructionMetaData();
    }
    setCenterViewResetValue(centerViewA, metaData.getTiltAngleSpecA());
  }
  
  private void setCenterViewBResetValue() {
    if (metaData == null) {
      metaData = EtomoDirector.getInstance().getCurrentReconstructionMetaData();
    }
    setCenterViewResetValue(centerViewB, metaData.getTiltAngleSpecB());
  }
  
  private void setCenterViewResetValue(EtomoNumber centerView,
      TiltAngleSpec tiltAngleSpec) {
    if (tiltAngleSpec.getType() != TiltAngleType.RANGE) {
      return;
    }
    centerView.setDisplayValue(Math.round(1 - tiltAngleSpec.getRangeMin() / tiltAngleSpec.getRangeStep()));
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
  public String getCommandString() {
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    StringBuffer commandLine = new StringBuffer("tcsh -f "
        + ApplicationManager.getIMODBinPath() + "transferfid -P ");

    if (bToA.is()) {
      commandLine.append("-b ");
    }
    if (!inputImageFile.equals("")) {
      commandLine.append("-ia " + inputImageFile + " ");
    }

    if (!outputImageFile.equals("")) {
      commandLine.append("-ib " + outputImageFile + " ");
    }

    if (!inputModelFile.equals("")) {
      commandLine.append("-f " + inputModelFile + " ");
    }

    if (!outputModelFile.equals("")) {
      commandLine.append("-o " + outputModelFile + " ");
    }

    if (!centerViewA.isNull()) {
      commandLine.append("-za " + centerViewA.toString() + " ");
    }

    if (!centerViewB.isNull()) {
      commandLine.append("-zb " + centerViewB.toString() + " ");
    }

    if (numberViews.isUseInScript()) {
      commandLine.append("-n " + numberViews.toString() + " ");
    }

    if (searchDirection.isPositive()) {
      commandLine.append("-a 90 ");
    }

    if (searchDirection.isNegative()) {
      commandLine.append("-a -90 ");
    }

    if (runMidas.is()) {
      commandLine.append("-m ");
    }

    commandLine.append(datasetName);
    return commandLine.toString();
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