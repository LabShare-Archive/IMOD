package etomo;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

import etomo.comscript.FinishjoinParam;
import etomo.comscript.FlipyzParam;
import etomo.comscript.MakejoincomParam;
import etomo.comscript.MidasParam;
import etomo.comscript.XfalignParam;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.JoinProcessManager;
import etomo.process.SystemProcessException;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.EtomoNumber;
import etomo.type.JoinMetaData;
import etomo.type.JoinProcessTrack;
import etomo.type.ProcessName;
import etomo.type.SlicerAngles;
import etomo.ui.JoinDialog;
import etomo.ui.MainJoinPanel;
import etomo.ui.MainPanel;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.7  2004/12/04 00:32:13  sueh
* <p> bug# 570 Setting paramFile by calling endSetupMode() when running
* <p> makejoincom.
* <p>
* <p> Revision 1.6  2004/11/24 18:10:12  sueh
* <p> bug# 520 Added binning in XY.
* <p>
* <p> Revision 1.5  2004/11/24 00:58:08  sueh
* <p> bug# 520 makejoincom: set nextProcess to "" when running process
* <p> makejoincom() throws anexception.
* <p>
* <p> Revision 1.4  2004/11/23 22:29:03  sueh
* <p> bug# 520 Fixing makejoincom - shouldn't do a set mode until the process
* <p> is finished.  Putting startProgressBar() call last is process run functions
* <p> and removing call to stopProgressBar() to prevent incorrect starting and
* <p> stopping
* <p>
* <p> Revision 1.3  2004/11/20 01:55:44  sueh
* <p> bug# 520 Added abortAddSection() to turn on Add Section button after
* <p> killing flipyz process.
* <p>
* <p> Revision 1.2  2004/11/19 22:34:55  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.31  2004/11/19 00:00:01  sueh
* <p> bug# 520 Overrode saveTestParamIfNecessary to ask to save the screen
* <p> if fields have changed.  Added updateMetaDataFromJoinDialog to call
* <p> JoinDialog.getMetaData(), change for a valid meta data and paramFile,
* <p> save the meta data to the paramFile.
* <p>
* <p> Revision 1.1.2.30  2004/11/17 02:20:47  sueh
* <p> bug# 520 Added endSetupMode() to do things that need to be done once,
* <p> such as set paramFile, place the param file name on the status bar, and
* <p> set the meta data in imod manager.  If paramFile is already set, it will only
* <p> call setMode().
* <p>
* <p> Revision 1.1.2.29  2004/11/16 23:26:24  sueh
* <p> bug# 520 JoinDialog mode names should end with _MODE.
* <p>
* <p> Revision 1.1.2.28  2004/11/16 02:20:03  sueh
* <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
* <p> EtomoLong with EtomoNumber.
* <p>
* <p> Revision 1.1.2.27  2004/11/15 22:06:55  sueh
* <p> bug# 520  Change endSetupMode() to setMode() and change it to handle
* <p> JoinDialog.SAMPLE_PRODUCED and JoinDialog.SAMPLE_PRODUCED.
* <p> Remove doneJoinDialog() because it is not being used.
* <p>
* <p> Revision 1.1.2.26  2004/11/12 22:44:33  sueh
* <p> bug# 520 Consolidated the imodOpen functions by passing an
* <p> ImodManager key.
* <p>
* <p> Revision 1.1.2.25  2004/11/11 01:34:23  sueh
* <p> bug# 520 Adding binning to open 3dmod functions.
* <p>
* <p> Revision 1.1.2.24  2004/11/08 22:08:04  sueh
* <p> bug# 520 consolidate functionality that calls finishjoin into runFinishjoin().
* <p> Add functions to set shift and size.
* <p>
* <p> Revision 1.1.2.23  2004/10/29 22:07:25  sueh
* <p> bug# 520 Don't run createEmptyXfFile when the .ejf file isn't loaded.  When
* <p> makejoincom is run, run createEmptyXfFile.
* <p>
* <p> Revision 1.1.2.22  2004/10/29 01:16:24  sueh
* <p> bug# 520 Removing unecessary functions that provided services to
* <p> BaseManager.  BaseManager can use get... functions to get the
* <p> mainPanel, metaData, and processTrack.  Setting workingDirectory in
* <p> JoinDialog from propertyUserDir, when paramFile is loaded.
* <p>
* <p> Revision 1.1.2.21  2004/10/28 16:51:46  sueh
* <p> bug# 520 Copying most recent .xf file to rootName.xf before running
* <p> finishjoin, midasSample, and refine xfalign.  Added copyXfFile,
* <p> createEmptyXfFile, and copyMostRecentXfFile.
* <p>
* <p> Revision 1.1.2.20  2004/10/21 17:50:48  sueh
* <p> bug# 520 In endSetupMode() set paramFile and status bar.
* <p>
* <p> Revision 1.1.2.19  2004/10/21 02:31:51  sueh
* <p> bug# 520 Added enableMidas, finishJoin, revertXfalign, xfalignInitial,
* <p> xfalignRefine.
* <p>
* <p> Revision 1.1.2.18  2004/10/18 19:05:09  sueh
* <p> bug# 520 Added midasSample().  Added validation to startjoin.
* <p>
* <p> Revision 1.1.2.17  2004/10/18 17:28:59  sueh
* <p> bug# 520 Added setSetupMode(), which tests for a valid meta data object
* <p> and then enables the other tabs and disables rootName.  This is done
* <p> when Make Sample is pressed and when a new join manager is created.
* <p> Added xfalign.
* <p>
* <p> Revision 1.1.2.16  2004/10/15 00:14:00  sueh
* <p> bug# 520 Initializing ui parameters on JoinManager construction.  Setting
* <p> metaData in JoinDialog.
* <p>
* <p> Revision 1.1.2.15  2004/10/14 17:09:30  sueh
* <p> bug# 520 Added imodOpenJoinSampleAverages.
* <p>
* <p> Revision 1.1.2.14  2004/10/14 03:24:33  sueh
* <p> bug# 520 Added open join samples in Imod.  Using the Make Samples as
* <p> a signal somewhat like exiting Setup successfully.  In this case I only
* <p> need to call imodManager.setMetaData and enable the other join tabs.
* <p>
* <p> Revision 1.1.2.13  2004/10/14 02:26:38  sueh
* <p> bug# 520 Added setWorkingDir() to set the propertyUserDir.
* <p>
* <p> Revision 1.1.2.12  2004/10/11 01:59:23  sueh
* <p> bug# 520 moved responsibility for mainPanel, metaData, processTrack,
* <p> and progressManager to child classes.  Used abstract functions to use
* <p> these variables in the base classes.  This is more reliable and doesn't
* <p> require casting.
* <p>
* <p> Revision 1.1.2.11  2004/10/08 15:44:52  sueh
* <p> bug# 520 Fixed Make Samples functionality.  Used startNextProcess to
* <p> call startjoin.com.  Used BackgroundProcess to call makejoincom
* <p>
* <p> Revision 1.1.2.10  2004/10/06 01:26:11  sueh
* <p> bug# 520 Changed Make Join button to Make Samples.  Added flip().
* <p>
* <p> Revision 1.1.2.9  2004/10/01 21:00:44  sueh
* <p> bug# 520 Added getMetaData()
* <p>
* <p> Revision 1.1.2.8  2004/09/29 17:42:26  sueh
* <p> bug# 520 Casting mainPanel and other members from BaseManager to
* <p> private local variables in the create functions.  Removed
* <p> openNewDataset() and openExistingDataset().  This functionality is
* <p> handled in EtomoDirector.  Added startJoin().  Added setTestParamFile()
* <p> implementation.
* <p>
* <p> Revision 1.1.2.7  2004/09/22 22:03:53  sueh
* <p> bug# 520 Made the imod functions more general by passing in the
* <p> ImodManager key.  Added imodGetSlicerAngles.
* <p>
* <p> Revision 1.1.2.6  2004/09/21 17:43:41  sueh
* <p> bug# 520 add imodTomogram and imodRemoveTomogram
* <p>
* <p> Revision 1.1.2.5  2004/09/15 22:34:34  sueh
* <p> bug# 520 casting  base manager when necessary.  Added JoinDialog
* <p>
* <p> Revision 1.1.2.4  2004/09/13 16:41:18  sueh
* <p> bug# 520 added isNewManager stub function
* <p>
* <p> Revision 1.1.2.3  2004/09/08 19:28:25  sueh
* <p> bug# 520 update call to BaseMAnager()
* <p>
* <p> Revision 1.1.2.2  2004/09/07 17:55:15  sueh
* <p> bug# 520 moved mainFrame show responsibility to EtomoDirector
* <p>
* <p> Revision 1.1.2.1  2004/09/03 21:03:27  sueh
* <p> bug# 520 adding place holders for create functions for now
* <p> </p>
*/
public class JoinManager extends BaseManager {
  public static  final String  rcsid =  "$Id$";
  
  //  Process dialog references
  private JoinDialog joinDialog = null;
  
  //variables cast from base class variables
  //initialized in create function
  private MainJoinPanel mainPanel;
  private JoinMetaData metaData;
  private JoinProcessManager processMgr;
  private JoinProcessTrack processTrack;
  
  public JoinManager(String paramFileName) {
    super();
    initializeUIParameters(paramFileName);
    // Open the etomo data file if one was found on the command line
    if (!test) {
      openJoinDialog();
      setMode();
    }
  }
  
  public boolean isNewManager() {
    return true;
  }

  protected void createComScriptManager() {
    
  }
  
  protected void createProcessManager() {
    processMgr = new JoinProcessManager(this);
  }
  
  protected void createProcessTrack() {
    processTrack = new JoinProcessTrack();
  }

  /**
   * Open the join dialog
   */
  public void openJoinDialog() {
    openProcessingPanel();
    if (joinDialog == null) {
      if (loadedTestParamFile) {
        joinDialog = new JoinDialog(this, propertyUserDir);
      }
      else {
        joinDialog = new JoinDialog(this);
      }
      joinDialog.setMetaData(metaData);
    }
    if (loadedTestParamFile) {
      createEmptyXfFile(metaData.getRootName());
    }
    mainPanel.showProcess(joinDialog.getContainer(), AxisID.ONLY);
  }
  
  protected void createMainPanel() {
    mainPanel = new MainJoinPanel(this);
  }
  
  protected void createMetaData() {
    metaData = new JoinMetaData();
  }
  
  /**
   * Open 3dmod with binning 
   */
  public void imodOpen(String imodKey, int binning) {
    try {
      imodManager.setBinningXY(imodKey, binning);
      imodManager.open(imodKey);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(),
        "Can't open " + imodKey + " in 3dmod ");
    }
  }
  
  /**
   * Open 3dmod
   */
  public void imodOpen(String imodKey) {
    try {
      imodManager.open(imodKey);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(),
        "Can't open " + imodKey + " in 3dmod ");
    }
  }

  /**
   * Open or raise a specific 3dmod to view a file with binning.
   * Or open a new 3dmod.
   * Return the index of the 3dmod opened or raised.
   */
  public int imodOpen(String imodKey, int imodIndex, File file, int binning) {
    try {
      if (imodIndex == -1) {
        imodIndex = imodManager.newImod(imodKey, file);
      }
      imodManager.setBinningXY(imodKey, imodIndex, binning);
      imodManager.open(imodKey, imodIndex, file);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(),
        "Can't open " + imodKey + " 3dmod with imodIndex=" + imodIndex);
    }
    return imodIndex;
  }
  
  /**
   * Remove a specific 3dmod
   * @param imodKey
   * @param imodIndex
   */
  public void imodRemove(String imodKey, int imodIndex) {
    if (imodIndex == -1) {
      return;
    }
    try {
      imodManager.delete(imodKey, imodIndex);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(),
        "Can't delete " + imodKey + " 3dmod with imodIndex=" + imodIndex);
    }
  }
  
  public SlicerAngles imodGetSlicerAngles(String imodKey, int imodIndex) {
    Vector results = null;
    try {
      if (imodIndex == -1) {
        mainPanel.openMessageDialog("The is no open " + imodKey
            + " 3dmod for the highlighted row.", "No 3dmod");
      }
      results = imodManager.getSlicerAngles(imodKey, imodIndex);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog(except.getMessage(),
          "Can't get rotation angles from " + imodKey
              + " 3dmod.");
    }
    Vector messageArray = new Vector();
    SlicerAngles slicerAngles = null;
    if (results == null) {
      messageArray.add("Unable to retrieve slicer angles.");
      messageArray.add("The " + imodKey + " may not be open in 3dmod.");
    }
    else {
      slicerAngles = new SlicerAngles();
      boolean foundResultLine1 = false;
      boolean foundResult = false;
      String result = null;
      for (int i = 0; i < results.size(); i++) {
        result = (String) results.get(i);
        if (result.indexOf(ImodProcess.IMOD_SEND_EVENT_STRING) != -1
            || result.indexOf(ImodProcess.ERROR_STRING) != -1
            || result.indexOf(ImodProcess.WARNING_STRING) != -1) {
          messageArray.add(result);
        }
        else if (!foundResultLine1 && !foundResult
            && result.equals(ImodProcess.SLICER_ANGLES_RESULTS_STRING1)) {
          foundResultLine1 = true;
        }
        else if (foundResultLine1 && !foundResult
            && result.equals(ImodProcess.SLICER_ANGLES_RESULTS_STRING2)) {
          foundResult = true;
        }
        else if (foundResult && !slicerAngles.isComplete()) {
          try {
            slicerAngles.add(result);
          }
          catch (NumberFormatException e) {
            messageArray.add(result);
          }
        }
        else {
          messageArray.add(result);
        }
      }
      if (!slicerAngles.isComplete()) {
        messageArray.add("Unable to retrieve slicer angles from " + imodKey
            + " 3dmod.");
        if (!slicerAngles.isEmpty()) {
          messageArray.add("slicerAngles=" + slicerAngles);
        }
      }
    }
    if (messageArray.size() > 0) {
      String[] messages = (String[]) messageArray
          .toArray(new String[messageArray.size()]);
      mainPanel.openMessageDialog(messages, "Slicer Angles");
    }
    return slicerAngles;
  }

  public void makejoincom() {
    if (!joinDialog.getMetaData(metaData)) {
      return;
    }
    if (!metaData.isValid(joinDialog.getWorkingDirName())) {
      mainPanel.openMessageDialog(metaData.getInvalidReason(), "Invalid Data");
      return;
    }
    nextProcess = "startjoin";
    String rootName = metaData.getRootName();
    EtomoDirector.getInstance().renameCurrentManager(rootName);
    createEmptyXfFile(rootName);
    MakejoincomParam makejoincomParam = new MakejoincomParam(metaData);
    if (paramFile == null) {
      endSetupMode();
    }
    try {
      threadNameA = processMgr.makejoincom(makejoincomParam);
    }
    catch (SystemProcessException except) {
      nextProcess = "";
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run makejoincom\n"
        + except.getMessage(), "SystemProcessException");
      return; 
    }
    mainPanel.startProgressBar("Makejoincom", AxisID.ONLY);
  }
  
  /**
   * if paramFile is not set, attempts to end setup mode and set the param file name.
   * @return
   */
  public boolean endSetupMode() {
    if (paramFile != null) {
      return setMode();
    }
    String workingDirName = joinDialog.getWorkingDirName();
    if (!setMode(workingDirName)) {
      return false;
    }
    propertyUserDir = workingDirName;
    imodManager.setMetaData(metaData);
    paramFile = new File(propertyUserDir, metaData.getRootName() + metaData.getFileExtension());
    loadedTestParamFile = true;
    mainPanel.updateDataParameters(paramFile, metaData);
    return true;
  }
  
  protected void saveTestParamOnExit() {
    if (!joinDialog.equalsSample(metaData)) {
      metaData.setSampleProduced(false);
    }
    joinDialog.getMetaData(metaData);
    // If the user selects Yes then try to save the current EDF file
    if (paramFile == null && !endSetupMode()) {
      return;
    }
    // Be sure the file saving was successful
    if (paramFile != null) {
      saveTestParamFile();
    }
  }
  
  public boolean canChangeParamFileName() {
    return false;
  }

  
  /**
   * Run midas on the sample
   */
  public void midasSample() {
    if (!updateMetaDataFromJoinDialog()) {
      return;
    }
    MidasParam midasParam = new MidasParam(metaData);
    if (!copyMostRecentXfFile(JoinDialog.MIDAS_TEXT)) {
      return;
    }
    try {
      processMgr.midasSample(midasParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run"+ JoinDialog.MIDAS_TEXT + "\n"
        + except.getMessage(), "SystemProcessException");
      return; 
    }
  }
  
  public void xfalignInitial() {
    if (!updateMetaDataFromJoinDialog()) {
      return;
    }
    XfalignParam xfalignParam = new XfalignParam(metaData,
        XfalignParam.INITIAL_MODE);
    try {
      threadNameA = processMgr.xfalign(xfalignParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run initial xfalign\n"
        + except.getMessage(), "SystemProcessException");
      joinDialog.enableMidas();
      return; 
    }
    mainPanel.startProgressBar("Initial xfalign", AxisID.ONLY);
  }
  
  public void xfalignRefine() {
    if (!updateMetaDataFromJoinDialog()) {
      return;
    }
    XfalignParam xfalignParam = new XfalignParam(metaData, XfalignParam.REFINE_MODE);
    if (!copyMostRecentXfFile(JoinDialog.REFINE_AUTO_ALIGNMENT_TEXT)) {
      return;
    }
    try {
      threadNameA = processMgr.xfalign(xfalignParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run "+ JoinDialog.REFINE_AUTO_ALIGNMENT_TEXT + "\n"
        + except.getMessage(), "SystemProcessException");
      joinDialog.enableMidas();
      return; 
    }
    mainPanel.startProgressBar("Refine xfalign", AxisID.ONLY);
  }
  
  private boolean copyMostRecentXfFile(String commandDescription) {
    String rootName = metaData.getRootName();
    String xfFileName = rootName + ".xf";
    File newXfFile = Utilities.mostRecentFile(xfFileName, rootName
        + MidasParam.getOutputFileExtension(), rootName
        + XfalignParam.getOutputFileExtension(), rootName + "_empty.xf");
    //If the most recent .xf file is not root.xf, copy it to root.xf
    if (!newXfFile.getName().equals(xfFileName)) {
      File xfFile = new File(propertyUserDir, xfFileName);
      try {
        Utilities.copyFile(newXfFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy " + newXfFile.getAbsolutePath() + " to "
                + xfFileName + ".",
            "Copy " + newXfFile.getName() + " to " + xfFileName,
            " and then rerun " + commandDescription + "." };
        mainPanel
            .openMessageDialog(message, "Cannot run " + commandDescription);
        return false;
      }
    }
    return true;
  }
  
  public void copyXfFile(File xfOutputFile) {
    File xfFile = new File(propertyUserDir, metaData.getRootName() + ".xf");
    if (xfOutputFile != null && xfOutputFile.exists()) {
      try {
        Utilities.copyFile(xfOutputFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy " + xfOutputFile.getAbsolutePath() + " to "
                + xfFile.getName() + ".",
            "Copy " + xfOutputFile.getName() + " to " + xfFile.getName() + "." };
        mainPanel.openMessageDialog(message, "Cannot Copy File");
      }
    }
  }
  
  public void createEmptyXfFile(String rootName) {
    File emptyXfFile = new File(propertyUserDir, rootName + "_empty.xf");
    if (!emptyXfFile.exists()) {
      String emptyLine = "   1.0000000   0.0000000   0.0000000   1.0000000       0.000       0.000";
      try {
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(
            emptyXfFile));
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.close();
      }
      catch (IOException e) {
        e.printStackTrace();
        return;
      }
    }
    File xfFile = new File(propertyUserDir, rootName + ".xf");
    if (!xfFile.exists()) {
      try {
        Utilities.copyFile(emptyXfFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
  
  /**
   * sets the mode in joinDialog based on whether the working directory and
   * root name are entered and whether a sample is saved
   * @param workingDirName
   * @return
   */
  public boolean setMode(String workingDirName) {
    if (!metaData.isValid(workingDirName)) {
      joinDialog.setMode(JoinDialog.SETUP_MODE);
      return false;
    }
    if (metaData.isSampleProduced()) {
      joinDialog.setMode(JoinDialog.SAMPLE_PRODUCED_MODE);
    }
    else {
      joinDialog.setMode(JoinDialog.SAMPLE_NOT_PRODUCED_MODE);
    }
    return true;
  }
  
  public boolean setMode() {
    return setMode(propertyUserDir);
  }
  
  public void startjoin() {
    if (!metaData.isValid(joinDialog.getWorkingDir())) {
      mainPanel.openMessageDialog(metaData.getInvalidReason(), "Invalid Data");
      return;
    }
    nextProcess = "";
    try {
      threadNameA = processMgr.startjoin();
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run startjoin.com\n"
        + except.getMessage(), "SystemProcessException");
      return; 
    }
    mainPanel.startProgressBar("Startjoin", AxisID.ONLY);
  }
  
  public void revertXfFileToMidas() {
    File midasOutputFile = new File(propertyUserDir, metaData.getRootName()
        + MidasParam.getOutputFileExtension());
    processMgr.touch(midasOutputFile);
    copyXfFile(midasOutputFile);
  }
  
  public void revertXfFileToEmpty() {
    File emptyFile = new File(propertyUserDir, metaData.getRootName()
        + "_empty.xf");
    processMgr.touch(emptyFile);
    copyXfFile(emptyFile);
  }
  
  public void enableMidas() {
    joinDialog.enableMidas();
  }
  
  public void runFinishjoin(int mode, String buttonText) {
    if (!updateMetaDataFromJoinDialog()) {
      return;
    }
    FinishjoinParam finishjoinParam = new FinishjoinParam(metaData, mode);
    if (!copyMostRecentXfFile(buttonText)) {
      return;
    }
    try {
      threadNameA = processMgr.finishjoin(finishjoinParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run " + buttonText
          + "\n" + except.getMessage(), "SystemProcessException");
      return; 
    }
    mainPanel.startProgressBar("Finishjoin: " + buttonText, AxisID.ONLY);
  }
  
  private boolean updateMetaDataFromJoinDialog() {
    if (!joinDialog.getMetaData(metaData)) {
      return false;
    }
    if (!metaData.isValid(propertyUserDir)) {
      mainPanel.openMessageDialog(metaData.getInvalidReason(), "Invalid Data");
      return false;
    }
    saveMetaData();
    return true;
  }
  
  public void setSize(String sizeInXString, String sizeInYString) {
    EtomoNumber sizeInX = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    EtomoNumber sizeInY = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    sizeInX.set(sizeInXString);
    joinDialog.setSizeInX(sizeInX);
    sizeInY.set(sizeInYString);
    joinDialog.setSizeInY(sizeInY);
  }
  
  public void setShift(ConstEtomoNumber shiftInX, ConstEtomoNumber shiftInY) {
    joinDialog.setShiftInX(shiftInX);
    joinDialog.setShiftInY(shiftInY);   
  }

   
  public void flip(File tomogram, File workingDir) {
    FlipyzParam flipyzParam = new FlipyzParam(tomogram, workingDir);
    try {
      threadNameA = processMgr.flipyz(flipyzParam);
    }
    catch (SystemProcessException except) {
      joinDialog.abortAddSection();
      except.printStackTrace();
      mainPanel.openMessageDialog("Can't run clip flipyz\n"
        + except.getMessage(), "SystemProcessException");
      return; 
    }
    mainPanel.startProgressBar("Flipping " + tomogram.getName(), AxisID.ONLY);
  }
  
  public void abortAddSection() {
    joinDialog.abortAddSection();
  }

  public void addSection(File tomogram) {
    joinDialog.addSection(tomogram);
  }

  /**
   * Open the main window in processing mode
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
  }
  
  /**
   * Set the data set parameter file. This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setTestParamFile(File paramFile) {
    this.paramFile = paramFile;
    //  Update main window information and status bar
    mainPanel.updateDataParameters(paramFile, metaData);
  }
  
  protected void updateDialog(ProcessName processName, AxisID axisID) {
    
  }
  
  public ConstJoinMetaData getMetaData() {
    return (ConstJoinMetaData) metaData;
  }
  
  public JoinMetaData getJoinMetaData() {
    return metaData;
  }
  
  /**
   * Start the next process specified by the nextProcess string
   */
  protected void startNextProcess(AxisID axisID) {
    if (nextProcess.equals("startjoin")) {
      startjoin();
      return;
    }
  }
  
  public BaseMetaData getBaseMetaData() {
    return (BaseMetaData) metaData;
  }
  
  protected void setMetaData(ImodManager imodManager) {
    imodManager.setMetaData(metaData);
  }
  
  public MainPanel getMainPanel() {
    return mainPanel;
  }
  
  protected BaseProcessTrack getProcessTrack() {
    return processTrack;
  }
  
  /**
   * Interrupt the currently running thread for this axis
   * 
   * @param axisID
   */
  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

}
