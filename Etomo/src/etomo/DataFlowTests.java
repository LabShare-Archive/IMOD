package etomo;

import java.io.File;
import java.io.IOException;

import etomo.process.SystemProcessException;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ViewType;
import etomo.ui.TomogramGenerationExpert;
import etomo.ui.TomogramPositioningExpert;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: This application and class runs through a preconfigured
 * tomogram generation sequence excerises much of the com script and program
 * execution code.
 * 
 * It requires the raw stack(s), the completed seed model(s), fiducial model(s)
 * and tomopitch model(s), com scripts and, if necessary the raw tilt files,
 * to exist in the subdirectory DataSource of the working directory.  It also
 * requires the .edf file named the same as the dataset name to exist in the
 * DataSource directory.</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class DataFlowTests {

  static ApplicationManager applicationManager;
  //static MainFrame mainFrame;
  static UIHarness uiHarness = UIHarness.INSTANCE;
  static String datasetName;
  static boolean fiducialSpecified = false;
  static boolean fiducialless = false;

  public static void main(String[] args) {
    // Parse the command line
    parseCommandLine(args);

    System.out.println("Current working directory: "
        + System.getProperty("user.dir"));
    System.out.println("Dataset name: " + datasetName);

    // Copy the EDF from the data source directory, also need a dataset for now
    // - since we don't have the EDF yet we don't know if its single or dual
    // axis.  When we relax the MetaData.isValid restriction on the data not
    // existing in the directory the data set copy can be moved into the next
    // copy block below.
    try {
      copyFromDataSource(datasetName + DatasetFiles.RECON_DATA_FILE_EXT);
      copyFromDataSource(datasetName + "a.st");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
    }

    //  Simple test: create a new ApplicationManager and cause it to quit
    String[] argsIn = new String[3];
    argsIn[0] = "--debug";
    argsIn[1] = "--selftest";
    argsIn[2] = System.getProperty("user.dir") + File.separator + datasetName
        + DatasetFiles.RECON_DATA_FILE_EXT;
    /*String[] argsIn = new String[1];
     argsIn[0] = System.getProperty("user.dir") + File.separator + datasetName
     + ".edf";*/
    EtomoDirector.main(argsIn);
    applicationManager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    //mainFrame = EtomoDirector.getInstance().getMainFrame();
    // A hack around the const object returned we really know is not const
    MetaData metaData = (MetaData) applicationManager.getMetaData();
    boolean montage = metaData.getViewType() == ViewType.MONTAGE;
    try {
      if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
        copyFromDataSource(datasetName + "b.st");
        copyFromDataSource(datasetName + "a.rawtlt");
        copyFromDataSource(datasetName + "b.rawtlt");
        uiHarness.showBothAxis();
        if (montage) {
          copyFromDataSource(datasetName + "a.pl");
          copyFromDataSource(datasetName + "b.pl");
        }
        copycoms("a", montage);
        copycoms("b", montage);
      }
      else {
        copyFromDataSource(datasetName + ".st");
        copyFromDataSource(datasetName + ".rawtlt");
        if (montage) {
          copyFromDataSource(datasetName + ".pl");
        }
        copycoms("", montage);
      }
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.exit(1);
    }

    // If the fiducialless state has been specfied override the EDF file 
    if (fiducialSpecified) {
      metaData.setFiducialessAlignment(AxisID.FIRST, fiducialless);
      if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
        metaData.setFiducialessAlignment(AxisID.SECOND, fiducialless);
      }
    }

    preProcessing(AxisID.FIRST);
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
      preProcessing(AxisID.SECOND);
    }

    coarseAlignment(AxisID.FIRST);
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
      coarseAlignment(AxisID.SECOND);
    }

    if (!applicationManager.getMetaData().isFiducialessAlignment(AxisID.FIRST)) {
      fiducialModelGen(AxisID.FIRST);
      fineAlignment(AxisID.FIRST);
      if (metaData.getAxisType() == AxisType.DUAL_AXIS
          && !applicationManager.getMetaData().isFiducialessAlignment(
              AxisID.SECOND)) {
        transferfid(AxisID.SECOND);
        fiducialModelGen(AxisID.SECOND);
        fineAlignment(AxisID.SECOND);
      }
    }

    tomogramPositioning(AxisID.FIRST);
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
      tomogramPositioning(AxisID.SECOND);
    }

    tomogramGeneration(AxisID.FIRST);
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
      tomogramGeneration(AxisID.SECOND);
      tomogramCombination();
    }
  }

  private static void copycoms(String axisExtension, boolean montage)
      throws SystemProcessException {
    copyFromDataSource("align" + axisExtension + ".com");
    copyFromDataSource("eraser" + axisExtension + ".com");
    copyFromDataSource("findsec" + axisExtension + ".com");
    copyFromDataSource("sample" + axisExtension + ".com");
    copyFromDataSource("tilt" + axisExtension + ".com");
    copyFromDataSource("tomopitch" + axisExtension + ".com");
    copyFromDataSource("track" + axisExtension + ".com");
    copyFromDataSource("xcorr" + axisExtension + ".com");
    if (montage) {
      copyFromDataSource("blend" + axisExtension + ".com");
      copyFromDataSource("preblend" + axisExtension + ".com");
    }
    else {
      copyFromDataSource("newst" + axisExtension + ".com");
      copyFromDataSource("prenewst" + axisExtension + ".com");
    }
  }

  private static void waitForThread(AxisID axisID) {
    while (applicationManager.getThreadName(axisID) != "none") {
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
  }

  private static void copyFromDataSource(String filename)
      throws SystemProcessException {
    System.out.println("Copying from DataSource directory: " + filename);
    File source = new File(System.getProperty("user.dir") + File.separator
        + "DataSource", filename);
    File dest = new File(System.getProperty("user.dir"), filename);
    try {
      Utilities.copyFile(source, dest);
    }
    catch (IOException e) {
      e.printStackTrace();
      System.err.println("Unable to copy " + source.getAbsolutePath() + " to "
          + dest.getAbsolutePath());
      System.exit(-1);
    }

  }

  private static void preProcessing(AxisID axisID) {
    applicationManager.openPreProcDialog(axisID);
    uiHarness.pack(applicationManager);
    applicationManager.findXrays(axisID, null, null, null, null, null, null);
    waitForThread(axisID);
    applicationManager.preEraser(axisID, null, null, null, null, null, null);
    waitForThread(axisID);
    applicationManager.replaceRawStack(axisID, null, null);
    applicationManager.donePreProcDialog(axisID);
  }

  private static void coarseAlignment(AxisID axisID) {
    applicationManager.openCoarseAlignDialog(axisID);
    uiHarness.pack(applicationManager);
    applicationManager.preCrossCorrelate(axisID, null, null, null, null);
    waitForThread(axisID);
    if (!applicationManager.getMetaData().isFiducialessAlignment(axisID)) {
      applicationManager.coarseAlign(axisID, null, null, null, null, null,null,null);
      waitForThread(axisID);
    }
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  private static void transferfid(AxisID destAxisID) {
    applicationManager.openFiducialModelDialog(destAxisID);
    uiHarness.pack(applicationManager);
    applicationManager.transferfid(destAxisID, null, null, null, null, null);
    waitForThread(destAxisID);

  }

  private static void fiducialModelGen(AxisID axisID) {
    if (axisID == AxisID.FIRST) {
      try {
        copyFromDataSource(datasetName + axisID.getExtension() + ".seed");
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
      }
    }
    applicationManager.openFiducialModelDialog(axisID);
    uiHarness.pack(applicationManager);
    applicationManager.fiducialModelTrack(axisID, null, null);
    waitForThread(axisID);
    applicationManager.doneFiducialModelDialog(axisID);
  }

  private static void fineAlignment(AxisID axisID) {
    try {
      copyFromDataSource(datasetName + axisID.getExtension() + ".fid");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      return;
    }
    applicationManager.openFineAlignmentDialog(axisID);
    uiHarness.pack(applicationManager);
    applicationManager.fineAlignment(axisID, null, null);
    waitForThread(axisID);
    applicationManager.doneAlignmentEstimationDialog(axisID);
  }

  private static void tomogramPositioning(AxisID axisID) {
    TomogramPositioningExpert expert = (TomogramPositioningExpert) applicationManager
        .getUIExpert(DialogType.TOMOGRAM_POSITIONING, axisID);
    expert.openDialog();
    uiHarness.pack(applicationManager);
    expert.createSample(null, null);
    waitForThread(axisID);
    try {
      copyFromDataSource("tomopitch" + axisID.getExtension() + ".mod");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      return;
    }
    if (applicationManager.getMetaData().isWholeTomogramSample(axisID)) {
      expert.wholeTomogram(null, null);
    }
    else {
      expert.tomopitch(null, null);
    }
    waitForThread(axisID);

    if (!applicationManager.getMetaData().isFiducialessAlignment(axisID)) {
      expert.finalAlign(null, null);
      waitForThread(axisID);
    }
    expert.doneDialog(DialogExitState.SAVE);
  }

  private static void tomogramGeneration(AxisID axisID) {
    TomogramGenerationExpert expert = (TomogramGenerationExpert) applicationManager
        .getUIExpert(DialogType.TOMOGRAM_GENERATION, axisID);
    expert.openDialog();
    uiHarness.pack(applicationManager);
    waitForThread(axisID);
    //applicationManager.mtffilter(axisID);
    //waitForThread(axisID);
    expert.tilt(null, null);
    waitForThread(axisID);
    applicationManager.deleteAlignedStacks(axisID, null);
  }

  private static void tomogramCombination() {
    applicationManager.openTomogramCombinationDialog();
    uiHarness.pack(applicationManager);
    applicationManager.createCombineScripts(null);
    waitForThread(AxisID.ONLY);
    applicationManager.combine(null, null, null, null, null);
    waitForThread(AxisID.ONLY);
  }

  /**
   * Parse the command line string array
   * @param args
   */
  private static void parseCommandLine(String[] args) {
    if (args.length < 1) {
      System.err.println("usage: dataFlowTests.jar [options] datasetName");
      System.err.println("");
      System.err.println("Options:");
      System.err
          .println("--fiducial      Force a fiducial alignment (overide .edf)");
      System.err.println("");
      System.err
          .println("--fiducialless  Force a fiducialless alignment (overide .edf)");
      System.err.println("");
      System.exit(1);
    }

    // Reset the dataset name 
    datasetName = "";
    //  Parse the command line arguments
    for (int i = 0; i < args.length; i++) {
      if (!args[i].startsWith("-")) {
        datasetName = args[i];
      }
      if (args[i].equals("--fiducial")) {
        fiducialSpecified = true;
        fiducialless = false;
      }

      if (args[i].equals("--fiducialless")) {
        fiducialSpecified = true;
        fiducialless = true;
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 3.29  2009/06/11 16:41:07  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Changed
 * <p> preProcessing.
 * <p>
 * <p> Revision 3.28  2008/12/15 22:56:20  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 3.27  2008/10/16 20:54:36  sueh
 * <p> bug# 1141 Moved newst to FinalAlignedStackDialog.  Not keeping
 * <p> DataFlowTests up to date.
 * <p>
 * <p> Revision 3.26  2008/05/28 02:46:57  sueh
 * <p> bug# 1111 Adding null for dialogType parameters.
 * <p>
 * <p> Revision 3.25  2008/05/13 20:54:05  sueh
 * <p> bug# 847 Keep up with changes to ApplicationManager process
 * <p> functions.
 * <p>
 * <p> Revision 3.24  2008/05/06 23:54:45  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 3.23  2008/05/03 00:31:02  sueh
 * <p> bug# 847 In manager classes passing ProcessSeries to the process
 * <p> manager, startNextProcess, and to all process functions.  To avoid having
 * <p> to decide which processes are next processes, pass it everywhere, even
 * <p> to processes that don't use ProcessResultDisplay.  The UI should not
 * <p> create any ProcessSeries and should pass them as null (since they don't
 * <p> know about processes).  Before adding to a process series, create it if it
 * <p> doesn't exist.  Before checking a process series, make sure it exists.
 * <p>  Since a series is only created when it doesn't exist and is needed, I don't
 * <p> have to keep track of which process comes first in a series.
 * <p>
 * <p> Revision 3.22  2007/12/26 21:56:25  sueh
 * <p> bug# 1052 Moved ".edf" to DatasetFiles.
 * <p>
 * <p> Revision 3.21  2007/09/07 00:15:40  sueh
 * <p> bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p> and createInstance.  Call EtomoDirector.main() as early as possible.
 * <p>
 * <p> Revision 3.20  2006/07/26 16:32:21  sueh
 * <p> bug# 868 Moved functions associated with TomogramGenerationDialog from
 * <p> ApplicationManager to TomogramGenerationExpert.
 * <p>
 * <p> Revision 3.19  2006/06/09 19:50:22  sueh
 * <p> bug# 870 Changed UIExpert.doneDialog() to doneDialog(DIalogExitState).
 * <p>
 * <p> Revision 3.18  2006/05/19 19:27:05  sueh
 * <p> bug# 866 Calling tomo pos function in TomogramPositioningExpert instead of
 * <p> ApplicationManager.
 * <p>
 * <p> Revision 3.17  2006/02/06 20:58:31  sueh
 * <p> bug# 521 Added ProcessResultDisplay to ApplicationManager.findXrays.
 * <p>
 * <p> Revision 3.16  2006/01/26 21:47:18  sueh
 * <p> bug# 401 Calling process functions with null when there is a
 * <p> ProcessResultDisplay parameter.
 * <p>
 * <p> Revision 3.15  2006/01/20 20:44:12  sueh
 * <p> bug# 401 Added nulls to calls because of ProcessResultDisplay.
 * <p>
 * <p> Revision 3.14  2005/12/09 20:21:52  sueh
 * <p> fixed file comment
 * <p>
 * <p> Revision 3.13  2005/10/27 00:08:27  sueh
 * <p> bug# 725 Calling preEraser and preCrossCorrelate to process the b stack
 * <p> before running eraser or xcorr.
 * <p>
 * <p> Revision 3.12  2005/08/22 15:57:55  sueh
 * <p> reformatting
 * <p>
 * <p> Revision 3.11  2005/08/04 19:07:18  sueh
 * <p> bug# 532  Sending the manager to UIHarness.pack() so that
 * <p> packDialogs() can be called.
 * <p>
 * <p> Revision 3.10  2005/06/16 19:52:31  sueh
 * <p> bug# 692 Added self test to command line in DataFlowTests.
 * <p>
 * <p> Revision 3.9  2005/04/27 02:10:10  sueh
 * <p> bug# 615 Showing both axes for dual axis.
 * <p>
 * <p> Revision 3.8  2005/04/26 17:35:00  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  All MainFrame
 * <p> functionality is handled through UIHarness to make Etomo more
 * <p> compatible with JUnit.  Removing the mainFrame member variable.
 * <p>
 * <p> Revision 3.7  2005/03/15 01:03:16  sueh
 * <p> bug# 533 Added montaging
 * <p>
 * <p> Revision 3.6  2005/02/18 23:59:29  sueh
 * <p> bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * <p> and localalignments.  Add them for A and B.
 * <p>
 * <p> Revision 3.5  2004/11/19 22:34:12  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.4.2.3  2004/10/08 15:41:59  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.4.2.2  2004/09/07 17:51:30  sueh
 * <p> bug# 520 getting mainFrame from ETomoDirector
 * <p>
 * <p> Revision 3.4.2.1  2004/09/03 20:57:52  sueh
 * <p> bug# 520 getting app mgr from EtomoDirector
 * <p>
 * <p> Revision 3.4  2004/06/30 17:36:46  rickg
 * <p> Added fiducialless capability and partial single axis handling
 * <p>
 * <p> Revision 3.3.2.1  2004/06/30 17:32:39  rickg
 * <p> Added fiducialless capability and partial single axis handling
 * <p>
 * <p> Revision 3.3  2004/04/26 23:41:37  rickg
 * <p> Use copy from Utilities class
 * <p>
 * <p> Revision 3.2  2004/04/26 22:45:17  rickg
 * <p> Copy a seed in from data source directory at beginning
 * <p>
 * <p> Revision 3.1  2004/03/29 21:01:52  sueh
 * <p> bug# 409 added commented out mtffilter code
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.2  2003/11/07 23:15:03  rickg
 * <p> Comments and command line args added
 * <p>
 * <p> Revision 1.1  2003/11/07 00:53:09  rickg
 * <p> *** empty log message ***
 * <p> </p>
 */
