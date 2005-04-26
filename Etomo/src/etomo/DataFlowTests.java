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
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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

package etomo;

import java.io.File;
import java.io.IOException;

import etomo.process.SystemProcessException;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.MetaData;
import etomo.type.ViewType;
import etomo.ui.UIHarness;
import etomo.util.Utilities;

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
      copyFromDataSource(datasetName + ".edf");
      copyFromDataSource(datasetName + "a.st");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
    }

    //  Simple test: create a new ApplicationManager and cause it to quit
    String[] argsIn = new String[2];
    argsIn[0] = "--debug";
    argsIn[1] = System.getProperty("user.dir") + File.separator + datasetName
      + ".edf";
    EtomoDirector.createInstance(argsIn);
    applicationManager = (ApplicationManager) EtomoDirector.getInstance().getCurrentManager();
    //mainFrame = EtomoDirector.getInstance().getMainFrame();
    // A hack around the const object returned we really know is not const
    MetaData metaData = (MetaData) applicationManager.getMetaData();
    boolean montage = metaData.getViewType() == ViewType.MONTAGE;
    try {
      if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
        copyFromDataSource(datasetName + "b.st");
        copyFromDataSource(datasetName + "a.rawtlt");
        copyFromDataSource(datasetName + "b.rawtlt");
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
    uiHarness.pack();
    applicationManager.findXrays(axisID);
    waitForThread(axisID);
    applicationManager.eraser(axisID);
    waitForThread(axisID);
    applicationManager.replaceRawStack(axisID);
    applicationManager.donePreProcDialog(axisID);
  }

  private static void coarseAlignment(AxisID axisID) {
    applicationManager.openCoarseAlignDialog(axisID);
    uiHarness.pack();
    applicationManager.crossCorrelate(axisID);
    waitForThread(axisID);
    if(!applicationManager.getMetaData().isFiducialessAlignment(axisID)) {
      applicationManager.coarseAlign(axisID);
      waitForThread(axisID);
    }
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  private static void transferfid(AxisID destAxisID) {
    applicationManager.openFiducialModelDialog(destAxisID);
    uiHarness.pack();
    applicationManager.transferfid(destAxisID);
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
    uiHarness.pack();
    applicationManager.fiducialModelTrack(axisID);
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
    uiHarness.pack();
    applicationManager.fineAlignment(axisID);
    waitForThread(axisID);
    applicationManager.doneAlignmentEstimationDialog(axisID);
  }

  private static void tomogramPositioning(AxisID axisID) {
    applicationManager.openTomogramPositioningDialog(axisID);
    uiHarness.pack();
    applicationManager.createSample(axisID);
    waitForThread(axisID);
    try {
      copyFromDataSource("tomopitch" + axisID.getExtension() + ".mod");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      return;
    }
    if(applicationManager.getMetaData().isWholeTomogramSample(axisID)) {
      applicationManager.wholeTomogram(axisID);
    }
    else {
      applicationManager.tomopitch(axisID);
    }
    waitForThread(axisID);

    if(!applicationManager.getMetaData().isFiducialessAlignment(axisID)) {
      applicationManager.finalAlign(axisID);
      waitForThread(axisID);
    }
    applicationManager.doneTomogramPositioningDialog(axisID);
  }

  private static void tomogramGeneration(AxisID axisID) {
    applicationManager.openTomogramGenerationDialog(axisID);
    uiHarness.pack();
    applicationManager.newst(axisID);
    waitForThread(axisID);
    //applicationManager.mtffilter(axisID);
    //waitForThread(axisID);
    applicationManager.tilt(axisID);
    waitForThread(axisID);
    applicationManager.deleteAlignedStacks(axisID);
  }

  private static void tomogramCombination() {
    applicationManager.openTomogramCombinationDialog();
    uiHarness.pack();
    applicationManager.createCombineScripts();
    waitForThread(AxisID.ONLY);
    applicationManager.combine();
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