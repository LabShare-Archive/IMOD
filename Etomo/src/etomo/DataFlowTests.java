/**
 * <p>Description: </p>
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
 * <p> $Log$ </p>
 */
package etomo;

import java.io.File;

import etomo.process.SystemProcessException;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.ui.MainFrame;

public class DataFlowTests {
  public static final String rcsid = "$Id$";

  static ApplicationManager applicationManager;
  static MainFrame mainFrame;
  static String datasetName = "hvemuni5_by2";
  public static void main(String[] args) {

    File workingDirectory = new File("/scratch/wanderer/rickg/DataFlowTest");
    System.setProperty("user.dir", workingDirectory.getAbsolutePath());

    try {
      copyFromDataSource(datasetName + "a.st");
      copyFromDataSource(datasetName + "b.st");
      copyFromDataSource(datasetName + "a.rawtlt");
      copyFromDataSource(datasetName + "b.rawtlt");
      copyFromDataSource(datasetName + ".edf");
    }
    catch (SystemProcessException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    //  Simple test: create a new ApplicationManager and cause it to quit
    String[] argsIn = new String[2];
    argsIn[0] = "--debug";
    argsIn[1] =
      System.getProperty("user.dir") + File.separator + datasetName + ".edf";
    applicationManager = new ApplicationManager(argsIn);
    mainFrame = applicationManager.getMainFrame();

    preProcessing(AxisID.FIRST);
    preProcessing(AxisID.SECOND);

    coarseAlignment(AxisID.FIRST);
    coarseAlignment(AxisID.SECOND);

    fiducialModelGen(AxisID.FIRST);
    fineAlignment(AxisID.FIRST);
		transferfid(AxisID.SECOND);
		fiducialModelGen(AxisID.SECOND);
		fineAlignment(AxisID.SECOND);

    tomogramPositioning(AxisID.FIRST);
		tomogramPositioning(AxisID.SECOND);
    
    tomogramGeneration(AxisID.FIRST);
		tomogramGeneration(AxisID.SECOND);
	
		tomogramCombination();
  }

  private static void waitForThread(AxisID axisID) {
    while (applicationManager.getThreadName(axisID) != "none") {
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private static void copyFromDataSource(String filename)
    throws SystemProcessException {
    System.out.println("Copying from DataSource directory: " + filename);
    String commandLine =
      "cp "
        + System.getProperty("user.dir")
        + File.separator
        + "DataSource"
        + File.separator
        + filename
        + " "
        + System.getProperty("user.dir")
        + File.separator
        + filename;
    SystemProgram cp = new SystemProgram(commandLine);
    cp.run();
    if (cp.getExitValue() != 0) {
      throw new SystemProcessException(
        "Unable to copy " + filename + " from DataSource directory");
    }
  }

  private static void preProcessing(AxisID axisID) {
    applicationManager.openPreProcDialog(axisID);
    mainFrame.pack();
    applicationManager.findXrays(axisID);
    waitForThread(axisID);
    applicationManager.eraser(axisID);
    waitForThread(axisID);
    applicationManager.replaceRawStack(axisID);
    applicationManager.donePreProcDialog(axisID);
  }

  private static void coarseAlignment(AxisID axisID) {
    applicationManager.openCoarseAlignDialog(axisID);
    mainFrame.pack();
    applicationManager.crossCorrelate(axisID);
    waitForThread(axisID);
    applicationManager.coarseAlign(axisID);
    waitForThread(axisID);
    applicationManager.doneCoarseAlignDialog(axisID);
  }

  private static void transferfid(AxisID destAxisID) {
		applicationManager.openFiducialModelDialog(destAxisID);
		mainFrame.pack();
		applicationManager.transferfid(destAxisID);
		waitForThread(destAxisID);
		
  }
  
  private static void fiducialModelGen(AxisID axisID) {
    try {
      copyFromDataSource(datasetName + axisID.getExtension() + ".seed");
    }
    catch (SystemProcessException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return;
    }
    applicationManager.openFiducialModelDialog(axisID);
		mainFrame.pack();
    mainFrame.pack();
    applicationManager.fiducialModelTrack(axisID);
    waitForThread(axisID);
    applicationManager.doneFiducialModelDialog(axisID);
  }

  private static void fineAlignment(AxisID axisID) {
    try {
      copyFromDataSource(datasetName + axisID.getExtension() + ".fid");
    }
    catch (SystemProcessException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return;
    }
    applicationManager.openFineAlignmentDialog(axisID);
    mainFrame.pack();
    //  FIXME: obviously one of these names should be changed
    applicationManager.fineAlignment(axisID);
    waitForThread(axisID);
    applicationManager.doneAlignmentEstimationDialog(axisID);
  }

  private static void tomogramPositioning(AxisID axisID) {
    applicationManager.openTomogramPositioningDialog(axisID);
		mainFrame.pack();
    applicationManager.createSample(axisID);
    waitForThread(axisID);
    try {
      copyFromDataSource("tomopitch" + axisID.getExtension() + ".mod");
    }
    catch (SystemProcessException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return;
    }

    applicationManager.tomopitch(axisID);
    waitForThread(axisID);
    // TODO modify thickness angle offset and z shift
    applicationManager.finalAlign(axisID);
    waitForThread(axisID);
    applicationManager.doneTomogramPositioningDialog(axisID);
  }

  private static void tomogramGeneration(AxisID axisID) {
    applicationManager.openTomogramGenerationDialog(axisID);
		mainFrame.pack();
    applicationManager.newst(axisID);
    waitForThread(axisID);
    applicationManager.tilt(axisID);
    waitForThread(axisID);
    applicationManager.deleteAlignedStacks(axisID);
  }

  private static void tomogramCombination() {
    applicationManager.openTomogramCombinationDialog();
		mainFrame.pack();
    applicationManager.createCombineScripts();
    waitForThread(AxisID.ONLY);
    applicationManager.combine();
    waitForThread(AxisID.ONLY);

  }
}
