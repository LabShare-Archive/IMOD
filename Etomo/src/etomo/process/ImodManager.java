package etomo.process;
import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstMetaData;

import java.io.File;
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
    if(axisType == AxisType.SINGLE_AXIS) {
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
  public void openRawStack(AxisID axisID) throws Exception {
    checkAxisID(axisID);
    if(axisID == AxisID.SECOND) {
      rawStackB.open();
    }
    else {
      rawStackA.open(); 
    }
  }


  /**
   * Open the specified model with the course aligned imod
   */
  public void modelRawStack(String modelName, AxisID axisID)
  throws Exception {
    // Make sure there is an imod with right course aligned data set that
    // is already open
    openRawStack(axisID);
    if(axisID == AxisID.SECOND) {
      rawStackB.openModel(modelName);
    }
    else {
      rawStackA.openModel(modelName);
    }    
  }


  /**
   * Open the specified coarse aligned stack in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openCoarseAligned(AxisID axisID) throws Exception {
    checkAxisID(axisID);
    if(axisID == AxisID.SECOND) {
      coarseAlignedB.open();
    }
    else {
      coarseAlignedA.open(); 
    }
  }


  /**
   * Open the specified model with the course aligned imod
   */
  public void modelCoarseAligned(String modelName, AxisID axisID)
  throws Exception {
    // Make sure there is an imod with right coarse aligned data set that
    // is already open
    openCoarseAligned(axisID);
    if(axisID == AxisID.SECOND) {
      coarseAlignedB.openModel(modelName);
    }
    else {
      coarseAlignedA.openModel(modelName);
    }    
  }
  

  /**
   * Open the specified fine aligned stack in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openFineAligned(AxisID axisID) throws Exception {
    checkAxisID(axisID);
    if(axisID == AxisID.SECOND) {
      fineAlignedB.open();
    }
    else {
      fineAlignedA.open(); 
    }
  }


  /**
   * Open the specified tomograph samples in imod if they are not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openSample(AxisID axisID) throws Exception {
    checkAxisID(axisID);
    if(axisID == AxisID.SECOND) {
      sampleB.open();
    }
    else {
      sampleA.open(); 
    }
  }

  /**
   * Open the specified tomogram in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openTomogram(AxisID axisID) throws Exception {
    checkAxisID(axisID);
    if(axisID == AxisID.SECOND) {
      tomogramB.open();
    }
    else {
      tomogramA.open(); 
    }
  }

  /**
   * Open the specified tomogram in imod if it is not already open
   * @param axisID the AxisID of the desired axis.
   */
  public void openCombinedTomogram(AxisID axisID) throws Exception {
    checkAxisID(axisID);
    if(axisID == AxisID.SECOND) {
      combinedTomogram.open();
    }
    else {
      combinedTomogram.open(); 
    }
  }

  private void checkAxisID(AxisID axisID) throws Exception {
    if(axisType == AxisType.SINGLE_AXIS  && axisID == AxisID.SECOND) {
      throw new Exception("Second axis requested in a single axis data set");
    }
  }  
}
