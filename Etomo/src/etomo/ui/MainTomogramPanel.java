package etomo.ui;

import java.io.File;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.storage.EtomoFileFilter;
import etomo.type.AxisID;
import etomo.type.MetaData;
import etomo.type.ProcessTrack;
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
* <p> Revision 1.6  2005/03/01 22:06:14  sueh
* <p> bug# 610 Set ApplicationManager.currentDialogType to null when
* <p> displaying an empty dialog.
* <p>
* <p> Revision 1.5  2005/02/25 19:11:56  sueh
* <p> working on java 1.5 fitting issues.  Added commented out override of
* <p> packaxis().
* <p>
* <p> Revision 1.4  2005/02/17 02:44:31  sueh
* <p> bug# 605 Added saveDisplayState() to call saveDisplayState() in axis
* <p> panels.
* <p>
* <p> Revision 1.3  2004/12/16 02:52:32  sueh
* <p> bug# 559 Removing everything from the status bar except the param file
* <p> and path.
* <p>
* <p> Revision 1.2  2004/11/19 23:59:02  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.6  2004/10/15 00:50:34  sueh
* <p> bug# 520 Added implementation of getDataFileFilter.
* <p>
* <p> Revision 1.1.2.5  2004/10/11 02:16:18  sueh
* <p> bug# 520 Moved responsibility for axisPanelA and axisPanelB member
* <p> variables to the child classes.  Used abstract functions to use these
* <p> variables in the base class.  This is more reliable and doesn't require
* <p> casting.
* <p>
* <p> Revision 1.1.2.4  2004/10/08 16:34:23  sueh
* <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
* <p> member variables non-static.
* <p>
* <p> Revision 1.1.2.3  2004/09/29 19:38:03  sueh
* <p> bug# 520 Created private variables that are cast from base-class
* <p> member variables during construction.  Moved status bar initiailization from
* <p> base class.
* <p>
* <p> Revision 1.1.2.2  2004/09/15 22:46:52  sueh
* <p> bug# 520 Moved openSetupPanel back to this class.  Moved
* <p> showProcessingPanel() to this base class.  Created AxisProcessPanel
* <p> creation functions.
* <p>
* <p> Revision 1.1.2.1  2004/09/08 20:13:41  sueh
* <p> bug# 520 class contains tomogram specific functionality from MainPAnel,
* <p> which is its base class.  Casts member variables which are used as super
* <p> classes in MainPanel.
* <p> </p>
*/
public class MainTomogramPanel extends MainPanel {
  public static  final String  rcsid =  "$Id$";

  private TomogramProcessPanel axisPanelA;
  private TomogramProcessPanel axisPanelB;
  
  /**
   * @param appManager
   */
  public MainTomogramPanel(ApplicationManager appManager) {
    super(appManager);
  }
  
  protected DataFileFilter getDataFileFilter() {
    return new EtomoFileFilter();
  }
  
  public void saveDisplayState() {
    if (axisPanelA != null) {
      axisPanelA.saveDisplayState();
    }
    if (axisPanelB != null) {
     axisPanelB.saveDisplayState();
    }
  }
  
  /**
   * Update the state of all the process control panels
   * @param processTrack the process track object containing the state to be
   * displayed
   */
  public void updateAllProcessingStates(ProcessTrack processTrack) {
    if (axisPanelA == null) {
      return;
    }

    axisPanelA.setPreProcState(processTrack.getPreProcessingState(AxisID.ONLY));
    axisPanelA.setCoarseAlignState(
      processTrack.getCoarseAlignmentState(AxisID.ONLY));
    axisPanelA.setFiducialModelState(
      processTrack.getFiducialModelState(AxisID.ONLY));
    axisPanelA.setFineAlignmentState(
      processTrack.getFineAlignmentState(AxisID.ONLY));
    axisPanelA.setTomogramPositioningState(
      processTrack.getTomogramPositioningState(AxisID.ONLY));
    axisPanelA.setTomogramGenerationState(
      processTrack.getTomogramGenerationState(AxisID.ONLY));
    axisPanelA.setTomogramCombinationState(
      processTrack.getTomogramCombinationState());
    if (manager.isDualAxis()) {
      axisPanelB.setPreProcState(
        processTrack.getPreProcessingState(AxisID.SECOND));
      axisPanelB.setCoarseAlignState(
        processTrack.getCoarseAlignmentState(AxisID.SECOND));
      axisPanelB.setFiducialModelState(
        processTrack.getFiducialModelState(AxisID.SECOND));
      axisPanelB.setFineAlignmentState(
        processTrack.getFineAlignmentState(AxisID.SECOND));
      axisPanelB.setTomogramPositioningState(
        processTrack.getTomogramPositioningState(AxisID.SECOND));
      axisPanelB.setTomogramGenerationState(
        processTrack.getTomogramGenerationState(AxisID.SECOND));
    }
    axisPanelA.setPostProcessingState(processTrack.getPostProcessingState());
    axisPanelA.setCleanUpState(processTrack.getCleanUpState());
  }

  /**
   * Open the setup panel
   */
  public void openSetupPanel(SetupDialog setupDialog) {
    panelCenter.removeAll();
    panelCenter.add(setupDialog.getContainer());
    revalidate();
    EtomoDirector.getInstance().getMainFrame().pack();
  }

  /**
   * Set the specified button as selected
   * @param axisID
   * @param name
   */
  public void selectButton(AxisID axisID, String name) {
    mapAxis(axisID).selectButton(name);
  }
  
  /**
   * 
   * @param state
   * @param axisID
   */
  public void setPreProcessingState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setPreProcState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setCoarseAlignState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setCoarseAlignState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setFiducialModelState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setFiducialModelState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setFineAlignmentState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setFineAlignmentState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setTomogramPositioningState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setTomogramPositioningState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setTomogramGenerationState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setTomogramGenerationState(state);
  }

  /**
   * 
   * @param state
   */
  public void setTomogramCombinationState(ProcessState state) {
    axisPanelA.setTomogramCombinationState(state);
  }

  /**
   * 
   * @param state
   */
  public void setPostProcessingState(ProcessState state) {
    axisPanelA.setPostProcessingState(state);
  }
  
  /**
   * 
   * @param state
   */
  public void setCleanUpState(ProcessState state) {
    axisPanelA.setCleanUpState(state);
  }
  
  protected void createAxisPanelA(AxisID axisID) {
    axisPanelA = new TomogramProcessPanel((ApplicationManager) manager, axisID);
    
  }

  protected void createAxisPanelB() {
    axisPanelB = new TomogramProcessPanel((ApplicationManager) manager, AxisID.SECOND);
  }
  
  /**
   * Set the status bar with the file name of the data parameter file
   */
  public void updateDataParameters(File paramFile, MetaData metaData) {
    StringBuffer buffer = new StringBuffer();
    if (metaData == null) {
      buffer.append("No data set loaded");
    }
    else {
      if (paramFile == null) {
        buffer.append("Data file: NOT SAVED");
      }
      else {
        buffer.append("Data file: " + paramFile.getAbsolutePath());
      }
      //buffer.append("   Axis type: ");
      //buffer.append(metaData.getAxisType().toString());
    }
    statusBar.setText(buffer.toString());
  }
  
  protected void resetAxisPanels() {
    axisPanelA = null;
    axisPanelB = null;
  }
  
  protected void addAxisPanelA() {
    scrollA.add(axisPanelA.getContainer());
  }
  
  protected void addAxisPanelB() {
    scrollB.add(axisPanelB.getContainer());
  }
  
  protected boolean AxisPanelAIsNull() {
    return axisPanelA == null;
  }
  
  protected boolean AxisPanelBIsNull() {
    return axisPanelB == null;
  }
  
  protected boolean hideAxisPanelA() {
    return axisPanelA.hide();
  }
  
  protected boolean hideAxisPanelB() {
    return axisPanelB.hide();
  }
  
  protected void showAxisPanelA() {
    axisPanelA.show();
  }
  
  protected void showAxisPanelB() {
    axisPanelB.show();
  }
  /*
  protected void packAxis() {
    //(new Exception()).printStackTrace();
    if (splitPane == null) {
      EtomoDirector.getInstance().getMainFrame().pack();
      return;
    }
    int widthA = axisPanelA.getWidth();
    int widthB = axisPanelB.getWidth();
    if (widthA == 0 && widthB == 0) {
      EtomoDirector.getInstance().getMainFrame().pack();
      return;
    }
    //System.out.println("dual panel:widthA="+widthA+",widthB="+widthB);
    if (widthA < 20) {
      axisPanelA.show(false);
    }
    else if (widthB < 20) {
      axisPanelB.show(false);
    }
    EtomoDirector.getInstance().getMainFrame().pack();
    if (widthA < 20) {
      axisPanelA.show(true);
      splitPane.setDividerLocation(0);
    }
    else if (widthB < 20) {
      axisPanelB.show(true);
      splitPane.setDividerLocation(1);
    }
  }*/
  
  protected boolean isAxisPanelAFitScreenError() {
    return isFitScreenError(axisPanelA);
  }
  
  /**
   * Show a blank processing panel
   */
  public void showBlankProcess(AxisID axisID) {
    ((ApplicationManager) manager).setCurrentDialogType(null, axisID);
    super.showBlankProcess(axisID);
  }
  
  /**
   * Convienence function to return a reference to the correct AxisProcessPanel
   * @param axisID
   * @return
   */
  private TomogramProcessPanel mapAxis(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return axisPanelB;
    }
    return axisPanelA;
  }
  
  protected AxisProcessPanel mapBaseAxis(AxisID axisID) {
    return (AxisProcessPanel)  mapAxis(axisID);
  }

}
