package etomo.ui.swing;

import javax.swing.JScrollPane;

import etomo.ApplicationManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.storage.EtomoFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DialogType;
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
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.24  2009/06/11 16:57:16  sueh
 * <p> bug# 1221 Formatted.
 * <p>
 * <p> Revision 1.23  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.22  2008/10/16 21:37:02  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 1.21  2007/12/26 22:25:17  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.20  2007/02/19 22:03:02  sueh
 * <p> bug# 964 Fixed function names:  was AxisPanelIsNull, now its isAxisPanelNull.
 * <p>
 * <p> Revision 1.19  2006/07/26 16:40:28  sueh
 * <p> bug# 868 Made setState() public
 * <p>
 * <p> Revision 1.18  2006/03/20 18:04:38  sueh
 * <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
 * <p>
 * <p> Revision 1.17  2005/10/29 00:06:03  sueh
 * <p> bug# 532 Turning off parallel panel when displaying a blank dialog.
 * <p>
 * <p> Revision 1.16  2005/09/21 16:44:30  sueh
 * <p> bug# 532 Added setState(ProcessState, AxisID, ParallelDialog) and
 * <p> setState(ProcessState, AxisID, DialogType) so that one processchunks
 * <p> function in BaseManager can handle multiple dialogs.
 * <p>
 * <p> Revision 1.15  2005/08/04 20:12:43  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 1.14  2005/05/17 19:38:51  sueh
 * <p> bug# 663 Renamed updateDataParameters() to setStatusBarText() and
 * <p> moved the common functionality to MainPanel.setStatusBarText().
 * <p>
 * <p> Revision 1.13  2005/04/26 17:40:53  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  All MainFrame
 * <p> functionality is handled through UIHarness to make Etomo more
 * <p> compatible with JUnit.
 * <p>
 * <p> Revision 1.12  2005/04/25 21:10:48  sueh
 * <p> bug# 615 Moved showingSetup to the parent class so it can be queried
 * <p> there.
 * <p>
 * <p> Revision 1.11  2005/04/21 20:45:40  sueh
 * <p> bug# 615 Moved two frame code out of newstuff.  Removed
 * <p> isAxisPanelAFitScreenError(), since it is not necessary.  Using
 * <p> showBothAxis() to display the B axis in SubFrame.
 * <p>
 * <p> Revision 1.10  2005/04/20 01:51:46  sueh
 * <p> bug# 615 Added showingSetup boolean to prevent from showing axis A
 * <p> when setup tomogram is displayed.
 * <p>
 * <p> Revision 1.9  2005/04/01 02:54:43  sueh
 * <p> bug# 622 Added getAxisPAnelA and B.
 * <p>
 * <p> Revision 1.8  2005/04/01 00:16:07  sueh
 * <p> bug# 622 Overriding showAxisA, B, and Both.  Need to change the button
 * <p> names on the TomogramProcessPanel before calling the base class
 * <p> functions.
 * <p>
 * <p> Revision 1.7  2005/03/24 17:52:43  sueh
 * <p> bug# 621 Added Clean Up dialog.
 * <p>
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
  public static final String rcsid = "$Id$";

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

  void showAxisA() {
    if (showingSetup || axisType == AxisType.SINGLE_AXIS) {
      UIHarness.INSTANCE.pack(true, manager);
    }
    else {
      if (axisPanelA != null) {
        axisPanelA.showAxisA();
        super.showAxisA();
      }
    }
  }

  void showAxisB() {
    axisPanelB.showAxisB();
    super.showAxisB();
  }

  JScrollPane showBothAxis() {
    axisPanelB.showAxisB();
    return super.showBothAxis();
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
    axisPanelA.setCoarseAlignState(processTrack.getCoarseAlignmentState(AxisID.ONLY));
    axisPanelA.setFiducialModelState(processTrack.getFiducialModelState(AxisID.ONLY));
    axisPanelA.setFineAlignmentState(processTrack.getFineAlignmentState(AxisID.ONLY));
    axisPanelA.setTomogramPositioningState(processTrack
        .getTomogramPositioningState(AxisID.ONLY));
    axisPanelA.setFinalAlignedStackState(processTrack
        .getFinalAlignedStackState(AxisID.ONLY));
    axisPanelA.setTomogramGenerationState(processTrack
        .getTomogramGenerationState(AxisID.ONLY));
    axisPanelA.setTomogramCombinationState(processTrack.getTomogramCombinationState());
    if (manager.isDualAxis()) {
      axisPanelB.setPreProcState(processTrack.getPreProcessingState(AxisID.SECOND));
      axisPanelB.setCoarseAlignState(processTrack.getCoarseAlignmentState(AxisID.SECOND));
      axisPanelB.setFiducialModelState(processTrack.getFiducialModelState(AxisID.SECOND));
      axisPanelB.setFineAlignmentState(processTrack.getFineAlignmentState(AxisID.SECOND));
      axisPanelB.setTomogramPositioningState(processTrack
          .getTomogramPositioningState(AxisID.SECOND));
      axisPanelB.setFinalAlignedStackState(processTrack
          .getFinalAlignedStackState(AxisID.SECOND));
      axisPanelB.setTomogramGenerationState(processTrack
          .getTomogramGenerationState(AxisID.SECOND));
    }
    axisPanelA.setPostProcessingState(processTrack.getPostProcessingState());
    axisPanelA.setCleanUpState(processTrack.getCleanUpState());
  }

  public void showProcessingPanel(AxisType axisType) {
    showingSetup = false;
    super.showProcessingPanel(axisType);
  }

  /**
   * Open the setup panel
   */
  public void openSetupPanel(SetupDialogExpert setupDialogExpert) {
    showingSetup = true;
    panelCenter.removeAll();
    panelCenter.add(setupDialogExpert.getContainer());
    revalidate();
    UIHarness.INSTANCE.pack(manager);
  }

  /**
   * Set the specified button as selected
   * @param axisID
   * @param name
   */
  public void selectButton(AxisID axisID, String name) {
    mapAxis(axisID).selectButton(name);
  }

  public final void setState(ProcessState processState, AxisID axisID,
      AbstractParallelDialog parallelDialog) {
    setState(processState, axisID, parallelDialog.getDialogType());
  }

  public final void setState(ProcessState processState, AxisID axisID,
      DialogType dialogType) {
    if (dialogType == DialogType.CLEAN_UP) {
      setCleanUpState(processState);
    }
    else if (dialogType == DialogType.COARSE_ALIGNMENT) {
      setCoarseAlignState(processState, axisID);
    }
    else if (dialogType == DialogType.FIDUCIAL_MODEL) {
      setFiducialModelState(processState, axisID);
    }
    else if (dialogType == DialogType.FINE_ALIGNMENT) {
      setFineAlignmentState(processState, axisID);
    }
    else if (dialogType == DialogType.POST_PROCESSING) {
      setPostProcessingState(processState);
    }
    else if (dialogType == DialogType.PRE_PROCESSING) {
      setPreProcessingState(processState, axisID);
    }
    else if (dialogType == DialogType.TOMOGRAM_COMBINATION) {
      setTomogramCombinationState(processState);
    }
    else if (dialogType == DialogType.FINAL_ALIGNED_STACK) {
      setFinalAlignedStackState(processState, axisID);
    }
    else if (dialogType == DialogType.TOMOGRAM_GENERATION) {
      setTomogramGenerationState(processState, axisID);
    }
    else if (dialogType == DialogType.TOMOGRAM_POSITIONING) {
      setTomogramPositioningState(processState, axisID);
    }
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
  public void setFinalAlignedStackState(ProcessState state, AxisID axisID) {
    TomogramProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setFinalAlignedStackState(state);
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

  protected boolean isAxisPanelANull() {
    return axisPanelA == null;
  }

  protected boolean isAxisPanelBNull() {
    return axisPanelB == null;
  }

  protected AxisProcessPanel getAxisPanelA() {
    return axisPanelA;
  }

  protected AxisProcessPanel getAxisPanelB() {
    return axisPanelB;
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
   }
   
   protected boolean isAxisPanelAFitScreenError() {
   return isFitScreenError(axisPanelA);
   }*/

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
    return (AxisProcessPanel) mapAxis(axisID);
  }

}
