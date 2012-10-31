package etomo.type;

import etomo.process.ProcessState;
import etomo.ui.swing.AbstractParallelDialog;

import java.util.Properties;

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
 * <p> Revision 3.11  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.10  2009/06/11 16:49:11  sueh
 * <p> bug# 1221 Formatted.
 * <p>
 * <p> Revision 3.9  2008/10/16 21:00:00  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 3.8  2006/07/26 16:39:28  sueh
 * <p> bug# 868 Made setState() public
 * <p>
 * <p> Revision 3.7  2006/06/27 17:48:44  sueh
 * <p> bug# 879 Fixed problem:  wrong post processing string being written.
 * <p>
 * <p> Revision 3.6  2006/04/25 18:57:30  sueh
 * <p> bug# 787 Changed DialogType.SETUP to SETUP_RECON.
 * <p>
 * <p> Revision 3.5  2006/03/20 17:59:32  sueh
 * <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
 * <p>
 * <p> Revision 3.4  2005/09/21 16:18:06  sueh
 * <p> bug# 532 Added setState(ProcessState, AxisID, ParallelDialog) and
 * <p> setState(ProcessState, AxisID, DialogType) so that one processchunks
 * <p> function in BaseManager can handle multiple dialogs.
 * <p>
 * <p> Revision 3.3  2005/03/24 17:49:08  sueh
 * <p> bug# 621 Added Clean Up dialog state.
 * <p>
 * <p> Revision 3.2  2004/12/14 21:48:28  sueh
 * <p> bug# 565 Turning base class into an interface.
 * <p>
 * <p> Revision 3.1  2004/11/19 23:36:32  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.0.6.1  2004/09/29 19:31:50  sueh
 * <p> bug# 520 Added base class BaseProcessTrack.  Moved Storable to base
 * <p> class.  Moved generic load and store functions, modified functionality,
 * <p> and revision functionality to base class.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.3  2003/01/27 17:36:28  rickg
 * <p> Fixed set* logic bug that set the A axis when B was selected
 * <p>
 * <p> Revision 2.2  2003/01/27 15:26:06  rickg
 * <p> Static function fix
 * <p>
 * <p> Revision 2.1  2003/01/25 00:09:50  rickg
 * <p> B axis saving grabbing A
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2002/10/07 22:29:51  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessTrack implements BaseProcessTrack {
  public static final String rcsid = "$Id$";

  private ProcessState setup = ProcessState.NOTSTARTED;

  private ProcessState preProcessingA = ProcessState.NOTSTARTED;
  private ProcessState coarseAlignmentA = ProcessState.NOTSTARTED;
  private ProcessState fiducialModelA = ProcessState.NOTSTARTED;
  private ProcessState fineAlignmentA = ProcessState.NOTSTARTED;
  private ProcessState tomogramPositioningA = ProcessState.NOTSTARTED;
  private ProcessState finalAlignedStackA = ProcessState.NOTSTARTED;
  private ProcessState tomogramGenerationA = ProcessState.NOTSTARTED;

  private ProcessState tomogramCombination = ProcessState.NOTSTARTED;
  private ProcessState postProcessing = ProcessState.NOTSTARTED;
  private ProcessState cleanUp = ProcessState.NOTSTARTED;

  private ProcessState preProcessingB = ProcessState.NOTSTARTED;
  private ProcessState coarseAlignmentB = ProcessState.NOTSTARTED;
  private ProcessState fiducialModelB = ProcessState.NOTSTARTED;
  private ProcessState fineAlignmentB = ProcessState.NOTSTARTED;
  private ProcessState tomogramPositioningB = ProcessState.NOTSTARTED;
  private ProcessState finalAlignedStackB = ProcessState.NOTSTARTED;
  private ProcessState tomogramGenerationB = ProcessState.NOTSTARTED;

  protected String revisionNumber;
  protected boolean isModified = false;

  public ProcessTrack() {
    revisionNumber = "2.0";
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void load(Properties props) {
    load(props, "");
  }

  public String getRevisionNumber() {
    return revisionNumber;
  }

  public boolean isModified() {
    return isModified;
  }

  public void resetModified() {
    isModified = false;
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "ProcessTrack.";
    }
    else {
      group = prepend + ".ProcessTrack.";
    }
    props.setProperty(group + "RevisionNumber", revisionNumber);
    props.setProperty(group + "Setup", setup.toString());

    props.setProperty(group + "PreProcessing-A", preProcessingA.toString());
    props.setProperty(group + "PreProcessing-B", preProcessingB.toString());

    props.setProperty(group + "CoarseAlignment-A", coarseAlignmentA.toString());
    props.setProperty(group + "CoarseAlignment-B", coarseAlignmentB.toString());

    props.setProperty(group + "FiducialModel-A", fiducialModelA.toString());
    props.setProperty(group + "FiducialModel-B", fiducialModelB.toString());

    props.setProperty(group + "FineAlignment-A", fineAlignmentA.toString());
    props.setProperty(group + "FineAlignment-B", fineAlignmentB.toString());

    props.setProperty(group + "TomogramPositioning-A", tomogramPositioningA.toString());
    props.setProperty(group + "TomogramPositioning-B", tomogramPositioningB.toString());

    props.setProperty(group + "FinalAlignedStack-A", finalAlignedStackA.toString());
    props.setProperty(group + "FinalAlignedStack-B", finalAlignedStackB.toString());

    props.setProperty(group + "TomogramGeneration-A", tomogramGenerationA.toString());
    props.setProperty(group + "TomogramGeneration-B", tomogramGenerationB.toString());

    props.setProperty(group + "TomogramCombination", tomogramCombination.toString());

    props.setProperty(group + "PostProcessing", postProcessing.toString());
    props.setProperty(group + "CleanUp", cleanUp.toString());
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "ProcessTrack.";
    }
    else {
      group = prepend + ".ProcessTrack.";
    }
    revisionNumber = props.getProperty(group + "RevisionNumber", "1.0");

    setup = ProcessState.fromString(props.getProperty(group + "Setup", "Not started"));

    tomogramCombination = ProcessState.fromString(props.getProperty(group
        + "TomogramCombination", "Not started"));

    postProcessing = ProcessState.fromString(props.getProperty(group + "PostProcessing",
        "Not started"));
    cleanUp = ProcessState
        .fromString(props.getProperty(group + "CleanUp", "Not started"));

    // Added separate process for A and B axis for 2.0 layout
    if (Double.parseDouble(revisionNumber) < 2.0) {
      preProcessingA = ProcessState.fromString(props.getProperty(group + "PreProcessing",
          "Not started"));
      preProcessingB = preProcessingA;

      coarseAlignmentA = ProcessState.fromString(props.getProperty(group
          + "CoarseAlignment", "Not started"));
      coarseAlignmentB = coarseAlignmentA;

      fiducialModelA = ProcessState.fromString(props.getProperty(group + "FiducialModel",
          "Not started"));
      fiducialModelB = fiducialModelA;

      fineAlignmentA = ProcessState.fromString(props.getProperty(group + "FineAlignment",
          "Not started"));
      fineAlignmentB = fineAlignmentA;

      tomogramPositioningA = ProcessState.fromString(props.getProperty(group
          + "TomogramPositioning", "Not started"));
      tomogramPositioningB = tomogramPositioningA;

      finalAlignedStackA = ProcessState.fromString(props.getProperty(group
          + "FinalAlignedStack", "Not started"));
      finalAlignedStackB = finalAlignedStackA;

      tomogramGenerationA = ProcessState.fromString(props.getProperty(group
          + "TomogramGeneration", "Not started"));
      tomogramGenerationB = tomogramGenerationA;
    }
    else {
      preProcessingA = ProcessState.fromString(props.getProperty(group
          + "PreProcessing-A", "Not started"));
      preProcessingB = ProcessState.fromString(props.getProperty(group
          + "PreProcessing-B", "Not started"));

      coarseAlignmentA = ProcessState.fromString(props.getProperty(group
          + "CoarseAlignment-A", "Not started"));
      coarseAlignmentB = ProcessState.fromString(props.getProperty(group
          + "CoarseAlignment-B", "Not started"));

      fiducialModelA = ProcessState.fromString(props.getProperty(group
          + "FiducialModel-A", "Not started"));
      fiducialModelB = ProcessState.fromString(props.getProperty(group
          + "FiducialModel-B", "Not started"));

      fineAlignmentA = ProcessState.fromString(props.getProperty(group
          + "FineAlignment-A", "Not started"));
      fineAlignmentB = ProcessState.fromString(props.getProperty(group
          + "FineAlignment-B", "Not started"));

      tomogramPositioningA = ProcessState.fromString(props.getProperty(group
          + "TomogramPositioning-A", "Not started"));
      tomogramPositioningB = ProcessState.fromString(props.getProperty(group
          + "TomogramPositioning-B", "Not started"));

      finalAlignedStackA = ProcessState.fromString(props.getProperty(group
          + "FinalAlignedStack-A", "Not started"));
      finalAlignedStackB = ProcessState.fromString(props.getProperty(group
          + "FinalAlignedStack-B", "Not started"));

      tomogramGenerationA = ProcessState.fromString(props.getProperty(group
          + "TomogramGeneration-A", "Not started"));
      tomogramGenerationB = ProcessState.fromString(props.getProperty(group
          + "TomogramGeneration-B", "Not started"));

    }

  }

  /**
   * Set all processes to the specfied state
   */
  public void setAll(ProcessState state) {
    setup = state;
    tomogramCombination = state;
    postProcessing = state;
    cleanUp = state;

    preProcessingA = state;
    coarseAlignmentA = state;
    fiducialModelA = state;
    fineAlignmentA = state;
    tomogramPositioningA = state;
    finalAlignedStackA = state;
    tomogramGenerationA = state;

    preProcessingB = state;
    coarseAlignmentB = state;
    fiducialModelB = state;
    fineAlignmentB = state;
    tomogramPositioningB = state;
    finalAlignedStackB = state;
    tomogramGenerationB = state;
    isModified = true;
  }

  /**
   * Set the setup state
   */
  public void setSetupState(ProcessState state) {
    setup = state;
    isModified = true;
  }

  public ProcessState getSetupState() {
    return setup;
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
      setCoarseAlignmentState(processState, axisID);
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
    else if (dialogType == DialogType.SETUP_RECON) {
      setSetupState(processState);
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

  public void setPreProcessingState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      preProcessingB = state;
    }
    else {
      preProcessingA = state;
    }
    isModified = true;
  }

  public ProcessState getPreProcessingState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return preProcessingB;
    }
    return preProcessingA;
  }

  public void setCoarseAlignmentState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      coarseAlignmentB = state;
    }
    else {
      coarseAlignmentA = state;
    }
    isModified = true;
  }

  public ProcessState getCoarseAlignmentState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return coarseAlignmentB;
    }
    return coarseAlignmentA;
  }

  public void setFiducialModelState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      fiducialModelB = state;
    }
    else {
      fiducialModelA = state;
    }
    isModified = true;
  }

  public ProcessState getFiducialModelState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fiducialModelB;
    }
    return fiducialModelA;
  }

  public void setFineAlignmentState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      fineAlignmentB = state;
    }
    else {
      fineAlignmentA = state;
    }
    isModified = true;
  }

  public ProcessState getFineAlignmentState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fineAlignmentB;
    }
    return fineAlignmentA;
  }

  public void setTomogramPositioningState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      tomogramPositioningB = state;
    }
    else {
      tomogramPositioningA = state;
    }
    isModified = true;
  }

  public ProcessState getTomogramPositioningState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramPositioningB;
    }
    return tomogramPositioningA;
  }

  public void setFinalAlignedStackState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      finalAlignedStackB = state;
    }
    else {
      finalAlignedStackA = state;
    }
    isModified = true;
  }

  public ProcessState getFinalAlignedStackState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalAlignedStackB;
    }
    return finalAlignedStackA;
  }

  public void setTomogramGenerationState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      tomogramGenerationB = state;
    }
    else {
      tomogramGenerationA = state;
    }
    isModified = true;
  }

  public ProcessState getTomogramGenerationState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramGenerationB;
    }
    return tomogramGenerationA;
  }

  public void setTomogramCombinationState(ProcessState state) {
    tomogramCombination = state;
    isModified = true;
  }

  public ProcessState getTomogramCombinationState() {
    return tomogramCombination;
  }

  public void setPostProcessingState(ProcessState state) {
    postProcessing = state;
    isModified = true;
  }

  public ProcessState getPostProcessingState() {
    return postProcessing;
  }

  public void setCleanUpState(ProcessState state) {
    cleanUp = state;
    isModified = true;
  }

  public ProcessState getCleanUpState() {
    return cleanUp;
  }

  private ProcessState mapAxis(AxisID axisID, ProcessState processStateA,
      ProcessState processStateB) {
    if (axisID == AxisID.SECOND) {
      return processStateB;
    }
    return processStateA;
  }

  public void printState(AxisType type) {
    if (type == AxisType.SINGLE_AXIS) {
      System.out.println("setup: " + setup);
      System.out.println("preProcessingA: " + preProcessingA);
      System.out.println("coarseAlignmentA: " + coarseAlignmentA);
      System.out.println("fineAlignmentA: " + fineAlignmentA);
      System.out.println("tomogramPositioningA: " + tomogramPositioningA);
      System.out.println("finalAlignedStackA: " + finalAlignedStackA);
      System.out.println("tomogramGenerationA: " + tomogramGenerationA);
      System.out.println("tomogramCombination: " + tomogramCombination);
      System.out.println("postProcessing: " + postProcessing);
      System.out.println("cleanUp: " + cleanUp);
    }
    else {
      System.out.println("setup: " + setup);
      System.out.println("preProcessingA: " + preProcessingA);
      System.out.println("coarseAlignmentA: " + coarseAlignmentA);
      System.out.println("fineAlignmentA: " + fineAlignmentA);
      System.out.println("tomogramPositioningA: " + tomogramPositioningA);
      System.out.println("finalAlignedStackA: " + finalAlignedStackA);
      System.out.println("tomogramGenerationA: " + tomogramGenerationA);

      System.out.println("preProcessingB: " + preProcessingB);
      System.out.println("coarseAlignmentB: " + coarseAlignmentB);
      System.out.println("fiducialModelB: " + fiducialModelB);
      System.out.println("fineAlignmentB: " + fineAlignmentB);
      System.out.println("tomogramPositioningB: " + tomogramPositioningB);
      System.out.println("finalAlignesStackB: " + finalAlignedStackB);
      System.out.println("tomogramGenerationB: " + tomogramGenerationB);

      System.out.println("tomogramCombination: " + tomogramCombination);
      System.out.println("postProcessing: " + postProcessing);
      System.out.println("cleanUp: " + cleanUp);
    }
  }
}
