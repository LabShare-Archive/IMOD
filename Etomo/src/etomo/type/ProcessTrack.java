package etomo.type;

import etomo.storage.Storable;
import etomo.process.ProcessState;
import java.util.Properties;

/*
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
public class ProcessTrack implements Storable {
  public static final String rcsid =
    "$Id$";

  protected String revisionNumber = "2.0";
  private boolean isModified = false;
  private ProcessState setup = ProcessState.NOTSTARTED;

  private ProcessState preProcessingA = ProcessState.NOTSTARTED;
  private ProcessState coarseAlignmentA = ProcessState.NOTSTARTED;
  private ProcessState fiducialModelA = ProcessState.NOTSTARTED;
  private ProcessState fineAlignmentA = ProcessState.NOTSTARTED;
  private ProcessState tomogramPositioningA = ProcessState.NOTSTARTED;
  private ProcessState tomogramGenerationA = ProcessState.NOTSTARTED;

  private ProcessState tomogramCombination = ProcessState.NOTSTARTED;
  private ProcessState postProcessing = ProcessState.NOTSTARTED;

  private ProcessState preProcessingB = ProcessState.NOTSTARTED;
  private ProcessState coarseAlignmentB = ProcessState.NOTSTARTED;
  private ProcessState fiducialModelB = ProcessState.NOTSTARTED;
  private ProcessState fineAlignmentB = ProcessState.NOTSTARTED;
  private ProcessState tomogramPositioningB = ProcessState.NOTSTARTED;
  private ProcessState tomogramGenerationB = ProcessState.NOTSTARTED;

  public ProcessTrack() {
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }
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

    props.setProperty(
      group + "TomogramPositioning-A",
      tomogramPositioningA.toString());
    props.setProperty(
      group + "TomogramPositioning-B",
      tomogramPositioningB.toString());

    props.setProperty(
      group + "TomogramGeneration-A",
      tomogramGenerationA.toString());
    props.setProperty(
      group + "TomogramGeneration-B",
      tomogramGenerationB.toString());

    props.setProperty(
      group + "TomogramCombination",
      tomogramCombination.toString());
    props.setProperty(group + "PostProcessing", postProcessing.toString());
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }
  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "ProcessTrack.";
    }
    else {
      group = prepend + ".ProcessTrack.";
    }
    revisionNumber = props.getProperty(group + "RevisionNumber", "1.0");

    setup = setup.fromString(props.getProperty(group + "Setup", "Not started"));

    tomogramCombination =
      tomogramCombination.fromString(
        props.getProperty(group + "TomogramCombination", "Not started"));

    postProcessing =
      postProcessing.fromString(
        props.getProperty(group + "PostProcessing", "Not started"));

    // Added separate process for A and B axis for 2.0 layout
    if (Float.parseFloat(revisionNumber) < 2.0) {
      preProcessingA =
        preProcessingA.fromString(
          props.getProperty(group + "PreProcessing", "Not started"));
      preProcessingB = preProcessingA;

      coarseAlignmentA =
        coarseAlignmentA.fromString(
          props.getProperty(group + "CoarseAlignment", "Not started"));
      coarseAlignmentB = coarseAlignmentA;

      fiducialModelA =
        fiducialModelA.fromString(
          props.getProperty(group + "FiducialModel", "Not started"));
      fiducialModelB = fiducialModelA;

      fineAlignmentA =
        fineAlignmentA.fromString(
          props.getProperty(group + "FineAlignment", "Not started"));
      fineAlignmentB = fineAlignmentA;

      tomogramPositioningA =
        tomogramPositioningA.fromString(
          props.getProperty(group + "TomogramPositioning", "Not started"));
      tomogramPositioningB = tomogramPositioningA;

      tomogramGenerationA =
        tomogramGenerationA.fromString(
          props.getProperty(group + "TomogramGeneration", "Not started"));
      tomogramGenerationB = tomogramGenerationA;
    }
    else {
      preProcessingA =
        preProcessingA.fromString(
          props.getProperty(group + "PreProcessing-A", "Not started"));
      preProcessingB =
        preProcessingB.fromString(
          props.getProperty(group + "PreProcessing-B", "Not started"));

      coarseAlignmentA =
        coarseAlignmentA.fromString(
          props.getProperty(group + "CoarseAlignment-A", "Not started"));
      coarseAlignmentB =
        coarseAlignmentB.fromString(
          props.getProperty(group + "CoarseAlignment-B", "Not started"));

      fiducialModelA =
        fiducialModelA.fromString(
          props.getProperty(group + "FiducialModel-A", "Not started"));
      fiducialModelB =
        fiducialModelB.fromString(
          props.getProperty(group + "FiducialModel-B", "Not started"));

      fineAlignmentA =
        fineAlignmentA.fromString(
          props.getProperty(group + "FineAlignment-A", "Not started"));
      fineAlignmentB =
        fineAlignmentB.fromString(
          props.getProperty(group + "FineAlignment-B", "Not started"));

      tomogramPositioningA =
        tomogramPositioningA.fromString(
          props.getProperty(group + "TomogramPositioning-A", "Not started"));
      tomogramPositioningB =
        tomogramPositioningB.fromString(
          props.getProperty(group + "TomogramPositioning-B", "Not started"));

      tomogramGenerationA =
        tomogramGenerationA.fromString(
          props.getProperty(group + "TomogramGeneration-A", "Not started"));
      tomogramGenerationB =
        tomogramGenerationB.fromString(
          props.getProperty(group + "TomogramGeneration-B", "Not started"));

    }

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
   * Set all processes to the specfied state
   */
  public void setAll(ProcessState state) {
    setup = state;
    tomogramCombination = state;
    postProcessing = state;

    preProcessingA = state;
    coarseAlignmentA = state;
    fiducialModelA = state;
    fineAlignmentA = state;
    tomogramPositioningA = state;
    tomogramGenerationA = state;

    preProcessingB = state;
    coarseAlignmentB = state;
    fiducialModelB = state;
    fineAlignmentB = state;
    tomogramPositioningB = state;
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

  public void setPreProcessingState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      preProcessingB = state;
    }
    preProcessingA = state;
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
    coarseAlignmentA = state;
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
    fiducialModelA = state;
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
    fineAlignmentA = state;
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
    tomogramPositioningA = state;
    isModified = true;
  }

  public ProcessState getTomogramPositioningState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramPositioningB;
    }
    return tomogramPositioningA;
  }

  public void setTomogramGenerationState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      tomogramGenerationB = state;
    }
    tomogramGenerationA = state;
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

  private ProcessState mapAxis(
    AxisID axisID,
    ProcessState processStateA,
    ProcessState processStateB) {
    if (axisID == AxisID.SECOND) {
      return processStateB;
    }
    return processStateA;
  }
}
