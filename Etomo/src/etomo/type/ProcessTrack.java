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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessTrack implements Storable {
  public static final String rcsid =
    "$Id$";

  protected String revisionNumber = "1.1";
  private boolean isModified = false;
  private ProcessState setup = ProcessState.NOTSTARTED;
  private ProcessState preProcessing = ProcessState.NOTSTARTED;
  private ProcessState coarseAlignment = ProcessState.NOTSTARTED;
  private ProcessState fiducialModel = ProcessState.NOTSTARTED;
  private ProcessState fineAlignment = ProcessState.NOTSTARTED;
  private ProcessState tomogramPositioning = ProcessState.NOTSTARTED;
  private ProcessState tomogramGeneration = ProcessState.NOTSTARTED;
  private ProcessState tomogramCombination = ProcessState.NOTSTARTED;
  private ProcessState postProcessing = ProcessState.NOTSTARTED;

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
    props.setProperty(group + "PreProcessing", preProcessing.toString());
    props.setProperty(group + "CoarseAlignment", coarseAlignment.toString());
    props.setProperty(group + "FiducialModel", fiducialModel.toString());
    props.setProperty(group + "FineAlignment", fineAlignment.toString());
    props.setProperty(
      group + "TomogramPositioning",
      tomogramPositioning.toString());
    props.setProperty(
      group + "TomogramGeneration",
      tomogramGeneration.toString());
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
    preProcessing =
      preProcessing.fromString(
        props.getProperty(group + "PreProcessing", "Not started"));
    coarseAlignment =
      coarseAlignment.fromString(
        props.getProperty(group + "CoarseAlignment", "Not started"));
    fiducialModel =
      fiducialModel.fromString(
        props.getProperty(group + "FiducialModel", "Not started"));
    fineAlignment =
      fineAlignment.fromString(
        props.getProperty(group + "FineAlignment", "Not started"));
    tomogramPositioning =
      tomogramPositioning.fromString(
        props.getProperty(group + "TomogramPositioning", "Not started"));
    tomogramGeneration =
      tomogramGeneration.fromString(
        props.getProperty(group + "TomogramGeneration", "Not started"));
    tomogramCombination =
      tomogramCombination.fromString(
        props.getProperty(group + "TomogramCombination", "Not started"));
    postProcessing =
      postProcessing.fromString(
        props.getProperty(group + "PostProcessing", "Not started"));
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
   * Set the setup state
   */
  public void setSetupState(ProcessState state) {
    setup = state;
    isModified = true;
  }

  public ProcessState getSetupState() {
    return setup;
  }

  public void setPreProcessingState(ProcessState state) {
    preProcessing = state;
    isModified = true;
  }

  public ProcessState getPreProcessingState() {
    return preProcessing;
  }

  public void setCoarseAlignmentState(ProcessState state) {
    coarseAlignment = state;
    isModified = true;
  }

  public ProcessState getCoarseAlignmentState() {
    return coarseAlignment;
  }

  public void setFiducialModelState(ProcessState state) {
    fiducialModel = state;
    isModified = true;
  }

  public ProcessState getFiducialModelState() {
    return fiducialModel;
  }

  public void setFineAlignmentState(ProcessState state) {
    fineAlignment = state;
    isModified = true;
  }

  public ProcessState getFineAlignmentState() {
    return fineAlignment;
  }

  public void setTomogramPositioningState(ProcessState state) {
    tomogramPositioning = state;
    isModified = true;
  }

  public ProcessState getTomogramPositioningState() {
    return tomogramPositioning;
  }

  public void setTomogramGenerationState(ProcessState state) {
    tomogramGeneration = state;
    isModified = true;
  }

  public ProcessState getTomogramGenerationState() {
    return tomogramGeneration;
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

}
