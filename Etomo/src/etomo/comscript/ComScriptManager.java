package etomo.comscript;

import java.io.File;
import javax.swing.JOptionPane;

import etomo.ApplicationManager;
import etomo.type.*;

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
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/09 00:00:34  rickg
 * <p> Fixed formatting due to emacs
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ComScriptManager {
  public static final String rcsid =
    "$Id$";

  ApplicationManager appManager;

  private ComScript scriptEraserA;
  private ComScript scriptEraserB;
  private ComScript scriptXcorrA;
  private ComScript scriptXcorrB;
  private ComScript scriptTrackA;
  private ComScript scriptTrackB;
  private ComScript scriptAlignA;
  private ComScript scriptAlignB;
  private ComScript scriptNewstA;
  private ComScript scriptNewstB;
  private ComScript scriptTiltA;
  private ComScript scriptTiltB;
  private ComScript scriptSolvematchshift;
  private ComScript scriptSolvematchmod;
  private ComScript scriptPatchcorr;
  private ComScript scriptMatchorwarp;

  public ComScriptManager(ApplicationManager appManager) {
    this.appManager = appManager;
  }

  /**
   * Load the specified eraser com script
   * @param axisID the AxisID to load.
   */
  public void loadEraser(AxisID axisID) {
    //  Assign the new ComScript object object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptEraserB = loadComScript("eraser", axisID);
    }
    else {
      scriptEraserA = loadComScript("eraser", axisID);
    }
  }

  /**
   * Get the CCD eraser parameters from the specified eraser script object
   * @param axisID the AxisID to read.
   * @return a CCDEraserParam object that will be created and initialized
   * with the input arguments from eraser in the com script.
   */
  public CCDEraserParam getCCDEraserParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript eraser;
    if (axisID == AxisID.SECOND) {
      eraser = scriptEraserB;
    }
    else {
      eraser = scriptEraserA;
    }

    // Initialize a CCDEraserParam object from the com script command object
    CCDEraserParam ccdEraserParam = new CCDEraserParam();
    initialize(ccdEraserParam, eraser, "ccderaser", axisID);
    return ccdEraserParam;
  }

  /**
   * Save the specified eraser com script updating the ccderaser parmaeters
   * @param axisID the AxisID to load.
   * @param ccdEraserParam a CCDEraserParam object containing the new input
   * parameters for the ccderaser command.
   */
  public void saveEraser(CCDEraserParam ccdEraserParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptEraser;
    if (axisID == AxisID.SECOND) {
      scriptEraser = scriptEraserB;
    }
    else {
      scriptEraser = scriptEraserA;
    }

    // update the ccderaser parameters
    updateComScript(scriptEraser, ccdEraserParam, "ccderaser", axisID);
  }

  /**
   * Load the specified xcorr com script and initialize the TiltXcorrParam
   * object
   * @param axisID the AxisID to load.
   */
  public void loadXcorr(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptXcorrB = loadComScript("xcorr", axisID);
    }
    else {
      scriptXcorrA = loadComScript("xcorr", axisID);
    }
  }

  /**
   * Get the tiltxcorr parameters from the specified xcorr script object
   * @param axisID the AxisID to read.
   * @return a TiltxcorrParam object that will be created and initialized
   * with the input arguments from xcorr in the com script.
   */
  public TiltxcorrParam getTiltxcorrParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript xcorr;
    if (axisID == AxisID.SECOND) {
      xcorr = scriptXcorrB;
    }
    else {
      xcorr = scriptXcorrA;
    }

    // Initialize a TiltxcorrParam object from the com script command object
    TiltxcorrParam tiltXcorrParam = new TiltxcorrParam();
    initialize(tiltXcorrParam, xcorr, "tiltxcorr", axisID);
    return tiltXcorrParam;
  }

  /**
   * Save the specified xcorr com script updating the tiltxcorr parameters
   * @param axisID the AxisID to load.
   * @param tiltXcorrParam a TiltxcorrParam object that will be used to update
   * the xcorr com script
   */
  public void saveXcorr(TiltxcorrParam tiltXcorrParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptXcorr;
    if (axisID == AxisID.SECOND) {
      scriptXcorr = scriptXcorrB;
    }
    else {
      scriptXcorr = scriptXcorrA;
    }
    updateComScript(scriptXcorr, tiltXcorrParam, "tiltxcorr", axisID);
  }

  /**
   * Load the specified track com script
   * @param axisID the AxisID to load.
   */
  public void loadTrack(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTrackB = loadComScript("track", axisID);
    }
    else {
      scriptTrackA = loadComScript("track", axisID);
    }
  }

  /**
   * Get the beadtrack parameters from the specified track script object
   * @param axisID the AxisID to read.
   * @return a BeadtrackParam object that will be created and initialized
   * with the input arguments from beadtrack in the com script.
   */
  public BeadtrackParam getBeadtrackParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript track;
    if (axisID == AxisID.SECOND) {
      track = scriptTrackB;
    }
    else {
      track = scriptTrackA;
    }

    // Initialize a BeadtrckParam object from the com script command object
    BeadtrackParam beadtrackParam = new BeadtrackParam();
    initialize(beadtrackParam, track, "beadtrack", axisID);
    return beadtrackParam;
  }

  /**
   * Save the specified track com script updating the beadtrack parameters
   * @param axisID the AxisID to load.
   * @param beadtrackParam a BeadtrackParam object that will be used to update
   * the track com script
   */
  public void saveTrack(BeadtrackParam beadtrackParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptTrack;
    if (axisID == AxisID.SECOND) {
      scriptTrack = scriptTrackB;
    }
    else {
      scriptTrack = scriptTrackA;
    }
    // update the beadtrack parameters
    updateComScript(scriptTrack, beadtrackParam, "beadtrack", axisID);
  }

  /**
   * Load the specified align com script object
   * @param axisID the AxisID to load.
   */
  public void loadAlign(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptAlignB = loadComScript("align", axisID);
    }
    else {
      scriptAlignA = loadComScript("align", axisID);
    }
  }

  /**
   * Get the tiltalign parameters from the specified align script object
   * @param axisID the AxisID to read.
   * @return a TiltalignParam object that will be created and initialized
   * with the input arguments from tiltalign in the com script.
   */
  public TiltalignParam getTiltalignParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript align;
    if (axisID == AxisID.SECOND) {
      align = scriptAlignB;
    }
    else {
      align = scriptAlignA;
    }

    // Initialize a BeadtrckParam object from the com script command object
    TiltalignParam tiltalignParam = new TiltalignParam();
    initialize(tiltalignParam, align, "tiltalign", axisID);
    return tiltalignParam;
  }

  /**
   * Save the specified align com script updating the tiltalign parameters
   * @param axisID the AxisID to load.
   * @param tiltalignParam a TiltalignParam object that will be used to update
   * tiltalign command in the align com script
   */
  public void saveAlign(TiltalignParam tiltalignParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptAlign;
    if (axisID == AxisID.SECOND) {
      scriptAlign = scriptAlignB;
    }
    else {
      scriptAlign = scriptAlignA;
    }

    //  update the tiltalign parameters
    updateComScript(scriptAlign, tiltalignParam, "tiltalign", axisID);
  }

  /**
   * Load the specified newst com script
   * @param axisID the AxisID to load.
   */
  public void loadNewst(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptNewstB = loadComScript("newst", axisID);
    }
    else {
      scriptNewstA = loadComScript("newst", axisID);
    }
  }

  /**
   * Get the newst parameters from the specified newst script object
   * @param axisID the AxisID to read.
   * @return a NewstParam object that will be created and initialized
   * with the input arguments from newst in the com script.
   */
  public NewstParam getNewstComNewstParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript newst;
    if (axisID == AxisID.SECOND) {
      newst = scriptNewstB;
    }
    else {
      newst = scriptNewstA;
    }

    // Initialize a NewstParam object from the com script command object
    NewstParam newstParam = new NewstParam();
    initialize(newstParam, newst, "newst", axisID);
    return newstParam;
  }

  /**
   * Save the specified newst com script updating the newst parameters
   * @param axisID the AxisID to load.
   * @param tiltalignParam a TiltalignParam object that will be used to update
   * tiltalign command in the align com script
   */
  public void saveNewst(NewstParam newstParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptNewst;
    if (axisID == AxisID.SECOND) {
      scriptNewst = scriptNewstB;
    }
    else {
      scriptNewst = scriptNewstA;
    }

    // update the newst parameters
    updateComScript(scriptNewst, newstParam, "newst", axisID);
  }

  /**
   * Load the specified tilt com script
   * @param axisID the AxisID to load.
   */
  public void loadTilt(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTiltB = loadComScript("tilt", axisID);
    }
    else {
      scriptTiltA = loadComScript("tilt", axisID);
    }
  }

  /**
   * Get the tilt parameters from the specified tilt script object
   * @param axisID the AxisID to read.
   * @return a TiltParam object that will be created and initialized
   * with the input arguments from tilt in the com script.
   */
  public TiltParam getTiltParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript tilt;
    if (axisID == AxisID.SECOND) {
      tilt = scriptTiltB;
    }
    else {
      tilt = scriptTiltA;
    }

    // Initialize a TiltParam object from the com script command object
    TiltParam tiltParam = new TiltParam();
    initialize(tiltParam, tilt, "tilt", axisID);
    return tiltParam;
  }

  /**
   * Save the specified tilt com script updating the tilt parameters
   * @param axisID the AxisID to load.
   * @param tiltalignParam a TiltalignParam object that will be used to update
   * tiltalign command in the align com script
   */
  public void saveTilt(TiltParam tiltParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptTilt;
    if (axisID == AxisID.SECOND) {
      scriptTilt = scriptTiltB;
    }
    else {
      scriptTilt = scriptTiltA;
    }
    updateComScript(scriptTilt, tiltParam, "tilt", axisID);
  }

  /**
   * Load the solvematchshift com script
   */
  public void loadSolvematchshift() {
    scriptSolvematchshift = loadComScript("solvematchshift", AxisID.ONLY);
  }

  /**
   * Parse the patchrawl3D command from the patchcorr script
   * @return MatchorwarpParam
   */
  public SolvematchshiftParam getSolvematchshift() {

    // Initialize a SolvematchshiftParam object from the com script command
    // object
    SolvematchshiftParam solveMatchshiftParam = new SolvematchshiftParam();
    initialize(
      solveMatchshiftParam,
      scriptSolvematchshift,
      "solvematch",
      AxisID.ONLY);
    return solveMatchshiftParam;
  }

  /**
   * Save the solvematchshift com script updating the patchcrawl3d parameters
   * @param patchcrawl3DParam
   */
  public void saveSolvematchshift(SolvematchshiftParam solveMatchshiftParam) {

    updateComScript(
      scriptSolvematchshift,
      solveMatchshiftParam,
      "solvematch",
      AxisID.ONLY);
  }

  /**
   * Load the solvematchshift com script
   */
  public void loadSolvematchmod() {
    scriptSolvematchmod = loadComScript("solvematchmod", AxisID.ONLY);
  }

  /**
   * Load the patchcorr com script
   */
  public void loadPatchcorr() {
    scriptPatchcorr = loadComScript("patchcorr", AxisID.ONLY);
  }

  /**
   * Parse the patchrawl3D command from the patchcorr script
   * @return MatchorwarpParam
   */
  public Patchcrawl3DParam getPatchcrawl3D() {

    // Initialize a CCDEraserParam object from the com script command object
    Patchcrawl3DParam patchcrawl3DParam = new Patchcrawl3DParam();
    initialize(patchcrawl3DParam, scriptPatchcorr, "patchcrawl3d", AxisID.ONLY);
    return patchcrawl3DParam;
  }

  /**
   * Save the patchcorr com script updating the patchcrawl3d parameters
   * @param patchcrawl3DParam
   */
  public void savePatchcorr(Patchcrawl3DParam patchcrawl3DParam) {

    updateComScript(
      scriptPatchcorr,
      patchcrawl3DParam,
      "patchcrawl3D",
      AxisID.ONLY);
  }

  /**
   * Load the matchorwarp com script
   */
  public void loadMatchorwarp() {
    scriptMatchorwarp = loadComScript("matchorwarp", AxisID.ONLY);
  }

  /**
   * Parse the matchorwarp command from the matchorwarp script
   * @return MatchorwarpParam
   */
  public MatchorwarpParam getMatchorwarParam() {

    // Initialize a CCDEraserParam object from the com script command object
    MatchorwarpParam matchorwarpParam = new MatchorwarpParam();
    initialize(matchorwarpParam, scriptMatchorwarp, "matchorwarp", AxisID.ONLY);
    return matchorwarpParam;
  }

  /**
   * Save the matchorwarp com script updating the matchorwarp parameters
   * @param matchorwarpParam
   */
  public void saveMatchorwarp(MatchorwarpParam matchorwarpParam) {

    updateComScript(
      scriptMatchorwarp,
      matchorwarpParam,
      "matchorwarp",
      AxisID.ONLY);
  }

  /**
   * Load the specified Com script 
   * @param scriptName
   * @param axisID
   * @return ComScript
   */
  private ComScript loadComScript(String scriptName, AxisID axisID) {

    String command = scriptName + axisID.getExtension() + ".com";
    File comFile = new File(appManager.getWorkingDirectory(), command);
    ComScript comScript = new ComScript(comFile);
    try {
      comScript.readComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't parse "
          + scriptName
          + axisID.getExtension()
          + ".com file: "
          + comScript.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return null;
    }
    return comScript;
  }

  /**
   * Update the specified comscript with 
   * @param script
   * @param params
   * @param command
   * @param axisID
   */
  private void updateComScript(
    ComScript script,
    CommandParam params,
    String command,
    AxisID axisID) {

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = script.getScriptCommand(command);
    try {
      params.updateComScript(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update " + command + " command in " + script.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
    }

    // Replace the first script command with the updated ComScriptCommand
    // object

    script.setScriptComand(0, comScriptCommand);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't write " + command + axisID.getExtension() + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
  }

  /**
   * Initialize the CommandParam object from the specified command in the
   * comscript.  True is returned if the initialization is successful, false if
   * the initialization fails.
   * 
   * @param param
   * @param comScript
   * @param command
   * @param axisID
   * @return boolean
   */
  private boolean initialize(
    CommandParam param,
    ComScript comScript,
    String command,
    AxisID axisID) {

    try {
      param.initialize(comScript.getScriptCommand(command));
    }
    catch (Exception except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't parse "
          + command
          + axisID.getExtension()
          + ".com file: "
          + comScript.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return false;
    }
    return true;
  }
}
