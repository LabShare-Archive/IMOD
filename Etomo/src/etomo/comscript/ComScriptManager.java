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
    try {
      ccdEraserParam.initialize(eraser.getScriptCommand("ccderaser"));
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't parse eraser"
          + axisID.getExtension()
          + ".com file: "
          + eraser.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return ccdEraserParam;
    }
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
    //  get the ccderaser command object from the script object
    //  update the input parameters and reinsert it back into script object
    ComScriptCommand ccdEraserCmd = scriptEraser.getScriptCommand("ccderaser");
    try {
      ccdEraserParam.updateComScript(ccdEraserCmd);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update ccderaser command in xcorr"
          + axisID.getExtension()
          + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
    //  FIXME can't always assume that the ccderaser command is index zero
    scriptEraser.setScriptComand(0, ccdEraserCmd);

    // write out the script
    writeComScript(scriptEraser, "ccderaser", axisID);
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
      ;
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
    try {
      tiltXcorrParam.initialize(xcorr.getScriptCommand("tiltxcorr"));
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't parse xcorr"
          + axisID.getExtension()
          + ".com file: "
          + xcorr.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return tiltXcorrParam;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] messages = new String[4];
      messages[0] = "Input parameter syntax error";
      messages[1] =
        "xcorr"
          + axisID.getExtension()
          + ".com file: "
          + xcorr.getComFileName();
      messages[2] = except.getMessage();
      messages[3] = "Input string: " + except.getNewString();
      JOptionPane.showMessageDialog(
        null,
        messages,
        messages[0],
        JOptionPane.ERROR_MESSAGE);
      return tiltXcorrParam;
    }
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

    // update the tiltxcorr parameters
    //  get the tiltxcorr command object from the script object
    //  update the input parameters and reinsert it back into script object
    //  FIXME can't always assume that the tiltxcorr command is index zero
    ComScriptCommand tiltXcorrCmd = scriptXcorr.getScriptCommand("tiltxcorr");
    try {
      tiltXcorrParam.updateComScript(tiltXcorrCmd);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update tiltxcorr command in xcorr"
          + axisID.getExtension()
          + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
    scriptXcorr.setScriptComand(0, tiltXcorrCmd);

    // write out the script
    writeComScript(scriptXcorr, "xcorr", axisID);
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
      ;
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
    try {
      beadtrackParam.initialize(track.getScriptCommand("beadtrack"));
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't initi"
          + axisID.getExtension()
          + ".com file: "
          + track.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return beadtrackParam;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] messages = new String[4];
      messages[0] = "Input parameter syntax error";
      messages[1] =
        "track"
          + axisID.getExtension()
          + ".com file: "
          + track.getComFileName();
      messages[2] = except.getMessage();
      messages[3] = "Input string: " + except.getNewString();
      JOptionPane.showMessageDialog(
        null,
        messages,
        messages[0],
        JOptionPane.ERROR_MESSAGE);
      return beadtrackParam;
    }
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
    //  get the beadtrack command object from the script object
    //  update the input parameters and reinsert it back into script object
    //  FIXME can't always assume that the beadtrackcommand is the only one
    ComScriptCommand beadtrackCmd = scriptTrack.getScriptCommand("beadtrack");
    try {
      beadtrackParam.updateComScript(beadtrackCmd);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update beadtrack command in track"
          + axisID.getExtension()
          + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
    scriptTrack.setScriptComand(0, beadtrackCmd);

    // write out the script
    writeComScript(scriptTrack, "track", axisID);
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
    try {
      tiltalignParam.initialize(align.getScriptCommand("tiltalign"));
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't initialize  align"
          + axisID.getExtension()
          + ".com file: "
          + align.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return tiltalignParam;
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't initialize  align"
          + axisID.getExtension()
          + ".com file: "
          + align.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return tiltalignParam;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] messages = new String[4];
      messages[0] = "Input parameter syntax error";
      messages[1] =
        "align"
          + axisID.getExtension()
          + ".com file: "
          + align.getComFileName();
      messages[2] = except.getMessage();
      messages[3] = "Input string: " + except.getNewString();
      JOptionPane.showMessageDialog(
        null,
        messages,
        messages[0],
        JOptionPane.ERROR_MESSAGE);
      return tiltalignParam;
    }
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

    // update the tiltalign parameters
    //  get the tiltalign command object from the script object
    //  update the input parameters and reinsert it back into script object
    //  FIXME can't always assume that the tiltalign command is the only one
    ComScriptCommand tiltalignCmd = scriptAlign.getScriptCommand("tiltalign");
    try {
      tiltalignParam.updateComScript(tiltalignCmd);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update tiltalign command in align"
          + axisID.getExtension()
          + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
    scriptAlign.setScriptComand(0, tiltalignCmd);

    // write out the script
    writeComScript(scriptAlign, "align", axisID);
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
    newstParam.initialize(newst.getScriptCommand("newst"));
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
    //  get the newst command object from the script object
    //  update the input parameters and reinsert it back into script object
    //  FIXME can't always assume that the newst command is the only one
    ComScriptCommand newstCmd = scriptNewst.getScriptCommand("newst");
    try {
      newstParam.updateComScript(newstCmd);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update newst command in newst" + axisID.getExtension() + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
    scriptNewst.setScriptComand(0, newstCmd);

    // write out the script
    writeComScript(scriptNewst, "newst", axisID);
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
    try {
      tiltParam.initialize(tilt.getScriptCommand("tilt"));
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't initialize tilt"
          + axisID.getExtension()
          + ".com file: "
          + tilt.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return tiltParam;
    }
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

    // update the tilt parameters
    //  get the tilt command object from the script object
    //  update the input parameters and reinsert it back into script object
    //  FIXME can't always assume that the tilt command is the only one
    ComScriptCommand tiltCmd = scriptTilt.getScriptCommand("tilt");
    try {
      tiltParam.updateComScript(tiltCmd);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't update tilt command in tilt" + axisID.getExtension() + ".com",
        JOptionPane.ERROR_MESSAGE);
    }
    scriptTilt.setScriptComand(0, tiltCmd);

    // write out the script
    writeComScript(scriptTilt, "tilt", axisID);
  }

  /**
   * Load the solvematchshift com script
   */
  public void loadSolvematchshift() {
    scriptSolvematchshift = loadComScript("solvematchshift", AxisID.ONLY);
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
    try {
      matchorwarpParam.initialize(
        scriptMatchorwarp.getScriptCommand("matchorwarp"));
    }
    catch (Exception except) {
      except.printStackTrace();
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        "Can't parse matchorwarp.com file: "
          + scriptMatchorwarp.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
      return matchorwarpParam;
    }
    return matchorwarpParam;
  }

  //  Check to see that this works!!
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
   * Write out the com file catching any exceptions and displaying an
   * error dialog box
   */
  private void writeComScript(
    ComScript script,
    String command,
    AxisID axisID) {

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
}
