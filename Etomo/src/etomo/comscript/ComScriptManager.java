package etomo.comscript;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JOptionPane;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.util.Utilities;

/**
 * <p>Description: This class provides a high level manager for loading and
 * saving particlar com scripts and extracting the parameter sets for the 
 * commands within those scripts.</p>
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
 * <p> Revision 3.14  2004/06/24 18:36:20  sueh
 * <p> bug# 482 Removing proof-of-concept test code.  Add functions
 * <p> to retrieve matchshifts from solvematchshift and add it to
 * <p> solvematch.
 * <p>
 * <p> Revision 3.13  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.12  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.11  2004/05/05 19:15:05  sueh
 * <p> param test - proof of concept
 * <p>
 * <p> Revision 3.10  2004/05/03 17:59:36  sueh
 * <p> param testing proof of concept
 * <p>
 * <p> Revision 3.9  2004/04/27 00:50:16  sueh
 * <p> bug# 427 parse comments for tomopitch
 * <p>
 * <p> Revision 3.8  2004/04/26 21:09:50  sueh
 * <p> bug# 427 added tomopitch
 * <p>
 * <p> Revision 3.7  2004/04/19 19:24:46  sueh
 * <p> bug# 409 putting text back to pre-409, handling changes in
 * <p> ComScript
 * <p>
 * <p> Revision 3.6  2004/04/16 01:45:25  sueh
 * <p> bug# 409 changes for mtffilter where not working for newst - fixed
 * <p>
 * <p> Revision 3.5  2004/04/12 17:11:25  sueh
 * <p> bug# 409  In initialize() allow the param to initialize itself if necessary.  In update
 * <p> ComScript, get the commandIndex after running
 * <p> script.getScriptCommand(command) to make sure that the command exists in
 * <p> the ComScript object.
 * <p>
 * <p> Revision 3.4  2004/03/29 20:46:57  sueh
 * <p> bug# 409 add MTF Filter
 * <p>
 * <p> Revision 3.3  2004/03/13 00:30:49  rickg
 * <p> Bug# 390 Add prenewst and xfproduct management
 * <p>
 * <p> Revision 3.2  2004/03/12 00:04:10  rickg
 * <p> Bug #410 Newstack PIP transition
 * <p> Handle newst or newstack commands the same way
 * <p>
 * <p> Revision 3.1  2004/03/04 00:46:54  rickg
 * <p> Bug# 406 Correctly write out command when it isn't the first in the
 * <p> script
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.9  2003/07/25 22:57:30  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.8  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.7  2003/06/23 23:28:32  rickg
 * <p> Return exception class name in error dialog
 * <p>
 * <p> Revision 2.6  2003/05/07 22:32:42  rickg
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.5  2003/03/27 00:27:49  rickg
 * <p> Fixed but in loading tilt with with respect to parsing comments.
 * <p>
 * <p> Revision 2.4  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.3  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
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
  public static final String rcsid = "$Id$";

  ApplicationManager appManager;

  public static final String MTFFILTER_COMMAND = "mtffilter";

  private ComScript scriptEraserA;
  private ComScript scriptEraserB;
  private ComScript scriptXcorrA;
  private ComScript scriptXcorrB;
  private ComScript scriptPrenewstA;
  private ComScript scriptPrenewstB;
  private ComScript scriptTrackA;
  private ComScript scriptTrackB;
  private ComScript scriptAlignA;
  private ComScript scriptAlignB;
  private ComScript scriptNewstA;
  private ComScript scriptNewstB;
  private ComScript scriptTiltA;
  private ComScript scriptTiltB;
  private ComScript scriptMTFFilterA;
  private ComScript scriptMTFFilterB;
  // The solvematch com script replaces the functionality of the
  // solvematchshift and solvematchmod com scripts
  private ComScript scriptSolvematch;
  private ComScript scriptSolvematchshift;
  private ComScript scriptSolvematchmod;
  private ComScript scriptPatchcorr;
  private ComScript scriptMatchorwarp;
  private ComScript scriptTomopitchA;
  private ComScript scriptTomopitchB;
  private ComScript scriptCombine;

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
      scriptEraserB = loadComScript("eraser", axisID, true);
    }
    else {
      scriptEraserA = loadComScript("eraser", axisID, true);
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
      scriptXcorrB = loadComScript("xcorr", axisID, true);
    }
    else {
      scriptXcorrA = loadComScript("xcorr", axisID, true);
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
   * Load the specified prenewst com script and initialize the NewstParam
   * object
   * @param axisID the AxisID to load.
   */
  public void loadPrenewst(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptPrenewstB = loadComScript("prenewst", axisID, true);
    }
    else {
      scriptPrenewstA = loadComScript("prenewst", axisID, true);
    }
  }

  /**
   * Get the newstack parameters from the specified prenewst script object
   * @param axisID the AxisID to read.
   * @return a NewstParam object that will be created and initialized
   * with the input arguments from prenewst in the com script.
   */
  public NewstParam getPrenewstParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptPrenewst;
    if (axisID == AxisID.SECOND) {
      scriptPrenewst = scriptPrenewstB;
    }
    else {
      scriptPrenewst = scriptPrenewstA;
    }

    // Initialize a NewstParam object from the com script command object
    NewstParam prenewstParam = new NewstParam();

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptPrenewst);
    initialize(prenewstParam, scriptPrenewst, cmdName, axisID);
    return prenewstParam;
  }

  /**
   * Save the specified prenewst com script updating the newst parameters
   * @param axisID the AxisID to load.
   * @param tiltXcorrParam a TiltxcorrParam object that will be used to update
   * the xcorr com script
   */
  public void savePrenewst(NewstParam prenewstParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptPrenewst;
    if (axisID == AxisID.SECOND) {
      scriptPrenewst = scriptPrenewstB;
    }
    else {
      scriptPrenewst = scriptPrenewstA;
    }

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptPrenewst);
    updateComScript(scriptPrenewst, prenewstParam, cmdName, axisID);
  }

  /**
   * Load the specified track com script
   * @param axisID the AxisID to load.
   */
  public void loadTrack(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTrackB = loadComScript("track", axisID, true);
    }
    else {
      scriptTrackA = loadComScript("track", axisID, true);
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
      scriptAlignB = loadComScript("align", axisID, true);
    }
    else {
      scriptAlignA = loadComScript("align", axisID, true);
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
   * Get the xfproduct parameter from the align script
   * @param axisID
   * @return
   */
  public XfproductParam getXfproductInAlign(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript align;
    if (axisID == AxisID.SECOND) {
      align = scriptAlignB;
    }
    else {
      align = scriptAlignA;
    }

    // Initialize a BeadtrckParam object from the com script command object
    XfproductParam xfproductParam = new XfproductParam();
    initialize(xfproductParam, align, "xfproduct", axisID);
    return xfproductParam;
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
   * Save the xfproduct command to the specified align com script
   * @param xfproductParam
   * @param axisID
   */
  public void saveXfproductInAlign(XfproductParam xfproductParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptAlign;
    if (axisID == AxisID.SECOND) {
      scriptAlign = scriptAlignB;
    }
    else {
      scriptAlign = scriptAlignA;
    }

    //  update the tiltalign parameters
    updateComScript(scriptAlign, xfproductParam, "xfproduct", axisID);
  }

  /**
   * Load the specified newst com script
   * @param axisID the AxisID to load.
   */
  public void loadNewst(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptNewstB = loadComScript("newst", axisID, true);
    }
    else {
      scriptNewstA = loadComScript("newst", axisID, true);
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
    ComScript scriptNewst;
    if (axisID == AxisID.SECOND) {
      scriptNewst = scriptNewstB;
    }
    else {
      scriptNewst = scriptNewstA;
    }

    // Initialize a NewstParam object from the com script command object
    NewstParam newstParam = new NewstParam();

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptNewst);
    initialize(newstParam, scriptNewst, cmdName, axisID);
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

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptNewst);

    // update the newst parameters
    updateComScript(scriptNewst, newstParam, cmdName, axisID);
  }

  /**
   * Load the specified tilt com script
   * @param axisID the AxisID to load.
   */
  public void loadTilt(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTiltB = loadComScript("tilt", axisID, false);
    }
    else {
      scriptTiltA = loadComScript("tilt", axisID, false);
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
   * Load the specified tomopitch com script
   * @param axisID the AxisID to load.
   */
  public void loadTomopitch(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTomopitchB = loadComScript("tomopitch", axisID, true);
    }
    else {
      scriptTomopitchA = loadComScript("tomopitch", axisID, true);
    }
  }

  /**
   * Get the tomopitch parameters from the specified tomopitch script object
   * @param axisID the AxisID to read.
   * @return a TomopitchParam object that will be created and initialized
   * with the input arguments from tomopitch in the com script.
   */
  public TomopitchParam getTomopitchParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript tomopitch;
    if (axisID == AxisID.SECOND) {
      tomopitch = scriptTomopitchB;
    }
    else {
      tomopitch = scriptTomopitchA;
    }

    // Initialize a TomopitchParam object from the com script command object
    TomopitchParam tomopitchParam = new TomopitchParam();
    initialize(tomopitchParam, tomopitch, "tomopitch", axisID);
    return tomopitchParam;
  }

  /**
   * Save the specified tomopitch com script updating the tomopitch parameters
   * @param axisID the AxisID to load.
   * @param tomopitchParam a TomopitchParam object that will be used to update
   * tomopitch command in the tomopitch com script
   */
  public void saveTomopitch(TomopitchParam tomopitchParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptTomopitch;
    if (axisID == AxisID.SECOND) {
      scriptTomopitch = scriptTomopitchB;
    }
    else {
      scriptTomopitch = scriptTomopitchA;
    }
    updateComScript(scriptTomopitch, tomopitchParam, "tomopitch", axisID);
  }

  public void loadMTFFilter(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptMTFFilterB = loadComScript(MTFFILTER_COMMAND, axisID, false);
    }
    else {
      scriptMTFFilterA = loadComScript(MTFFILTER_COMMAND, axisID, false);
    }
  }

  public MTFFilterParam getMTFFilterParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript mtfFilter;
    if (axisID == AxisID.SECOND) {
      mtfFilter = scriptMTFFilterB;
    }
    else {
      mtfFilter = scriptMTFFilterA;
    }

    // Initialize a TiltParam object from the com script command object
    MTFFilterParam mtfFilterParam = new MTFFilterParam();
    initialize(mtfFilterParam, mtfFilter, MTFFILTER_COMMAND, axisID);
    return mtfFilterParam;
  }

  public void saveMTFFilter(MTFFilterParam mtfFilterParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptMTFFilter;
    if (axisID == AxisID.SECOND) {
      scriptMTFFilter = scriptMTFFilterB;
    }
    else {
      scriptMTFFilter = scriptMTFFilterA;
    }
    updateComScript(scriptMTFFilter, mtfFilterParam, MTFFILTER_COMMAND, axisID);
  }

  /**
   * Load in the solvematch com script.
   */
  public void loadSolvematch() {
    scriptSolvematch = loadComScript("solvematch", AxisID.ONLY, true);
  }

  /**
   * Return the solvematch parameter object from the currently loaded 
   * solvematch com script.
   * @return
   */
  public SolvematchParam getSolvematch() {

    // Initialize a SolvematchParam object from the com script command
    // object
    SolvematchParam solveMatchParam = new SolvematchParam();
    initialize(solveMatchParam, scriptSolvematch, "solvematch", AxisID.ONLY);
    return solveMatchParam;
  }

  /**
   * Replace the solvematch command in the solvematch com script with the info
   * in the specified SolvematchParam object
   * @param solveMatchParam
   */
  public void saveSolvematch(SolvematchParam solveMatchParam) {

    updateComScript(scriptSolvematch, solveMatchParam, "solvematch",
      AxisID.ONLY);
  }
  
  /**
   * Save the matchshifts command to the solvematch com script
   * @param matchshiftsParam
   */
  public void saveSolvematch(MatchshiftsParam matchshiftsParam) {
    updateComScript(
      scriptSolvematch,
      matchshiftsParam,
      matchshiftsParam.getCommand(),
      AxisID.ONLY,
      true);
  }

  /**
   * Load the solvematchshift com script
   */
  public void loadSolvematchshift() {
    scriptSolvematchshift = loadComScript("solvematchshift", AxisID.ONLY, true);
  }

  /**
   * Parse the solvematch command from the solvematchshift script
   * @return MatchorwarpParam
   */
  public SolvematchshiftParam getSolvematchshift() {

    // Initialize a SolvematchshiftParam object from the com script command
    // object
    SolvematchshiftParam solveMatchshiftParam = new SolvematchshiftParam();
    initialize(solveMatchshiftParam, scriptSolvematchshift, "solvematch",
      AxisID.ONLY);
    return solveMatchshiftParam;
  }
  
  public MatchshiftsParam getMatchshiftsFromSolvematchshifts() {

    // Initialize a MatchshiftsParam object from the com script command
    // object
    MatchshiftsParam matchshiftsParam = new MatchshiftsParam();
    initialize(matchshiftsParam, scriptSolvematchshift, "matchshifts",
      AxisID.ONLY);
    return matchshiftsParam;
  }

  /**
   * Save the solvematchshift com script updating the solveMatchshiftParam
   * parameters
   * @param solveMatchshiftParam
   */
  public void saveSolvematchshift(SolvematchshiftParam solveMatchshiftParam) {
    Exception except = new Exception();
    except.printStackTrace();
    System.err.println("WARNING: call to saveSolvematchshift");
    updateComScript(scriptSolvematchshift, solveMatchshiftParam, "solvematch",
      AxisID.ONLY);
  }

  /**
   * Load the solvematchmod com script
   */
  public void loadSolvematchmod() {
    scriptSolvematchmod = loadComScript("solvematchmod", AxisID.ONLY, true);
  }

  /**
   * Parse the solvematch command from the solvematchmod script
   * @return MatchorwarpParam
   */
  public SolvematchmodParam getSolvematchmod() {

    // Initialize a SolvematchmodParam object from the com script command
    // object
    SolvematchmodParam solveMatchmodParam = new SolvematchmodParam();
    initialize(solveMatchmodParam, scriptSolvematchmod, "solvematch",
      AxisID.ONLY);
    return solveMatchmodParam;
  }

  /**
   * Save the solvematchmod com script updating the solveMatchmodParam
   * parameters
   * @param solveMatchmodParam
   */
  public void saveSolvematchmod(SolvematchmodParam solveMatchmodParam) {

    Exception except = new Exception();
    except.printStackTrace();
    System.err.println("WARNING: call to saveSolvematchshift");
    updateComScript(scriptSolvematchmod, solveMatchmodParam, "solvematch",
      AxisID.ONLY);
  }

  /**
   * Load the patchcorr com script
   */
  public void loadPatchcorr() {
    scriptPatchcorr = loadComScript("patchcorr", AxisID.ONLY, true);
  }

  /**
   * Parse the patchrawl3D command from the patchcorr script
   * @return MatchorwarpParam
   */
  public Patchcrawl3DParam getPatchcrawl3D() {

    // Initialize a Patchcrawl3DParam object from the com script command object
    Patchcrawl3DParam patchcrawl3DParam = new Patchcrawl3DParam();
    initialize(patchcrawl3DParam, scriptPatchcorr, "patchcrawl3d", AxisID.ONLY);
    return patchcrawl3DParam;
  }

  /**
   * Save the patchcorr com script updating the patchcrawl3d parameters
   * @param patchcrawl3DParam
   */
  public void savePatchcorr(Patchcrawl3DParam patchcrawl3DParam) {

    updateComScript(scriptPatchcorr, patchcrawl3DParam, "patchcrawl3d",
      AxisID.ONLY);
  }

  /**
   * Load the matchorwarp com script
   */
  public void loadMatchorwarp() {
    scriptMatchorwarp = loadComScript("matchorwarp", AxisID.ONLY, true);
  }

  /**
   * Parse the matchorwarp command from the matchorwarp script
   * @return MatchorwarpParam
   */
  public MatchorwarpParam getMatchorwarParam() {

    // Initialize a MatchorwarpParam object from the com script command object
    MatchorwarpParam matchorwarpParam = new MatchorwarpParam();
    initialize(matchorwarpParam, scriptMatchorwarp, "matchorwarp", AxisID.ONLY);
    return matchorwarpParam;
  }

  /**
   * Save the matchorwarp com script updating the matchorwarp parameters
   * @param matchorwarpParam
   */
  public void saveMatchorwarp(MatchorwarpParam matchorwarpParam) {

    updateComScript(scriptMatchorwarp, matchorwarpParam, "matchorwarp",
      AxisID.ONLY);
  }
  
  /**
   * 
   * @return
   */
  public CombineComscriptState getCombineComscript() {
    // Initialize a CombineComscript object from the com script command object
    CombineComscriptState combineComscriptState = new CombineComscriptState();
    if (!combineComscriptState.initialize(this)) {
      return null;
    }
    return combineComscriptState;
  }
  /**
   * Load the combine com script
   */
  public void loadCombine() {
    scriptCombine =
      loadComScript(CombineComscriptState.COMSCRIPT_NAME, AxisID.ONLY, true);
  }
  
  /**
   * 
   * @param gotoParam
   */
  public void saveCombine(GotoParam gotoParam) {
    updateComScript(scriptCombine, gotoParam, GotoParam.COMMAND_NAME,
      AxisID.ONLY);
  }

  
  /**
   * returns index of saved command
   * @param echoParam
   * @param previousCommand
   */
  public int saveCombine(EchoParam echoParam, String previousCommand) {
    return updateComScript(scriptCombine, echoParam, EchoParam.COMMAND_NAME,
      AxisID.ONLY, true, previousCommand);
  }
  
  /**
   * 
   * @param gotoParam
   */
  public void saveCombine(ExitParam exitParam, int previousCommandIndex) {
    updateComScript(scriptCombine, exitParam, ExitParam.COMMAND_NAME,
      AxisID.ONLY, true, previousCommandIndex);
  }

  /**
   * 
   * @param gotoParam
   * @param previousCommandIndex
   */
  public void saveCombine(GotoParam gotoParam, int previousCommandIndex) {
    updateComScript(scriptCombine, gotoParam, GotoParam.COMMAND_NAME,
      AxisID.ONLY, true, previousCommandIndex);
  }

  /**
   * returns index of saved command
   * @param echoParam
   * @param previousCommand
   */
  public void deleteFromCombine(String command, String previousCommand) {
    deleteCommand(scriptCombine, command, AxisID.ONLY, previousCommand);
  }


  /**
   * 
   * @return
   */
  public GotoParam getGotoParamFromCombine() {
    // Initialize a GotoParam object from the com script command
    // object
    GotoParam gotoParam = new GotoParam();
    if (!initialize(gotoParam, scriptCombine, GotoParam.COMMAND_NAME, 
      AxisID.ONLY, true)) {
      return null;
    }
    return gotoParam;
  }
  
  /**
   * 
   * @param previousCommand
   * @return
   */
  public EchoParam getEchoParamFromCombine(String previousCommand) {
    // Initialize an EchoParam object from a location after previousCommand
    //in the com script command
    // object
    EchoParam echoParam = new EchoParam();
    if (!initialize(echoParam, scriptCombine, EchoParam.COMMAND_NAME, 
      AxisID.ONLY, previousCommand)) {
        return null;
    }
    return echoParam;
  }

  /**
   * Use a template script from the $IMOD_DIR/com directory.  This is useful if
   * we encounter an old data set that does not have a current complete set of
   * com scripts
   * @param scriptName the basename of the template file
   * @param axisType axisType Specify whether this is a single or dual axis
   * tomogram. 
   * @param axisID the AxisID to create.  This is ignored for single axis 
   * tomogram.  For a dual axis tomogram AxisID.FIRST will create an a.com
   * script, AxisID.SECOND will create a b.com script.  AxisID.ONLY will
   * create a .com script replacing the g5a and g5b tags.
   *  
   */
  public void useTemplate(String scriptName, String datasetName,
    AxisType axisType, AxisID axisID) throws BadComScriptException, IOException {
    // Read in the template file from the IMOD_DIR/com directory replacing all
    // instances of the tag g5a and g5b with the appropriate dataset name
    String comDirectory = ApplicationManager.getIMODDirectory()
      .getAbsolutePath()
      + File.separator + "com";

    File template = new File(comDirectory, scriptName + ".com");
    if (!template.exists()) {
      String message = "Unknown template: " + scriptName;
      throw new BadComScriptException(message);
    }
    BufferedReader templateReader = new BufferedReader(new FileReader(template));

    // The ouput script
    File script;
    BufferedWriter scriptWriter;

    // Open the appropriate output script and change the dataset name if
    // necessary.
    if (axisType == AxisType.SINGLE_AXIS) {
      script = new File(System.getProperty("user.dir"), scriptName + ".com");
      scriptWriter = new BufferedWriter(new FileWriter(script));
    }
    else {
      script = new File(System.getProperty("user.dir"), scriptName
        + axisID.getExtension() + ".com");
      scriptWriter = new BufferedWriter(new FileWriter(script));
      datasetName = datasetName + axisID.getExtension();
    }

    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      String line;
      while ((line = templateReader.readLine()) != null) {
        line = line.replaceAll("g5a", datasetName + "a");
        line = line.replaceAll("g5b", datasetName + "b");
        scriptWriter.write(line);
        scriptWriter.newLine();
      }
    }
    else {
      String line;
      while ((line = templateReader.readLine()) != null) {
        line = line.replaceAll("g5a", datasetName);
        scriptWriter.write(line);
        scriptWriter.newLine();
      }
    }
    templateReader.close();
    scriptWriter.close();
  }
  
  /**
  /**
   * Use a template script from the $IMOD_DIR/com directory.  This is useful if
   * we encounter an old data set that does not have a current complete set of
   * com scripts
   * @param scriptName the basename of the template file
   * @param axisType axisType Specify whether this is a single or dual axis
   * tomogram. 
   * @param axisID the AxisID to create.  This is ignored for single axis 
   * tomogram.  For a dual axis tomogram AxisID.FIRST will create an a.com
   * script, AxisID.SECOND will create a b.com script.  AxisID.ONLY will
   * create a .com script.
   * @param rename rename the original script file
   * @throws BadComScriptException
   * @throws IOException
   */
  public void useTemplate(String scriptName, AxisType axisType, AxisID axisID, 
    boolean rename) throws BadComScriptException, IOException {
    // Copy the template file from the IMOD_DIR/com directory to the script
    String comDirectory = ApplicationManager.getIMODDirectory()
      .getAbsolutePath()
      + File.separator + "com";

    File template = new File(comDirectory, scriptName + ".com");
    if (!template.exists()) {
      String message = "Unknown template: " + scriptName;
      throw new BadComScriptException(message);
    }

    // The ouput script
    File script;
    if (axisType == AxisType.SINGLE_AXIS) {
      script = new File(System.getProperty("user.dir"), scriptName + ".com");
    }
    else {
      script = new File(System.getProperty("user.dir"), scriptName
        + axisID.getExtension() + ".com");
    }
    
    if (rename) {
      Utilities.renameFile(script, new File(script.getAbsolutePath() + "~"));
    }
    Utilities.copyFile(template, script);
  }
 

  /**
   * Load the specified Com script 
   * @param scriptName
   * @param axisID
   * @return ComScript
   */
  private ComScript loadComScript(String scriptName, AxisID axisID,
    boolean parseComments) {
    String command = scriptName + axisID.getExtension() + ".com";
    File comFile = new File(System.getProperty("user.dir"), command);

    ComScript comScript = new ComScript(comFile);
    try {
      comScript.setParseComments(parseComments);
      comScript.readComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Com file: " + comScript.getComFileName();
      errorMessage[1] = except.getMessage();

      JOptionPane.showMessageDialog(null, errorMessage, "Can't parse "
        + scriptName + axisID.getExtension() + ".com file: "
        + comScript.getComFileName(), JOptionPane.ERROR_MESSAGE);
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
  private void updateComScript(ComScript script, CommandParam params,
      String command, AxisID axisID) {
    updateComScript(script, params, command, axisID, false); 
  }
  private void updateComScript(ComScript script, CommandParam params,
    String command, AxisID axisID, boolean addNew) {

    if (script == null) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to update comscript for command:"
        + command;
      errorMessage[1] = "It needs to be loaded first";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return;
    }

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command, addNew);

    try {
      comScriptCommand = script.getScriptCommand(command);
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
        .showMessageDialog(null, errorMessage, "Can't update " + command
          + " in " + script.getComFileName(), JOptionPane.ERROR_MESSAGE);
      return;
    }

    // Replace the specified command by the updated comScriptCommand
    script.setScriptComand(commandIndex, comScriptCommand);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, except.getMessage(), "Can't write "
        + command + axisID.getExtension() + ".com", JOptionPane.ERROR_MESSAGE);
    }
  }
  
  /**
   * 
   * @param script
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   * @param previousCommand
   * @return index of updated command
   */
  private int updateComScript(ComScript script, CommandParam params,
    String command, AxisID axisID, boolean addNew, String previousCommand) {
    if (script == null) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to update comscript for command:"
        + command;
      errorMessage[1] = "It needs to be loaded first";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return -1;
    }

    //locate previous command
    ComScriptCommand previousComScriptCommand = null;
    int previousCommandIndex = script.getScriptCommandIndex(previousCommand);
    
    if (previousCommandIndex == -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to update comscript for command:"
        + command;
      errorMessage[1] = "previous command:" + previousCommand + "is missing.";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return -1;
    }
    
    return updateComScript(script, params, command, axisID, addNew, 
      previousCommandIndex);
  }
  
  /**
   * 
   * @param script
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   * @param previousCommandIndex
   * @return
   */
  private int updateComScript(ComScript script, CommandParam params,
    String command, AxisID axisID, boolean addNew, int previousCommandIndex) {
    if (script == null) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to update comscript for command:"
        + command;
      errorMessage[1] = "It needs to be loaded first";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return -1;
    }
    
    if (previousCommandIndex < -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to update comscript for command:"
        + command;
      errorMessage[1] = "previous index, " + previousCommandIndex
        + ", is invalid.";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return -1;
    }
    
    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = previousCommandIndex + 1;

    try {
      comScriptCommand = script.getScriptCommand(command, commandIndex, addNew);
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
        .showMessageDialog(null, errorMessage, "Can't update " + command
          + " in " + script.getComFileName(), JOptionPane.ERROR_MESSAGE);
      return commandIndex;
    }

    // Replace the specified command by the updated comScriptCommand
    script.setScriptComand(commandIndex, comScriptCommand);
    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, except.getMessage(), "Can't write "
        + command + axisID.getExtension() + ".com", JOptionPane.ERROR_MESSAGE);
    }
    return commandIndex;
  }


  private void deleteCommand(ComScript script, String command, AxisID axisID, 
    String previousCommand) {
    if (script == null) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to delete command:"
        + command;
      errorMessage[1] = "It needs to be loaded first";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return;
    }

    //locate previous command
    ComScriptCommand previousComScriptCommand = null;
    int previousCommandIndex = script.getScriptCommandIndex(previousCommand);
    
    if (previousCommandIndex == -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Failed attempt to delete command:"
        + command;
      errorMessage[1] = "previous command:" + previousCommand + "is missing.";
      appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
      return;
    }
    
    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = 
      script.getScriptCommandIndex(command, previousCommandIndex + 1);
    
    if (commandIndex == -1) {
      return;
    }
    script.deleteCommand(commandIndex);
     
     //  Write the script back out to disk
     try {
       script.writeComFile();
     }
     catch (Exception except) {
       except.printStackTrace();
       String[] errorMessage = new String[3];
       errorMessage[0] = "Com file: " + script.getComFileName();
       errorMessage[1] = "Command: " + command;
       errorMessage[2] = except.getMessage();
       JOptionPane.showMessageDialog(null, except.getMessage(), "Can't write "
         + command + axisID.getExtension() + ".com", JOptionPane.ERROR_MESSAGE);
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
  private boolean initialize(CommandParam param, ComScript comScript,
    String command, AxisID axisID) {
    return initialize(param, comScript, command, axisID, false);
  }
  private boolean initialize(CommandParam param, ComScript comScript,
    String command, AxisID axisID, boolean optionalCommand) {
    if (!comScript.isCommandLoaded()) {
      param.initializeDefaults();
    }
    else {
      if (optionalCommand) {
        if (comScript.getScriptCommandIndex(command) == -1) {
          return false;
        }
      }
      try {
        param.parseComScriptCommand(comScript.getScriptCommand(command));
      }
      catch (Exception except) {
        except.printStackTrace();
        String[] errorMessage = new String[4];
        errorMessage[0] = "Com file: " + comScript.getComFileName();
        errorMessage[1] = "Command: " + command;
        errorMessage[2] = except.getClass().getName();
        errorMessage[3] = except.getMessage();
        JOptionPane.showMessageDialog(
          null,
          errorMessage,
          "Com Script Command Parse Error",
          JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }
    return true;
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
  private boolean initialize(CommandParam param, ComScript comScript,
    String command, AxisID axisID, String previousCommand) {
    if (previousCommand == null) {
      return initialize(param, comScript, command, axisID);
    }
    if (!comScript.isCommandLoaded()) {
      param.initializeDefaults();
    }
    else {
      //locate previous command
      ComScriptCommand previousComScriptCommand = null;
      int previousCommandIndex = comScript.getScriptCommandIndex(previousCommand);
    
      if (previousCommandIndex == -1) {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Failed attempt to initialize comscript for command:"
          + command;
        errorMessage[1] = "previous command:" + previousCommand + "is missing.";
        appManager.openMessageDialog(errorMessage, "ComScriptManager Error");
        return false;
      }
      
      ComScriptCommand comScriptCommand = null;
      int commandIndex =
        comScript.getScriptCommandIndex(command, previousCommandIndex + 1);
          
      if (commandIndex == -1) {
        return false;
      }
      
      try {
        param.parseComScriptCommand(
          comScript.getScriptCommand(command, commandIndex, false));
      }
      catch (Exception except) {
        except.printStackTrace();
        String[] errorMessage = new String[4];
        errorMessage[0] = "Com file: " + comScript.getComFileName();
        errorMessage[1] = "Command: " + command + " after " + previousCommand;
        errorMessage[2] = except.getClass().getName();
        errorMessage[3] = except.getMessage();
        JOptionPane.showMessageDialog(
          null,
          errorMessage,
          "Com Script Command Parse Error",
          JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }
    return true;
  }

  /**
   * Examine the com script to see whether it contains newst or newstack
   * commands.
   * @param comScript  The com script object to examine
   * @return The 
   */
  private String newstOrNewstack(ComScript comScript) {
    String[] commands = comScript.getCommandArray();
    for (int i = 0; i < commands.length; i++) {
      if (commands[i].equals("newst")) {
        return "newst";
      }
      if (commands[i].equals("newstack")) {
        return "newstack";
      }
    }
    return "";
  }
}