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
package etomo.comscript;

import java.io.File;

import javax.swing.JOptionPane;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.util.TestFiles;


public class ComScriptManager {
  public static final String rcsid =
    "$Id$";

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
  private ComScript scriptSolvematchshift;
  private ComScript scriptSolvematchmod;
  private ComScript scriptPatchcorr;
  private ComScript scriptMatchorwarp;
  private ComScript scriptTomopitchA;
  private ComScript scriptTomopitchB;

  private ComScript selfTest1OldScriptXcorrA;
  private ComScript selfTest1NewScriptXcorrA;
  private ComScript selfTest2OldScriptXcorrA;
  private ComScript selfTest2NewScriptXcorrA;
  private ComScript selfTest1OldScriptXcorrB;
  private ComScript selfTest1NewScriptXcorrB;
  private ComScript selfTest2OldScriptXcorrB;
  private ComScript selfTest2NewScriptXcorrB;
  
  private CommandParam selfTest1OldParamXcorrA;
  private CommandParam selfTest1NewParamXcorrA;
  private CommandParam selfTest2OldParamXcorrA;
  private CommandParam selfTest2NewParamXcorrA;
  private CommandParam selfTest1OldParamXcorrB;
  private CommandParam selfTest1NewParamXcorrB;
  private CommandParam selfTest2OldParamXcorrB;
  private CommandParam selfTest2NewParamXcorrB;
  
  private boolean selfTest = false;
  
  public ComScriptManager(ApplicationManager appManager) {
    this.appManager = appManager;
  }
  public ComScriptManager(ApplicationManager appManager, boolean selfTest) {
    this.appManager = appManager;
    this.selfTest = true;
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
    loadTest(new ConstTiltxcorrParam(), axisID);
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
    initializeTestParam(axisID);
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
      scriptPrenewstA= loadComScript("prenewst", axisID, true);
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
    String cmdName = newstOrNewstack(scriptPrenewst) ;
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
    String cmdName = newstOrNewstack(scriptNewst) ;
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
    String cmdName = newstOrNewstack(scriptNewst) ;
    
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
    initialize(
      solveMatchshiftParam,
      scriptSolvematchshift,
      "solvematch",
      AxisID.ONLY);
    return solveMatchshiftParam;
  }

  /**
   * Save the solvematchshift com script updating the solveMatchshiftParam
   * parameters
   * @param solveMatchshiftParam
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
    initialize(
      solveMatchmodParam,
      scriptSolvematchmod,
      "solvematch",
      AxisID.ONLY);
    return solveMatchmodParam;
  }

  /**
   * Save the solvematchmod com script updating the solveMatchmodParam
   * parameters
   * @param solveMatchmodParam
   */
  public void saveSolvematchmod(SolvematchmodParam solveMatchmodParam) {

    updateComScript(
      scriptSolvematchmod,
      solveMatchmodParam,
      "solvematch",
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

    updateComScript(
      scriptPatchcorr,
      patchcrawl3DParam,
      "patchcrawl3d",
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

    updateComScript(
      scriptMatchorwarp,
      matchorwarpParam,
      "matchorwarp",
      AxisID.ONLY);
  }

  private ComScript loadComScript(
    String scriptName,
    AxisID axisID,
    boolean parseComments) {
      return loadComScript(scriptName, axisID, parseComments, -1, -1);
  }
  /**
   * Load the specified Com script 
   * @param scriptName
   * @param axisID
   * @return ComScript
   */
  private ComScript loadComScript(
    String scriptName,
    AxisID axisID,
    boolean parseComments, int testNumber, int testScriptVersion) {

    File comFile;
    if (selfTest && testNumber < 0 && testScriptVersion < 0) {
      comFile =
        TestFiles.getTestFiles().getComscript(
          scriptName,
          axisID,
          testNumber,
          testScriptVersion,
          true);
      if (comFile == null) {
        return null;
      }
    }
    else {
      String command = scriptName + axisID.getExtension() + ".com";

      comFile = new File(System.getProperty("user.dir"), command);
    }
    
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

      JOptionPane.showMessageDialog(
        null,
        errorMessage,
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
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command);
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
      JOptionPane.showMessageDialog(
        null,
        errorMessage,
        "Can't update " + command + " in " + script.getComFileName(),
        JOptionPane.ERROR_MESSAGE);
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

    if (!comScript.isCommandLoaded()) {
      param.initializeDefaults();
    }
    else {
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
   * Examine the com script to see whether it contains newst or newstack
   * commands.
   * @param comScript  The com script object to examine
   * @return The 
   */
  private String newstOrNewstack(ComScript comScript){
    String[] commands = comScript.getCommandArray();
    for(int i = 0; i < commands.length; i++){
      if(commands[i].equals("newst")) {
        return "newst";
      }
      if(commands[i].equals("newstack")) {
        return "newstack";
      }
    }
    return "";
  }
  
  
  protected void loadTest(ConstCommandParam constParam, AxisID axisID) {
    if (!selfTest) {
      return;
    }
    if (axisID == AxisID.SECOND) {
      selfTest1OldScriptXcorrB =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_LOAD,
          TestFiles.OLD_VERSION);
      selfTest1NewScriptXcorrB =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_LOAD,
          TestFiles.NEW_VERSION);
      selfTest2OldScriptXcorrB =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_SET,
          TestFiles.OLD_VERSION);
      selfTest2NewScriptXcorrB =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_SET,
          TestFiles.NEW_VERSION);
    }
    else {
      selfTest1OldScriptXcorrA =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_LOAD,
          TestFiles.OLD_VERSION);
      selfTest1NewScriptXcorrA =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_LOAD,
          TestFiles.NEW_VERSION);
      selfTest2OldScriptXcorrA =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_SET,
          TestFiles.OLD_VERSION);
      selfTest2NewScriptXcorrA =
        loadComScript(
          constParam.getProcessNameString(),
          axisID,
          constParam.isParseComments(),
          TestFiles.TEST_TYPE_SET,
          TestFiles.NEW_VERSION);
    }
  }
  
  protected void initializeTestParam(AxisID axisID) {
    if (!selfTest) {
      return;
    }
    if (axisID == AxisID.SECOND) {
      selfTest1OldParamXcorrB = new TiltxcorrParam();
      initialize(selfTest1OldParamXcorrB, selfTest1OldScriptXcorrB, "tiltxcorr", axisID);
      selfTest1NewParamXcorrB = new TiltxcorrParam();
      initialize(selfTest2OldParamXcorrB, selfTest2OldScriptXcorrB, "tiltxcorr", axisID);
      selfTest2OldParamXcorrB = new TiltxcorrParam();
      initialize(selfTest1NewParamXcorrB, selfTest1NewScriptXcorrB, "tiltxcorr", axisID);
      selfTest2NewParamXcorrB = new TiltxcorrParam();
      initialize(selfTest2NewParamXcorrB, selfTest2NewScriptXcorrB, "tiltxcorr", axisID);
    }
    else {
      selfTest1OldParamXcorrA = new TiltxcorrParam();
      initialize(selfTest1OldParamXcorrA, selfTest1OldScriptXcorrA, "tiltxcorr", axisID);
      selfTest1NewParamXcorrA = new TiltxcorrParam();
      initialize(selfTest2OldParamXcorrA, selfTest2OldScriptXcorrA, "tiltxcorr", axisID);
      selfTest2OldParamXcorrA = new TiltxcorrParam();
      initialize(selfTest1NewParamXcorrA, selfTest1NewScriptXcorrA, "tiltxcorr", axisID);
      selfTest2OldParamXcorrA = new TiltxcorrParam();
      initialize(selfTest2NewParamXcorrA, selfTest2NewScriptXcorrA, "tiltxcorr", axisID);
    }
  }

}
