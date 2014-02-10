package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.logic.DatasetTool;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;
import etomo.ui.swing.UIHarness;
import etomo.util.MRCHeader;

/**
 * <p>Description: Newstack command model</p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.33  2011/05/10 16:49:36  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 3.32  2011/04/09 06:25:17  sueh
 * <p> bug# 1416 Replaced FileType..DUAL_AXIS_TOMOGRAM and SINGLE_AXIS_TOMOGRAM with TILT_OUTPUT.
 * <p>
 * <p> Revision 3.31  2011/02/28 22:45:12  sueh
 * <p> bug# 1452 Making imageRotation double.
 * <p>
 * <p> Revision 3.30  2011/02/24 23:34:54  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 3.29  2011/02/22 03:18:13  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.28  2010/04/28 16:03:57  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.  Changed outputFile
 * <p> from Vector to String because current code does not work with an output
 * <p> file vector of more then one.
 * <p>
 * <p> Revision 3.27  2010/02/17 04:47:53  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.26  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 3.25  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 3.24  2009/12/08 02:37:36  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 3.23  2009/09/21 17:45:56  sueh
 * <p> bug# 1267 Reformatted.
 * <p>
 * <p> Revision 3.22  2009/09/17 19:10:43  sueh
 * <p> bug# 1257 In setSizeToOutputInXandY forgot to read the header.  Adding
 * <p> read call and throwing InvalidParameterException and IOException.
 * <p>
 * <p> Revision 3.21  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 3.20  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.19  2009/03/17 00:32:21  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.18  2008/12/15 23:00:26  sueh
 * <p> bug# 1161 In setSizeToOutputInXandY handle 90 degree tilt axis angles.
 * <p>
 * <p> Revision 3.17  2007/12/13 01:05:34  sueh
 * <p> bug# 1056 Added adjustOrigin.
 * <p>
 * <p> Revision 3.16  2007/08/16 16:30:13  sueh
 * <p> bug# 1035 In setSizeToOutputInXandY divided values by binning.
 * <p>
 * <p> Revision 3.15  2007/02/05 22:39:26  sueh
 * <p> bug# 962 Changed getCommandMode to return CommandMode.
 * <p>
 * <p> Revision 3.14  2006/03/22 21:28:42  sueh
 * <p> bug# 803 Added DATA_MODE_OPTION and
 * <p> FLOAT_DENSITIES_OPTION.
 * <p>
 * <p> Revision 3.13  2006/03/22 17:53:26  sueh
 * <p> bug# 803 In updatecomScriptCommand() changed -mode to -mo, since
 * <p> that is what copytomocoms is using.
 * <p>
 * <p> Revision 3.12  2006/01/20 20:47:39  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 3.11  2005/11/04 00:52:45  sueh
 * <p> fixed file comment
 * <p>
 * <p> Revision 3.10  2005/09/02 18:56:08  sueh
 * <p> bug# 721 Adding magGradientFile.
 * <p>
 * <p> Revision 3.9  2005/01/08 01:40:34  sueh
 * <p> bug# 578 Create 2 modes - whole tomo sample and full aligned stack.
 * <p> Implement Command.  Add fiducialessAlignment variable and make it
 * <p> available through the Command interface.
 * <p>
 * <p> Revision 3.8  2004/06/24 22:22:59  sueh
 * <p> bug# 451 switching to short param names
 * <p>
 * <p> Revision 3.7  2004/04/12 16:49:51  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.6  2004/03/13 00:31:36  rickg
 * <p> Bug# 390 Add parsing for the rest of the options
 * <p>
 * <p> Revision 3.5  2004/03/12 00:16:37  rickg
 * <p> Bug #410 Newstack PIP transition
 * <p> Change instances of newst command to newstack when updating
 * <p> the com script
 * <p>
 * <p> Revision 3.4  2004/03/12 00:00:22  rickg
 * <p> Bug #410 Newstack PIP transition
 * <p> Check for default values in FortranInputStrings, don't write out the
 * <p> parameter in this case.
 * <p> Removed setSize method
 * <p>
 * <p> Revision 3.3  2004/02/18 00:51:22  rickg
 * <p> Removed CVS tag
 * <p>
 * <p> Revision 3.2  2004/02/14 00:14:30  rickg
 * <p> Parse all command line opts
 * <p>
 * <p> Revision 3.1  2004/02/13 01:04:17  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/10/28 18:46:59  sueh
 * <p> removing prints
 * <p>
 * <p> Revision 2.6  2003/10/02 18:57:46  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.5  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
 * <p>
 * <p> Revision 2.4  2003/07/25 22:54:14  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.2  2003/03/20 17:23:37  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public final class NewstParam implements ConstNewstParam, CommandParam {
  public static final String rcsid = "$Id$";

  private static final String INPUT_FILE_KEY = "InputFile";
  private static final String SECTIONS_TO_READ_KEY = "SectionsToRead";
  private static final String NUMBER_TO_OUTPUT_KEY = "NumberToOutput";
  public static final String SIZE_TO_OUTPUT_IN_X_AND_Y = "SizeToOutputInXandY";
  public static final String IMAGES_ARE_BINNED_KEY = "ImagesAreBinned";
  public static final String DISTORTION_FIELD_KEY = "DistortionField";
  public static final String OFFSETS_IN_X_AND_Y_KEY = "OffsetsInXandY";
  public static final String BIN_BY_FACTOR_KEY = "BinByFactor";
  public static final String FILL_VALUE_KEY = "FillValue";
  public static final String MODE_TO_OUTPUT_KEY = "ModeToOutput";
  // data mode
  public static final String DATA_MODE_OPTION = "-mo";
  public static final int DATA_MODE_DEFAULT = Integer.MIN_VALUE;
  public static final int DATA_MODE_BYTE = 0;
  // float densities
  public static final String FLOAT_DENSITIES_OPTION = "-fl";
  public static final int FLOAT_DENSITIES_DEFAULT = Integer.MIN_VALUE;
  public static final int FLOAT_DENSITIES_MEAN = 2;
  private static final String COMMAND_FILE_EXTENSION = ".com";
  private static final int DEFAULT_ANTIALIAS_FILTER = -1;

  private final List<String> inputFile = new Vector<String>();
  private final StringParameter outputFile = new StringParameter("OutputFile");
  private final StringParameter fileOfInputs = new StringParameter("FileOfInputs");
  private final StringParameter fileOfOutputs = new StringParameter("FileOfOutputs");
  private final List<String> sectionsToRead = new Vector<String>();
  private final List<String> numberToOutput = new Vector<String>();
  private final FortranInputString sizeToOutputInXandY = new FortranInputString(
      "SizeToOutputInXandY", 2);
  private final ScriptParameter modeToOutput = new ScriptParameter(MODE_TO_OUTPUT_KEY);
  private final FortranInputString userSizeToOutputInXandY = new FortranInputString(2);
  private final FortranInputString offsetsInXandY = new FortranInputString(
      OFFSETS_IN_X_AND_Y_KEY, 2);
  private final Vector<FortranInputString> offsetsInXandYExtraEntries = new Vector<FortranInputString>();
  private final EtomoBoolean2 applyOffsetsFirst = new EtomoBoolean2("ApplyOffsetsFirst");
  private final StringParameter transformFile = new StringParameter("TransformFile");
  private final StringParameter useTransformLines = new StringParameter(
      "UseTransformLines");
  private final ScriptParameter rotateByAngle = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "RotateByAngle");
  private final ScriptParameter expandByFactor = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "ExpandByFactor");
  private final ScriptParameter binByFactor = new ScriptParameter("BinByFactor");
  private final EtomoBoolean2 linearInterpolation = new EtomoBoolean2(
      "LinearInterpolation");
  private final ScriptParameter floatDensities = new ScriptParameter("FloatDensities");
  private final FortranInputString contrastBlackWhite = new FortranInputString(
      "ContrastBlackWhite", 2);
  private final FortranInputString scaleMinAndMax = new FortranInputString(
      "ScaleMinAndMax", 2);
  private final StringParameter distortionField = new StringParameter("DistortionField");
  private final ScriptParameter imagesAreBinned = new ScriptParameter("ImagesAreBinned");
  private final FortranInputString testLimits = new FortranInputString("TestLimits", 2);
  private final StringParameter gradientFile = new StringParameter("GradientFile");
  /**
   * @version 3.10
   * Script is from an earlier version if false.
   */
  private final EtomoBoolean2 adjustOrigin = new EtomoBoolean2("AdjustOrigin");
  private final FortranInputString taperAtFill = new FortranInputString("TaperAtFill", 2);
  private final ScriptParameter fillValue = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      "FillValue");
  private final EtomoNumber imageRotation = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final ScriptParameter antialiasFilter = new ScriptParameter("AntialiasFilter");

  // colornewst only parameters
  private EtomoBoolean2 cntiff = new EtomoBoolean2("-cntiff");
  private final ScriptParameter cntempdir = new ScriptParameter("-cntempdir");
  private final EtomoNumber cnmaxtemp = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "-cnmaxtemp");
  private EtomoBoolean2 cnverbose = new EtomoBoolean2("-cnverbose");

  private final AxisID axisID;
  private final BaseManager manager;
  private final boolean useColorNewst;

  /**
   * Set when outputFile is set from the dialog, otherwise set to null when
   * outputFile changed.
   */
  private FileType outputFileType = null;
  private ProcessName processName = ProcessName.NEWST;
  private boolean validate = false;

  private boolean fiducialessAlignment;
  private Mode mode;

  private NewstParam(final BaseManager manager, final AxisID axisID,
      final boolean useColorNewst) {
    this.manager = manager;
    this.axisID = axisID;
    this.useColorNewst = useColorNewst;
    sizeToOutputInXandY.setIntegerType(true);
    userSizeToOutputInXandY.setIntegerType(true);
    binByFactor.setDisplayValue(1);
    contrastBlackWhite.setIntegerType(true);
    testLimits.setIntegerType(true);
    taperAtFill.setIntegerType(true);
    reset();
  }

  public static NewstParam getInstance(final BaseManager manager, final AxisID axisID) {
    return new NewstParam(manager, axisID, false);
  }

  public static NewstParam getColorInstance(final BaseManager manager, final AxisID axisID) {
    return new NewstParam(manager, axisID, true);
  }

  public void setValidate(final boolean validate) {
    this.validate = validate;
  }

  private void reset() {
    validate = false;
    inputFile.clear();
    outputFile.reset();
    outputFileType = null;
    fileOfInputs.reset();
    fileOfOutputs.reset();
    sectionsToRead.clear();
    numberToOutput.clear();
    sizeToOutputInXandY.reset();
    userSizeToOutputInXandY.reset();
    modeToOutput.reset();
    offsetsInXandY.reset();
    offsetsInXandYExtraEntries.clear();
    applyOffsetsFirst.reset();
    transformFile.reset();
    useTransformLines.reset();
    rotateByAngle.reset();
    expandByFactor.reset();
    binByFactor.reset();
    linearInterpolation.reset();
    floatDensities.reset();
    contrastBlackWhite.reset();
    scaleMinAndMax.reset();
    distortionField.reset();
    imagesAreBinned.reset();
    testLimits.reset();
    fiducialessAlignment = false;
    gradientFile.reset();
    adjustOrigin.reset();
    taperAtFill.reset();
    imageRotation.reset();
    fillValue.reset();
    antialiasFilter.reset();
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws FortranInputSyntaxException, InvalidParameterException {
    reset();
    if (scriptCommand.isKeywordValuePairs()) {
      // Read in everything that the backwards compatibility code is reading in, even if
      // it is not used or changed.
      String[] array = scriptCommand.getValues(INPUT_FILE_KEY);
      if (array != null) {
        for (int i = 0; i < array.length; i++) {
          inputFile.add(array[i]);
        }
      }
      outputFile.parse(scriptCommand);
      fileOfInputs.parse(scriptCommand);
      fileOfOutputs.parse(scriptCommand);
      array = scriptCommand.getValues(SECTIONS_TO_READ_KEY);
      if (array != null) {
        for (int i = 0; i < array.length; i++) {
          sectionsToRead.add(array[i]);
        }
      }
      array = scriptCommand.getValues(NUMBER_TO_OUTPUT_KEY);
      if (array != null) {
        for (int i = 0; i < array.length; i++) {
          numberToOutput.add(array[i]);
        }
      }
      sizeToOutputInXandY.validateAndSet(scriptCommand);
      modeToOutput.parse(scriptCommand);
      array = scriptCommand.getValues(OFFSETS_IN_X_AND_Y_KEY);
      if (array != null) {
        for (int i = 0; i < array.length; i++) {
          if (i == 0) {
            offsetsInXandY.validateAndSet(array[i]);
          }
          else {
            FortranInputString fis = new FortranInputString(OFFSETS_IN_X_AND_Y_KEY, 2);
            fis.validateAndSet(array[i]);
            offsetsInXandYExtraEntries.add(fis);
          }
        }
      }
      applyOffsetsFirst.parse(scriptCommand);
      transformFile.parse(scriptCommand);
      useTransformLines.parse(scriptCommand);
      rotateByAngle.parse(scriptCommand);
      expandByFactor.parse(scriptCommand);
      binByFactor.parse(scriptCommand);
      linearInterpolation.parse(scriptCommand);
      floatDensities.parse(scriptCommand);
      contrastBlackWhite.validateAndSet(scriptCommand);
      scaleMinAndMax.validateAndSet(scriptCommand);
      distortionField.parse(scriptCommand);
      imagesAreBinned.parse(scriptCommand);
      testLimits.validateAndSet(scriptCommand);
      gradientFile.parse(scriptCommand);
      adjustOrigin.parse(scriptCommand);
      taperAtFill.validateAndSet(scriptCommand);
      fillValue.parse(scriptCommand);
      antialiasFilter.parse(scriptCommand);
    }
    else {
      // Backwards compatibility
      String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
      reset();
      for (int i = 0; i < cmdLineArgs.length; i++) {
        // Is it an argument or filename
        if (cmdLineArgs[i].startsWith("-")) {
          // Handle all the colornewst parameters
          if (useColorNewst
              && (cmdLineArgs[i].startsWith("-cn") || cmdLineArgs[i].startsWith("--cn"))) {
            if (cmdLineArgs[i].toLowerCase().endsWith(cntiff.getName())) {
              cntiff.set(true);
            }
            else if (cmdLineArgs[i].endsWith(cntempdir.getName())) {
              i++;
              cntempdir.set(cmdLineArgs[i]);
            }
            else if (cmdLineArgs[i].endsWith(cnmaxtemp.getName())) {
              i++;
              cnmaxtemp.set(cmdLineArgs[i]);
            }
            else if (cmdLineArgs[i].toLowerCase().endsWith(cnverbose.getName())) {
              cnverbose.set(true);
            }
            else {
              String message = "Unknown argument: " + cmdLineArgs[i];
              throw new InvalidParameterException(message);
            }
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-inp")) {
            i++;
            inputFile.add(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-out")) {
            i++;
            outputFile.set(cmdLineArgs[i]);
            outputFileType = null;
          }
          else if (cmdLineArgs[i].startsWith("-filei")
              || cmdLineArgs[i].startsWith("-FileOfI")) {
            i++;
            fileOfInputs.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].startsWith("-fileo")
              || cmdLineArgs[i].startsWith("-FileOfO")) {
            i++;
            fileOfOutputs.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-sec")) {
            i++;
            sectionsToRead.add(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-num")) {
            i++;
            numberToOutput.add(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-siz")) {
            i++;
            sizeToOutputInXandY.validateAndSet(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-mod")) {
            i++;
            modeToOutput.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-off")) {
            i++;
            // The first entry goes into the single variable, the next entries go into the
            // vector.
            if (offsetsInXandY.isNull()) {
              offsetsInXandY.validateAndSet(cmdLineArgs[i]);
            }
            else {
              FortranInputString input = new FortranInputString(2);
              input.validateAndSet(cmdLineArgs[i]);
              offsetsInXandYExtraEntries.add(input);
            }
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-appl")) {
            applyOffsetsFirst.set(true);
          }
          else if (cmdLineArgs[i].startsWith("-xfo") || cmdLineArgs[i].startsWith("-Tra")) {
            i++;
            transformFile.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-use")) {
            i++;
            useTransformLines.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-rot")) {
            i++;
            rotateByAngle.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-exp")) {
            i++;
            expandByFactor.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-bin")) {
            i++;
            binByFactor.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-lin")) {
            linearInterpolation.set(true);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-flo")) {
            i++;
            floatDensities.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-con")) {
            i++;
            contrastBlackWhite.validateAndSet(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-sca")) {
            i++;
            scaleMinAndMax.validateAndSet(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-dis")) {
            i++;
            distortionField.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-ima")) {
            i++;
            imagesAreBinned.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-tes")) {
            i++;
            testLimits.validateAndSet(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-grad")) {
            i++;
            gradientFile.set(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-ori")
              || cmdLineArgs[i].startsWith("-Adj")) {
            adjustOrigin.set(true);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-taper")) {
            i++;
            taperAtFill.validateAndSet(cmdLineArgs[i]);
          }
          else if (cmdLineArgs[i].toLowerCase().startsWith("-fill")) {
            i++;
            fillValue.set(cmdLineArgs[i]);
          }
          else {
            String message = "Unknown argument: " + cmdLineArgs[i];
            throw new InvalidParameterException(message);
          }
        }
        // input and output filename arguments
        else {
          if (i == (cmdLineArgs.length - 1)) {
            outputFile.set(cmdLineArgs[i]);
            outputFileType = null;
          }
          else {
            inputFile.add(cmdLineArgs[i]);
          }
        }
      }
    }
  }

  /**
   * Update the script command with the current valus of this NewstParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    if (!useColorNewst) {
      scriptCommand.useKeywordValue();
      for (Iterator<String> i = inputFile.iterator(); i.hasNext();) {
        scriptCommand.setValue(INPUT_FILE_KEY, i.next());
      }
      outputFile.updateComScript(scriptCommand);
      fileOfInputs.updateComScript(scriptCommand);
      fileOfOutputs.updateComScript(scriptCommand);
      for (Iterator<String> i = sectionsToRead.iterator(); i.hasNext();) {
        scriptCommand.setValue(SECTIONS_TO_READ_KEY, i.next());
      }
      for (Iterator<String> i = numberToOutput.iterator(); i.hasNext();) {
        scriptCommand.setValue(NUMBER_TO_OUTPUT_KEY, i.next());
      }
      sizeToOutputInXandY.updateScriptParameter(scriptCommand);
      modeToOutput.updateComScript(scriptCommand);
      offsetsInXandY.updateScriptParameter(scriptCommand);
      for (Iterator<FortranInputString> i = offsetsInXandYExtraEntries.iterator(); i
          .hasNext();) {
        i.next().updateScriptParameter(scriptCommand);
      }
      applyOffsetsFirst.updateComScript(scriptCommand);
      transformFile.updateComScript(scriptCommand);
      useTransformLines.updateComScript(scriptCommand);
      rotateByAngle.updateComScript(scriptCommand);
      expandByFactor.updateComScript(scriptCommand);
      binByFactor.updateComScript(scriptCommand);
      linearInterpolation.updateComScript(scriptCommand);
      floatDensities.updateComScript(scriptCommand);
      contrastBlackWhite.updateScriptParameter(scriptCommand);
      scaleMinAndMax.updateScriptParameter(scriptCommand);
      distortionField.updateComScript(scriptCommand);
      imagesAreBinned.updateComScript(scriptCommand);
      testLimits.updateScriptParameter(scriptCommand);
      gradientFile.updateComScript(scriptCommand);
      adjustOrigin.updateComScript(scriptCommand);
      taperAtFill.updateScriptParameter(scriptCommand);
      fillValue.updateComScript(scriptCommand);
      antialiasFilter.updateComScript(scriptCommand);
    }
    else {
      // Create a new command line argument array
      List<String> cmdLineArgs = new ArrayList<String>();
      // colornewst
      if (cntiff.is()) {
        cmdLineArgs.add(cntiff.getName());
      }
      if (!cntempdir.isNull()) {
        cmdLineArgs.add(cntempdir.getName());
        cmdLineArgs.add(cntempdir.toString());
      }
      if (!cnmaxtemp.isNull()) {
        cmdLineArgs.add(cnmaxtemp.getName());
        cmdLineArgs.add(cnmaxtemp.toString());
      }
      if (cnverbose.is()) {
        cmdLineArgs.add(cnverbose.getName());
      }
      if (!fileOfInputs.isEmpty()) {
        cmdLineArgs.add("-fileinlist");
        cmdLineArgs.add(fileOfInputs.toString());
      }
      if (!fileOfOutputs.isEmpty()) {
        cmdLineArgs.add("-fileoutlist");
        cmdLineArgs.add(fileOfOutputs.toString());
      }
      for (Iterator i = sectionsToRead.iterator(); i.hasNext();) {
        cmdLineArgs.add("-secs");
        cmdLineArgs.add((String) i.next());
      }
      for (Iterator i = numberToOutput.iterator(); i.hasNext();) {
        cmdLineArgs.add("-numout");
        cmdLineArgs.add((String) i.next());
      }
      if (sizeToOutputInXandY.valuesSet() && (!sizeToOutputInXandY.isDefault())) {
        cmdLineArgs.add("-size");
        cmdLineArgs.add(sizeToOutputInXandY.toString());
      }
      if (!modeToOutput.isNull()) {
        cmdLineArgs.add(DATA_MODE_OPTION);
        cmdLineArgs.add(modeToOutput.toString());
      }
      if (!offsetsInXandY.isNull()) {
        cmdLineArgs.add("-offset");
        cmdLineArgs.add(offsetsInXandY.toString(true));
      }
      for (Iterator<FortranInputString> i = offsetsInXandYExtraEntries.iterator(); i
          .hasNext();) {
        FortranInputString fis = i.next();
        if (!fis.isNull()) {
          cmdLineArgs.add("-offset");
          cmdLineArgs.add(fis.toString(true));
        }
      }
      if (applyOffsetsFirst.is()) {
        cmdLineArgs.add("-applyfirst");
      }
      if (!transformFile.isEmpty()) {
        cmdLineArgs.add("-xform");
        cmdLineArgs.add(transformFile.toString());
      }
      if (!useTransformLines.isEmpty()) {
        cmdLineArgs.add("-uselines");
        cmdLineArgs.add(useTransformLines.toString());
      }
      if (!rotateByAngle.isNull()) {
        cmdLineArgs.add("-rotate");
        cmdLineArgs.add(rotateByAngle.toString());
      }
      if (!expandByFactor.isNull()) {
        cmdLineArgs.add("-expand");
        cmdLineArgs.add(expandByFactor.toString());
      }
      if (!binByFactor.isNull()) {
        cmdLineArgs.add("-bin");
        cmdLineArgs.add(binByFactor.toString());
      }
      if (linearInterpolation.is()) {
        cmdLineArgs.add("-linear");
      }
      if (!floatDensities.isNull()) {
        cmdLineArgs.add(FLOAT_DENSITIES_OPTION);
        cmdLineArgs.add(floatDensities.toString());
      }
      if (contrastBlackWhite.valuesSet() && (!contrastBlackWhite.isDefault())) {
        cmdLineArgs.add("-contrast");
        cmdLineArgs.add(contrastBlackWhite.toString());
      }
      if (scaleMinAndMax.valuesSet() && (!scaleMinAndMax.isDefault())) {
        cmdLineArgs.add("-scale");
        cmdLineArgs.add(scaleMinAndMax.toString());
      }
      if (!distortionField.isEmpty()) {
        cmdLineArgs.add("-distort");
        cmdLineArgs.add(distortionField.toString());
      }
      if (!imagesAreBinned.isNull()) {
        cmdLineArgs.add("-imagebinned");
        cmdLineArgs.add(imagesAreBinned.toString());
      }
      if (testLimits.valuesSet() && (!testLimits.isDefault())) {
        cmdLineArgs.add("-test");
        cmdLineArgs.add(testLimits.toString());
      }
      if (!gradientFile.isEmpty()) {
        cmdLineArgs.add("-grad");
        cmdLineArgs.add(gradientFile.toString());
      }
      if (adjustOrigin.is()) {
        cmdLineArgs.add("-origin");
      }
      if (taperAtFill.valuesSet() && (!taperAtFill.isDefault())) {
        cmdLineArgs.add("-taper");
        cmdLineArgs.add(taperAtFill.toString());
      }
      if (!fillValue.isNull()) {
        cmdLineArgs.add("-fill");
        cmdLineArgs.add(fillValue.toString());
      }
      // Add input file(s) and output file last and without a parameter tag.
      for (Iterator i = inputFile.iterator(); i.hasNext();) {
        // cmdLineArgs.add("-input");
        cmdLineArgs.add((String) i.next());
      }
      // cmdLineArgs.add("-output");
      cmdLineArgs.add(outputFile.toString());
      int nArgs = cmdLineArgs.size();
      scriptCommand.setCommandLineArgs((String[]) cmdLineArgs.toArray(new String[nArgs]));

      // If the command is currently newst change it to newstack
      scriptCommand.setCommand(getCommandName());
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println(scriptCommand.getCommand());
        String[] commandArray = scriptCommand.getCommandLineArgs();
        if (commandArray != null) {
          for (int i = 0; i < commandArray.length; i++) {
            System.err.print(commandArray[i] + " ");
          }
          System.err.println();
        }
      }
    }
  }

  public void initializeDefaults() {
  }

  public void setBinByFactor(final Number input) {
    if (input == null) {
      binByFactor.set(1);
    }
    else {
      binByFactor.set(input);
    }
  }

  public void setCnverbose(final boolean input) {
    cnverbose.set(input);
  }

  public void resetDistortionField() {
    distortionField.reset();
  }

  /**
   * @param distortionField The distortionField to set.
   */
  public void setDistortionField(final String distortionField) {
    this.distortionField.set(distortionField);
  }

  /**
   * @param floatDensities The floatDensities to set.
   */
  public void setFloatDensities(final int floatDensities) {
    this.floatDensities.set(floatDensities);
  }

  public void setFiducialessAlignment(final boolean fiducialessAlignment) {
    this.fiducialessAlignment = fiducialessAlignment;
  }

  public void setFillValue(final int input) {
    fillValue.set(input);
  }

  public void setImagesAreBinned(final Number input) {
    if (input == null) {
      imagesAreBinned.reset();
    }
    else {
      imagesAreBinned.set(input.intValue());
    }
  }

  public void resetInputFile() {
    inputFile.clear();
  }

  public void setInputFile(final String input) {
    inputFile.clear();
    if (input == null) {
      return;
    }
    inputFile.add(input);
  }

  /**
   * @param linearInterpolation The linearInterpolation to set.
   */
  public void setLinearInterpolation(final boolean linearInterpolation) {
    this.linearInterpolation.set(linearInterpolation);
  }

  /**
   * @param modeToOutput The modeToOutput to set.
   */
  public void setModeToOutput(final int modeToOutput) {
    this.modeToOutput.set(modeToOutput);
  }

  public void setOffsetsInXandY(final ConstEtomoNumber[] pair) {
    if (pair == null) {
      offsetsInXandY.reset();
    }
    else {
      for (int i = 0; i < pair.length; i++) {
        offsetsInXandY.set(i, pair[i]);
      }
    }
  }

  /**
   * Set the output file before the manager is set up.
   * @param fileType
   */
  public void setOutputFile(final FileType fileType, final String rootName,
      final AxisType axisType) {
    outputFile.set(fileType.deriveFileName(rootName, axisType, manager, axisID));
    outputFileType = fileType;
  }

  /**
   * @param outputFile The outputFile to set.
   */
  public void setOutputFile(final FileType fileType) {
    outputFile.set(fileType.getFileName(manager, axisID));
    outputFileType = fileType;
  }

  public void setAdjustOrigin(final boolean input) {
    adjustOrigin.set(input);
  }

  public void setAntialiasFilter(final boolean input) {
    if (input) {
      antialiasFilter.set(DEFAULT_ANTIALIAS_FILTER);
    }
    else {
      antialiasFilter.reset();
    }
  }

  public void setAntialiasFilterValue(final ConstEtomoNumber input) {
    if (!input.isNull()) {
      antialiasFilter.set(input);
    }
  }

  public void resetSizeToOutputInXandY() throws FortranInputSyntaxException {
    sizeToOutputInXandY.reset();
    userSizeToOutputInXandY.reset();
    imageRotation.reset();
  }

  /**
   * calls setSizeToOutputInXandY.
   * @param userSizeX
   * @param userSizeY
   * @param binning
   * @param imageRotation
   * @param description
   * @return
   * @throws FortranInputSyntaxException
   * @throws etomo.util.InvalidParameterException
   * @throws IOException
   */
  public boolean setSizeToOutputInXandY(final String userSizeX, final String userSizeY,
      final int binning, final double imageRotation, final String description)
      throws FortranInputSyntaxException, etomo.util.InvalidParameterException,
      IOException {
    String userSize = "";
    // make sure an empty string really causes sizeToOutputInXandY to be empty.
    if (!userSizeX.matches("\\s*") || !userSizeY.matches("\\s*")) {
      userSize = userSizeX + FortranInputString.DEFAULT_DIVIDER + userSizeY;
    }
    return setSizeToOutputInXandY(userSize, binning, imageRotation, description);
  }

  /**
   * @param sizeToOutputInXandY The sizeToOutputInXandY to set.
   * if the user size is set, then this works even if the tilt axis angle is
   * closer to 90 degress.  If the user size is not set and the tilt axis angle
   * is closer to 90 degrees, then use x and y from the raw stack and transpose
   * then.  In either case, when sizeToOutputInXandY is set always apply
   * binning.
   * 
   * Will be called with an empty string by Positioning - whole tomogram.
   * 
   * Save the userSize so it can be stored as a state value.  It may be reused
   * to run newst_3dfind.com, which may have a different binning.  Also save
   * image rotation so that fiducialess parameters, which usually come from the
   * dialog, can be set for newst_3dfind.com.
   */
  public boolean setSizeToOutputInXandY(String userSize, final int binning,
      final double imageRotation, final String description)
      throws FortranInputSyntaxException, etomo.util.InvalidParameterException,
      IOException {
    // make sure an empty string really causes sizeToOutputInXandY to be empty.
    if (userSize.equals("")) {
      userSize = "/";
    }
    sizeToOutputInXandY.validateAndSet(userSize);
    userSizeToOutputInXandY.validateAndSet(userSize);
    this.imageRotation.set(imageRotation);
    // UserSize is empty, check for an angle close to 90 degrees.
    if ((sizeToOutputInXandY.isDefault() || sizeToOutputInXandY.isEmpty())
        && Utilities.is90DegreeImageRotation(imageRotation)) {
      MRCHeader header = MRCHeader.getInstance(manager, axisID,
          DatasetTool.STANDARD_DATASET_EXT);
      header.read(manager);
      // Set y from columns (x)
      sizeToOutputInXandY.set(1, header.getNColumns());
      // Set x from rows (y)
      sizeToOutputInXandY.set(0, header.getNRows());
    }
    if (binning != 1 && !sizeToOutputInXandY.isDefault()
        && !sizeToOutputInXandY.isEmpty()) {
      int iSize = sizeToOutputInXandY.getInt(0);
      sizeToOutputInXandY.set(0, (int) iSize / binning);
      iSize = sizeToOutputInXandY.getInt(1);
      sizeToOutputInXandY.set(1, (int) iSize / binning);
    }
    if (validate && description != null) {
      if (!userSizeToOutputInXandY.isNull(0) && userSizeToOutputInXandY.isNull(1)) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Two values are required for "
            + description + ".", "Entry Error", axisID);
        return false;
      }
    }
    return true;
  }

  /**
   * @param transformFile The transformFile to set.
   */
  public void setTransformFile(final String transformFile) {
    this.transformFile.set(transformFile);
  }

  public void setProcessName(final ProcessName input) {
    processName = input;
  }

  public void setCommandMode(final Mode input) {
    mode = input;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public boolean fillValueEquals(final int value) {
    return fillValue.equals(value);
  }

  /**
   * @return Returns the binByFactor.
   */
  public int getBinByFactor() {
    return binByFactor.getInt();
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  /**
   * @return Returns the floatDensities.
   */
  public int getFloatDensities() {
    return floatDensities.getInt();
  }

  /**
   * Backward compatibility with pre PIP structure, just return the first input
   * file
   * @return Returns the inputFile.
   */
  public String getInputFile() {
    return inputFile.get(0);
  }

  public boolean isAntialiasFilterNull() {
    return antialiasFilter.isNull();
  }

  /**
   * @return Returns the linearInterpolation.
   */
  public boolean isLinearInterpolation() {
    return linearInterpolation.is();
  }

  /**
   * @return Returns the modeToOutput.
   */
  public int getModeToOutput() {
    return modeToOutput.getInt();
  }

  public String getAntialiasFilter() {
    return antialiasFilter.toString();
  }

  /**
   * Returns the X value of offsetsInXandY
   * @return
   */
  public String getOffsetInX() {
    return offsetsInXandY.toString(0);
  }

  /**
   * Returns the Y value of offsetsInXandY
   * @return
   */
  public String getOffsetInY() {
    return offsetsInXandY.toString(1);
  }

  /**
   * @return Returns the outputFile.
   */
  public String getOutputFile() {
    if (outputFile.isEmpty()) {
      return "";
    }
    return outputFile.toString();
  }

  public FileType getOutputImageFileType() {
    if (outputFileType != null) {
      return outputFileType;
    }
    return FileType.getInstance(manager, axisID, true, true, outputFile.toString());
  }

  public FileType getOutputImageFileType2() {
    if (mode == Mode.WHOLE_TOMOGRAM_SAMPLE) {
      // Handle tiltParam here so the user doesn't have to wait.
      AxisType axisType = manager.getBaseMetaData().getAxisType();
      return FileType.TILT_OUTPUT;
    }
    return null;
  }

  public int getSizeToOutputInX() {
    return sizeToOutputInXandY.getInt(0);
  }

  public int getSizeToOutputInY() {
    return sizeToOutputInXandY.getInt(1);
  }

  public boolean isSizeToOutputInXandYSet() {
    return !sizeToOutputInXandY.isNull();
  }

  public String getCommand() {
    return getCommandFileName(axisID);
  }

  public String getCommandFileName(final AxisID axisID) {
    return processName.toString() + axisID.getExtension() + COMMAND_FILE_EXTENSION;
  }

  public String getCommandLine() {
    return getCommandFileName(axisID);
  }

  public String getCommandName() {
    if (useColorNewst) {
      return "colornewst";
    }
    return "newstack";
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public String getName() {
    return processName.toString();
  }

  public ProcessName getProcessName() {
    return processName;
  }

  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }

  public CommandMode getCommandMode() {
    return mode;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public int getIntValue(final FieldInterface field) {
    if (field == Field.BINNING) {
      return getBinByFactor();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(final FieldInterface field) {
    if (field == Field.FIDUCIALESS_ALIGNMENT) {
      return fiducialessAlignment;
    }
    if (field == Field.USE_LINEAR_INTERPOLATION) {
      return linearInterpolation.is();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(final FieldInterface field) {
    if (field == Field.USER_SIZE_TO_OUTPUT_IN_X_AND_Y) {
      return userSizeToOutputInXandY.toString(true);
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(final FieldInterface field) {
    if (field == Field.IMAGE_ROTATION) {
      return imageRotation;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Field implements FieldInterface {
    private Field() {
    }

    public static final Field FIDUCIALESS_ALIGNMENT = new Field();
    public static final Field BINNING = new Field();
    public static final Field USE_LINEAR_INTERPOLATION = new Field();
    public static final Field USER_SIZE_TO_OUTPUT_IN_X_AND_Y = new Field();
    public static final Field IMAGE_ROTATION = new Field();
  }

  public static final class Mode implements CommandMode {
    public static final Mode WHOLE_TOMOGRAM_SAMPLE = new Mode("WholeTomogramSample");
    public static final Mode FULL_ALIGNED_STACK = new Mode("FullAlignedStack");

    private final String string;

    private Mode(final String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
