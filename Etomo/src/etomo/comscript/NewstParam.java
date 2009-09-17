package etomo.comscript;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
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

  public static final String SIZE_TO_OUTPUT_IN_X_AND_Y = "SizeToOutputInXandY";
  //data mode
  public static final String DATA_MODE_OPTION = "-mo";
  public static final int DATA_MODE_DEFAULT = Integer.MIN_VALUE;
  public static final int DATA_MODE_BYTE = 0;

  //float densities
  public static final String FLOAT_DENSITIES_OPTION = "-fl";
  public static final int FLOAT_DENSITIES_DEFAULT = Integer.MIN_VALUE;
  public static final int FLOAT_DENSITIES_MEAN = 2;

  //defaults
  public static final int BIN_BY_FACTOR_DEFAULT = 1;

  private static final String COMMAND_FILE_EXTENSION = ".com";

  private final Vector inputFile = new Vector();
  private final Vector outputFile = new Vector();
  private final Vector sectionsToRead = new Vector();
  private final FortranInputString sizeToOutputInXandY = new FortranInputString(
      2);
  private final FortranInputString userSizeToOutputInXandY = new FortranInputString(
      2);
  private final Vector offsetsInXandY = new Vector();
  private final FortranInputString contrastBlackWhite = new FortranInputString(
      2);
  private final FortranInputString testLimits = new FortranInputString(2);
  private final EtomoNumber imageRotation = new EtomoNumber(
      EtomoNumber.Type.FLOAT);
  
  private  ProcessName processName = ProcessName.NEWST;
  /**
   * @version 3.10
   * Script is from an earlier version if false.
   */
  private final EtomoBoolean2 adjustOrigin = new EtomoBoolean2();

  private Vector numberToOutput;
  private FortranInputString scaleMinAndMax = new FortranInputString(2);
  private String fileOfInputs;
  private String fileOfOutputs;
  private int modeToOutput;
  private boolean applyOffsetsFirst;
  private String transformFile;
  private String useTransformLines;
  private float rotateByAngle;
  private float expandByFactor;
  private int binByFactor;
  private boolean linearInterpolation;
  private int floatDensities;
  private String distortionField;
  private int imagesAreBinned;
  private String parameterFile;
  private boolean fiducialessAlignment;
  private String magGradientFile;
  private Mode commandMode;
  private AxisID axisID;

  public NewstParam(final AxisID axisID) {
    this.axisID = axisID;
    boolean[] allIntegerType = new boolean[] { true, true };
    sizeToOutputInXandY.setIntegerType(allIntegerType);
    userSizeToOutputInXandY.setIntegerType(allIntegerType);
    contrastBlackWhite.setIntegerType(allIntegerType);
    testLimits.setIntegerType(allIntegerType);
    reset();
  }

  private void reset() {
    inputFile.clear();
    outputFile.clear();
    fileOfInputs = "";
    fileOfOutputs = "";
    sectionsToRead.clear();
    numberToOutput = new Vector();
    sizeToOutputInXandY.reset();
    userSizeToOutputInXandY.reset();
    modeToOutput = Integer.MIN_VALUE;
    offsetsInXandY.clear();
    applyOffsetsFirst = false;
    transformFile = "";
    useTransformLines = "";
    rotateByAngle = Float.NaN;
    expandByFactor = Float.NaN;
    binByFactor = Integer.MIN_VALUE;
    linearInterpolation = false;
    floatDensities = Integer.MIN_VALUE;
    contrastBlackWhite.reset();
    scaleMinAndMax = new FortranInputString(2);
    distortionField = "";
    imagesAreBinned = Integer.MIN_VALUE;
    testLimits.reset();
    parameterFile = "";
    fiducialessAlignment = false;
    magGradientFile = null;
    adjustOrigin.reset();
    imageRotation.reset();
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws FortranInputSyntaxException, InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();
    for (int i = 0; i < cmdLineArgs.length; i++) {
      //  Is it an argument or filename
      if (cmdLineArgs[i].startsWith("-")) {
        if (cmdLineArgs[i].toLowerCase().startsWith("-in")) {
          i++;
          inputFile.add(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-ou")) {
          i++;
          outputFile.add(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].startsWith("-filei")
            || cmdLineArgs[i].startsWith("-FileOfI")) {
          i++;
          fileOfInputs = cmdLineArgs[i];
        }
        else if (cmdLineArgs[i].startsWith("-fileo")
            || cmdLineArgs[i].startsWith("-FileOfO")) {
          i++;
          fileOfOutputs = cmdLineArgs[i];
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-se")) {
          i++;
          sectionsToRead.add(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-nu")) {
          i++;
          numberToOutput.add(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-si")) {
          i++;
          sizeToOutputInXandY.validateAndSet(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-mo")) {
          i++;
          modeToOutput = Integer.parseInt(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-of")) {
          i++;
          offsetsInXandY.add(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-a")) {
          applyOffsetsFirst = true;
        }
        else if (cmdLineArgs[i].startsWith("-x")
            || cmdLineArgs[i].startsWith("-Tr")) {
          i++;
          transformFile = cmdLineArgs[i];
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-use")) {
          i++;
          useTransformLines = cmdLineArgs[i];
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-r")) {
          i++;
          rotateByAngle = Float.parseFloat(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-e")) {
          i++;
          expandByFactor = Float.parseFloat(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-b")) {
          i++;
          binByFactor = Integer.parseInt(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-l")) {
          linearInterpolation = true;
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-fl")) {
          i++;
          floatDensities = Integer.parseInt(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-c")) {
          i++;
          contrastBlackWhite.validateAndSet(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-sc")) {
          i++;
          scaleMinAndMax.validateAndSet(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-d")) {
          i++;
          distortionField = cmdLineArgs[i];
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-im")) {
          i++;
          imagesAreBinned = Integer.parseInt(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-te")) {
          i++;
          testLimits.validateAndSet(cmdLineArgs[i]);
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-grad")) {
          i++;
          magGradientFile = cmdLineArgs[i];
        }
        else if (cmdLineArgs[i].toLowerCase().startsWith("-or")) {
          adjustOrigin.set(true);
        }
        else {
          String message = "Unknown argument: " + cmdLineArgs[i];
          throw new InvalidParameterException(message);
        }
      }
      // input and output filename arguments
      else {
        if (i == (cmdLineArgs.length - 1)) {
          outputFile.add(cmdLineArgs[i]);
        }
        else {
          inputFile.add(cmdLineArgs[i]);
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
    // Create a new command line argument array

    ArrayList cmdLineArgs = new ArrayList(20);
    for (Iterator i = inputFile.iterator(); i.hasNext();) {
      cmdLineArgs.add("-input");
      cmdLineArgs.add((String) i.next());
    }
    for (Iterator i = outputFile.iterator(); i.hasNext();) {
      cmdLineArgs.add("-output");
      cmdLineArgs.add((String) i.next());
    }
    if (!fileOfInputs.equals("")) {
      cmdLineArgs.add("-fileinlist");
      cmdLineArgs.add(fileOfInputs);
    }
    if (!fileOfOutputs.equals("")) {
      cmdLineArgs.add("-fileoutlist");
      cmdLineArgs.add(fileOfOutputs);
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
    if (modeToOutput > Integer.MIN_VALUE) {
      cmdLineArgs.add(DATA_MODE_OPTION);
      cmdLineArgs.add(String.valueOf(modeToOutput));
    }
    for (Iterator i = offsetsInXandY.iterator(); i.hasNext();) {
      cmdLineArgs.add("-offset");
      cmdLineArgs.add((String) i.next());
    }
    if (applyOffsetsFirst) {
      cmdLineArgs.add("-applyfirst");
    }
    if (!transformFile.equals("")) {
      cmdLineArgs.add("-xform");
      cmdLineArgs.add(transformFile);
    }
    if (!useTransformLines.equals("")) {
      cmdLineArgs.add("-uselines");
      cmdLineArgs.add(useTransformLines);
    }
    if (!Float.isNaN(rotateByAngle)) {
      cmdLineArgs.add("-rotate");
      cmdLineArgs.add(String.valueOf(rotateByAngle));
    }
    if (!Float.isNaN(expandByFactor)) {
      cmdLineArgs.add("-expand");
      cmdLineArgs.add(String.valueOf(expandByFactor));
    }
    if (binByFactor > Integer.MIN_VALUE) {
      cmdLineArgs.add("-bin");
      cmdLineArgs.add(String.valueOf(binByFactor));
    }
    if (linearInterpolation) {
      cmdLineArgs.add("-linear");
    }
    if (floatDensities > Integer.MIN_VALUE) {
      cmdLineArgs.add(FLOAT_DENSITIES_OPTION);
      cmdLineArgs.add(String.valueOf(floatDensities));
    }
    if (contrastBlackWhite.valuesSet() && (!contrastBlackWhite.isDefault())) {
      cmdLineArgs.add("-contrast");
      cmdLineArgs.add(String.valueOf(contrastBlackWhite.toString()));
    }
    if (scaleMinAndMax.valuesSet() && (!scaleMinAndMax.isDefault())) {
      cmdLineArgs.add("-scale");
      cmdLineArgs.add(String.valueOf(scaleMinAndMax.toString()));
    }
    if (!distortionField.equals("")) {
      cmdLineArgs.add("-distort");
      cmdLineArgs.add(distortionField);
    }
    if (imagesAreBinned > Integer.MIN_VALUE) {
      cmdLineArgs.add("-imagebinned");
      cmdLineArgs.add(String.valueOf(imagesAreBinned));
    }
    if (testLimits.valuesSet() && (!testLimits.isDefault())) {
      cmdLineArgs.add("-test");
      cmdLineArgs.add(String.valueOf(testLimits.toString()));
    }
    if (!parameterFile.equals("")) {
      cmdLineArgs.add("-param");
      cmdLineArgs.add(parameterFile);
    }
    if (magGradientFile != null && !magGradientFile.matches("\\s*+")) {
      cmdLineArgs.add("-grad");
      cmdLineArgs.add(magGradientFile);
    }
    if (adjustOrigin.is()) {
      cmdLineArgs.add("-origin");
    }
    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs
        .toArray(new String[nArgs]));

    // If the command is currently newst change it to newstack
    if (scriptCommand.getCommand().equals("newst")) {
      scriptCommand.setCommand("newstack");
    }
  }

  public void initializeDefaults() {
  }

  public void setOffset(final String newOffset)
      throws FortranInputSyntaxException {
    offsetsInXandY.clear();
    offsetsInXandY.add(newOffset);
  }

  /**
   * @param applyOffsetsFirst The applyOffsetsFirst to set.
   */
  public void setApplyOffsetsFirst(final boolean applyOffsetsFirst) {
    this.applyOffsetsFirst = applyOffsetsFirst;
  }

  /**
   * @param binByFactor The binByFactor to set.
   */
  public void setBinByFactor(int binByFactor) {
    this.binByFactor = binByFactor;
  }

  /**
   * @param contrast The contrastBlackWhite to set.
   */
  public void setContrastBlackWhite(final String contrast)
      throws FortranInputSyntaxException {
    contrastBlackWhite.validateAndSet(contrast);
  }

  /**
   * @param distortionField The distortionField to set.
   */
  public void setDistortionField(final String distortionField) {
    this.distortionField = distortionField;
  }

  /**
   * @param expandByFactor The expandByFactor to set.
   */
  public void setExpandByFactor(final float expandByFactor) {
    this.expandByFactor = expandByFactor;
  }

  /**
   * @param fileOfInputs The fileOfInputs to set.
   */
  public void setFileOfInputs(final String fileOfInputs) {
    this.fileOfInputs = fileOfInputs;
  }

  /**
   * @param fileOfOutputs The fileOfOutputs to set.
   */
  public void setFileOfOutputs(final String fileOfOutputs) {
    this.fileOfOutputs = fileOfOutputs;
  }

  /**
   * @param floatDensities The floatDensities to set.
   */
  public void setFloatDensities(final int floatDensities) {
    this.floatDensities = floatDensities;
  }

  public void setFiducialessAlignment(final boolean fiducialessAlignment) {
    this.fiducialessAlignment = fiducialessAlignment;
  }

  /**
   * @param imagesAreBinned The imagesAreBinned to set.
   */
  public void setImagesAreBinned(final int imagesAreBinned) {
    this.imagesAreBinned = imagesAreBinned;
  }

  /**
   * @param inputFile The inputFile to set.
   */
  public void setInputFile(final Vector files) {
    // Defensively copy argument, since the objects are strings we only need
    // copy the collection of references
    inputFile.clear();
    inputFile.addAll(files);
  }

  /**
   * @param linearInterpolation The linearInterpolation to set.
   */
  public void setLinearInterpolation(final boolean linearInterpolation) {
    this.linearInterpolation = linearInterpolation;
  }

  /**
   * @param modeToOutput The modeToOutput to set.
   */
  public void setModeToOutput(final int modeToOutput) {
    this.modeToOutput = modeToOutput;
  }

  /**
   * @param numberToOutput The numberToOutput to set.
   */
  public void setNumberToOutput(final Vector numberToOutput) {
    this.numberToOutput = numberToOutput;
  }

  /**
   * @param offsetsInXandY The offsetsInXandY to set.
   */
  public void setOffsetsInXandY(final Vector offsets) {
    // Defensively copy argument, since the objects are primatives we only need
    // copy the collection of references
    offsetsInXandY.clear();
    offsetsInXandY.addAll(offsets);
  }

  /**
   * @param outputFile The outputFile to set.
   */
  public void setOutputFile(final Vector files) {
    // Defensively copy argument, since the objects are strings we only need
    // copy the collection of references
    outputFile.clear();
    outputFile.addAll(files);
  }

  /**
   * @param parameterFile The parameterFile to set.
   */
  public void setParameterFile(final String parameterFile) {
    this.parameterFile = parameterFile;
  }

  /**
   * @param rotateByAngle The rotateByAngle to set.
   */
  public void setRotateByAngle(final float rotateByAngle) {
    this.rotateByAngle = rotateByAngle;
  }

  /**
   * @param scaleMinAndMax The scaleMinAndMax to set.
   */
  public void setScaleMinAndMax(final FortranInputString scaleMinAndMax) {
    this.scaleMinAndMax = scaleMinAndMax;
  }

  /**
   * @param sectionsToRead The sectionsToRead to set.
   */
  public void setSectionsToRead(final Vector sections) {
    // Defensively copy argument, since the objects are primatives we only need
    // copy the collection of references
    sectionsToRead.clear();
    sectionsToRead.addAll(sections);
  }

  public void resetSizeToOutputInXandY() throws FortranInputSyntaxException {
    sizeToOutputInXandY.validateAndSet("/");
    userSizeToOutputInXandY.validateAndSet("/");
    imageRotation.reset();
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
  public void setSizeToOutputInXandY(String userSize, final int binning,
      final float imageRotation, final BaseManager manager)
      throws FortranInputSyntaxException,etomo.util.InvalidParameterException ,IOException{
    //make sure an empty string really causes sizeToOutputInXandY to be empty.
    if (userSize.equals("")) {
      userSize = "/";
    }
    sizeToOutputInXandY.validateAndSet(userSize);
    userSizeToOutputInXandY.validateAndSet(userSize);
    this.imageRotation.set(imageRotation);
    //UserSize is empty, check for an angle close to 90 degrees.
    if ((sizeToOutputInXandY.isDefault() || sizeToOutputInXandY.isEmpty())
        && Utilities.is90DegreeImageRotation(imageRotation)) {
      MRCHeader header = MRCHeader.getInstance(manager, axisID, ".st", manager
          .getManagerKey());
      header.read();
      //Set y from columns (x)
      sizeToOutputInXandY.set(1, header.getNColumns());
      //Set x from rows (y)
      sizeToOutputInXandY.set(0, header.getNRows());
    }
    if (binning != 1 && !sizeToOutputInXandY.isDefault()
        && !sizeToOutputInXandY.isEmpty()) {
      int iSize = sizeToOutputInXandY.getInt(0);
      sizeToOutputInXandY.set(0, (int) iSize / binning);
      iSize = sizeToOutputInXandY.getInt(1);
      sizeToOutputInXandY.set(1, (int) iSize / binning);
    }
  }

  /**
   * @param testLimits The testLimits to set.
   */
  public void setTestLimits(final String limits)
      throws FortranInputSyntaxException {
    testLimits.validateAndSet(limits);
  }

  /**
   * @param transformFile The transformFile to set.
   */
  public void setTransformFile(final String transformFile) {
    this.transformFile = transformFile;
  }

  /**
   * @param useTransformLines The useTransformLines to set.
   */
  public void setUseTransformLines(final String useTransformLines) {
    this.useTransformLines = useTransformLines;
  }
  
  public void setProcessName(final ProcessName input) {
    processName=input;
  }

  public void setCommandMode(final Mode commandMode) {
    this.commandMode = commandMode;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  /**
   * @return Returns the applyOffsetsFirst.
   */
  public boolean isApplyOffsetsFirst() {
    return applyOffsetsFirst;
  }

  /**
   * @return Returns the binByFactor.
   */
  public int getBinByFactor() {
    return ParamUtilities.get(binByFactor, BIN_BY_FACTOR_DEFAULT);
  }

  /**
   * @return Returns the contrastBlackWhite.
   */
  public String getContrastBlackWhite() {
    return contrastBlackWhite.toString();
  }

  /**
   * @return Returns the distortionField.
   */
  public String getDistortionField() {
    return distortionField;
  }

  /**
   * @return Returns the expandByFactor.
   */
  public float getExpandByFactor() {
    return expandByFactor;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }
  
  public ProcessName getSubcommandProcessName() {
    return null;
  }

  /**
   * @return Returns the fileOfInputs.
   */
  public String getFileOfInputs() {
    return fileOfInputs;
  }

  /**
   * @return Returns the fileOfOutputs.
   */
  public String getFileOfOutputs() {
    return fileOfOutputs;
  }

  /**
   * @return Returns the floatDensities.
   */
  public int getFloatDensities() {
    return floatDensities;
  }

  /**
   * @return Returns the imagesAreBinned.
   */
  public int getImagesAreBinned() {
    return imagesAreBinned;
  }

  /**
   * Backward compatibility with pre PIP structure, just return the first input
   * file
   * @return Returns the inputFile.
   */
  public String getInputFile() {
    return (String) inputFile.get(0);
  }

  /**
   * Create a defensive copy of the internal object inputFile
   * @return
   */
  public Vector getInputFiles() {
    Vector copy = new Vector(inputFile);
    return copy;
  }

  /**
   * @return Returns the linearInterpolation.
   */
  public boolean isLinearInterpolation() {
    return linearInterpolation;
  }

  /**
   * @return Returns the modeToOutput.
   */
  public int getModeToOutput() {
    return modeToOutput;
  }

  /**
   * @return Returns the numberToOutput.
   */
  public Vector getNumberToOutput() {
    return numberToOutput;
  }

  /**
   * @return Returns the offsetsInXandY.
   */
  public String getOffsetsInXandY() {
    StringBuffer buffer = new StringBuffer();
    for (Iterator i = offsetsInXandY.iterator(); i.hasNext();) {
      buffer.append((String) i.next());
      buffer.append(",");
    }
    // Remove the trailing comma
    if (buffer.length() > 0) {
      buffer.deleteCharAt(buffer.length() - 1);
    }
    return buffer.toString();
  }

  /**
   * Backward compatibility with pre PIP structure, just return the first ouput
   * file
   * @return Returns the inputFile.
   */
  public String getOutputFile() {
    if (outputFile.size() == 0) {
      return "";
    }
    return (String) outputFile.get(0);
  }

  /**
   * Create a defensive copy of the internal object inputFile
   * @return
   */
  public Vector getOutputFiles() {
    Vector copy = new Vector(outputFile);
    return copy;
  }

  /**
   * @return Returns the parameterFile.
   */
  public String getParameterFile() {
    return parameterFile;
  }

  /**
   * @return Returns the rotateByAngle.
   */
  public float getRotateByAngle() {
    return rotateByAngle;
  }

  /**
   * @return Returns the scaleMinAndMax.
   */
  public FortranInputString getScaleMinAndMax() {
    return scaleMinAndMax;
  }

  /**
   * @return Returns the sectionsToRead.
   */
  public Vector getSectionsToRead() {
    Vector copy = new Vector(sectionsToRead);
    return copy;
  }

  /**
   * @return Returns the sizeToOutputInXandY.
   */
  public String getSizeToOutputInXandY() {
    return sizeToOutputInXandY.toString();
  }

  public int getSizeToOutputInX() {
    return sizeToOutputInXandY.getInt(0);
  }

  public int getSizeToOutputInY() {
    return sizeToOutputInXandY.getInt(1);
  }

  /**
   * @return Returns the testLimits.
   */
  public String getTestLimits() {
    return testLimits.toString();
  }

  /**
   * @return Returns the transformFile.
   */
  public String getTransformFile() {
    return transformFile;
  }

  /**
   * @return Returns the useTransformLines.
   */
  public String getUseTransformLines() {
    return useTransformLines;
  }

  public boolean isSizeToOutputInXandYSet() {
    return sizeToOutputInXandY.valuesSet()
        && (!sizeToOutputInXandY.isDefault());
  }

  public String getCommand() {
    return getCommandFileName(axisID);
  }

  public  String getCommandFileName(final AxisID axisID) {
    return processName.toString() + axisID.getExtension() + COMMAND_FILE_EXTENSION;
  }

  public String getCommandLine() {
    return getCommandFileName(axisID);
  }

  public String getCommandName() {
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
    return commandMode;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public int getIntValue(final FieldInterface field) {
    if (field == Field.BINNING) {
      return getBinByFactor();
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public IteratorElementList getIteratorElementList(
      final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(final FieldInterface field) {
    if (field == Field.FIDUCIALESS_ALIGNMENT) {
      return fiducialessAlignment;
    }
    if (field == Field.USE_LINEAR_INTERPOLATION) {
      return linearInterpolation;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(final FieldInterface field) {
    if (field == Field.USER_SIZE_TO_OUTPUT_IN_X_AND_Y) {
      return userSizeToOutputInXandY.toString();
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

    private Mode(String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
