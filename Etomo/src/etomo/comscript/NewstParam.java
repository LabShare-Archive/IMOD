/*
 * <p>Description: Newstack command model</p>
 *
 * <p>Copyright: Copyright (c) 2002,2003,2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
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

package etomo.comscript;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;

import etomo.type.AxisID;

public class NewstParam extends ConstNewstParam implements CommandParam {
  public static final String rcsid = "$Id$";

  public NewstParam(AxisID axisID) {
    super(axisID);
  }
  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
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
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
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
      cmdLineArgs.add("-mode");
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
      cmdLineArgs.add("-float");
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

  public void setOffset(String newOffset) throws FortranInputSyntaxException {
    offsetsInXandY.clear();
    offsetsInXandY.add(newOffset);
  }

  private void reset() {
    initializeEmpty();
  }

  /**
   * @param applyOffsetsFirst The applyOffsetsFirst to set.
   */
  public void setApplyOffsetsFirst(boolean applyOffsetsFirst) {
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
  public void setContrastBlackWhite(String contrast)
      throws FortranInputSyntaxException {
    contrastBlackWhite.validateAndSet(contrast);
  }

  /**
   * @param distortionField The distortionField to set.
   */
  public void setDistortionField(String distortionField) {
    this.distortionField = distortionField;
  }

  /**
   * @param expandByFactor The expandByFactor to set.
   */
  public void setExpandByFactor(float expandByFactor) {
    this.expandByFactor = expandByFactor;
  }

  /**
   * @param fileOfInputs The fileOfInputs to set.
   */
  public void setFileOfInputs(String fileOfInputs) {
    this.fileOfInputs = fileOfInputs;
  }

  /**
   * @param fileOfOutputs The fileOfOutputs to set.
   */
  public void setFileOfOutputs(String fileOfOutputs) {
    this.fileOfOutputs = fileOfOutputs;
  }

  /**
   * @param floatDensities The floatDensities to set.
   */
  public void setFloatDensities(int floatDensities) {
    this.floatDensities = floatDensities;
  }
  
  public void setFiducialessAlignment(boolean fiducialessAlignment) {
    this.fiducialessAlignment = fiducialessAlignment;
  }

  /**
   * @param imagesAreBinned The imagesAreBinned to set.
   */
  public void setImagesAreBinned(int imagesAreBinned) {
    this.imagesAreBinned = imagesAreBinned;
  }

  /**
   * @param inputFile The inputFile to set.
   */
  public void setInputFile(Vector files) {
    // Defensively copy argument, since the objects are strings we only need
    // copy the collection of references
    inputFile.clear();
    inputFile.addAll(files);
  }

  /**
   * @param linearInterpolation The linearInterpolation to set.
   */
  public void setLinearInterpolation(boolean linearInterpolation) {
    this.linearInterpolation = linearInterpolation;
  }

  /**
   * @param modeToOutput The modeToOutput to set.
   */
  public void setModeToOutput(int modeToOutput) {
    this.modeToOutput = modeToOutput;
  }

  /**
   * @param numberToOutput The numberToOutput to set.
   */
  public void setNumberToOutput(Vector numberToOutput) {
    this.numberToOutput = numberToOutput;
  }

  /**
   * @param offsetsInXandY The offsetsInXandY to set.
   */
  public void setOffsetsInXandY(Vector offsets) {
    // Defensively copy argument, since the objects are primatives we only need
    // copy the collection of references
    offsetsInXandY.clear();
    offsetsInXandY.addAll(offsets);
  }

  /**
   * @param outputFile The outputFile to set.
   */
  public void setOutputFile(Vector files) {
    // Defensively copy argument, since the objects are strings we only need
    // copy the collection of references
    outputFile.clear();
    outputFile.addAll(files);
  }

  /**
   * @param parameterFile The parameterFile to set.
   */
  public void setParameterFile(String parameterFile) {
    this.parameterFile = parameterFile;
  }

  /**
   * @param rotateByAngle The rotateByAngle to set.
   */
  public void setRotateByAngle(float rotateByAngle) {
    this.rotateByAngle = rotateByAngle;
  }

  /**
   * @param scaleMinAndMax The scaleMinAndMax to set.
   */
  public void setScaleMinAndMax(FortranInputString scaleMinAndMax) {
    this.scaleMinAndMax = scaleMinAndMax;
  }

  /**
   * @param sectionsToRead The sectionsToRead to set.
   */
  public void setSectionsToRead(Vector sections) {
    // Defensively copy argument, since the objects are primatives we only need
    // copy the collection of references
    sectionsToRead.clear();
    sectionsToRead.addAll(sections);
  }

  /**
   * @param sizeToOutputInXandY The sizeToOutputInXandY to set.
   */
  public void setSizeToOutputInXandY(String size)
      throws FortranInputSyntaxException {
    sizeToOutputInXandY.validateAndSet(size);
  }

  /**
   * @param testLimits The testLimits to set.
   */
  public void setTestLimits(String limits) throws FortranInputSyntaxException {
    testLimits.validateAndSet(limits);
  }

  /**
   * @param transformFile The transformFile to set.
   */
  public void setTransformFile(String transformFile) {
    this.transformFile = transformFile;
  }

  /**
   * @param useTransformLines The useTransformLines to set.
   */
  public void setUseTransformLines(String useTransformLines) {
    this.useTransformLines = useTransformLines;
  }
  
  public void setCommandMode(int commandMode) {
    this.commandMode = commandMode;
  }
}
