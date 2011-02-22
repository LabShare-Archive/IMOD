package etomo.comscript;

import java.util.Vector;

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
 * <p> Revision 1.1  2006/08/25 22:51:29  sueh
 * <p> bug# 918 Save the pre-pip version of patchcrawl3d
 * <p>
 * <p> Revision 3.4  2004/04/12 16:50:22  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.3  2004/03/11 18:14:32  sueh
 * <p> bug# 386 changing updateComScriptCommand()
 * <p>
 * <p> Revision 3.2  2004/03/05 18:17:44  sueh
 * <p> bug# 250 changed updateComScriptCommand() - allow cmdLineArgs to grow
 * <p>
 * <p> Revision 3.1  2004/03/02 21:52:19  sueh
 * <p> bug# 250 changed parseComScriptCommand() - correcting parameters
 * <p> changed updateComScriptCommand() - adding parameters
 * <p> added borders, move reset() to const
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/07/25 22:54:14  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.5  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.4  2003/04/29 20:13:40  rickg
 * <p> Corrected range for number of patchcrawl args
 * <p>
 * <p> Revision 2.3  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */
/**
 * @author rickg
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
public class Patchcrawl3DPrePIPParam extends ConstPatchcrawl3DPrePIPParam implements
    CommandParam {
  public static final String rcsid = "$Id:";

  public static final String COMMAND = "patchcrawl3d";

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {

    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();

    if (cmdLineArgs.length < 16 || cmdLineArgs.length > 20) {
      String message = "Incorrect number of arguments, expected 16 - 20 found: "
          + String.valueOf(cmdLineArgs.length);
      throw (new BadComScriptException(message));
    }

    int i = 0;
    String parameterID = "";
    try {
      parameterID = "xsize";
      xPatchSize = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "ysize";
      yPatchSize = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "zsize";
      zPatchSize = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "nx";
      nX = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "ny";
      nY = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "nz";
      nZ = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "xlo";
      xLow = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "xhi";
      xHigh = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "ylo";
      yLow = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "yhi";
      yHigh = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "zlo";
      zLow = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "zhi";
      zHigh = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "matchshift";
      maxShift = Integer.parseInt(cmdLineArgs[i++]);
      parameterID = "filea";
      fileA = cmdLineArgs[i++];
      parameterID = "fileb";
      fileB = cmdLineArgs[i++];
      parameterID = "output_file";
      outputFile = cmdLineArgs[i++];
      int requiredLength = i;
      if (cmdLineArgs.length == requiredLength + 1) {
        parameterID = "boundary_model";
        boundaryModel = cmdLineArgs[i++];
      }
      else if (cmdLineArgs.length == requiredLength + 3) {
        parameterID = "transform_file";
        transformFile = cmdLineArgs[i++];
        parameterID = "original_fileb";
        originalFileB = cmdLineArgs[i++];
        parameterID = "borders";
        borders.validateAndSet(cmdLineArgs[i++]);
      }
      else if (cmdLineArgs.length == requiredLength + 4) {
        parameterID = "transform_file";
        transformFile = cmdLineArgs[i++];
        parameterID = "original_fileb";
        originalFileB = cmdLineArgs[i++];
        parameterID = "borders";
        borders.validateAndSet(cmdLineArgs[i++]);
        parameterID = "boundary_model";
        boundaryModel = cmdLineArgs[i++];
      }
    }
    catch (NumberFormatException except) {
      String message = "NumberFormatException Argument #: " + String.valueOf(i)
          + " value :" + cmdLineArgs[i] + " for parameter: " + parameterID;
      throw new BadComScriptException(message);
    }

  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript(etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand) {
    Vector cmdLineArgs = new Vector(scriptCommand.getCommandLineLength());
    String badParameter = "";
    badParameter = "xsize";
    cmdLineArgs.add(String.valueOf(xPatchSize));
    badParameter = "ysize";
    cmdLineArgs.add(String.valueOf(yPatchSize));
    badParameter = "zsize";
    cmdLineArgs.add(String.valueOf(zPatchSize));
    badParameter = "nx";
    cmdLineArgs.add(String.valueOf(nX));
    badParameter = "ny";
    cmdLineArgs.add(String.valueOf(nY));
    badParameter = "nz";
    cmdLineArgs.add(String.valueOf(nZ));
    badParameter = "xlo";
    cmdLineArgs.add(String.valueOf(xLow));
    badParameter = "xhi";
    cmdLineArgs.add(String.valueOf(xHigh));
    badParameter = "ylo";
    cmdLineArgs.add(String.valueOf(yLow));
    badParameter = "yhi";
    cmdLineArgs.add(String.valueOf(yHigh));
    badParameter = "Zlo";
    cmdLineArgs.add(String.valueOf(zLow));
    badParameter = "Zhi";
    cmdLineArgs.add(String.valueOf(zHigh));
    badParameter = "max_shift";
    cmdLineArgs.add(String.valueOf(maxShift));
    badParameter = "filea";
    cmdLineArgs.add(fileA);
    badParameter = "fileb";
    cmdLineArgs.add(fileB);
    badParameter = "output_file";
    cmdLineArgs.add(outputFile);
    if (!transformFile.equals("")) {
      badParameter = "transform_file";
      cmdLineArgs.add(transformFile);
      badParameter = "original_fileb";
      cmdLineArgs.add(originalFileB);
      badParameter = "borders";
      cmdLineArgs.add(borders.toString());
    }
    if (!boundaryModel.equals("")) {
      badParameter = "boundary_model";
      cmdLineArgs.add(boundaryModel);
    }
    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs
        .toArray(new String[cmdLineArgs.size()]));
  }

  public void initializeDefaults() {
  }

  /**
   * Sets the fileA.
   * @param fileA The fileA to set
   */
  public void setFileA(String fileA) {
    this.fileA = fileA;
  }

  /**
   * Sets the fileB.
   * @param fileB The fileB to set
   */
  public void setFileB(String fileB) {
    this.fileB = fileB;
  }

  /**
   * Sets the nX.
   * @param nX The nX to set
   */
  public void setNX(int nX) {
    this.nX = nX;
  }

  /**
   * Sets the nY.
   * @param nY The nY to set
   */
  public void setNY(int nY) {
    this.nY = nY;
  }

  /**
   * Sets the nZ.
   * @param nZ The nZ to set
   */
  public void setNZ(int nZ) {
    this.nZ = nZ;
  }

  /**
   * Sets the originalFileB.
   * @param originalFileB The originalFileB to set
   */
  public void setOriginalFileB(String originalFileB) {
    this.originalFileB = originalFileB;
  }

  /**
   * Sets the transformFile.
   * @param transformFile The transformFile to set
   */
  public void setTransformFile(String transformFile) {
    this.transformFile = transformFile;
  }

  public void setBorders(String borders) throws FortranInputSyntaxException {
    this.borders.validateAndSet(borders);
  }

  /**
   * Sets the xHigh.
   * @param xHigh The xHigh to set
   */
  public void setXHigh(int xHigh) {
    this.xHigh = xHigh;
  }

  /**
   * Sets the xLow.
   * @param xLow The xLow to set
   */
  public void setXLow(int xLow) {
    this.xLow = xLow;
  }

  /**
   * Sets the xPatchSize.
   * @param xPatchSize The xPatchSize to set
   */
  public void setXPatchSize(int xPatchSize) {
    this.xPatchSize = xPatchSize;
  }

  /**
   * Sets the yHigh.
   * @param yHigh The yHigh to set
   */
  public void setYHigh(int yHigh) {
    this.yHigh = yHigh;
  }

  /**
   * Sets the yLow.
   * @param yLow The yLow to set
   */
  public void setYLow(int yLow) {
    this.yLow = yLow;
  }

  /**
   * Sets the yPatchSize.
   * @param yPatchSize The yPatchSize to set
   */
  public void setYPatchSize(int yPatchSize) {
    this.yPatchSize = yPatchSize;
  }

  /**
   * Sets the zHigh.
   * @param zHigh The zHigh to set
   */
  public void setZHigh(int zHigh) {
    this.zHigh = zHigh;
  }

  /**
   * Sets the zLow.
   * @param zLow The zLow to set
   */
  public void setZLow(int zLow) {
    this.zLow = zLow;
  }

  /**
   * Sets the zPatchSize.
   * @param zPatchSize The zPatchSize to set
   */
  public void setZPatchSize(int zPatchSize) {
    this.zPatchSize = zPatchSize;
  }

  /**
   * Sets the maxShift.
   * @param maxShift The maxShift to set
   */
  public void setMaxShift(int maxShift) {
    this.maxShift = maxShift;
  }

  /**
   * Sets the boundaryModel.
   * @param boundaryModel The boundaryModel to set
   */
  public void setBoundaryModel(String boundaryModel) {
    this.boundaryModel = boundaryModel;
  }

  public void setUseBoundaryModel(boolean useBoundaryModel) {
    if (useBoundaryModel) {
      boundaryModel = ConstMatchorwarpParam.getDefaultPatchRegionModel();
    }
    else {
      boundaryModel = "";
    }
  }

  /**
   * Sets the outputFile.
   * @param outputFile The outputFile to set
   */
  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

}
