package etomo.comscript;

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
public class Patchcrawl3DParam
  extends ConstPatchcrawl3DParam
  implements CommandParam {

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException {

    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();

    if (cmdLineArgs.length < 16 || cmdLineArgs.length > 20) {
      String message =
        "Incorrect number of arguments, expected 16 - 20 found: "
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
      if (cmdLineArgs.length > 16) {
        parameterID = "transform_file";
        transformFile = cmdLineArgs[i++];
      }
      if (cmdLineArgs.length > 17) {
        parameterID = "original_fileb";
        originalFileB = cmdLineArgs[i++];
      }
      if (cmdLineArgs.length > 18) {
        parameterID = "boundary_model";
        boundaryModel = cmdLineArgs[i++];
      }
    }
    catch (NumberFormatException except) {
      String message =
        "NumberFormatException Argument #: "
          + String.valueOf(i)
          + " value :"
          + cmdLineArgs[i]
          + " for parameter: "
          + parameterID;
      throw new BadComScriptException(message);
    }

  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript(etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand) {
    // TODO Complete parameter => command line mapping
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    String badParameter = "";
    badParameter = "xsize";
    cmdLineArgs[0] = String.valueOf(xPatchSize);
    badParameter = "ysize";
    cmdLineArgs[1] = String.valueOf(yPatchSize);
    badParameter = "zsize";
    cmdLineArgs[2] = String.valueOf(zPatchSize);
    badParameter = "nx";
    cmdLineArgs[3] = String.valueOf(nX);
    badParameter = "ny";
    cmdLineArgs[4] = String.valueOf(nY);
    badParameter = "nz";
    cmdLineArgs[5] = String.valueOf(nZ);
    badParameter = "xlo";
    cmdLineArgs[6] = String.valueOf(xLow);
    badParameter = "xhi";
    cmdLineArgs[7] = String.valueOf(xHigh);
    badParameter = "ylo";
    cmdLineArgs[8] = String.valueOf(yLow);
    badParameter = "yhi";
    cmdLineArgs[9] = String.valueOf(yHigh);
    badParameter = "Zlo";
    cmdLineArgs[10] = String.valueOf(zLow);
    badParameter = "Zhi";
    cmdLineArgs[11] = String.valueOf(zHigh);

    scriptCommand.setCommandLineArgs(cmdLineArgs);
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

  /**
   * Sets the outputFile.
   * @param outputFile The outputFile to set
   */
  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  private void reset() {
    xPatchSize = 0;
    yPatchSize = 0;
    zPatchSize = 0;
    nX = 0;
    nY = 0;
    nZ = 0;
    xLow = 0;
    xHigh = 0;
    yLow = 0;
    yHigh = 0;
    zLow = 0;
    zHigh = 0;
    maxShift = 0;
    fileA = "";
    fileB = "";
    outputFile = "";
    transformFile = "";
    originalFileB = "";
    boundaryModel = "";
  }

}
