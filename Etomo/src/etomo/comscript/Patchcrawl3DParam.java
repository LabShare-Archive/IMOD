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
 * <p> $Log$ </p>
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
  public void initialize(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException {
    // FIXME this needs to throw some exceptions
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();
    if (cmdLineArgs.length < 16 || cmdLineArgs.length > 19) {
      throw (new BadComScriptException("Incorrect number of arguments"));
    }

    int i = 0;
    xPatchSize = Integer.parseInt(cmdLineArgs[i++]);
    yPatchSize = Integer.parseInt(cmdLineArgs[i++]);
    zPatchSize = Integer.parseInt(cmdLineArgs[i++]);
    nX = Integer.parseInt(cmdLineArgs[i++]);
    nY = Integer.parseInt(cmdLineArgs[i++]);
    nZ = Integer.parseInt(cmdLineArgs[i++]);
    xLow = Integer.parseInt(cmdLineArgs[i++]);
    xHigh = Integer.parseInt(cmdLineArgs[i++]);
    yLow = Integer.parseInt(cmdLineArgs[i++]);
    yHigh = Integer.parseInt(cmdLineArgs[i++]);
    zLow = Integer.parseInt(cmdLineArgs[i++]);
    zHigh = Integer.parseInt(cmdLineArgs[i++]);
    maxShift = Integer.parseInt(cmdLineArgs[i++]);
    fileA = cmdLineArgs[i++];
    fileB = cmdLineArgs[i++];
    outputFile = cmdLineArgs[i++];

    if (cmdLineArgs.length > 16) {
      transformFile = cmdLineArgs[i++];
    }
    if (cmdLineArgs.length > 17) {
      originalFileB = cmdLineArgs[i++];
    }
    if (cmdLineArgs.length > 18) {
      boundaryModel = cmdLineArgs[i++];
    }
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript(etomo.comscript.ComScriptCommand)
   */
  public void updateComScript(ComScriptCommand scriptCommand) {
    // TODO Complete parameter => command line mapping
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    String badParameter = "";
    badParameter = "X patch size";
    cmdLineArgs[0] = String.valueOf(xPatchSize);
    badParameter = "Y patch size";
    cmdLineArgs[1] = String.valueOf(yPatchSize);
    badParameter = "Z patch size";
    cmdLineArgs[2] = String.valueOf(zPatchSize);
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
