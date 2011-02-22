package etomo.comscript;

import java.util.ArrayList;

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
 * <p> Revision 3.4  2010/04/28 15:59:50  sueh
 * <p> bug# 1344 Reformatted.
 * <p>
 * <p> Revision 3.3  2006/09/19 21:59:52  sueh
 * <p> bug# 928 Added residualFile, vectormodel, and clipsize.
 * <p>
 * <p> Revision 3.2  2004/04/12 16:49:27  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.1  2004/03/06 03:46:55  sueh
 * <p> bug# 380 added useLinearInterpolation
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/07/25 22:54:14  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.4  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.3  2003/03/20 17:23:23  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */

public class MatchorwarpParam extends ConstMatchorwarpParam implements CommandParam {
  /* (non-Javadoc)
   * @see 
   * etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    // TODO error checking - throw exceptions for bad syntax
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();

    for (int i = 0; i < cmdLineArgs.length - 2; i++) {
      if (cmdLineArgs[i].startsWith("-si")) {
        i++;
        size = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-refinel")) {
        i++;
        refineLimit = Double.parseDouble(cmdLineArgs[i]);
        useRefinelimit = true;
      }
      if (cmdLineArgs[i].startsWith(RESIDUAL_FILE_KEY)) {
        i++;
        residualFile = cmdLineArgs[i];
      }
      if (cmdLineArgs[i].startsWith(VECTOR_MODEL_KEY)) {
        i++;
        vectormodel = cmdLineArgs[i];
      }
      if (cmdLineArgs[i].startsWith(clipsize.getName())) {
        i++;
        clipsize.set(cmdLineArgs[i]);
      }
      if (cmdLineArgs[i].startsWith("-warpl")) {
        i++;
        warpLimit = cmdLineArgs[i];
      }
      if (cmdLineArgs[i].startsWith("-m")) {
        i++;
        modelFile = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-p")) {
        i++;
        patchFile = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-so")) {
        i++;
        solveFile = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-refinef")) {
        i++;
        refineFile = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-i")) {
        i++;
        inverseFile = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-warpf")) {
        i++;
        warpFile = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-te")) {
        i++;
        tempDir = cmdLineArgs[i];
      }

      if (cmdLineArgs[i].startsWith("-xl")) {
        i++;
        xLowerExclude = Integer.parseInt(cmdLineArgs[i]);
      }

      if (cmdLineArgs[i].startsWith("-xu")) {
        i++;
        xUpperExclude = Integer.parseInt(cmdLineArgs[i]);
      }

      if (cmdLineArgs[i].startsWith("-zl")) {
        i++;
        zLowerExclude = Integer.parseInt(cmdLineArgs[i]);
      }

      if (cmdLineArgs[i].startsWith("-zu")) {
        i++;
        zUpperExclude = Integer.parseInt(cmdLineArgs[i]);
      }

      if (cmdLineArgs[i].startsWith("-l")) {
        i++;
        useLinearInterpolation = true;
      }

      if (cmdLineArgs[i].startsWith("-tr")) {
        trial = true;
      }
    }

    inputFile = cmdLineArgs[cmdLineArgs.length - 2];
    outputFile = cmdLineArgs[cmdLineArgs.length - 1];

    //Backwards compatibility (before 3.8.25):
    //Update so that the user can look at the .resid file.
    if (residualFile == null && vectormodel == null && clipsize.isNull()) {
      residualFile = RESIDUAL_FILE_DEFAULT;
      vectormodel = VECTOR_MODEL_DEFAULT;
      clipsize.set(CLIP_SIZE_DEFAULT);
    }
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript 
   * (etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {

    // Create a new command line argument array
    ArrayList cmdLineArgs = new ArrayList(20);

    if (!size.equals("")) {
      cmdLineArgs.add("-size");
      cmdLineArgs.add(size);
    }

    if (useRefinelimit) {
      cmdLineArgs.add("-refinelimit");
      cmdLineArgs.add(String.valueOf(refineLimit));
    }

    if (residualFile != null) {
      cmdLineArgs.add(RESIDUAL_FILE_KEY);
      cmdLineArgs.add(residualFile);
    }

    if (vectormodel != null) {
      cmdLineArgs.add(VECTOR_MODEL_KEY);
      cmdLineArgs.add(vectormodel);
    }

    if (!clipsize.isNull()) {
      cmdLineArgs.add(clipsize.getName());
      cmdLineArgs.add(clipsize.toString());
    }

    if (!warpLimit.equals("")) {
      cmdLineArgs.add("-warplimit");
      cmdLineArgs.add(warpLimit);
    }

    if (!modelFile.equals("")) {
      cmdLineArgs.add("-modelfile");
      cmdLineArgs.add(modelFile);
    }

    if (!patchFile.equals("")) {
      cmdLineArgs.add("-patchfile");
      cmdLineArgs.add(patchFile);
    }

    if (!solveFile.equals("")) {
      cmdLineArgs.add("-solvefile");
      cmdLineArgs.add(solveFile);
    }

    if (!refineFile.equals("")) {
      cmdLineArgs.add("-refinefile");
      cmdLineArgs.add(refineFile);
    }
    if (!inverseFile.equals("")) {
      cmdLineArgs.add("-inversefile");
      cmdLineArgs.add(inverseFile);
    }

    if (!warpFile.equals("")) {
      cmdLineArgs.add("-warpfile");
      cmdLineArgs.add(warpFile);
    }

    if (!tempDir.equals("")) {
      cmdLineArgs.add("-tempdir");
      cmdLineArgs.add(tempDir);
    }

    if (xLowerExclude > 0) {
      cmdLineArgs.add("-xlowerexclude");
      cmdLineArgs.add(String.valueOf(xLowerExclude));
    }

    if (xUpperExclude > 0) {
      cmdLineArgs.add("-xupperexclude");
      cmdLineArgs.add(String.valueOf(xUpperExclude));
    }

    if (zLowerExclude > 0) {
      cmdLineArgs.add("-zlowerexclude");
      cmdLineArgs.add(String.valueOf(zLowerExclude));
    }

    if (zUpperExclude > 0) {
      cmdLineArgs.add("-zupperexclude");
      cmdLineArgs.add(String.valueOf(zUpperExclude));
    }

    if (useLinearInterpolation) {
      cmdLineArgs.add("-linear");
    }

    if (trial) {
      cmdLineArgs.add("-trial");
    }

    cmdLineArgs.add(inputFile);
    cmdLineArgs.add(outputFile);

    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs.toArray(new String[nArgs]));
  }

  public void initializeDefaults() {
  }

  /**
   * Sets the inverseFile.
   * @param inverseFile The inverseFile to set
   */
  public void setInverseFile(String inverseFile) {
    this.inverseFile = inverseFile;
  }

  /**
   * Sets the modelFile.
   * @param modelFile The modelFile to set
   */
  public void setModelFile(String modelFile) {
    this.modelFile = modelFile;
  }

  /**
   * Set the default patch region model file
   * @param patchFile
   */
  public void setDefaultModelFile() {
    modelFile = ConstMatchorwarpParam.getDefaultPatchRegionModel();
  }

  /**
   * Sets the patchFile.
   * @param patchFile The patchFile to set
   */
  public void setPatchFile(String patchFile) {
    this.patchFile = patchFile;
  }

  /**
   * Sets the refineLimit.
   * @param refineLimit The refineLimit to set
   */
  public void setRefineLimit(double refineLimit) {
    this.refineLimit = refineLimit;
  }

  /**
   * Sets the size.
   * @param size The size to set
   */
  public void setSize(String size) {
    this.size = size;
  }

  /**
   * Sets the solveFile.
   * @param solveFile The solveFile to set
   */
  public void setSolveFile(String solveFile) {
    this.solveFile = solveFile;
  }

  /**
   * Sets the tempDir.
   * @param tempDir The tempDir to set
   */
  public void setTempDir(String tempDir) {
    this.tempDir = tempDir;
  }

  /**
   * Sets the warpFile.
   * @param warpFile The warpFile to set
   */
  public void setWarpFile(String warpFile) {
    this.warpFile = warpFile;
  }

  /**
   * Sets the warpLimit.
   * @param warpLimit The warpLimit to set
   */
  public void setWarpLimit(String warpLimit) {
    this.warpLimit = warpLimit;
  }

  /**
   * Sets the xLowerExclude.
   * @param xLowerExclude The xLowerExclude to set
   */
  public void setXLowerExclude(int xLowerExclude) {
    this.xLowerExclude = xLowerExclude;
  }

  /**
   * Sets the xUpperExclude.
   * @param xUpperExclude The xUpperExclude to set
   */
  public void setXUpperExclude(int xUpperExclude) {
    this.xUpperExclude = xUpperExclude;
  }

  /**
   * Sets the zLowerExclude.
   * @param zLowerExclude The zLowerExclude to set
   */
  public void setZLowerExclude(int zLowerExclude) {
    this.zLowerExclude = zLowerExclude;
  }

  /**
   * Sets the zUpperExclude.
   * @param zUpperExclude The zUpperExclude to set
   */
  public void setZUpperExclude(int zUpperExclude) {
    this.zUpperExclude = zUpperExclude;
  }

  /**
   * Sets the inputFile.
   * @param inputFile The inputFile to set
   */
  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  /**
   * Sets the outputFile.
   * @param outputFile The outputFile to set
   */
  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  /**
   * Sets the trial flag.
   * @param trial Specify the trial state
   */
  public void setTrial(boolean trial) {
    this.trial = trial;
  }

  /**
   * Sets the useRefinelimit.
   * @param useRefinelimit The useRefinelimit to set
   */
  public void setUseRefinelimit(boolean useRefinelimit) {
    this.useRefinelimit = useRefinelimit;
  }

  /**
   * Sets the refineFile.
   * @param refineFile The refineFile to set
   */
  public void setRefineFile(String refineFile) {
    this.refineFile = refineFile;
  }

  public void setUseLinearInterpolation(boolean useLinearInterpolation) {
    this.useLinearInterpolation = useLinearInterpolation;
  }

  /**
   * Reset the state of the object to it initial defaults
   */
  private void reset() {
    size = "";
    useRefinelimit = false;
    residualFile = null;
    vectormodel = null;
    clipsize.reset();
    warpLimit = "";
    modelFile = "";
    patchFile = "";
    solveFile = "";
    refineFile = "";
    inverseFile = "";
    warpFile = "";
    tempDir = "";
    xLowerExclude = 0;
    xUpperExclude = 0;
    zLowerExclude = 0;
    zUpperExclude = 0;
    trial = false;
    inputFile = "";
    outputFile = "";
    useLinearInterpolation = false;
  }
}
