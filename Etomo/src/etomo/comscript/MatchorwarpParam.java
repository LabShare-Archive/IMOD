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
 * <p> $Log$ </p>
 */

public class MatchorwarpParam
  extends ConstMatchorwarpParam
  implements CommandParam {

  /* (non-Javadoc)
   * @see 
   * etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void initialize(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException {
    // FIXME this needs to throw some exceptions
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

      if (cmdLineArgs[i].startsWith("-tr")) {
        trial = true;
      }

    }

    inputFile = cmdLineArgs[cmdLineArgs.length - 2];
    outputFile = cmdLineArgs[cmdLineArgs.length - 1];
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript 
   * (etomo.comscript.ComScriptCommand)
   */
  public void updateComScript(ComScriptCommand scriptCommand)
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

    if (trial) {
      cmdLineArgs.add("-trial");
    }

    cmdLineArgs.add(inputFile);
    cmdLineArgs.add(outputFile);

    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs(
      (String[]) cmdLineArgs.toArray(new String[nArgs]));
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

  /**
   * Reset the state of the object to it initial defaults
   */
  private void reset() {
    size = "";
    useRefinelimit = false;
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
  }
}
