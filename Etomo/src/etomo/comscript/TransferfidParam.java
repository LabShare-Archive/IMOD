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
 * <p> </p>
 */
public class TransferfidParam {
public static final String rcsid =
    "$Id$";
  String inputImageFile = "";
  String outputImageFile = "";
  String inputModelFile = "";
  String outputModelFile = "";
  boolean runMidas = false;
  
  public TransferfidParam() {
  }


  /**
   * Get the command string specified by the current state
   */
  public String getCommandString() {
    StringBuffer commandLine = new StringBuffer("transferfid ");

    if( ! inputImageFile.equals("")) {
      commandLine.append("-ia " + inputImageFile);
    }

    if( ! outputImageFile.equals("")) {
      commandLine.append("-ib " + outputImageFile);
    }

    if( ! inputModelFile.equals("")) {
      commandLine.append("-f " + inputModelFile);
    }

    if( ! outputModelFile.equals("")) {
      commandLine.append("-o " + outputModelFile);
    }
    
    if(runMidas) {
      commandLine.append("-m ");
    }
    return commandLine.toString();
  }  

  /**
   * Returns the inputImageFile.
   * @return String
   */
  public String getInputImageFile() {
    return inputImageFile;
  }

  /**
   * Returns the inputModelFile.
   * @return String
   */
  public String getInputModelFile() {
    return inputModelFile;
  }

  /**
   * Returns the outputImageFile.
   * @return String
   */
  public String getOutputImageFile() {
    return outputImageFile;
  }

  /**
   * Returns the outputModelFile.
   * @return String
   */
  public String getOutputModelFile() {
    return outputModelFile;
  }

  /**
   * Returns the runMidas.
   * @return boolean
   */
  public boolean isRunMidas() {
    return runMidas;
  }

  /**
   * Sets the inputImageFile.
   * @param inputImageFile The inputImageFile to set
   */
  public void setInputImageFile(String inputImageFile) {
    this.inputImageFile = inputImageFile;
  }

  /**
   * Sets the inputModelFile.
   * @param inputModelFile The inputModelFile to set
   */
  public void setInputModelFile(String inputModelFile) {
    this.inputModelFile = inputModelFile;
  }

  /**
   * Sets the outputImageFile.
   * @param outputImageFile The outputImageFile to set
   */
  public void setOutputImageFile(String outputImageFile) {
    this.outputImageFile = outputImageFile;
  }

  /**
   * Sets the outputModelFile.
   * @param outputModelFile The outputModelFile to set
   */
  public void setOutputModelFile(String outputModelFile) {
    this.outputModelFile = outputModelFile;
  }

  /**
   * Sets the runMidas.
   * @param runMidas The runMidas to set
   */
  public void setRunMidas(boolean runMidas) {
    this.runMidas = runMidas;
  }

}
