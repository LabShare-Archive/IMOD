package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.Storable;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.EtomoNumber;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.4  2004/12/16 02:12:28  sueh
* <p> bug# 564 Implemented Command.  Saved flipped status.
* <p>
* <p> Revision 1.3  2004/12/14 21:32:29  sueh
* <p> bug# 557 Made separate variables for x and y reduction factors to handle
* <p> an unflipped tomogram.
* <p>
* <p> Revision 1.2  2004/12/08 21:19:54  sueh
* <p> bug# 564 Changed TrimvolParam set and get, input and output File
* <p> functions to ...FileName to avoid confusion with the new getOutputFile()
* <p> function.
* <p>
* <p> Revision 1.1  2004/12/02 18:24:23  sueh
* <p> bug# 557 Manages squeezevol parameters.  Creates command line.
* <p> Stores parameters.
* <p> </p>
*/
public abstract class ConstSqueezevolParam implements Command, Storable {
  public static  final String  rcsid =  "$Id$";
  
  public static final int GET_FLIPPED = -1;
  protected static final String groupString = "Squeezevol";
  protected static final String linearInterpolationString = "LinearInterpolation";
  protected static final boolean defaultLinearInterpolation = false;
  private static final int commandSize = 3;
  private static final String commandName = "squeezevol";
  
  protected EtomoNumber reductionFactorX = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "ReductionFactorX");
  protected EtomoNumber reductionFactorY = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "ReductionFactorY");
  protected EtomoNumber reductionFactorZ = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "ReductionFactorZ");
  protected boolean linearInterpolation;
  protected boolean flipped = false;
  private String[] commandArray = null;
  private File outputFile;

  public ConstSqueezevolParam() {
    reductionFactorX.setResetValue(1.25);
    reductionFactorY.setResetValue(1.25);
    reductionFactorZ.setResetValue(1.25);
    reset();
  }
  
  protected void reset() {
    reductionFactorX.reset();
    reductionFactorY.reset();
    reductionFactorZ.reset();
    linearInterpolation = defaultLinearInterpolation;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-x");
    options.add(reductionFactorX.toString());
    options.add("-y");
    options.add(reductionFactorY.toString());
    options.add("-z");
    options.add(reductionFactorZ.toString());
    if (linearInterpolation) {
      options.add("-l");
    }
    //create input file name
    String inputFileName;
    ApplicationManager manager = (ApplicationManager) EtomoDirector
        .getInstance().getCurrentManager();
    ConstMetaData metaData = manager.getMetaData();
    String datasetName = metaData.getDatasetName();
    //try to take the trimvol output file as input
    inputFileName = TrimvolParam.getOutputFileName(datasetName);
   /* if (!(new File(manager.getPropertyUserDir(), inputFileName).exists())) {
      String[] message = { inputFileName + " does not exist.",
          "Trim volume before running squeeze volume." };
      manager.getMainPanel().openMessageDialog(message,
          "Error Running Squeezevol");
    }*/
    options.add(inputFileName);
    //output is dataset.sqz
    outputFile = new File(EtomoDirector.getInstance().getCurrentPropertyUserDir(), datasetName + ".sqz");
    options.add(outputFile.getName());
    return options;
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    
    reductionFactorX.store(props, prepend);
    reductionFactorY.store(props, prepend);
    reductionFactorZ.store(props, prepend);
    props.setProperty(group + linearInterpolationString, Boolean.toString(linearInterpolation));
  }
  
  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  private void createCommand() {
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + commandName;          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
  }
  
  /**
   * Get command array used to run command.  Not for running the command
   * @return
   */
  public String getCommandLine() {
    if (commandArray == null) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }
  
  /**
   * Get command array to run
   * @return
   */
  public String[] getCommandArray() {
    createCommand();
    return commandArray;
  }
  
  public boolean getBooleanValue(int name) {
    switch (name) {
    case GET_FLIPPED:
      return flipped;
    }
    return false;
  }
  
  public int getIntegerValue(int name) {
    return Integer.MIN_VALUE;
  }
  
  public String getCommandName() {
    return commandName;
  }
  
  public static String getName() {
    return commandName;
  }
  
  public int getCommandMode() {
    return 0;
  }
  
  public File getCommandOutputFile() {
    return outputFile;
  }
  
  public ConstEtomoNumber getReductionFactorX() {
    return reductionFactorX;
  }
  
  public ConstEtomoNumber getReductionFactorY() {
    return reductionFactorY;
  }
  
  public ConstEtomoNumber getReductionFactorZ() {
    return reductionFactorZ;
  }
  
  public boolean isLinearInterpolation() {
    return linearInterpolation;
  }
  
  public boolean isFlipped() {
    return flipped;
  }
  
  public boolean equals(ConstSqueezevolParam that) {
    if (!reductionFactorX.equals(that.reductionFactorX)) {
      return false;
    }
    if (!reductionFactorY.equals(that.reductionFactorY)) {
      return false;
    }
    if (!reductionFactorZ.equals(that.reductionFactorZ)) {
      return false;
    }
    if (linearInterpolation != that.linearInterpolation) {
      return false;
    }
    return true;
  }
}
