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
* <p> $Log$ </p>
*/
public abstract class ConstSqueezevolParam implements Storable {
  public static  final String  rcsid =  "$Id$";
  
  protected static final String groupString = "Squeezevol";
  protected static final String linearInterpolationString = "LinearInterpolation";
  protected static final boolean defaultLinearInterpolation = false;
  private static final int commandSize = 3;
  
  protected EtomoNumber reductionFactorXY = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "ReductionFactorXY");
  protected EtomoNumber reductionFactorZ = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "ReductionFactorZ");
  protected boolean linearInterpolation;
  private String[] commandArray = null;

  public ConstSqueezevolParam() {
    reset();
    reductionFactorXY.setRecommendedValue(1.25);
    reductionFactorZ.setRecommendedValue(1.25);
  }
  
  protected void reset() {
    reductionFactorXY.reset();
    reductionFactorZ.reset();
    linearInterpolation = defaultLinearInterpolation;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-x");
    options.add(reductionFactorXY.toString());
    options.add("-y");
    options.add(reductionFactorXY.toString());
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
    inputFileName = TrimvolParam.getOutputFile(datasetName);
    if (!(new File(manager.getPropertyUserDir(), inputFileName).exists())) {
      String[] message = { inputFileName + " does not exist.",
          "Trim volume before running squeeze volume." };
      manager.getMainPanel().openMessageDialog(message,
          "Error Running Squeezevol");
    }
    options.add(inputFileName);
    //output is dataset.sqz
    options.add(datasetName + ".sqz");
    return options;
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    
    reductionFactorXY.store(props, prepend);
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
    commandArray[2] = BaseManager.getIMODBinPath() + "squeezevol";          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
  }
  
  public String getCommandLine() {
    if (commandArray == null) {
      createCommand();
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }
  
  public ConstEtomoNumber getReductionFactorXY() {
    return reductionFactorXY;
  }
  
  public ConstEtomoNumber getReductionFactorZ() {
    return reductionFactorZ;
  }
  
  public boolean isLinearInterpolation() {
    return linearInterpolation;
  }
  
  public boolean equals(ConstSqueezevolParam that) {
    if (!reductionFactorXY.equals(that.reductionFactorXY)) {
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
