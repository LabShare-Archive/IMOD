package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
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
public class SqueezevolParam {
  public static  final String  rcsid =  "$Id$";

  private static final int commandSize = 3;
  
  private String[] commandArray;
  
  private EtomoNumber reductionFactorXY = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
  private EtomoNumber reductionFactorZ = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
  private boolean linearInterpolation = false;
  private String inputFileName = "";

  public SqueezevolParam() {
    reductionFactorXY.setRecommendedValue(1.25);
    reductionFactorZ.setRecommendedValue(1.25);
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + "squeezevol";          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
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
    options.add(inputFileName);
    options.add(inputFileName.substring(0, inputFileName.lastIndexOf('.')) + ".sqz");
    return options;
  }

}
