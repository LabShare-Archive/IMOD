package etomo.comscript;

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
public class ConstSetParam {
  public static  final String  rcsid =  "$Id$";
  public static final String COMMAND_NAME = "set";
  
  protected static final String delimiter = "=";
  
  protected String expectedName = null;
  protected String name = "";
  protected int etomoNumberType;
  protected EtomoNumber numericValue = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
  protected String value;
  protected boolean numeric = false;
  
  public ConstSetParam(String expectedName, int etomoNumberType) {
    this.etomoNumberType = etomoNumberType;
    numericValue = new EtomoNumber(etomoNumberType, expectedName);
    numericValue.setDefault(0);
    reset();
    numeric = true;
    
    this.expectedName = expectedName;
  }

  protected void reset() {
    name = "";
    value = "";
    numericValue.reset();
  }
  
  public String getValue(boolean displayDefault) {
    if (numeric) {
      return numericValue.toString(displayDefault);
    }
    return value;
  }

}
