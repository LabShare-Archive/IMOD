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
* <p> $Log$
* <p> Revision 1.2  2004/12/03 20:21:03  sueh
* <p> bug# 556 Support older versions of volcombine.com.  Check for an
* <p> incorrect set name and set "valid = false" instead of throw an exception.
* <p>
* <p> Revision 1.1  2004/11/30 00:33:51  sueh
* <p> bug# 556 Object to parse the first set commmand in volcombine.com.
* <p> </p>
*/
public class ConstSetParam {
  public static  final String  rcsid =  "$Id$";
  public static final String COMMAND_NAME = "set";
  
  protected static final String delimiter = "=";
  
  protected String expectedName = null;
  protected String name = "";
  protected int etomoNumberType;
  protected EtomoNumber numericValue;
  protected String value;
  protected boolean numeric = false;
  protected boolean valid = true;
  
  public ConstSetParam(String expectedName, int etomoNumberType) {
    this.etomoNumberType = etomoNumberType;
    numericValue = new EtomoNumber(etomoNumberType, expectedName);
    numericValue.setDisplayValue(0);
    reset();
    numeric = true;
    
    this.expectedName = expectedName;
  }

  protected void reset() {
    name = "";
    value = "";
    numericValue.reset();
    valid = true;
  }
  
  public boolean isValid() {
    return valid;
  }
  
  public String getValue() {
    if (numeric) {
      return numericValue.toString();
    }
    return value;
  }

}
