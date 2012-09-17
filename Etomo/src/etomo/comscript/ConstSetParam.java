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
* <p> Revision 1.4  2006/10/13 22:27:51  sueh
* <p> bug# 927 Added constants for set names and types.
* <p>
* <p> Revision 1.3  2005/01/25 21:26:29  sueh
* <p> No longer using EtomoNumber.default for display.
* <p>
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
  public static final String COMBINEFFT_REDUCTION_FACTOR_NAME = "combinefft_reduce";
  public static final EtomoNumber.Type COMBINEFFT_REDUCTION_FACTOR_TYPE = EtomoNumber.Type.DOUBLE;
  public static final String COMBINEFFT_LOW_FROM_BOTH_RADIUS_NAME = "combinefft_lowboth";
  public static final EtomoNumber.Type COMBINEFFT_LOW_FROM_BOTH_RADIUS_TYPE = EtomoNumber.Type.DOUBLE;
  
  protected static final String delimiter = "=";
  
  protected String expectedName = null;
  protected String name = "";
  protected EtomoNumber.Type type;
  protected EtomoNumber numericValue;
  protected String value;
  protected boolean numeric = false;
  protected boolean valid = true;
  
  public ConstSetParam(String expectedName, EtomoNumber.Type type) {
    this.type = type;
    numericValue = new EtomoNumber(type, expectedName);
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
