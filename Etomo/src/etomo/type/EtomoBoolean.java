package etomo.type;

import java.util.Properties;

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
public class EtomoBoolean extends ConstEtomoBoolean {
  public static  final String  rcsid =  "$Id$";
  
  public EtomoBoolean(String name) {
    super(name);
  }
  
  public void load(Properties props) {
    set(props.getProperty(name, toString(resetValue)));
  }
  public void load(Properties props, String prepend) {
    set(props.getProperty(prepend + "." + name, toString(resetValue)));
  }
  
  public ConstEtomoBoolean set(String value) {
    this.value = toInteger(value);
    return this;
  }
  
  public ConstEtomoBoolean set(boolean value) {
    this.value = toInteger(value);
    return this;
  }  
  public ConstEtomoBoolean reset() {
    value = resetValue;
    return this;
  }

}
