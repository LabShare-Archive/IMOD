package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class EtomoNumericState {
  public static  final String  rcsid =  "$Id$";
  
  EtomoNumber currentValue;
  boolean resultSet = true;
  
  public EtomoNumericState(EtomoNumber.Type type, String name) {
    currentValue = new EtomoNumber(type, name);
  }

  public EtomoNumericState setResultSet(boolean resultSet) {
    this.resultSet = resultSet;
    return this;
  }
  
  public EtomoNumericState setFloor(int floorValue) {
    currentValue.setFloor(floorValue);
    return this;
  }
  
  public EtomoNumericState reset() {
    currentValue.reset();
    resultSet = true;
    return this;
  }
  
  public void store(Properties props, String prepend) {
    currentValue.store(props, prepend);
  }
  
  public void load(Properties props, String prepend) {
    EtomoState state = new EtomoState(currentValue.getName());
    try {
      state.load(props, prepend);
    }
    catch (IllegalArgumentException e) {
    }
    resultSet = state.isResultSet();
    currentValue.set(state);
  }
  
  public EtomoNumericState set(int value) {
    currentValue.set(value);
    resultSet = true;
    return this;
  }
  
  public boolean isNull() {
    return resultSet && currentValue.isNull();
  }
  
  public boolean isResultSet() {
    return resultSet;
  }
  
  public int getInteger() {
    return currentValue.getInt();
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/07/29 19:47:07  sueh
* <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
* <p>
* <p> Revision 1.1  2005/06/11 02:37:05  sueh
* <p> EtomoNumericState functions like EtomoState, except that it can contain
* <p> the same set of numeric values that EtomoNumber can.
* <p> </p>
*/
