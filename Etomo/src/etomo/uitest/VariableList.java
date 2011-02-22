package etomo.uitest;

import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.2  2009/09/01 03:18:33  sueh
* <p> bug# 1222
* <p>
* <p> Revision 1.1  2009/01/20 20:51:46  sueh
* <p> bug# 1102 Interface for classes that hold list(s) of variable name/value
* <p> pairs.
* <p> </p>
*/
interface VariableList {
  public static final String rcsid = "$Id$";

  public String getVariableValue(String variableName, AxisID axisID);

  public boolean isVariableSet(String variableName, AxisID axisID);

  public void setVariable(String variableName, Object variableValue);
}
