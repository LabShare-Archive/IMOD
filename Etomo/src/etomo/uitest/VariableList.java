package etomo.uitest;
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
* <p> $Log$ </p>
*/
interface VariableList {
  public static  final String  rcsid =  "$Id$";
  public String getVariableValue(String variableName);
  public boolean isVariableSet(String variableName);
}
