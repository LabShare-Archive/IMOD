package etomo.comscript;

/**
* <p>Description: A collection of FortranInputStrings.  Useful when success
* com script entries accumulate.  Should handle FortranInputString of different
* lengths and types.</p>
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
public class FortranInputStringList {
  public static final String rcsid = "$Id$";

  String key = null;
  FortranInputString[] array = null;

  public FortranInputStringList(String key) {
    this.key = key;
  }

  public void parse(ComScriptCommand scriptCommand) throws FortranInputSyntaxException {
    array = null;
    String[] values = scriptCommand.getValues(key);
    if (values == null || values.length == 0) {
      return;
    }
    for (int i = 0; i < values.length; i++) {
      array[i] = FortranInputString.getInstance(values[i]);
    }
  }

  public double[] getDouble() {
    if (array == null) {
      return null;
    }
    int totalElements = 0;
    for (int i = 0; i < array.length; i++) {
      totalElements += array[i].size();
    }
    double[] doubleList = new double[totalElements];
    int currentIndex = 0;
    for (int i = 0; i < array.length; i++) {
      for (int j = 0; j < array[i].size(); j++) {
        doubleList[currentIndex++] = array[i].getDouble(j);
      }
    }
    return doubleList;
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/05/09 22:57:12  sueh
* <p> bug# 658 FortranInputStringList replaces FortranInputString[] and will be
* <p> simpler to use.
* <p> </p>
*/
