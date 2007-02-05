package etomo.type;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
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
public interface ConstIntKeyList {
  public static  final String  rcsid =  "$Id$";
  
  public int getFirstKey();
  public int getLastKey();
  public String get(int key);
  public ConstEtomoNumber getNumeric(int key);
  public boolean containsKey(int key);
  public int size();
}
