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
* <p> $Log$
* <p> Revision 1.1  2007/02/05 23:23:04  sueh
* <p> bug# 962 Const interface for IntKeyList.
* <p> </p>
*/
public interface ConstIntKeyList {
  public static  final String  rcsid =  "$Id$";
  
  public int getFirstKey();
  public int getLastKey();
  public String getString(int key);
  public ConstEtomoNumber getEtomoNumber(int key);
  public boolean containsKey(int key);
  public int size();
}
