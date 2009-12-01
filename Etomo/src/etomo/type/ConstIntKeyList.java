package etomo.type;

import etomo.type.IntKeyList.Walker;

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
* <p> Revision 1.3  2009/02/05 23:43:24  sueh
* <p> bug# 1148 Added getWalker.  IntKeyList.Walker cannot modify the main
* <p> class.
* <p>
* <p> Revision 1.2  2007/03/01 01:24:05  sueh
* <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
* <p> in IntKeyList.
* <p>
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
  public Walker getWalker();
  public boolean isEmpty();
}
