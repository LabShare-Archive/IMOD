package etomo.ui.swing;
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
* <p> Revision 1.1  2009/04/27 17:59:28  sueh
* <p> bug# 1211 Interface for classes containing absolute file paths.  Allows the
* <p> user to fix the file paths if the files are moved.
* <p> </p>
*/
interface FileContainer {
  public static  final String  rcsid =  "$Id$";
  
  public void fixIncorrectPaths(boolean choosePathEveryRow);
}
