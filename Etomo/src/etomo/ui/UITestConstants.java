package etomo.ui;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
final class UITestConstants {
  public static  final String  rcsid =  "$Id$";
  
  static final String DIALOG_SECTION_TYPE = "Dialog";
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/01/11 23:19:08  sueh
* <p> bug# 675 Added TOGGLE_BUTTON_ATTRIB
* <p>
* <p> Revision 1.1  2006/01/04 20:28:21  sueh
* <p> bug# 675 Moved constants that must be shared by non-test objects to an
* <p> object which doesn't know about junit.  Overwise junit would have to be in
* <p> the path for compiling and running EtomoDirector.
* <p> </p>
*/