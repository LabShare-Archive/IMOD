package etomo.ui.swing;

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
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.1  2008/02/28 21:16:48  sueh
* <p> bug# 1085 GUI object that contains a RubberbandPanel and needs to be
* <p> able to set coordinate values in the parent.  Can currently set Z min and
* <p> max.
* <p> </p>
*/
interface RubberbandContainer {
  public static final String rcsid = "$Id$";

  public void setZMin(String zMin);

  public void setZMax(String zMax);
}
