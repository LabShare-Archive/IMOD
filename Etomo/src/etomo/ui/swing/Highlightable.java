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
* <p> Revision 1.1  2007/03/01 01:35:47  sueh
* <p> bug# 964 Interface to use with HighlighterButton.
* <p> </p>
*/
interface Highlightable {
  public static final String rcsid = "$Id$";

  void highlight(boolean highlight);
}
