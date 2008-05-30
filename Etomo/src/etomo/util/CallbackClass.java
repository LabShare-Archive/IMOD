package etomo.util;

import etomo.uitest.UITestCommand;

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
*/ 
public interface CallbackClass {
  public static final String rcsid = "$Id$";
  
  public void callback(UITestCommand command);
}
/**
* 
* <p> $Log$
* <p> Revision 1.2  2007/03/21 19:50:06  sueh
* <p> bug# 964 Moved AdocCommand classes out of the autodoc package.
* <p>
* <p> Revision 1.1  2006/06/27 22:36:59  sueh
* <p> bug# 852 An interface to allow selected methods on the implementing class to be
* <p> called generically.
* <p> </p>
*/