package etomo.uitest;



/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public interface UITestCommandFactory {
  public static final String rcsid = "$Id$";
  
  public UITestCommand newCommand();
}
/**
* <p> $Log$
* <p> Revision 1.1  2007/03/21 18:10:04  sueh
* <p> bug# 964 Moved AdocCommand classes out of the autodoc package because
* <p> they not part of the autodoc.
* <p>
* <p> Revision 1.1  2006/04/28 20:52:21  sueh
* <p> bug# 787 Interface for factories which create AdocCommands.
* <p> </p>
*/