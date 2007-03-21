package etomo.storage;


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
public interface AdocCommandFactory {
  public static final String rcsid = "$Id$";
  
  public AdocCommand newAdocCommand();
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/04/28 20:52:21  sueh
* <p> bug# 787 Interface for factories which create AdocCommands.
* <p> </p>
*/