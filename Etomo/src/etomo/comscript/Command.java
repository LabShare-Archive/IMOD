package etomo.comscript;
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
public interface Command {
  public static  final String  rcsid =  "$Id$";
  
  public String getCommandName();
  public String getCommandLine();
  public String[] getCommandArray();
}
/**
* <p> $Log$ </p>
*/