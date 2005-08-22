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
public interface IntermittentCommand {
  public static  final String  rcsid =  "$Id$";
  
  public String[] getCommand();
  public String getIntermittentCommand();
  public int getInterval();
  public String getKey();
}
/**
* <p> $Log$ </p>
*/