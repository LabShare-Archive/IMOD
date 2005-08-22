package etomo.process;
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
public interface SystemProgramMonitor extends Runnable {
  public static  final String  rcsid =  "$Id$";
  
  public void setIntermittentSystemProgram(IntermittentSystemProgram intermittentSystemProgram);
  public void stop(String computer);
}
/**
* <p> $Log$ </p>
*/