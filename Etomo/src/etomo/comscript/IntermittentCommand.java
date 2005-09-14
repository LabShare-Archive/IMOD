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
  public String getEndCommand();
  public int getInterval();
  public String getKey();
  public boolean notifySentIntermittentCommand();
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/08/24 00:19:17  sueh
* <p> bug #532 Added getEndCommand().  The exit command used in
* <p> IntermittentSystemProgram should be generic.
* <p>
* <p> Revision 1.1  2005/08/22 16:03:13  sueh
* <p> bug# 532 Interface for a param which can be used by
* <p> IntermittentSystemProgram.
* <p> </p>
*/