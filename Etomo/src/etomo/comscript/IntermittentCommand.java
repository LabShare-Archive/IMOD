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
  
  public String[] getLocalStartCommand();
  public String[] getRemoteStartCommand();
  public String getIntermittentCommand();
  public String getEndCommand();
  public int getInterval();
  public String getComputer();
  public boolean notifySentIntermittentCommand();
}
/**
* <p> $Log$
* <p> Revision 1.4  2005/12/01 00:22:46  sueh
* <p> bug# 775   The command that makes the connection can be different
* <p> depending on whether is it talking to local or remote computer.  Removed
* <p> getCommand and added getLocalCommand and getRemoteCommand.
* <p> This is interface is also about distributing commands across multiple
* <p> computers.  Remove getKey and added getComputer.
* <p>
* <p> Revision 1.3  2005/09/14 20:20:27  sueh
* <p> bug# 532 Added notifySentIntermittentCommand() so that notifying the
* <p> monitor that the intermittent command was sent can be optional.
* <p>
* <p> Revision 1.2  2005/08/24 00:19:17  sueh
* <p> bug #532 Added getEndCommand().  The exit command used in
* <p> IntermittentSystemProgram should be generic.
* <p>
* <p> Revision 1.1  2005/08/22 16:03:13  sueh
* <p> bug# 532 Interface for a param which can be used by
* <p> IntermittentSystemProgram.
* <p> </p>
*/