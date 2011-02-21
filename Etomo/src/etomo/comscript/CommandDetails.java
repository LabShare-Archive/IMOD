package etomo.comscript;

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
public interface CommandDetails extends Command, ProcessDetails {
  public static final String rcsid = "$Id$";
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/05/11 19:38:43  sueh
* <p> bug# 838 Add CommandDetails, which extends Command and
* <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
* <p> functions.  Command contains all the command oriented functions.
* <p> </p>
*/
