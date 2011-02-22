package etomo.type;

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
public class ProcessResult {
  public static final String rcsid = "$Id$";

  public static final ProcessResult FAILED_TO_START = new ProcessResult();
  public static final ProcessResult FAILED = new ProcessResult();
  public static final ProcessResult SUCCEEDED = new ProcessResult();
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/07/26 16:34:57  sueh
* <p> bug# 868 Contains the three possible results of a process.
* <p> </p>
*/
