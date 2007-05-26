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
* 
* <p> $Log$
* <p> Revision 1.1  2007/05/25 00:25:41  sueh
* <p> bug# 994 Generic interface for the FailureReason class.
* <p> </p>
*/
public interface FailureReasonInterface {
  public static  final String  rcsid =  "$Id$";
  
  public String getReason();

  public String getTooltip();
  
  public String toString();
}
