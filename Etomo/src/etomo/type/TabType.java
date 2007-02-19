package etomo.type;
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
public final class TabType {
  public static  final String  rcsid =  "$Id$";
  
  public static final TabType RECON = new TabType();
  public static final TabType JOIN = new TabType();
  public static final TabType PARALLEL = new TabType();
  public static final TabType PEET = new TabType();
  
  private TabType() {
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/03/20 18:00:26  sueh
* <p> bug# 835 Type of manager (ApplicationManager, JoinManager, or
* <p> ParallelManager).
* <p> </p>
*/