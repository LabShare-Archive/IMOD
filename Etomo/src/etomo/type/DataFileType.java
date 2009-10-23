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
public final class DataFileType {
  public static  final String  rcsid =  "$Id$";
  
  public static final DataFileType RECON = new DataFileType();
  public static final DataFileType JOIN = new DataFileType();
  public static final DataFileType PARALLEL = new DataFileType();
  public static final DataFileType PEET = new DataFileType();
  
  private DataFileType() {
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2007/02/19 22:00:03  sueh
* <p> bug# 964 Added PEET tab type.
* <p>
* <p> Revision 1.1  2006/03/20 18:00:26  sueh
* <p> bug# 835 Type of manager (ApplicationManager, JoinManager, or
* <p> ParallelManager).
* <p> </p>
*/