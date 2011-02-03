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
* <p> Revision 1.1  2007/02/05 23:10:27  sueh
* <p> bug# 962 A const interface for EtomoVersion.
* <p> </p>
*/
public interface ConstEtomoVersion {
  public static  final String  rcsid =  "$Id$";
  
  public boolean ge(String version);
  public boolean ge(EtomoVersion version);
  public boolean lt( String version);
}
