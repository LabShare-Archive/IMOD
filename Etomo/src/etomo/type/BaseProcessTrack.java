package etomo.type;

import etomo.storage.Storable;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.2  2004/11/19 23:31:43  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.1  2004/09/29 19:16:18  sueh
* <p> bug# 520 Base class for ProcessTrack and JoinProcessTrack.  Implements
* <p> Storable with generic functions and abstract functions.
* <p> </p>
*/
public interface BaseProcessTrack extends Storable {
  public static  final String  rcsid =  "$Id$";
  
  public String getRevisionNumber();
  public boolean isModified();
  public void resetModified();
}
