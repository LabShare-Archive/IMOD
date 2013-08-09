package etomo.logic;

import etomo.type.MetaData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class TrimvolReorientation {
  public static  final String  rcsid =  "$Id:$";
  

  public static final TrimvolReorientation NONE = new TrimvolReorientation(0);
  public static final TrimvolReorientation FLIP = new TrimvolReorientation(1);
  public static final TrimvolReorientation ROTATE = new TrimvolReorientation(2);
  
  public static final TrimvolReorientation DEFAULT=ROTATE;

  public final int value;

  private TrimvolReorientation(final int value) {
    this.value = value;
  }


  public static int toDirectiveValue(final MetaData metaData) {
    if (metaData.isPostTrimvolSwapYZ()) {
      return FLIP.value;
    }
    if (metaData.isPostTrimvolRotateX()) {
      return ROTATE.value;
    }
    return NONE.value;
  }

}
