package etomo.type;

import java.util.Properties;

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
 * <p> Revision 1.2  2007/08/29 21:44:28  sueh
 * <p> bug# 1041 Made BaseState an abstract class, added
 * <p> killedProcesschunksProcessName.
 * <p>
 * <p> Revision 1.1  2004/12/14 21:40:52  sueh
 * <p> bug# 572 Interface for TomogramState and JoinState.
 * <p> </p>
 */
public abstract class BaseState implements Storable {
  public static final String rcsid = "$Id$";

  abstract String createPrepend(String prepend);

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
  }

  public void load(Properties props, String prepend) {
    //reset
    //load
    prepend = createPrepend(prepend);
  }

  public boolean equals(BaseState input) {
    return true;
  }
}
