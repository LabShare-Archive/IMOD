package etomo.ui;

import java.util.Properties;

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

public interface LogProperties {
  public static final String rcsid = "$Id:$";

  public void store(Properties props, String prepend);

  public void load(Properties props, String prepend);
}
