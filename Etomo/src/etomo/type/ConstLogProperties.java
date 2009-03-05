package etomo.type;

import java.awt.Dimension;
import java.util.Properties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
public interface ConstLogProperties {
  public static final String rcsid = "$Id$";

  public void store(Properties props, String prepend);

  public void load(Properties props, String prepend);

  public Dimension getFrameSize();

  public int getFrameLocationX();

  public int getFrameLocationY();
}
