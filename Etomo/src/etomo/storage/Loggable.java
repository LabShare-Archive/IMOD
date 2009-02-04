package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

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

public interface Loggable {
  public static final String rcsid = "$Id$";

  public List getLogMessage() throws LogFile.LockException,
      FileNotFoundException, IOException;

  public String getName();
}
