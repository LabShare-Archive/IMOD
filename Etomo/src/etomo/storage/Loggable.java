package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import etomo.ManagerKey;

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
 * <p> $Log$
 * <p> Revision 1.1  2009/02/04 23:27:58  sueh
 * <p> bug# 1158 Interface for classes that will send log entries to LogPanel.
 * <p> </p>
 */

public interface Loggable {
  public static final String rcsid = "$Id$";

  public List getLogMessage(ManagerKey managerKey) throws LogFile.LockException,
      FileNotFoundException, IOException;

  public String getName();
}
