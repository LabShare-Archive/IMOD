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
 * <p> $Log$
 * <p> Revision 1.3  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.1  2009/02/04 23:27:58  sueh
 * <p> bug# 1158 Interface for classes that will send log entries to LogPanel.
 * <p> </p>
 */
public interface Loggable {
  public static final String rcsid = "$Id$";

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException;

  public String getName();
}
