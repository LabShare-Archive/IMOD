package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class LoggableCollection implements Loggable {
  public static final String rcsid = "$Id:$";

  private final List<Loggable> loggableList = new ArrayList<Loggable>();

  public void addLoggable(final Loggable loggable) {
    loggableList.add(loggable);
  }

  /**
   * Returns the name of the first Loggable in the collection.  Returns an empty string
   * if the collection is empty.
   */
  public String getName() {
    if (loggableList.isEmpty()) {
      return "";
    }
    return loggableList.get(0).getName();
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    List logMessageList = new ArrayList();
    Iterator<Loggable> i = loggableList.iterator();
    while (i.hasNext()) {
      logMessageList.addAll(i.next().getLogMessage());
    }
    return logMessageList;
  }
}
