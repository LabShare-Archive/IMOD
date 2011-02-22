package etomo.process;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import etomo.util.HashedArray;

/**
 * <p>Description: Runnable class to keep the output buffers of the child process
 * from filling up and locking up the process.  See Java bugs #: 4750978,
 * 4098442, etc.
 * 
 * Was part of SystemProgram.</p>
 * 
 * @ThreadSafe
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class OutputBufferManager implements Runnable {
  public static final String rcsid = "$Id$";

  private static final String MESSAGE_TOKEN = "MESSAGE:";

  private final List outputList = new Vector();

  private final BufferedReader outputReader;

  private boolean processDone = false;
  private boolean collectOutput = true;
  private String keyPhrase = null;
  private HashedArray listenerList = null;
  private boolean debug = false;
  private boolean printMessages = false;

  OutputBufferManager(BufferedReader reader) {
    outputReader = reader;
  }

  OutputBufferManager(BufferedReader reader, String keyPhrase) {
    outputReader = reader;
    this.keyPhrase = keyPhrase;
  }

  void setPrintMessages(boolean input) {
    printMessages = input;
  }

  public void run() {
    String line;
    try {
      while (!processDone) {
        while ((line = outputReader.readLine()) != null) {
          add(line);
        }
        Thread.sleep(100);
      }
      while ((line = outputReader.readLine()) != null) {
        add(line);
      }
    }
    catch (IOException except) {
      //  Assume the stream is closed by the program exiting.  
      processDone = true;
      return;
    }
    catch (InterruptedException except) {
      except.printStackTrace();
      System.err.println("SystemProgram::OuputBufferManager interrupted!");
    }
  }

  void setCollectOutput(boolean collectOutput) {
    this.collectOutput = collectOutput;
  }

  void setProcessDone(boolean state) {
    processDone = state;
  }

  /**
   * <p>Returns the size of the outputList, or the size of the first listener's
   * output list if there are multiple listeners.  The function does not clear
   * the output list.</p>
   * @return
   */
  int size() {
    return outputList.size();
  }

  /**
   * <p>Gets a line from outputList, or a line from the first listener's output
   * list if there are multiple listeners.</p>
   * @param index
   * @return
   */
  String get(int index) {
    return (String) outputList.get(index);
  }

  void setDebug(boolean debug) {
    this.debug = debug;
  }

  /**
   * <p>Add the line param to outputList or, if listenerList is in use, add the
   * line to all the output lists in listenerList.  Since the first listener
   * uses the outputList instead of creating a new output list, outputList is
   * always added to.  If the keyPhrase is set, only add the line if it contains
   * the key phrase.</p>
   * @param line
   */
  private synchronized void add(String line) {
    if (keyPhrase == null || line.indexOf(keyPhrase) != -1) {
      //Add line to outputList and/or to all listeners; if there are listeners,
      //of them is using outputList.
      if (listenerList != null && listenerList.size() > 0) {
        for (int i = 0; i < listenerList.size(); i++) {
          ((List) listenerList.get(i)).add(line);
        }
      }
      else {
        outputList.add(line);
      }
    }
  }

  /**
   * <p>Get the outputList or get and clear the first listener's output list if
   * there are multiple listeners.  If collectOutput is false clear outputList.</p>
   * @return
   */
  synchronized String[] get() {
    String[] stringArray = (String[]) outputList.toArray(new String[outputList.size()]);
    if (!collectOutput) {
      outputList.clear();
    }
    return stringArray;
  }

  /**
   * Clear outputList and all the output lists in listenerList.
   */
  synchronized void clear() {
    outputList.clear();
    if (listenerList != null) {
      for (int i = 0; i < listenerList.size(); i++) {
        ((List) listenerList.get(i)).clear();
      }
    }
  }

  /**
   * <p>Get the output list in the listenerList with the key equal to the
   * listenerKey param.  If collectOutput is false clear the output list.</p>
   * If there is no output list pointed to by listenerKey:
   * <p>If this is the first listenerKey to be added (or the first since all
   * listeners where dropped) use outputList as this listenerKey's output list.
   * If it is not the first add an empty output list.</p>
   * @param listenerKey
   * @return
   */
  synchronized String[] get(Object listenerKey) {
    //Get the listenerKey's output list or add it if it doesn't exist.
    if (listenerList == null) {
      listenerList = new HashedArray();
    }
    List listenerOutputList = (List) listenerList.get(listenerKey);
    if (listenerOutputList == null) {
      //If the listenerList is empty, then lines where being added to outputList
      //so use outputList as the listenerOutputList for this listenerKey.
      listenerOutputList = outputList;
      if (listenerList.size() == 0) {
        listenerList.add(listenerKey, outputList);
      }
      else {
        //This is not the first listenerKey to be added so the outputList is
        //already in use.
        listenerOutputList = new ArrayList();
        listenerList.add(listenerKey, listenerOutputList);
        return new String[0];
      }
    }
    //Return and clear the output list.
    String[] stringArray = (String[]) listenerOutputList
        .toArray(new String[listenerOutputList.size()]);
    if (!collectOutput) {
      listenerOutputList.clear();
    }
    return stringArray;
  }

  /**
   * Drop a listenerKey from the listenerList.  Clean the listener's output
   * list.
   * @param listenerKey
   */
  synchronized void dropListener(Object listenerKey) {
    if (listenerList == null) {
      return;
    }
    //The listener output list may be the outputList, so clear it before
    //removing it.
    List listenerOutputList = (List) listenerList.get(listenerKey);
    if (listenerOutputList != null) {
      listenerOutputList.clear();
    }
    listenerList.remove(listenerKey);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.6  2010/01/11 23:56:33  sueh
 * <p> bug# 1299 Added useMessageReporter.
 * <p>
 * <p> Revision 1.5  2009/09/02 22:42:22  sueh
 * <p> bug# 1254 Getting rid of duplicate debug prints.
 * <p>
 * <p> Revision 1.4  2008/02/16 01:52:16  sueh
 * <p> bug# 1080 Rewrote functionality to handle multiple listeners.  The first
 * <p> listener listens to outputList.  New output list are added for the other
 * <p> listeners.
 * <p>
 * <p> Revision 1.3  2007/05/26 00:29:24  sueh
 * <p> bug# 994 Added setDebug().
 * <p>
 * <p> Revision 1.2  2005/09/14 20:25:39  sueh
 * <p> bug# 532 Added drop() to remove a monitor from the listener list.  It is
 * <p> important for the called to prevent any last-minute gets after the drop() is
 * <p> called; the get() will add the monitor back to the listener list, if it is sent
 * <p> after the drop().
 * <p>
 * <p> Revision 1.1  2005/09/10 01:51:08  sueh
 * <p> bug# 532 Changed IntermittentSystemProgram to
 * <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
 * <p> of SystemProgram.  Made OutputBufferManager in independent class
 * <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
 * <p> use OutputBufferManager to do things only necessary for intermittent
 * <p> programs, such as deleting standard output after it is processed,
 * <p> keeping separate lists of standard output for separate monitors, and
 * <p> setting a key phrase in OutputBufferManager so that only useful lines from
 * <p> standard output will be saved.
 * <p> </p>
 */
