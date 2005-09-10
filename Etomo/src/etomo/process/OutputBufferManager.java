package etomo.process;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;

import etomo.util.HashedArray;

/**
* <p>Description: Runnable class to keep the output buffers of the child process
* from filling up and locking up the process.  See Java bugs #: 4750978,
* 4098442, etc.
* 
* Was part of SystemProgram.</p>
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
class OutputBufferManager implements Runnable {
  public static final String rcsid = "$Id$";

  private final BufferedReader outputReader;
  private final ArrayList outputList = new ArrayList();
  private boolean processDone = false;
  private boolean collectOutput = true;
  private String keyPhrase = null;
  private HashedArray listenerList = null;

  OutputBufferManager(BufferedReader reader) {
    outputReader = reader;
  }
  
  OutputBufferManager(BufferedReader reader, String keyPhrase) {
    outputReader = reader;
    this.keyPhrase = keyPhrase;
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

  final void setCollectOutput(boolean collectOutput) {
    this.collectOutput = collectOutput;
  }
  
  final void setProcessDone(boolean state) {
    processDone = state;
  }

  final int size() {
    return outputList.size();
  }

  final String get(int index) {
    return (String) outputList.get(index);
  }

  /**
   * Add the line to the output list.  If the keyPhrase is set, only add the
   * line if it contains the key phrase.  If the listener list is not empty,
   * also add a copy of the line to each listener output list.
   * @param line
   */
  private synchronized void add(String line) {
    if (keyPhrase == null || line.indexOf(keyPhrase) != -1) {
      outputList.add(line);
      if (listenerList != null) {
        for (int i = 0; i < listenerList.size(); i++) {
          ((ArrayList) listenerList.get(i)).add(new String(line));
        }
      }
    }
  }

  /**
   * Get the current output list.  If collectOutput is false, clear the output
   * list after is it copied to the string array to be returned.
   * @throws IllegalStateException if this function is call when the listener
   * list is not empty and collectOutput is false.  This is because
   * get(Object listenerKey) destroys the output list when collectOutput is
   * false.
   * @return
   */
  final synchronized String[] get() {
    if (listenerList != null && !collectOutput) {
      throw new IllegalStateException("the listener list is active and collectOutput is false");
    }
    String[] stringArray = (String[]) outputList.toArray(new String[outputList.size()]);
    //if not collecting output, clear outputList after each get.
    if (!collectOutput && outputList.size() > 0) {
      outputList.clear();
    }
    return stringArray;
  }
  
  /**
   * Get the output list for a listener.  If the listener isn't on the list, add
   * a new listener output list to the listener list.  Then copy the current
   * output list to the listener output list.  If the listener is already on the
   * list, return the listener output list.  If collectOutput is false, clear
   * the output list.
   * @param listenerKey
   * @return
   */
  final synchronized String[] get(Object listenerKey) {
    //get the listenerOutputList
    ArrayList listenerOutputList = null;
    if (listenerList == null) {
      listenerList = new HashedArray();
    }
    else {
      listenerOutputList = (ArrayList) listenerList.get(listenerKey);
    }
    if (listenerOutputList == null) {
      listenerOutputList = new ArrayList();
      for (int i = 0; i < outputList.size(); i++) {
        listenerOutputList.add(new String((String) outputList.get(i)));
      }
      listenerList.add(listenerKey, listenerOutputList);
    }
    //create string array to return
    int listenerOutputListSize = listenerOutputList.size();
    String[] stringArray = (String[]) listenerOutputList.toArray(new String[listenerOutputListSize]);
    //if not collecting output, clear outputList and the current
    //listenerOutputList after each get.
    if (!collectOutput) {
      if (outputList.size() > 0) {
        outputList.clear();
      }
      if (listenerOutputListSize > 0) {
        listenerOutputList.clear();
      }
    }
    return stringArray;
  }
}
/**
* <p> $Log$ </p>
*/