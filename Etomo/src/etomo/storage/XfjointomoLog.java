package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Vector;

import etomo.BaseManager;
import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;

/**
 * <p>Description: Class to read the xfjointomo log.  This class should try to
 * avoid throwing runtime errors because this will keep the process tracking
 * functionality from completing.</p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.5  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.4  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.3  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.2  2008/01/31 20:24:14  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.1  2007/02/05 23:08:48  sueh
 * <p> bug# 962 Class for reading xfjointomo log file.
 * <p> </p>
 */
public final class XfjointomoLog {
  public static final String rcsid = "$Id$";

  private static final Hashtable INSTANCE_LIST = new Hashtable();
  private static final int MAX_ERROR_INDEX = 14;

  private final Hashtable rowList = new Hashtable();
  private final Vector rowArray = new Vector();

  private final String dir;

  private LogFile logFile = null;

  public static XfjointomoLog getInstance(BaseManager manager) {
    XfjointomoLog instance = (XfjointomoLog) INSTANCE_LIST.get(getUniqueKey(manager));
    if (instance != null) {
      return instance;
    }
    return createInstance(manager);
  }

  public void reset() {
    logFile = null;
  }

  public boolean rowExists(String boundary) throws LogFile.LockException,
      FileNotFoundException, IOException {
    load();
    return rowList.containsKey(boundary);
  }

  public String getBestGap(String boundary) throws LogFile.LockException,
      FileNotFoundException, IOException {
    load();
    Row row = (Row) rowList.get(boundary);
    if (row == null) {
      return null;
    }
    return row.getBestGap();
  }

  public String getMeanError(String boundary) throws LogFile.LockException {
    Row row = (Row) rowList.get(boundary);
    if (row == null) {
      return null;
    }
    return row.getMeanError();
  }

  public String getMaxError(String boundary) throws LogFile.LockException,
      FileNotFoundException, IOException {
    load();
    Row row = (Row) rowList.get(boundary);
    if (row == null) {
      return null;
    }
    return row.getMaxError();
  }

  public synchronized boolean gapsExist() throws LogFile.LockException, IOException,
      FileNotFoundException {
    load();
    boolean gapsExist = false;
    for (int i = 0; i < rowArray.size(); i++) {
      gapsExist = gapsExist || ((Row) rowArray.get(i)).gapExists();
    }
    return gapsExist;
  }

  private XfjointomoLog(BaseManager manager) {
    dir = manager.getPropertyUserDir();
  }

  private synchronized static XfjointomoLog createInstance(BaseManager manager) {
    //make sure another thread didn't already run createInstance for this manager
    XfjointomoLog instance = (XfjointomoLog) INSTANCE_LIST.get(getUniqueKey(manager));
    if (instance != null) {
      return instance;
    }
    //create instance
    instance = new XfjointomoLog(manager);
    INSTANCE_LIST.put(getUniqueKey(manager), instance);
    return instance;
  }

  /**
   * Unlike .edj files, multiple .ejf files can be placed in one directory.
   * @param manager
   * @return
   */
  private static String getUniqueKey(BaseManager manager) {
    return manager.getPropertyUserDir() + manager.getName();
  }

  /**
   * Load data from the log file.  Or reload data, if reset() has been called.
   * @throws LogFile.ReadException
   */
  private synchronized void load() throws LogFile.LockException, FileNotFoundException,
      IOException {
    if (logFile != null) {
      return;
    }
    rowList.clear();
    logFile = LogFile.getInstance(dir, DatasetFiles.XFJOINTOMO_LOG);
    LogFile.ReaderId readerId = logFile.openReader();
    String boundary = null;
    String line = null;
    while ((line = logFile.readLine(readerId)) != null) {
      if (line.indexOf("At boundary") != -1) {
        //load the data from the line
        String[] stringArray = line.trim().split("\\s+");
        if (stringArray.length < MAX_ERROR_INDEX + 1) {
          continue;
        }
        boundary = parseBoundary(stringArray);
        if (boundary == null || boundary.equals("")) {
          continue;
        }
        Row row = new Row(parseBestGap(stringArray), parseMeanError(stringArray),
            parseMaxError(stringArray));
        rowList.put(boundary, row);
        rowArray.add(row);
      }
    }
    logFile.closeRead(readerId);
  }

  private String parseBoundary(String[] stringArray) {
    return trimTrailingComma(stringArray[2].trim());
  }

  private String parseBestGap(String[] stringArray) {
    return stringArray[6].trim();
  }

  private String parseMeanError(String[] stringArray) {
    return trimTrailingComma(stringArray[11].trim());
  }

  private String parseMaxError(String[] stringArray) {
    return stringArray[MAX_ERROR_INDEX].trim();
  }

  private String trimTrailingComma(String string) {
    int lastCharIndex = string.length() - 1;
    if (string.charAt(lastCharIndex) == ',') {
      if (string.length() == 1) {
        return "";
      }
      return string.substring(0, lastCharIndex);
    }
    return string;
  }

  private final class Row {
    private final EtomoNumber bestGap = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    private final String meanError;
    private final String maxError;

    Row(String bestGap, String meanError, String maxError) {
      this.bestGap.set(bestGap);
      this.meanError = meanError;
      this.maxError = maxError;
    }

    String getBestGap() {
      return bestGap.toString();
    }

    String getMeanError() {
      return meanError;
    }

    String getMaxError() {
      return maxError;
    }

    boolean gapExists() {
      return !bestGap.equals(0);
    }
  }
}
