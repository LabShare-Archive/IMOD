package etomo.storage;

import java.io.*;
import java.util.*;

/*
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.7  2007/02/21 04:17:48  sueh
 * <p> bug# 964 Removing unnecessary print.
 * <p>
 * <p> Revision 3.6  2007/02/05 23:05:55  sueh
 * <p> bug# 962 Added debug member variable.
 * <p>
 * <p> Revision 3.5  2006/11/15 20:37:48  sueh
 * <p> bug# 872 Rewrote class to retain a Properties member variable as a
 * <p> representation of the data file.  Backing up the data file the first time it is saved
 * <p> while the instance exists.  Added autoStore:  default true - saves to the file with
 * <p> every store, true - to allow multiple saves to the properties and must be saved by
 * <p> call storeProperties.
 * <p>
 * <p> Revision 3.4  2006/09/19 22:30:47  sueh
 * <p> bug# 920 Do not allow a storable to be null.
 * <p>
 * <p> Revision 3.3  2006/09/13 23:30:09  sueh
 * <p> bug# 921 Preventing null pointer exception in save(Storable).
 * <p>
 * <p> Revision 3.2  2006/06/05 18:05:20  sueh
 * <p> bug# 766 Added save(Storable), to save a single Storable without overwriting the
 * <p> other Storables in the data file.
 * <p>
 * <p> Revision 3.1  2005/09/12 23:58:38  sueh
 * <p> bug# 532 Added save() to save a Storable class without overwriting
 * <p> preference entries from other Storable classes.  Added load(Storable) to
 * <p> load a single storable class.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/07 22:27:05  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public final class ParameterStore {
  public static final String rcsid = "$Id$";

  private final Properties properties = new Properties();

  private final LogFile dataFile;

  private boolean propertiesLoaded = false;
  private boolean autoStore = true;
  private boolean debug = false;

  /**
   * Construct a ParameterStore using the File specified
   * @param paramFile a File object specifying where the parameters are stored
   * or to be stored.  <i>What happens when the file does not exist</i>
   */
  private ParameterStore(File paramFile) {
    dataFile = LogFile.getInstance(paramFile);
  }

  /**
   * If paramFile is null, returns null.
   */
  public static ParameterStore getInstance(File paramFile) {
    if (paramFile == null) {
      return null;
    }
    return new ParameterStore(paramFile);
  }

  /**
   * Loads the properties if they haven't been loaded before.
   * @throws IOException
   */
  private void loadProperties() throws LogFile.WriteException {
    if (propertiesLoaded) {
      return;
    }
    //synchronize to avoid loading the properties more then once
    //The file should only be modified by this instance of this class, so there
    //should be no reason to reload.
    //To modify the data file by hand, exit etomo, edit the data file, and rerun
    //etomo.
    synchronized (this) {
      if (propertiesLoaded) {
        return;
      }
      if (!dataFile.exists()) {
        propertiesLoaded = true;
        return;
      }
      long writeId = dataFile.openInputStream();
      dataFile.load(properties, writeId);
      propertiesLoaded = true;
      dataFile.closeInputStream(writeId);
    }
  }

  /**
   * Saves properties to the paramFile.
   * @throws IOException
   */
  public void storeProperties() throws LogFile.FileException,
      LogFile.WriteException {
    dataFile.backupOnce();
    if (!dataFile.exists()) {
      dataFile.create();
    }
    long outputStreamId = dataFile.openOutputStream();
    dataFile.store(properties, outputStreamId);
    dataFile.closeOutputStream(outputStreamId);
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  /**
   * When autoStore is true (default), the paramFile is updated each time
   * save(Storable) is called.  When autoStore is false, storeProperties must be
   * called to write properties to the paramFile.
   * @param autoStore
   */
  public void setAutoStore(boolean autoStore) {
    this.autoStore = autoStore;
  }

  /**
   * Saved the values in storable to properties and then save properties to the
   * paramFile.  Loads the properties if they haven't been loaded before.
   * @param storable
   * @throws IOException
   */
  public void save(Storable storable) throws LogFile.WriteException,
      LogFile.FileException {
    loadProperties();
    //let the storable overwrite its values
    storable.store(properties);
    if (autoStore) {
      storeProperties();
    }
    if (debug) {
      System.err.println("save:JoinState.Join.Version="
          + properties.getProperty("JoinState.Join.Version"));
    }
  }

  /**
   * Load storable from properties.  Loads the properties if they haven't been
   * loaded before.
   * @param storable
   * @throws IOException
   */
  public void load(Storable storable) throws LogFile.WriteException {
    loadProperties();
    storable.load(properties);
  }

  public String getAbsolutePath() {
    return dataFile.getAbsolutePath();
  }
}
