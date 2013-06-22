package etomo.storage;

import java.io.*;
import java.util.*;

import etomo.EtomoDirector;

/**
 * <p>Description: Keeps a Properties instance that mirrors a properties file.
 * Loads the properties file once.  Updates the properties instance and saves as
 * requested.</p>  
 * <p>
 * @ThreadSafe Synchronized on the LogFile instance which represents the
 * properties file.  The LogFile class creates only one instance per physical
 * file so this allows synchronization of multiple ParameterStore instances that
 * write to the same file (however there is no reason to have multiple
 * instances that write to the same file).</p>
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
 * <p> Revision 3.15  2011/04/04 16:59:38  sueh
 * <p> bug# 1416 Changed to a numeric debug variable.  Modified save and setDebug.
 * <p>
 * <p> Revision 3.14  2011/02/22 04:52:30  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.13  2010/10/13 20:19:11  sueh
 * <p> bug# 1392 Added getFilelessInstance.  Handling null dataFile.
 * <p>
 * <p> Revision 3.12  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.11  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.10  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 3.9  2008/01/31 20:23:00  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.  Made
 * <p> ParametersStore thread safe.
 * <p>
 * <p> Revision 3.8  2007/07/30 18:53:24  sueh
 * <p> bug# 1002 Added ParameterStore.getInstance.
 * <p>
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
 **/
public final class ParameterStore {
  public static final String rcsid = "$Id$";

  private final Properties properties = new Properties();

  // initialized in initialize()
  private LogFile dataFile = null;

  private boolean autoStore = true;
  private int debug = 0;

  private ParameterStore() {
  }

  /**
   * If paramFile is null, returns null.
   */
  public static ParameterStore getInstance(File paramFile) throws LogFile.LockException {
    if (paramFile == null) {
      return null;
    }
    ParameterStore instance = new ParameterStore();
    instance.initialize(paramFile);
    return instance;
  }

  public static ParameterStore getFilelessInstance() throws LogFile.LockException {
    ParameterStore instance = new ParameterStore();
    instance.initialize(null);
    return instance;
  }

  private void initialize(File paramFile) throws LogFile.LockException {
    if (paramFile != null) {
      dataFile = LogFile.getInstance(paramFile);
    }
    if (dataFile != null && dataFile.exists()) {
      LogFile.InputStreamId inputStreamId = null;
      try {
        inputStreamId = dataFile.openInputStream();
        dataFile.load(properties, inputStreamId);
      }
      catch (LogFile.LockException e) {
        System.err.println("Unable to read " + dataFile.getAbsolutePath());
        e.printStackTrace();
      }
      catch (IOException e) {
        System.err.println("Unable to read " + dataFile.getAbsolutePath());
        e.printStackTrace();
      }
      if (inputStreamId != null && !inputStreamId.isEmpty()) {
        dataFile.closeInputStream(inputStreamId);
      }
    }
  }

  /**
   * Saves properties to the paramFile.
   * @throws IOException
   */
  public void storeProperties() throws LogFile.LockException, IOException {
    // If the file has not been set, don't save.
    if (dataFile != null) {
      synchronized (dataFile) {
        if (!dataFile.isDirectory()) {
          if (dataFile.getName().endsWith(EtomoDirector.USER_CONFIG_FILE_EXT)) {
            dataFile.backupOnce();
          }
          else {
            dataFile.doubleBackupOnce();
          }
        }
        if (!dataFile.exists()) {
          dataFile.create();
        }
        LogFile.OutputStreamId outputStreamId = dataFile.openOutputStream();
        dataFile.store(properties, outputStreamId);
        dataFile.closeOutputStream(outputStreamId);
      }
    }
  }

  public void setDebug(boolean debug) {
    this.debug = debug ? 1 : 0;
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
  public void save(Storable storable) throws LogFile.LockException, IOException {
    // If the file has not been set, don't save.
    if (dataFile != null) {
      synchronized (dataFile) {
        // let the storable overwrite its values
        if (storable != null) {
          storable.store(properties);
        }
        if (autoStore) {
          storeProperties();
        }
        if (debug == 1) {
          System.err.println("save:JoinState.Join.Version="
              + properties.getProperty("JoinState.Join.Version"));
        }
      }
    }
  }

  /**
   * Load storable from properties.  Loads the properties if they haven't been
   * loaded before.
   * @param storable
   * @throws IOException
   */
  public void load(Storable storable)/* throws LogFile.LockException */{
    if (dataFile != null) {
      // If the file has been set, synchronize on it.
      synchronized (dataFile) {
        storable.load(properties);
      }
    }
    else {
      storable.load(properties);
    }
  }

  public String getAbsolutePath() {
    return dataFile.getAbsolutePath();
  }
}
