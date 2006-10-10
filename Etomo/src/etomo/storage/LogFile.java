package etomo.storage;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;

import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: Class which controls the opening and closing of a log file.
 * This class is necessary because working with log files on Windows has
 * become unreliable and a separate class was needed to ensure that the log
 * files are not left open.
 * 
 * This class contains a semaphore called Lock, which can prevent the functions
 * backup() and open() from completing.  The waitLimit parameter can be used to
 * prevent eTomo from being deadlocked.
 * 
 * LogFile is an N'ton and stores its instances in a Hashtable, which is a
 * synchronized class.
 * 
 * Log File is a syncrhonized class:  all of its public and package-level
 * functions are synchronized (except functions which only call a synchronized
 * functions).</p>
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
 */

public final class LogFile {
  public static final String rcsid = "$Id$";

  public static final long NO_ID = -1;
  public static final long NO_WAIT_LIMIT = -1;
  private static final String PUBLIC_EXCEPTION_MESSAGE = "\nPlease inform the software developer.";

  private static Hashtable logFileHashTable = null;
  private static ReaderList readerList = null;

  private final Lock lock = new Lock();

  private final String userDir;
  private final String fileName;

  private File file = null;
  private File backupFile = null;
  private FileWriter fileWriter = null;
  private BufferedWriter bufferedWriter = null;
  private boolean writerOpen = false;

  private LogFile(String userDir, String fileName) {
    this.userDir = userDir;
    this.fileName = fileName;
  }

  public String toString() {
    return "\n[fileName=" + fileName + ",lock=" + lock + "]";
  }

  /**
   * Get an instance of LogFile based on a key constructed from parameters
   * describing the log file.
   * @param userDir
   * @param axisID
   * @param processName
   * @return retrieved instance
   */
  public static LogFile getInstance(String userDir, AxisID axisID,
      ProcessName processName) {
    return getInstance(userDir, axisID, processName.toString());
  }

  public static LogFile getInstance(String userDir, AxisID axisID, String name) {
    return getInstance(userDir, name + axisID.getExtension()
        + DatasetFiles.LOG_EXT);
  }

  public static LogFile getInstance(String userDir, String fileName) {
    LogFile logFile;
    String key = makeKey(userDir, fileName);
    if (logFileHashTable == null
        || (logFile = (LogFile) logFileHashTable.get(key)) == null) {
      //the instance doesn't exist - create it
      logFile = createInstance(userDir, fileName, key);
    }
    return logFile;
  }

  static void reset() {
    logFileHashTable = null;
    readerList = null;
  }

  /**
   * Check for the existance of the instance, because another thread could have
   * created before this thread called createInstance().  If the instance
   * isn't there, create an instance of LogFile and a key and add them to
   * logFileHashTable.
   * @param userDir
   * @param fileName
   * @param key
   * @return created instance
   */
  private static synchronized LogFile createInstance(String userDir,
      String fileName, String key) {
    LogFile logFile;
    //make sure that the instance wasn't created by another thread
    if (logFileHashTable != null
        && (logFile = (LogFile) logFileHashTable.get(key)) != null) {
      return logFile;
    }
    //create the instance
    logFile = new LogFile(userDir, fileName);
    if (logFileHashTable == null) {
      logFileHashTable = new Hashtable();
    }
    //save the instance
    logFileHashTable.put(key, logFile);
    return logFile;
  }

  /**
   * Delete the current backup file and rename the current file to be the new
   * backup file.  The current backup file will not be deleted unless the
   * current file exists.  A waitLimit < 0 can cause deadlock.
   * @see waitForLock()
   * @param waitLimit
   * @return true if a backup was done
   * @throws BackupException
   */
  public synchronized boolean backup() throws BackupException {
    long backupId = NO_ID;
    try {
      backupId = lock.lock(LockType.BACKUP);
    }
    catch (LockException e) {
      throw new BackupException(this, backupId, e);
    }
    createFile();
    createBackupFile();
    if (!file.exists()) {
      //nothing to backup
      try {
        lock.unlock(LockType.BACKUP, backupId);
      }
      catch (LockException e) {
        //only throw backup exception when the backup failed because of an error
        e.printStackTrace();
      }
      return false;
    }
    boolean success = true;
    //File logFile = new File(userDir, processName.toString()
    //    + axisID.getExtension() + ".log");
    //File backupLogFile = new File(file.getAbsolutePath() + '~');
    //don't delete backup file unless the file to be backed up exists
    if (backupFile.exists()) {
      Utilities.debugPrint(backupFile.getAbsolutePath() + " exists, deleting");
      if (!backupFile.delete()) {
        System.err.println("Unable to delete backup log file: "
            + backupFile.getAbsolutePath());
        if (backupFile.exists()) {
          success = false;
          System.err.println(backupFile.getAbsolutePath() + " still exists!");
        }
        else {
          System.err.println(backupFile.getAbsolutePath() + " does not exist!");
        }
      }
    }
    Utilities.debugPrint(file.getAbsolutePath() + " exists");
    if (!file.renameTo(backupFile)) {
      if (file.exists()) {
        System.err.println(file.getAbsolutePath() + " still exists");
        success = false;
      }
      else {
        System.err.println(file.getAbsolutePath() + " does not exist!");
      }
      if (backupFile.exists()) {
        System.err.println(backupFile.getAbsolutePath() + " still exists!");
      }
      else {
        System.err.println(backupFile.getAbsolutePath() + " does not exist");
      }
      System.err.println("Unable to rename log file to: "
          + backupFile.getAbsolutePath());
      StringBuffer message = new StringBuffer("Unable to rename "
          + file.getAbsolutePath() + " to " + file.getAbsolutePath());
      if (Utilities.isWindowsOS()) {
        message
            .append("\nIf either of these files is open in 3dmod, close 3dmod.");
      }
      throw new BackupException(this, backupId, message.toString());
    }
    //reset the File variables sinces the file names may have changed.
    file = null;
    backupFile = null;
    try {
      lock.unlock(LockType.BACKUP, backupId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    return success;
  }

  public long openWriter() throws WriteException {
    return openForWriting(true);
  }

  /**
   * Run open(long) with no wait limit.  This function can cause deadlock.
   * @see waitForLock()
   * @return
   */
  public long openForWriting() {
    long writeId = NO_ID;
    try {
      writeId = openForWriting(false);
    }
    catch (WriteException e) {
      e.printStackTrace();
    }
    return writeId;
  }

  private synchronized long openForWriting(boolean openWriter)
      throws WriteException {
    long writeId = NO_ID;
    try {
      writeId = lock.lock(LockType.WRITE);
    }
    catch (LockException e) {
      throw new WriteException(this, e);
    }
    if (openWriter) {
      try {
        createWriter();
      }
      catch (IOException e) {
        try {
          lock.unlock(LockType.WRITE, writeId);
        }
        catch (LockException e0) {
          e0.printStackTrace();
        }
        throw new WriteException(this, e);
      }
    }
    return writeId;
  }

  /**
   * Unlocks the open variable and closes the writer
   */
  public synchronized boolean closeForWriting(long writeId) {
    if (writerOpen) {
      new WriteException(this, writeId,
          "Must use closeWriter() when opened with openWriter()")
          .printStackTrace();
      return false;
    }
    try {
      lock.unlock(LockType.WRITE, writeId);
    }
    catch (LockException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public synchronized boolean closeWriter(long writeId) {
    //close the writer before unlocking
    try {
      lock.assertUnlockable(LockType.WRITE, writeId);
      closeWriter();
      lock.unlock(LockType.WRITE, writeId);
    }
    catch (IOException e) {
      e.printStackTrace();
      return false;
    }
    catch (LockException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public synchronized long openReader() throws ReadException {
    long readId = NO_ID;
    try {
      readId = lock.lock(LockType.READ);
    }
    catch (LockException e) {
      e.printStackTrace();
      throw new ReadException(this, e);
    }
    createFile();
    String idKey = ReaderList.makeKey(file, readId);
    createReaderList();
    try {
      readerList.openReader(idKey, file);
    }
    catch (FileNotFoundException e) {
      try {
        lock.unlock(LockType.READ, readId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw new ReadException(this, readId, e);
    }
    return readId;
  }

  public synchronized boolean closeReader(long readId) {
    //close the reader before unlocking
    try {
      lock.assertUnlockable(LockType.READ, readId);
      createFile();
      createReaderList();
      Reader reader = readerList.get(ReaderList.makeKey(file, readId));
      reader.close();
      lock.unlock(LockType.READ, readId);
    }
    catch (IOException e) {
      e.printStackTrace();
      return false;
    }
    catch (LockException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public synchronized String readLine(long readId) throws ReadException {
    if (!lock.isLocked(LockType.READ, readId)) {
      throw new ReadException(this, readId);
    }
    createFile();
    createReaderList();
    try {
      return readerList.get(ReaderList.makeKey(file, readId)).readLine();
    }
    catch (IOException e) {
      throw new ReadException(this, readId, e);
    }
  }

  public synchronized void write(String string, long writeId)
      throws WriteException {
    if (!lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    if (!writerOpen) {
      throw new WriteException(this, writeId);
    }
    try {
      bufferedWriter.write(string);
    }
    catch (IOException e) {
      throw new WriteException(this, writeId, e);
    }
  }

  public synchronized void newLine(long writeId) throws WriteException {
    if (!lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    if (!writerOpen) {
      throw new WriteException(this, writeId);
    }
    try {
      bufferedWriter.newLine();
    }
    catch (IOException e) {
      throw new WriteException(this, writeId, e);
    }
  }

  private void createFile() {
    if (file != null) {
      return;
    }
    file = new File(userDir, fileName);
  }

  private void createReaderList() {
    if (readerList != null) {
      return;
    }
    readerList = new ReaderList();
  }

  private void createBackupFile() {
    if (backupFile != null) {
      return;
    }
    backupFile = new File(userDir, fileName + DatasetFiles.BACKUP_CHAR);
  }

  private void createWriter() throws IOException {
    createFile();
    //check for and handle a partial create
    if (fileWriter == null || bufferedWriter == null) {
      closeWriter();
      fileWriter = null;
      bufferedWriter = null;
    }
    if (fileWriter == null) {
      fileWriter = new FileWriter(file);
      bufferedWriter = new BufferedWriter(fileWriter);
    }
    writerOpen = true;
  }

  private void closeWriter() throws IOException {
    if (bufferedWriter != null) {
      bufferedWriter.close();
    }
    if (fileWriter != null) {
      fileWriter.close();
    }
    writerOpen = false;
  }

  public synchronized boolean exists() {
    createFile();
    return file.exists();
  }

  public synchronized long lastModified() {
    createFile();
    return file.lastModified();
  }

  public synchronized String getAbsolutePath() {
    createFile();
    return file.getAbsolutePath();
  }

  public synchronized String getName() {
    createFile();
    return file.getName();
  }

  private static String makeKey(String userDir, String fileName) {
    return userDir + fileName;
  }

  /**
   * @return true if the open variabled is locked.
   */
  public synchronized boolean isOpen(LockType lockType, long id) {
    return lock.isLocked(lockType, id);
  }
  
  synchronized boolean noLocks()  {
    try {
    lock.assertNoLocks();}
    catch (LockException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public static final class LockType {
    public static final LockType READ = new LockType("read");
    public static final LockType WRITE = new LockType("write");
    public static final LockType BACKUP = new LockType("backup");

    private final String name;

    private LockType(String name) {
      this.name = name;
    }

    public String toString() {
      return name;
    }
  }

  public static final class ReadException extends Exception {
    ReadException(LogFile logFile, long id) {
      super("id=" + id + ",logFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
    }

    ReadException(LogFile logFile, Exception e) {
      super(e.toString() + "\nlogFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    ReadException(LogFile logFile, long id, Exception e) {
      super(e.toString() + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }
  }

  public static final class WriteException extends Exception {
    WriteException(LogFile logFile, long id) {
      super("id=" + id + ",logFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
    }

    WriteException(LogFile logFile, Exception e) {
      super(e.toString() + "\nlogFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    WriteException(LogFile logFile, long id, String message) {
      super(message + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
    }

    WriteException(LogFile logFile, long id, Exception e) {
      super(e.toString() + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }
  }

  public static final class BackupException extends Exception {
    BackupException(LogFile logFile, long id, Exception e) {
      super(e.toString() + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    BackupException(LogFile logFile, long id, String message) {
      super(message + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
    }
  }

  private static final class LockException extends Exception {
    LockException(Lock lock) {
      super("lock=" + lock);
    }

    LockException(Lock lock, LockType lockType) {
      super("lockType=" + lockType + ",lock=" + lock);
    }

    LockException(Lock lock, LockType lockType, long id) {
      super("lockType=" + lockType + ",id=" + id + ",lock=" + lock);
    }
  }

  private static final class Lock {
    private boolean locked = false;

    private long currentId = NO_ID;
    private HashMap readIdHashMap = null;
    private long writeId = NO_ID;
    private long backupId = NO_ID;

    private static String makeKey(long id) {
      return String.valueOf(id);
    }

    public String toString() {
      return "\n[readIdHashMap=" + readIdHashMap + ",\nwrite=Id=" + writeId
          + ",backupId=" + backupId + "]";
    }

    long lock(LockType lockType) throws LockException {
      assertLockable(lockType);
      //set the lock
      locked = true;
      //save the lock id in the variable matching the lock type
      //increment the current id
      if (++currentId < 0) {
        //catching overflow
        currentId = 0;
      }
      if (lockType == LockType.READ) {
        createReadIdHashMap();
        readIdHashMap.put(makeKey(currentId), null);
      }
      else if (lockType == LockType.WRITE) {
        writeId = currentId;
      }
      else {
        backupId = currentId;
      }
      return currentId;
    }

    void unlock(LockType lockType, long id) throws LockException {
      assertUnlockable(lockType, id);
      createReadIdHashMap();
      //unsetting the matching saved id
      String readKey = makeKey(id);
      if (lockType == LockType.READ && readIdHashMap.containsKey(readKey)) {
        readIdHashMap.remove(readKey);
      }
      else if (lockType == LockType.WRITE && id == writeId) {
        writeId = NO_ID;
      }
      else if (lockType == LockType.BACKUP && id == backupId) {
        backupId = NO_ID;
      }
      else {
        throw new LockException(this, lockType);
      }
      //turn off locked if all the saved ids are empty
      if (readIdHashMap.isEmpty() && writeId == NO_ID && backupId == NO_ID) {
        locked = false;
      }
      return;
    }

    boolean isLocked(LockType lockType, long id) {
      if (!locked || lockType == null || id == NO_ID) {
        return false;
      }
      createReadIdHashMap();
      return (lockType == LockType.READ && readIdHashMap
          .containsKey(makeKey(id)))
          || (lockType == LockType.WRITE && id == writeId)
          || (lockType == LockType.BACKUP && id == backupId);
    }

    boolean isLocked(LockType lockType) {
      if (!locked || lockType == null) {
        return false;
      }
      createReadIdHashMap();
      return (lockType == LockType.READ && !readIdHashMap.isEmpty())
          || (lockType == LockType.WRITE && writeId != NO_ID)
          || (lockType == LockType.BACKUP && backupId != NO_ID);
    }

    void assertNoLocks() throws LockException {
      if (locked) {
        throw new LockException(this);
      }
    }

    private void assertLockable(LockType lockType) throws LockException {
      if (!locked || lockType == null) {
        //succeed - not locked
        return;
      }
      //nothing else can be done during a backup
      //only one write can be done at a time
      if (lockType == LockType.BACKUP || backupId != NO_ID
          || (lockType == LockType.WRITE && writeId != NO_ID)) {
        throw new LockException(this, lockType);
      }
      //compatible:
      //multiple reads
      //a read and write (either can be started first)
    }

    void assertUnlockable(LockType lockType, long id) throws LockException {
      if (!locked) {
        throw new LockException(this, lockType, id);
      }
      createReadIdHashMap();
      if (readIdHashMap.isEmpty() && writeId == NO_ID && backupId == NO_ID) {
        throw new IllegalStateException(
            "Ids don't match the locked boolean:\nlocked=" + locked
                + ",readId=" + readIdHashMap.toString() + ",writeId=" + writeId
                + ",backup=" + backupId);
      }
      //checking for unlockability
      if ((lockType == LockType.READ && readIdHashMap.containsKey(makeKey(id)))
          || (lockType == LockType.WRITE && id == writeId)
          || (lockType == LockType.BACKUP && id == backupId)) {
        return;
      }
      throw new LockException(this, lockType, id);
    }

    private void createReadIdHashMap() {
      if (readIdHashMap == null) {
        readIdHashMap = new HashMap();
      }
    }
  }

  private static final class ReaderList {
    private final HashMap hashMap;
    private final ArrayList arrayList;

    ReaderList() {
      hashMap = new HashMap();
      arrayList = new ArrayList();
    }

    static String makeKey(File file, long id) {
      return file.getAbsolutePath() + String.valueOf(id);
    }

    synchronized Reader get(String key) {
      return (Reader) hashMap.get(key);
    }

    synchronized void openReader(String currentKey, File file)
        throws FileNotFoundException {
      Reader reader;
      for (int i = 0; i < arrayList.size(); i++) {
        reader = (Reader) arrayList.get(i);
        if (!reader.isOpen()) {
          //open the reader to get exclusive access to it
          try {
            reader.open();
          }
          catch (FileNotFoundException e) {
            throw new FileNotFoundException(e.getMessage() + "\ncurrentKey="
                + currentKey);
          }
          //Found a closed reader, so reuse it
          //get the old key from the reader and rekey the reader in the hash map
          //with the current key
          String oldKey = reader.getKey();
          hashMap.remove(oldKey);
          reader.setKey(currentKey);
          hashMap.put(currentKey, reader);

        }
      }
      //Can't find a closed reader, so create a new one
      reader = new Reader(file);
      //open the reader to get exclusive access to it
      reader.open();
      //store the current key in the reader and store it in the array list and
      //hash map
      reader.setKey(currentKey);
      hashMap.put(currentKey, reader);
      arrayList.add(reader);
    }

    synchronized Reader getReader(String key) {
      return (Reader) hashMap.get(key);
    }
  }

  private static final class Reader {
    private boolean open = true;

    private final File file;

    private FileReader fileReader = null;
    private BufferedReader bufferedReader = null;
    private String key = null;

    Reader(File file) {
      this.file = file;
    }

    void open() throws FileNotFoundException {
      if (fileReader == null) {
        try {
          fileReader = new FileReader(file.getAbsolutePath());
        }
        catch (FileNotFoundException e) {
          throw new FileNotFoundException(e.getMessage() + "\nfile="
              + file.getAbsolutePath());
        }
      }
      if (bufferedReader == null) {
        bufferedReader = new BufferedReader(fileReader);
      }
      open = true;
    }

    void close() throws IOException {
      if (fileReader != null) {
        fileReader.close();
      }
      if (bufferedReader != null) {
        bufferedReader.close();
      }
      open = false;
    }

    String readLine() throws IOException {
      return bufferedReader.readLine();
    }

    boolean isOpen() {
      return open;
    }

    void setKey(String key) {
      this.key = key;
    }

    String getKey() {
      return key;
    }
  }
}
/**
 * <p> $Log$ </p>
 */
