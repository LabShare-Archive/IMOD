package etomo.storage;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Properties;

import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: Class which controls the opening and closing of a log file.
 * This class is necessary because working with log files on Windows has
 * become unreliable and a separate class was needed to ensure that the log
 * files are not left open.
 * 
 * This class contains a semaphore called Lock, which contains three types of
 * locks:  Read, Write, and File.  File is the most exclusive.  Only one File
 * lock can exists at a time and it can't coexist with any other kind of lock.  
 * Only one Write lock can exist at a time, but it can coexist with Read locks.
 * Multiple Read locks can exist at a time.
 * 
 * LogFile is an N'ton and stores its instances in a Hashtable, which is a
 * synchronized class.
 * 
 * Log File is a syncrhonized class:  all of its public and package-level
 * functions are synchronized, except getInstance functions (createInstance() is
 * synchronized).</p>
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
  private static final String PUBLIC_EXCEPTION_MESSAGE = "\nPlease make all copy "
      + "of the current etomo_err.log file and inform the software developer.";

  private static final Hashtable logFileHashTable = new Hashtable();

  private final Lock lock;
  private ReadingTokenList readerList = ReadingTokenList.getReaderInstance();
  private ReadingTokenList readingTokenList = ReadingTokenList
      .getReadingTokenInstance();
  private final String fileAbsolutePath;

  private File file = null;
  private File backupFile = null;
  private FileWriter fileWriter = null;
  private BufferedWriter bufferedWriter = null;
  private FileInputStream inputStream = null;
  private FileOutputStream outputStream = null;
  private boolean backedUp = false;

  private LogFile(File file) {
    lock = new Lock(this);
    this.fileAbsolutePath = file.getAbsolutePath();
  }

  public String toString() {
    return "\n[fileAbsolutePath=" + fileAbsolutePath + ",lock=" + lock + "]";
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
      ProcessName processName) throws FileException {
    return getInstance(userDir, axisID, processName.toString());
  }

  public static LogFile getInstance(String userDir, AxisID axisID, String name)
      throws FileException {
    return getInstance(userDir, name + axisID.getExtension()
        + DatasetFiles.LOG_EXT);
  }

  public static LogFile getInstance(String userDir, String fileName)
      throws FileException {
    return getInstance(new File(userDir, fileName));
  }

  public static LogFile getInstance(File dir, String fileName)
      throws FileException {
    return getInstance(new File(dir, fileName));
  }

  public static LogFile getInstance(File file) throws FileException {
    if (file == null) {
      throw new FileException("Cannot create LogFile, file is null.");
    }
    LogFile logFile;
    String key = file.getAbsolutePath();
    if ((logFile = (LogFile) logFileHashTable.get(key)) == null) {
      //the instance doesn't exist - create it
      logFile = createInstance(file, key);
    }
    return logFile;
  }

  /**
   * For testing.  Removes all instances of LogFile.
   */
  synchronized static void reset() {
    logFileHashTable.clear();
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
  private static synchronized LogFile createInstance(File file, String key) {
    LogFile logFile;
    //make sure that the instance wasn't created by another thread
    if (logFileHashTable != null
        && (logFile = (LogFile) logFileHashTable.get(key)) != null) {
      return logFile;
    }
    //create the instance
    logFile = new LogFile(file);
    //save the instance
    logFileHashTable.put(key, logFile);
    return logFile;
  }

  /**
   * Try to do a backup and set backedUp to true.  This prevents more then one backup to
   * be done on the file during the lifetime of the instance.  This prevents the
   * loss of data from a previous session because of too many backups being
   * done.
   * If backup() throws an exception, set backedUp to false and rethrow the
   * exception.
   * @return true if backup() was called and was successful
   * @throws FileException
   */
  public synchronized boolean backupOnce() throws FileException {
    if (backedUp) {
      return false;
    }
    boolean backupResult = false;
    try {
      backupResult = backup();
      backedUp = true;
    }
    catch (FileException e) {
      throw e;
    }
    return backupResult;
  }

  /**
   * Delete the current backup file and rename the current file to be the new
   * backup file.  The current backup file will not be deleted unless the
   * current file exists.
   * Will not backup if backedUp is true (doesn't set backedUp)
   * @return true if backup() successful or already backed up
   * @throws FileException
   */
  public synchronized boolean backup() throws FileException {
    if (backedUp) {
      return false;
    }
    createFile();
    if (!file.exists()) {
      return false;
    }
    long fileId = NO_ID;
    try {
      fileId = lock.lock(LockType.FILE);
    }
    catch (LockException e) {
      throw new FileException(fileId, e);
    }
    createBackupFile();
    if (!file.exists()) {
      //nothing to backup
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        //Don't throw a file exception because the error didn't affect the
        //backup.
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
          + file.getAbsolutePath() + " to " + backupFile.getAbsolutePath());
      if (lock.isThrowException()) {
        message
            .append("\nIf either of these files is open in 3dmod, close 3dmod.");
      }
      throw new FileException(this, fileId, message.toString());
    }
    //reset the File variables sinces the file names may have changed.
    file = null;
    backupFile = null;
    try {
      lock.unlock(LockType.FILE, fileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    return success;
  }

  /**
   * Creates the file if it doesn't already exist.
   * @return
   * @throws FileException
   */
  public synchronized boolean create() throws FileException {
    createFile();
    if (file.exists()) {
      return false;
    }
    long fileId = NO_ID;
    try {
      fileId = lock.lock(LockType.FILE);
    }
    catch (LockException e) {
      throw new FileException(fileId, e);
    }
    if (file.exists()) {
      //nothing to create
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        //Don't throw a file exception because the error didn't affect the
        //create.
        e.printStackTrace();
      }
      return false;
    }
    try {
      file.createNewFile();
    }
    catch (IOException e) {
      throw new FileException(e);
    }
    boolean success = file.exists();
    try {
      lock.unlock(LockType.FILE, fileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    if (success) {
      file = null;
      return true;
    }
    String path = file.getAbsolutePath();
    file = null;
    throw new FileException(this, fileId, "Unable to create " + path);
  }

  public synchronized boolean delete() throws FileException {
    createFile();
    if (!file.exists()) {
      return false;
    }
    long fileId = NO_ID;
    try {
      fileId = lock.lock(LockType.FILE);
    }
    catch (LockException e) {
      throw new FileException(fileId, e);
    }
    if (!file.exists()) {
      //nothing to delete
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        //Don't throw a file exception because the error didn't affect the
        //delete.
        e.printStackTrace();
      }
      return false;
    }
    file.delete();
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    boolean success = !file.exists();
    try {
      lock.unlock(LockType.FILE, fileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    if (success) {
      file = null;
      return true;
    }
    String path = file.getAbsolutePath();
    file = null;
    throw new FileException(this, fileId, "Unable to delete " + path);
  }

  public synchronized boolean move(LogFile target) throws FileException {
    createFile();
    if (!file.exists()) {
      return false;
    }
    long fileId = NO_ID;
    try {
      fileId = lock.lock(LockType.FILE);
    }
    catch (LockException e) {
      throw new FileException(fileId, e);
    }
    if (!file.exists()) {
      //nothing to move
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        //Don't throw a file exception because the error didn't affect the
        //move.
        e.printStackTrace();
      }
      return false;
    }
    try {
      target.backup();
    }
    catch (FileException backupException) {
      //unable to backup
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        //Don't throw a file exception because the error didn't affect the
        //move.
        e.printStackTrace();
      }

      throw backupException;
    }
    //need to get a file lock on the target for this operation
    long targetFileId = NO_ID;
    try {
      targetFileId = target.lock.lock(LockType.FILE);
    }
    catch (LockException e) {
      throw new FileException(targetFileId, e);
    }
    target.createFile();
    file.renameTo(target.file);
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    boolean success = !file.exists();
    try {
      lock.unlock(LockType.FILE, fileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    //unlock target file lock
    try {
      target.lock.unlock(LockType.FILE, targetFileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    if (success) {
      file = null;
      return true;
    }
    String path = file.getAbsolutePath();
    file = null;
    throw new FileException(this, fileId, "Unable to delete " + path);
  }

  public synchronized long openWriter() throws WriteException {
    return openForWriting(true);
  }

  /**
   * Run open(long) with no wait limit.  This function can cause deadlock.
   * @see waitForLock()
   * @return
   */
  public synchronized long openForWriting() {
    long writeId = NO_ID;
    try {
      writeId = openForWriting(false);
    }
    catch (WriteException e) {
      e.printStackTrace();
    }
    return writeId;
  }

  private long openForWriting(boolean openWriter) throws WriteException {
    long writeId = NO_ID;
    try {
      writeId = lock.lock(LockType.WRITE);
    }
    catch (LockException e) {
      throw new WriteException(e);
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
        throw new WriteException(e);
      }
    }
    return writeId;
  }

  /**
   * Opens the input stream.  Although this is a reader, it needs to exclude
   * writers because it is used to read the entire file.  So I'm using the
   * writer lock for now.  If more then one input stream must be opened at a
   * time, I will add a new input stream lock that will allow multiple input
   * streams (and readers), but no writers.
   * @return
   * @throws InputStreamException
   */
  public synchronized long openInputStream() throws WriteException {
    long writeId = NO_ID;
    try {
      writeId = lock.lock(LockType.WRITE);
    }
    catch (LockException e) {
      throw new WriteException(e);
    }
    try {
      createInputStream();
    }
    catch (IOException e) {
      try {
        lock.unlock(LockType.WRITE, writeId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw new WriteException(e);
    }
    return writeId;
  }

  /**
   * Opens the output stream.  Locks the WRITE lock and returns a writeId.
   * @return
   * @throws WriteException
   */
  public synchronized long openOutputStream() throws WriteException {
    long writeId = NO_ID;
    try {
      writeId = lock.lock(LockType.WRITE);
    }
    catch (LockException e) {
      throw new WriteException(e);
    }
    try {
      createOutputStream();
    }
    catch (IOException e) {
      try {
        lock.unlock(LockType.WRITE, writeId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw new WriteException(e);
    }
    return writeId;
  }

  public synchronized boolean closeInputStream(long writeId) {
    if (fileWriter != null) {
      new WriteException(this, writeId,
          "Must use closeWriter() when opened with openWriter()")
          .printStackTrace();
      return false;
    }
    if (outputStream != null) {
      new WriteException(this, writeId,
          "Must use closeOutputStream() when opened with openOutputStream()")
          .printStackTrace();
      return false;
    }
    if (inputStream == null) {
      new WriteException(this, writeId,
          "Must use closeForWriting() when opened with openForWriting()")
          .printStackTrace();
      return false;
    }
    //close the input stream before unlocking
    try {
      lock.assertUnlockable(LockType.WRITE, writeId);
      closeInputStream();
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

  public synchronized boolean closeOutputStream(long writeId) {
    if (fileWriter != null) {
      new WriteException(this, writeId,
          "Must use closeWriter() when opened with openWriter()")
          .printStackTrace();
      return false;
    }
    if (inputStream != null) {
      new WriteException(this, writeId,
          "Must use closeInputStream() when opened with openInputStream()")
          .printStackTrace();
      return false;
    }
    if (outputStream == null) {
      new WriteException(this, writeId,
          "Must use closeForWriting() when opened with openForWriting()")
          .printStackTrace();
      return false;
    }
    //close the input stream before unlocking
    try {
      lock.assertUnlockable(LockType.WRITE, writeId);
      closeOutputStream();
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

  /**
   * Unlocks the open variable and closes the writer
   */
  public synchronized boolean closeForWriting(long writeId) {
    if (fileWriter != null) {
      new WriteException(this, writeId,
          "Must use closeWriter() when opened with openWriter()")
          .printStackTrace();
      return false;
    }
    if (inputStream != null) {
      new WriteException(this, writeId,
          "Must use closeInputStream() when opened with openInputStream()")
          .printStackTrace();
      return false;
    }
    if (outputStream != null) {
      new WriteException(this, writeId,
          "Must use closeOutputStream() when opened with openOutputStream()")
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
    if (inputStream != null) {
      new WriteException(this, writeId,
          "Must use closeInputStream() when opened with openInputStream()")
          .printStackTrace();
      return false;
    }
    if (outputStream != null) {
      new WriteException(this, writeId,
          "Must use closeOutputStream() when opened with openOutputStream()")
          .printStackTrace();
      return false;
    }
    if (fileWriter == null) {
      new WriteException(this, writeId,
          "Must use closeForWriting() when opened with openForWriting()")
          .printStackTrace();
      return false;
    }
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
      throw new ReadException(e);
    }
    createFile();
    String idKey = ReadingTokenList.makeKey(readId);
    try {
      readerList.openReadingToken(idKey, file);
    }
    catch (FileNotFoundException e) {
      try {
        lock.unlock(LockType.READ, readId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw new ReadException(readId, e);
    }
    return readId;
  }

  public synchronized long openForReading() throws ReadException {
    long readId = NO_ID;
    try {
      readId = lock.lock(LockType.READ);
    }
    catch (LockException e) {
      e.printStackTrace();
      throw new ReadException(e);
    }
    createFile();
    String idKey = ReadingTokenList.makeKey(readId);
    try {
      readingTokenList.openReadingToken(idKey, file);
    }
    catch (FileNotFoundException e) {
      try {
        lock.unlock(LockType.READ, readId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw new ReadException(readId, e);
    }
    return readId;
  }

  public synchronized boolean closeReader(long readId) {
    //close the reader before unlocking
    try {
      lock.assertUnlockable(LockType.READ, readId);
      createFile();
      ReadingToken readingToken = readerList.getReadingToken(ReadingTokenList
          .makeKey(readId));
      readingToken.close();
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

  public synchronized boolean closeForReading(long readId) {
    //close the reading token before unlocking
    try {
      lock.assertUnlockable(LockType.READ, readId);
      createFile();
      ReadingToken readingToken = readingTokenList
          .getReadingToken(ReadingTokenList.makeKey(readId));
      readingToken.close();
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
    try {
      return readerList.getReader(ReadingTokenList.makeKey(readId)).readLine();
    }
    catch (IOException e) {
      throw new ReadException(readId, e);
    }
  }

  public synchronized void load(Properties properties, long writeId)
      throws WriteException {
    if (inputStream == null || !lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    try {
      properties.load(inputStream);
    }
    catch (IOException e) {
      throw new WriteException(writeId, e);
    }
  }

  public synchronized void store(Properties properties, long writeId)
      throws WriteException {
    if (outputStream == null || !lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    try {
      properties.store(outputStream, null);
    }
    catch (IOException e) {
      throw new WriteException(writeId, e);
    }
  }

  public synchronized void write(String string, long writeId)
      throws WriteException {
    if (string == null) {
      return;
    }
    if (fileWriter == null || !lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    try {
      bufferedWriter.write(string);
    }
    catch (IOException e) {
      throw new WriteException(writeId, e);
    }
  }

  public synchronized void write(char ch, long writeId) throws WriteException {
    if (fileWriter == null || !lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    try {
      bufferedWriter.write(ch);
    }
    catch (IOException e) {
      throw new WriteException(writeId, e);
    }
  }

  public synchronized void write(Character ch, long writeId)
      throws WriteException {
    write(ch.charValue(), writeId);
  }

  public synchronized void newLine(long writeId) throws WriteException {
    if (fileWriter == null || !lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    try {
      bufferedWriter.newLine();
    }
    catch (IOException e) {
      throw new WriteException(writeId, e);
    }
  }

  public synchronized void flush(long writeId) throws WriteException {
    if (!lock.isLocked(LockType.WRITE, writeId)) {
      throw new WriteException(this, writeId);
    }
    try {
      bufferedWriter.flush();
    }
    catch (IOException e) {
      throw new WriteException(writeId, e);
    }
    catch (NullPointerException e) {
      throw new WriteException(writeId,
          "Must open with openWriter() to be able to call flush().", e);
    }
  }

  private void createFile() {
    if (file != null) {
      return;
    }
    file = new File(fileAbsolutePath);
  }

  private void createBackupFile() {
    if (backupFile != null) {
      return;
    }
    backupFile = new File(fileAbsolutePath + DatasetFiles.BACKUP_CHAR);
  }

  private void createWriter() throws IOException {
    createFile();
    try {
      if (fileWriter == null) {
        fileWriter = new FileWriter(file);
        bufferedWriter = new BufferedWriter(fileWriter);
      }
    }
    catch (IOException e) {
      try {
        closeWriter();
      }
      catch (IOException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
  }

  private void createInputStream() throws IOException {
    createFile();
    try {
      if (inputStream == null) {
        inputStream = new FileInputStream(file);
      }
    }
    catch (IOException e) {
      try {
        closeInputStream();
      }
      catch (IOException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
  }

  private void createOutputStream() throws IOException {
    createFile();
    try {
      if (outputStream == null) {
        outputStream = new FileOutputStream(file);
      }
    }
    catch (IOException e) {
      try {
        closeOutputStream();
      }
      catch (IOException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
  }

  private void closeWriter() throws IOException {
    if (bufferedWriter != null) {
      bufferedWriter.close();
      bufferedWriter = null;
    }
    if (fileWriter != null) {
      fileWriter.close();
      fileWriter = null;
    }
  }

  private void closeInputStream() throws IOException {
    if (inputStream != null) {
      inputStream.close();
      inputStream = null;
    }
  }

  private void closeOutputStream() throws IOException {
    if (outputStream != null) {
      outputStream.close();
      outputStream = null;
    }
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

  /**
   * @return true if the open variabled is locked.
   */
  public synchronized boolean isOpen(LockType lockType, long id) {
    return lock.isLocked(lockType, id);
  }

  synchronized boolean noLocks() {
    try {
      lock.assertNoLocks();
    }
    catch (LockException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  static final class LockType {
    static final LockType READ = new LockType("read");
    static final LockType WRITE = new LockType("write");
    static final LockType FILE = new LockType("file");

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

    ReadException(Exception e) {
      super(e.toString() + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    ReadException(long id, Exception e) {
      super(e.toString() + "\nid=" + id + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }
  }

  public static class WriteException extends Exception {
    WriteException(LogFile logFile, long id) {
      super("id=" + id + ",logFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
    }

    WriteException(Exception e) {
      super(e.toString() + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    WriteException(LogFile logFile, long id, String message) {
      super(message + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
    }

    WriteException(long id, Exception e) {
      super(e.toString() + "\nid=" + id + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    WriteException(long id, String message, Exception e) {
      super(message + '\n' + e.toString() + "\nid=" + id
          + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }
  }

  public static final class FileException extends Exception {
    FileException(long id, Exception e) {
      super(e.toString() + "\nid=" + id + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    FileException(LogFile logFile, long id, String message) {
      super(message + "\nid=" + id + ",logFile=" + logFile
          + PUBLIC_EXCEPTION_MESSAGE);
    }

    FileException(Exception e) {
      super(e.toString() + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    FileException(String message) {
      super(message + PUBLIC_EXCEPTION_MESSAGE);
    }
  }

  private static final class LockException extends Exception {
    LockException(LogFile logFile) {
      super("logFile=" + logFile);
    }

    LockException(LogFile logFile, LockType lockType) {
      super("lockType=" + lockType + ",logFile=" + logFile);
    }

    LockException(LogFile logFile, LockType lockType, long id) {
      super("lockType=" + lockType + ",id=" + id + ",logFile=" + logFile);
    }
  }

  private static final class Lock {
    private final HashMap readIdHashMap = new HashMap();
    private final boolean throwException;
    private final LogFile logFile;

    private boolean warningDisplayed = false;
    private boolean locked = false;
    private long currentId = NO_ID;
    private long writeId = NO_ID;
    private long fileId = NO_ID;

    private Lock(final LogFile logFile) {
      this.logFile = logFile;
      throwException = Utilities.isWindowsOS()
          || EtomoDirector.INSTANCE.getArguments().isTest();
    }

    private static String makeKey(final long id) {
      return String.valueOf(id);
    }

    private boolean isThrowException() {
      return throwException;
    }

    public String toString() {
      return "\n[readIdHashMap=" + readIdHashMap + ",\nwriteId=" + writeId
          + ",fileId=" + fileId + "]";
    }

    private long lock(final LockType lockType) throws LockException {
      assertLockable(lockType);
      //set the lock
      locked = true;
      //save the lock id in the variable matching the lock type
      //increment the current id
      if (++currentId < 0) {
        System.err
            .println("LogFile overflow - setting currentId to zero:currentId="
                + currentId);
        //catching overflow
        currentId = 0;
      }
      if (lockType == LockType.READ) {
        readIdHashMap.put(makeKey(currentId), null);
      }
      else if (lockType == LockType.WRITE) {
        writeId = currentId;
      }
      else {
        fileId = currentId;
      }
      return currentId;
    }

    private void unlock(final LockType lockType, final long id)
        throws LockException {
      assertUnlockable(lockType, id);
      //unsetting the matching saved id
      String readKey = makeKey(id);
      if (lockType == LockType.READ && readIdHashMap.containsKey(readKey)) {
        readIdHashMap.remove(readKey);
      }
      else if (lockType == LockType.WRITE && id == writeId) {
        writeId = NO_ID;
      }
      else if (lockType == LockType.FILE && id == fileId) {
        fileId = NO_ID;
      }
      else {
        LockException e = new LockException(logFile, lockType);
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          UIHarness.INSTANCE.openMessageDialog(e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
      //turn off locked if all the saved ids are empty
      if (readIdHashMap.isEmpty() && writeId == NO_ID && fileId == NO_ID) {
        locked = false;
      }
      return;
    }

    private boolean isLocked(final LockType lockType, final long id) {
      if (!locked || lockType == null || id == NO_ID) {
        return false;
      }
      return (lockType == LockType.READ && readIdHashMap
          .containsKey(makeKey(id)))
          || (lockType == LockType.WRITE && id == writeId)
          || (lockType == LockType.FILE && id == fileId);
    }

    private boolean isLocked(final LockType lockType) {
      if (!locked || lockType == null) {
        return false;
      }
      return (lockType == LockType.READ && !readIdHashMap.isEmpty())
          || (lockType == LockType.WRITE && writeId != NO_ID)
          || (lockType == LockType.FILE && fileId != NO_ID);
    }

    private void assertNoLocks() throws LockException {
      if (locked) {
        LockException e = new LockException(logFile);
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          UIHarness.INSTANCE.openMessageDialog(e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
    }

    private void assertLockable(final LockType lockType) throws LockException {
      if (!locked || lockType == null) {
        //succeed - not locked
        return;
      }
      //nothing else can be done during a file lock
      //only one write can be done at a time
      if (lockType == LockType.FILE || fileId != NO_ID
          || (lockType == LockType.WRITE && writeId != NO_ID)) {
        LockException e = new LockException(logFile, lockType);
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          UIHarness.INSTANCE.openMessageDialog(e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
      //compatible:
      //multiple reads
      //a read and write (either can be started first)
    }

    private void assertUnlockable(final LockType lockType, final long id)
        throws LockException {
      LockException e = new LockException(logFile, lockType, id);
      if (!locked) {
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          UIHarness.INSTANCE.openMessageDialog(e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
      if (readIdHashMap.isEmpty() && writeId == NO_ID && fileId == NO_ID) {
        throw new IllegalStateException(
            "Ids don't match the locked boolean:\nlocked=" + locked
                + ",readId=" + readIdHashMap.toString() + ",writeId=" + writeId
                + ",fileId=" + fileId);
      }
      //checking for unlockability
      if ((lockType == LockType.READ && readIdHashMap.containsKey(makeKey(id)))
          || (lockType == LockType.WRITE && id == writeId)
          || (lockType == LockType.FILE && id == fileId)) {
        return;
      }
      e = new LockException(logFile, lockType, id);
      if (throwException) {
        throw e;
      }
      if (!warningDisplayed) {
        warningDisplayed = true;
        UIHarness.INSTANCE.openMessageDialog(e.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
      }
    }

    private long getWriteId() {
      return writeId;
    }
  }

  private static final class ReadingTokenList {
    private final HashMap hashMap = new HashMap();
    private final ArrayList arrayList = new ArrayList();
    private final boolean storeReaders;

    static ReadingTokenList getReadingTokenInstance() {
      return new ReadingTokenList(false);
    }

    static ReadingTokenList getReaderInstance() {
      return new ReadingTokenList(true);
    }

    private ReadingTokenList(boolean storeReaders) {
      this.storeReaders = storeReaders;
    }

    static String makeKey(long id) {
      return String.valueOf(id);
    }

    synchronized ReadingToken getReadingToken(String key) {
      return (ReadingToken) hashMap.get(key);
    }

    synchronized Reader getReader(String key) {
      if (!storeReaders) {
        return null;
      }
      return (Reader) getReadingToken(key);
    }

    synchronized void openReadingToken(String currentKey, File file)
        throws FileNotFoundException {
      ReadingToken readingToken;
      for (int i = 0; i < arrayList.size(); i++) {
        readingToken = (ReadingToken) arrayList.get(i);
        if (!readingToken.isOpen()) {
          //open the reader to get exclusive access to it
          try {
            readingToken.open();
          }
          catch (FileNotFoundException e) {
            throw new FileNotFoundException(e.getMessage() + "\ncurrentKey="
                + currentKey);
          }
          //Found a closed reader, so reuse it
          //get the old key from the reader and rekey the reader in the hash map
          //with the current key
          String oldKey = readingToken.getKey();
          hashMap.remove(oldKey);
          readingToken.setKey(currentKey);
          hashMap.put(currentKey, readingToken);

        }
      }
      //Can't find a closed reader, so create a new one
      readingToken = newReadingToken(file);
      //open the reader to get exclusive access to it
      readingToken.open();
      //store the current key in the reader and store it in the array list and
      //hash map
      readingToken.setKey(currentKey);
      hashMap.put(currentKey, readingToken);
      arrayList.add(readingToken);
    }

    private ReadingToken newReadingToken(File file) {
      if (storeReaders) {
        return new Reader(file);
      }
      return new ReadingToken();
    }
  }

  private static class ReadingToken {
    private boolean open = true;
    private String key = null;

    void open() throws FileNotFoundException {
      open = true;
    }

    void close() throws IOException {
      open = false;
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

  private static final class Reader extends ReadingToken {
    private final File file;

    private FileReader fileReader = null;
    private BufferedReader bufferedReader = null;

    Reader(File file) {
      this.file = file;
    }

    void open() throws FileNotFoundException {
      if (fileReader == null) {
        fileReader = new FileReader(file.getAbsolutePath());
      }
      if (bufferedReader == null) {
        bufferedReader = new BufferedReader(fileReader);
      }
      super.open();
    }

    void close() throws IOException {
      if (fileReader != null) {
        fileReader.close();
      }
      if (bufferedReader != null) {
        bufferedReader.close();
      }
      super.close();
    }

    String readLine() throws IOException {
      return bufferedReader.readLine();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.18  2008/01/14 22:00:44  sueh
 * <p> bug# 1050 Added getInstance(File,String).
 * <p>
 * <p> Revision 1.17  2007/12/26 22:15:55  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.16  2007/11/06 19:29:46  sueh
 * <p> bug# 1047 Added comment.
 * <p>
 * <p> Revision 1.15  2007/09/07 00:23:12  sueh
 * <p> bug# 989 Only throw LockException when necessary.  Added boolean
 * <p> Lock.throwException, which is true when a windows OS is used or unit tests are
 * <p> being run.  When throwException is true, pop up a warning instead of throughing
 * <p> an exception.  Added boolean Lock.warningDisplayed, to prevent a warning from
 * <p> popping up multiple times per file.
 * <p>
 * <p> Revision 1.14  2007/09/05 17:08:49  sueh
 * <p> bug# 989 In Lock popping up a message (only once per logfile instance) instead
 * <p> of throwing an exception, except when using Windows.  Improved the
 * <p> LockException error message, since it is used in the popup.
 * <p>
 * <p> Revision 1.13  2007/08/01 22:42:28  sueh
 * <p> bug# 985 Changed the type of AutodocTokenizer.OPEN_CHAR and
 * <p> CLOSE_CHAR to Character.
 * <p>
 * <p> Revision 1.12  2007/07/18 23:20:16  sueh
 * <p> bug# 1025 Fixed a null pointer exception in write(String,long).
 * <p>
 * <p> Revision 1.11  2007/03/23 20:24:52  sueh
 * <p> bug# 964 Added write(char, long).
 * <p>
 * <p> Revision 1.10  2007/03/01 01:14:23  sueh
 * <p> bug# 964 Added openForReading and closeForReading.  Made reader list non-
 * <p> static for simplicity.
 * <p>
 * <p> Revision 1.9  2007/02/05 23:04:25  sueh
 * <p> bug# 962 Added move.
 * <p>
 * <p> Revision 1.8  2006/11/15 20:03:13  sueh
 * <p> bug# 872 Added backupOnce, input stream and output stream management,
 * <p> create, load(), and store().
 * <p>
 * <p> Revision 1.7  2006/10/16 22:46:54  sueh
 * <p> bug# 919  Reader.open():  Simplifying new FileReader error handling.
 * <p>
 * <p> Revision 1.6  2006/10/13 22:29:48  sueh
 * <p> bug# 931 Making LockType package level, since its no used outside of LogFile.
 * <p>
 * <p> Revision 1.5  2006/10/12 10:41:16  sueh
 * <p> bug# 931 Sleeping longer in delete because windows is slow.
 * <p>
 * <p> Revision 1.4  2006/10/12 03:19:56  sueh
 * <p> bug# 931 In newLine() an write() throw WriteException instead of
 * <p> NullPointerException when the writer is not open.
 * <p>
 * <p> Revision 1.3  2006/10/11 10:12:05  sueh
 * <p> bug# 931 Added delete functionality to LogFile - changed BackupException to
 * <p> FileException.
 * <p>
 * <p> Revision 1.2  2006/10/10 07:44:20  sueh
 * <p> bug# 931 When BufferedWriter.close() is called, the instance can't be
 * <p> reopened, so don't preserve the buffered writer instance.
 * <p>
 * <p> Revision 1.1  2006/10/10 05:18:57  sueh
 * <p> Bug# 931 Class to manage log files.  Prevents access that would violate  Windows file locking.  Handles backups, reading, and writing.  Also can use to
 * <p> prevent access while another process is writing to the file.
 * <p> </p>
 */
