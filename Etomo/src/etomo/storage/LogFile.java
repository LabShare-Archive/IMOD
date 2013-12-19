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
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Properties;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.ui.swing.UIHarness;
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

  private static final int NO_ID = -1;
  public static final int NO_WAIT_LIMIT = -1;
  private static final String PUBLIC_EXCEPTION_MESSAGE = "\nPlease make a copy "
      + "of the current etomo_err.log file and inform the software developer.";

  private static final Hashtable logFileHashTable = new Hashtable();

  private final Lock lock;
  private ReadingTokenList readingTokenList = new ReadingTokenList();
  private final String fileAbsolutePath;

  private File file = null;
  private File backupFile = null;
  private File doubleBackupFile = null;
  private FileWriter fileWriter = null;
  private BufferedWriter bufferedWriter = null;
  private FileInputStream inputStream = null;
  private FileOutputStream outputStream = null;
  private boolean backedUp = false;
  private boolean debug = false;

  public void dumpState() {
    System.err.println("[fileAbsolutePath:" + fileAbsolutePath + ",file:");
    if (file != null) {
      System.err.println(file.getAbsolutePath());
    }
    System.err.println(",backupFile:");
    if (backupFile != null) {
      System.err.println(backupFile.getAbsolutePath());
    }
    System.err.println(",backedUp:" + backedUp + ",debug:" + debug + "]");
  }

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
  public static LogFile getInstance(String userDir, AxisID axisID, ProcessName processName)
      throws LockException {
    return getInstance(userDir, axisID, processName.toString());
  }

  public static LogFile getInstance(String userDir, AxisID axisID, String name)
      throws LockException {
    return getInstance(userDir, name + axisID.getExtension() + DatasetFiles.LOG_EXT);
  }

  public static LogFile getInstance(String userDir, String fileName) throws LockException {
    return getInstance(new File(userDir, fileName));
  }

  public static LogFile getInstance(File dir, String fileName) throws LockException {
    return getInstance(new File(dir, fileName));
  }

  public static LogFile getInstance(File file) throws LockException {
    if (file == null) {
      LockException e = new LockException("Cannot create LogFile, file is null.");
      e.printStackTrace();
      throw e;
    }
    LogFile logFile;
    String key = file.getAbsolutePath();
    if ((logFile = (LogFile) logFileHashTable.get(key)) == null) {
      // the instance doesn't exist - create it
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
    // make sure that the instance wasn't created by another thread
    if (logFileHashTable != null
        && (logFile = (LogFile) logFileHashTable.get(key)) != null) {
      return logFile;
    }
    // create the instance
    logFile = new LogFile(file);
    // save the instance
    logFileHashTable.put(key, logFile);
    return logFile;
  }

  public static String getLineContaining(final File file, final String searchString) {
    LogFile logFile = null;
    ReaderId id = null;
    try {
      logFile = getInstance(file);
      id = logFile.openReader();
      if (id != null && !id.isEmpty()) {
        String line = logFile.getLineContaining(id, searchString);
        logFile.closeRead(id);
        return line;
      }
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    if (logFile != null && id != null && !id.isEmpty()) {
      logFile.closeRead(id);
    }
    return null;
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
  public synchronized boolean backupOnce() throws LockException {
    if (backedUp) {
      return false;
    }
    boolean backupResult = false;
    backupResult = backup();
    backedUp = true;
    return backupResult;
  }

  public boolean isBackedup() {
    return backedUp;
  }

  public synchronized boolean doubleBackupOnce() throws LockException {
    if (backedUp) {
      return false;
    }
    boolean backupResult = false;
    doubleBackup();
    backupResult = backup();
    backedUp = true;
    return backupResult;
  }

  private void doubleBackup() throws LockException {
    createBackupFile();
    createDoubleBackupFile();
    backup(backupFile, doubleBackupFile);
    backupFile = null;
    doubleBackupFile = null;
  }

  public synchronized boolean backup() throws LockException {
    createFile();
    createBackupFile();
    boolean retval = backup(file, backupFile);
    file = null;
    backupFile = null;
    return retval;
  }

  /**
   * Delete the current backup file and rename the current file to be the new
   * backup file.  The current backup file will not be deleted unless the
   * current file exists.
   * Will not backup if backedUp is true (doesn't set backedUp)
   * @return true if backup() successful or already backed up
   * @throws FileException
   */
  public synchronized boolean backup(final File file, final File backupFile)
      throws LockException {
    if (backedUp) {
      return false;
    }
    if (!file.exists()) {
      return false;
    }
    FileId fileId = new FileId();
    lock.lock(LockType.FILE, fileId);
    if (!file.exists()) {
      // nothing to backup
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        // Don't throw a file exception because the error didn't affect the
        // backup.
        e.printStackTrace();
      }
      return false;
    }
    boolean success = true;
    // File logFile = new File(userDir, processName.toString()
    // + axisID.getExtension() + ".log");
    // File backupLogFile = new File(file.getAbsolutePath() + '~');
    // don't delete backup file unless the file to be backed up exists
    if (backupFile.exists()) {
      Utilities.debugPrint(backupFile.getAbsolutePath() + " exists, deleting");
      if (!delete(backupFile)) {
        System.err.println("Unable to delete backup log file: "
            + backupFile.getAbsolutePath());
        if (backupFile.exists()) {
          success = false;
          System.err.println(backupFile.getAbsolutePath() + " still exists!");
          new Exception().printStackTrace();
        }
        else {
          System.err.println(backupFile.getAbsolutePath() + " does not exist!");
        }
      }
    }
    Utilities.debugPrint(file.getAbsolutePath() + " exists");
    String actionMessage = Utilities.prepareRenameActionMessage(file, backupFile);
    if (!file.renameTo(backupFile)) {
      if (file.exists()) {
        System.err.println(file.getAbsolutePath() + " still exists");
        new Exception().printStackTrace();
        success = false;
      }
      else {
        System.err.println(file.getAbsolutePath() + " does not exist!");
      }
      if (backupFile.exists()) {
        System.err.println(backupFile.getAbsolutePath() + " still exists!");
        new Exception().printStackTrace();
      }
      else {
        System.err.println(backupFile.getAbsolutePath() + " does not exist");
      }
      System.err.println("Unable to rename file to: " + backupFile.getAbsolutePath());
      System.err.println("lock.isThrowException()=" + lock.isThrowException());
      StringBuffer message = new StringBuffer("Unable to rename "
          + file.getAbsolutePath() + " to " + backupFile.getAbsolutePath());
      if (lock.isThrowException()) {
        message.append("\nIf either of these files is open in 3dmod, close 3dmod.");
      }
      throw new LockException(this, fileId, message.toString());
    }
    else if (actionMessage != null) {
      System.err.println(actionMessage);
    }
    // reset the File variables sinces the file names may have changed.
    try {
      lock.unlock(LockType.FILE, fileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    return success;
  }

  /**
   * Delete a file, allowing up to 10 tries.
   * @param file
   * @return
   */
  private boolean delete(final File file) {
    int i;
    for (i = 0; i < 20; i++) {
      if (file.delete()) {
        if (i > 0) {
          System.err.println("It took " + i + " tries to remove " + file.getName());
        }
        return true;
      }
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException e) {
      }
    }
    System.err.println("Unable to remove " + file.getName() + " after " + i + " tries.");
    return false;
  }

  /**
   * Creates the file if it doesn't already exist.
   * @return
   * @throws FileException
   */
  public synchronized boolean create() throws LockException, IOException {
    createFile();
    if (file.exists()) {
      return false;
    }
    FileId fileId = new FileId();
    lock.lock(LockType.FILE, fileId);
    if (file.exists()) {
      // nothing to create
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        // Don't throw a file exception because the error didn't affect the
        // create.
        e.printStackTrace();
      }
      return false;
    }
    file.createNewFile();
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
    throw new LockException(this, fileId, "Unable to create " + path);
  }

  public synchronized boolean delete() throws LockException {
    createFile();
    if (!file.exists()) {
      return false;
    }
    FileId fileId = new FileId();
    lock.lock(LockType.FILE, fileId);
    if (!file.exists()) {
      // nothing to delete
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        // Don't throw a file exception because the error didn't affect the
        // delete.
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
    throw new LockException(this, fileId, "Unable to delete " + path);
  }

  public synchronized boolean move(LogFile target) throws LockException {
    createFile();
    if (!file.exists()) {
      return false;
    }
    FileId fileId = new FileId();
    lock.lock(LockType.FILE, fileId);
    if (!file.exists()) {
      // nothing to move
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        // Don't throw a file exception because the error didn't affect the
        // move.
        e.printStackTrace();
      }
      return false;
    }
    try {
      target.backup();
    }
    catch (LockException backupException) {
      // unable to backup
      try {
        lock.unlock(LockType.FILE, fileId);
      }
      catch (LockException e) {
        // Don't throw a file exception because the error didn't affect the
        // move.
        e.printStackTrace();
      }

      throw backupException;
    }
    // need to get a file lock on the target for this operation
    FileId targetFileId = new FileId();
    target.lock.lock(LockType.FILE, targetFileId);
    target.createFile();
    String actionMessage = Utilities.prepareRenameActionMessage(file, target.file);
    boolean success = file.renameTo(target.file);
    if (actionMessage != null && success) {
      System.err.println(actionMessage);
    }
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    try {
      lock.unlock(LockType.FILE, fileId);
    }
    catch (LockException e) {
      e.printStackTrace();
    }
    // unlock target file lock
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
    throw new LockException(this, fileId, "Unable to rename " + path + " to "
        + target.fileAbsolutePath);
  }

  public synchronized WriterId openWriter(final boolean append) throws LockException,
      IOException {
    WriterId writerId = new WriterId();
    lock.lock(LockType.WRITE, writerId);
    try {
      createWriter(append);
    }
    catch (IOException e) {
      try {
        lock.unlock(LockType.WRITE, writerId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
    return writerId;
  }

  public synchronized WriterId openWriter() throws LockException, IOException {
    WriterId writerId = new WriterId();
    lock.lock(LockType.WRITE, writerId);
    try {
      createWriter(false);
    }
    catch (IOException e) {
      try {
        lock.unlock(LockType.WRITE, writerId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
    return writerId;
  }

  /**
   * Run open with no wait limit.  This function can cause deadlock.
   * @see waitForLock()
   * @return
   */
  public synchronized WritingId openForWriting() throws LockException {
    WritingId writingId = new WritingId();
    lock.lock(LockType.WRITE, writingId);
    return writingId;
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
  public synchronized InputStreamId openInputStream() throws LockException, IOException {
    InputStreamId inputStreamId = new InputStreamId();
    lock.lock(LockType.WRITE, inputStreamId);
    try {
      createInputStream();
    }
    catch (IOException e) {
      try {
        lock.unlock(LockType.WRITE, inputStreamId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
    return inputStreamId;
  }

  /**
   * Opens the output stream.  Locks the WRITE lock and returns a writeId.
   * @return
   * @throws WriteException
   */
  public synchronized OutputStreamId openOutputStream() throws LockException, IOException {
    OutputStreamId outputStreamId = new OutputStreamId();
    lock.lock(LockType.WRITE, outputStreamId);
    try {
      createOutputStream();
    }
    catch (IOException e) {
      try {
        lock.unlock(LockType.WRITE, outputStreamId);
      }
      catch (LockException e0) {
        e0.printStackTrace();
      }
      throw e;
    }
    return outputStreamId;
  }

  public synchronized boolean closeInputStream(InputStreamId inputStreamId) {
    if (inputStream == null) {
      new LockException(this, inputStreamId,
          "Must use closeForWriting() when opened with openForWriting()")
          .printStackTrace();
      return false;
    }
    // close the input stream before unlocking
    try {
      lock.assertUnlockable(LockType.WRITE, inputStreamId);
      closeInputStream();
      lock.unlock(LockType.WRITE, inputStreamId);
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

  public synchronized boolean closeOutputStream(OutputStreamId outputStreamId) {
    if (outputStream == null) {
      new LockException(this, outputStreamId,
          "Must use closeForWriting() when opened with openForWriting()")
          .printStackTrace();
      return false;
    }
    // close the input stream before unlocking
    try {
      lock.assertUnlockable(LockType.WRITE, outputStreamId);
      closeOutputStream();
      lock.unlock(LockType.WRITE, outputStreamId);
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
  public synchronized boolean closeForWriting(WritingId writingId) {
    try {
      lock.unlock(LockType.WRITE, writingId);
    }
    catch (LockException e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public synchronized boolean closeWriter(WriterId writerId) {
    if (fileWriter == null) {
      new LockException(this, writerId,
          "Must use closeForWriting() when opened with openForWriting()")
          .printStackTrace();
      return false;
    }
    // close the writer before unlocking
    try {
      lock.assertUnlockable(LockType.WRITE, writerId);
      closeWriter();
      lock.unlock(LockType.WRITE, writerId);
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

  public synchronized ReaderId openReader() throws LockException, FileNotFoundException {
    ReaderId readerId = new ReaderId();
    lock.lock(LockType.READ, readerId);
    createFile();
    String idKey = ReadingTokenList.makeKey(readerId);
    try {
      readingTokenList.openReadingToken(idKey, file, ReaderType.READER);
    }
    catch (FileNotFoundException e) {
      lock.unlock(LockType.READ, readerId);
      throw e;
    }
    return readerId;
  }

  public synchronized BigBufferReaderId openBigBufferReaderId() throws LockException,
      FileNotFoundException {
    BigBufferReaderId id = new BigBufferReaderId();
    lock.lock(LockType.READ, id);
    createFile();
    String idKey = ReadingTokenList.makeKey(id);
    try {
      readingTokenList.openReadingToken(idKey, file, ReaderType.BIG_BUFFER_READER);
    }
    catch (FileNotFoundException e) {
      lock.unlock(LockType.READ, id);
      throw e;
    }
    return id;
  }

  public synchronized ReadingId openForReading() throws LockException,
      FileNotFoundException {
    ReadingId readingId = new ReadingId();
    lock.lock(LockType.READ, readingId);
    createFile();
    String idKey = ReadingTokenList.makeKey(readingId);
    try {
      readingTokenList.openReadingToken(idKey, file, ReaderType.READING);
    }
    catch (FileNotFoundException e) {
      lock.unlock(LockType.READ, readingId);
    }
    return readingId;
  }

  public synchronized boolean closeRead(Id readId) {
    // close the reader before unlocking
    try {
      lock.assertUnlockable(LockType.READ, readId);
      createFile();
      ReadingToken readingToken = readingTokenList.getReadingToken(ReadingTokenList
          .makeKey(readId));
      if (readingToken != null) {
        readingToken.close();
      }
      else {
        new LockException(this, readId, "readingToken is null.").printStackTrace();
      }
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

  /**
   * Reads a line from the file.  Returns null when there are no more lines to
   * read.
   * @param readId
   * @return
   * @throws LockException
   * @throws IOException
   */
  public synchronized String readLine(ReaderId readId) throws LockException, IOException {
    if (!lock.isLocked(LockType.READ, readId)) {
      throw new LockException(this, readId);
    }
    createFile();
    Reader reader = readingTokenList.getReader(ReadingTokenList.makeKey(readId));
    if (reader != null) {
      return reader.readLine();
    }
    else {
      return null;
    }
  }

  /**
   * Returns true if the last line of the file equals the line parameter
   * @param readId
   * @param line
   * @return
   * @throws LockException
   * @throws IOException
   */
  public synchronized boolean searchForLastLine(BigBufferReaderId id, String line)
      throws LockException, IOException {
    if (!lock.isLocked(LockType.READ, id)) {
      throw new LockException(this, id);
    }
    createFile();
    BigBufferReader reader = readingTokenList.getBigBufferReader(ReadingTokenList
        .makeKey(id));
    if (reader != null) {
      return reader.searchForLastLine(line);
    }
    else {
      return false;
    }
  }

  /**
   * Returns the first line that contains searchString
   * @param id
   * @param searchString
   * @return
   * @throws LockException
   * @throws IOException
   */
  public synchronized String getLineContaining(ReaderId id, String searchString)
      throws LockException, IOException {
    if (!lock.isLocked(LockType.READ, id)) {
      throw new LockException(this, id);
    }
    createFile();
    Reader reader = readingTokenList.getReader(ReadingTokenList.makeKey(id));
    if (reader != null) {
      String line = reader.readLine();
      while (line != null) {
        if (line.indexOf(searchString) != -1) {
          return line;
        }
        line = reader.readLine();
      }
    }
    return null;
  }

  public synchronized void load(Properties properties, InputStreamId inputStreamId)
      throws LockException, IOException {
    if (inputStream == null || !lock.isLocked(LockType.WRITE, inputStreamId)) {
      throw new LockException(this, inputStreamId);
    }
    properties.load(inputStream);
  }

  public void setDebug(boolean input) {
    debug = input;
    readingTokenList.setDebug(debug);
  }

  public synchronized void store(Properties properties, OutputStreamId outputStreamId)
      throws LockException, IOException {
    if (outputStream == null || !lock.isLocked(LockType.WRITE, outputStreamId)) {
      throw new LockException(this, outputStreamId);
    }
    properties.store(outputStream, null);
  }

  public synchronized void write(String string, WriterId writerId) throws LockException,
      IOException {
    if (string == null) {
      return;
    }
    if (fileWriter == null) {
      throw new LockException(this, writerId, "fileWriter is null");
    }
    if (!lock.isLocked(LockType.WRITE, writerId)) {
      throw new LockException(this, writerId, "not locked");
    }
    bufferedWriter.write(string);
  }

  public synchronized void write(char ch, WriterId writerId) throws LockException,
      IOException {
    if (fileWriter == null || !lock.isLocked(LockType.WRITE, writerId)) {
      throw new LockException(this, writerId);
    }
    bufferedWriter.write(ch);
  }

  public synchronized void write(Character ch, WriterId writerId) throws LockException,
      IOException {
    write(ch.charValue(), writerId);
  }

  public synchronized void newLine(WriterId writerId) throws LockException, IOException {
    if (fileWriter == null || !lock.isLocked(LockType.WRITE, writerId)) {
      throw new LockException(this, writerId);
    }
    bufferedWriter.newLine();
  }

  public synchronized void flush(WriterId writerId) throws LockException, IOException {
    if (!lock.isLocked(LockType.WRITE, writerId)) {
      throw new LockException(this, writerId);
    }
    try {
      bufferedWriter.flush();
    }
    catch (NullPointerException e) {
      throw new LockException(writerId,
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

  private void createDoubleBackupFile() {
    if (doubleBackupFile != null) {
      return;
    }
    doubleBackupFile = new File(fileAbsolutePath + DatasetFiles.BACKUP_CHAR
        + DatasetFiles.BACKUP_CHAR);
  }

  private void createWriter(final boolean append) throws IOException {
    createFile();
    try {
      if (fileWriter == null) {
        fileWriter = new FileWriter(file, append);
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

  public synchronized boolean isDirectory() {
    createFile();
    return file.isDirectory();
  }

  public synchronized String getName() {
    createFile();
    return file.getName();
  }

  /**
   * @return true if the open variabled is locked.
   */
  synchronized boolean isOpen() {
    return lock.isLocked();
  }

  /**
   * @return true if the open variabled is locked.
   */
  synchronized boolean isOpen(LockType lockType, Id id) {
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

  public static final class LockException extends Exception {
    private LockException(String message) {
      super(message);
    }

    private LockException(LogFile logFile) {
      super("logFile=" + logFile);
    }

    private LockException(LogFile logFile, LockType lockType) {
      super("lockType=" + lockType + ",logFile=" + logFile);
    }

    private LockException(LogFile logFile, LockType lockType, Id id) {
      super("lockType=" + lockType + ",id=" + id + ",logFile=" + logFile);
    }

    private LockException(LogFile logFile, Id id) {
      super("id=" + id + ",logFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
      Thread.dumpStack();
    }

    private LockException(Exception e) {
      super(e.toString() + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }

    private LockException(LogFile logFile, Id id, String message) {
      super(message + "\nid=" + id + ",logFile=" + logFile + PUBLIC_EXCEPTION_MESSAGE);
      Thread.dumpStack();
    }

    private LockException(Id id, String message, Exception e) {
      super(message + "\nid=" + id + PUBLIC_EXCEPTION_MESSAGE);
      e.printStackTrace();
    }
  }

  private static final class Lock {
    private final HashMap readIdHashMap = new HashMap();
    private final boolean throwException;
    private final LogFile logFile;

    private boolean warningDisplayed = false;
    private boolean locked = false;
    private int currentId = NO_ID;
    private int writeId = NO_ID;
    private int fileId = NO_ID;

    private Lock(final LogFile logFile) {
      this.logFile = logFile;
      throwException = Utilities.isWindowsOS()
          || EtomoDirector.INSTANCE.getArguments().isTest();
    }

    private static String makeKey(final Id id) {
      if (id == null) {
        LockException idNull = new LockException("id is null");
        idNull.printStackTrace();
        // Popups from this class appear rarely and the class is used everywhere.
        // It is not a good idea to pass the manager to this class in order to
        // get the message behave perfectly.
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null, idNull.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        return null;
      }
      return id.toString();
    }

    private static String makeKey(final int id) {
      return String.valueOf(id);
    }

    private boolean isThrowException() {
      return throwException;
    }

    public String toString() {
      return "\n[readIdHashMap=" + readIdHashMap + ",\nwriteId=" + writeId + ",fileId="
          + fileId + "]";
    }

    private void lock(final LockType lockType, Id id) throws LockException {
      if (id == null) {
        LockException idNull = new LockException("id is null");
        idNull.printStackTrace();
        // Popups from this class appear rarely and the class is used everywhere.
        // It is not a good idea to pass the manager to this class in order to
        // get the message behave perfectly.
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null, idNull.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        if (throwException) {
          throw idNull;
        }
      }
      assertLockable(lockType);
      // set the lock
      locked = true;
      // save the lock id in the variable matching the lock type
      // increment the current id
      if (++currentId < 0) {
        System.err.println("LogFile overflow - setting currentId to zero:currentId="
            + currentId);
        // catching overflow
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
      id.set(currentId);
      return;
    }

    private void unlock(final LockType lockType, final Id id) throws LockException {
      if (id == null) {
        LockException idNull = new LockException("id is null");
        idNull.printStackTrace();
        // Popups from this class appear rarely and the class is used everywhere.
        // It is not a good idea to pass the manager to this class in order to
        // get the message behave perfectly.
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null, idNull.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        if (throwException) {
          throw idNull;
        }
      }
      assertUnlockable(lockType, id);
      // unsetting the matching saved id
      String readKey = makeKey(id);
      if (lockType == LockType.READ && readIdHashMap.containsKey(readKey)) {
        readIdHashMap.remove(readKey);
      }
      else if (lockType == LockType.WRITE && id.equals(writeId)) {
        writeId = NO_ID;
      }
      else if (lockType == LockType.FILE && id.equals(fileId)) {
        fileId = NO_ID;
      }
      else {
        LockException e = new LockException(logFile, lockType);
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          // Popups from this class appear rarely and the class is used everywhere.
          // It is not a good idea to pass the manager to this class in order to
          // get the message behave perfectly.
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
      // turn off locked if all the saved ids are empty
      if (readIdHashMap.isEmpty() && writeId == NO_ID && fileId == NO_ID) {
        locked = false;
      }
      return;
    }

    private boolean isLocked() {
      return locked;
    }

    private boolean isLocked(final LockType lockType, final Id id) {
      if (id == null || !locked || lockType == null || id.isEmpty()) {
        return false;
      }
      return (lockType == LockType.READ && readIdHashMap.containsKey(makeKey(id)))
          || (lockType == LockType.WRITE && id.equals(writeId))
          || (lockType == LockType.FILE && id.equals(fileId));
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
          // Popups from this class appear rarely and the class is used everywhere.
          // It is not a good idea to pass the manager to this class in order to
          // get the message behave perfectly.
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
    }

    private void assertLockable(final LockType lockType) throws LockException {
      if (!locked || lockType == null) {
        // succeed - not locked
        return;
      }
      // nothing else can be done during a file lock
      // only one write can be done at a time
      if (lockType == LockType.FILE || fileId != NO_ID
          || (lockType == LockType.WRITE && writeId != NO_ID)) {
        LockException e = new LockException(logFile, lockType);
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          // Popups from this class appear rarely and the class is used everywhere.
          // It is not a good idea to pass the manager to this class in order to
          // get the message behave perfectly.
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
      // compatible:
      // multiple reads
      // a read and write (either can be started first)
    }

    /**
     * 
     * @param lockType
     * @param id
     * @return
     * @throws LockException
     */
    private void assertUnlockable(final LockType lockType, final Id id)
        throws LockException {
      if (id == null) {
        LockException idNull = new LockException("id is null");
        idNull.printStackTrace();
        // Popups from this class appear rarely and the class is used everywhere.
        // It is not a good idea to pass the manager to this class in order to
        // get the message behave perfectly.
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null, idNull.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        if (throwException) {
          throw idNull;
        }
      }
      LockException e = new LockException(logFile, lockType, id);
      if (!locked) {
        if (throwException) {
          throw e;
        }
        if (!warningDisplayed) {
          warningDisplayed = true;
          // Popups from this class appear rarely and the class is used everywhere.
          // It is not a good idea to pass the manager to this class in order to
          // get the message behave perfectly.
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage()
              + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        }
      }
      if (readIdHashMap.isEmpty() && writeId == NO_ID && fileId == NO_ID) {
        throw new IllegalStateException("Ids don't match the locked boolean:\nlocked="
            + locked + ",readId=" + readIdHashMap.toString() + ",writeId=" + writeId
            + ",fileId=" + fileId);
      }
      // checking for unlockability
      if ((lockType == LockType.READ && readIdHashMap.containsKey(makeKey(id)))
          || (lockType == LockType.WRITE && id.equals(writeId))
          || (lockType == LockType.FILE && id.equals(fileId))) {
        return;
      }
      e = new LockException(logFile, lockType, id);
      if (throwException) {
        throw e;
      }
      if (!warningDisplayed) {
        warningDisplayed = true;
        // Popups from this class appear rarely and the class is used everywhere.
        // It is not a good idea to pass the manager to this class in order to
        // get the message behave perfectly.
        e.printStackTrace();
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
      }
    }

    private int getWriteId() {
      return writeId;
    }
  }

  private static final class ReadingTokenList {
    private final AbstractMap<String, ReadingToken> hashMap = new HashMap<String, ReadingToken>();
    private final List<ReadingToken> arrayList = new ArrayList<ReadingToken>();

    private boolean debug = false;

    private ReadingTokenList() {
    }

    static String makeKey(Id id) {
      if (id == null) {
        LockException idNull = new LockException("id is null");
        idNull.printStackTrace();
        // Popups from this class appear rarely and the class is used everywhere.
        // It is not a good idea to pass the manager to this class in order to
        // get the message behave perfectly.
        idNull.printStackTrace();
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null, idNull.getMessage()
            + PUBLIC_EXCEPTION_MESSAGE, "File Lock Warning");
        return "";
      }
      return (id.toString());
    }

    void setDebug(boolean input) {
      debug = input;
    }

    synchronized ReadingToken getReadingToken(String key) {
      return (ReadingToken) hashMap.get(key);
    }

    synchronized Reader getReader(String key) {
      ReadingToken readingToken = hashMap.get(key);
      if (readingToken.getReaderType() == ReaderType.READER) {
        return (Reader) readingToken;
      }
      return null;
    }

    synchronized BigBufferReader getBigBufferReader(String key) {
      ReadingToken readingToken = hashMap.get(key);
      if (readingToken.getReaderType() == ReaderType.BIG_BUFFER_READER) {
        return (BigBufferReader) readingToken;
      }
      return null;
    }

    synchronized void openReadingToken(final String currentKey, final File file,
        final ReaderType readerType) throws FileNotFoundException {
      ReadingToken readingToken;
      for (int i = 0; i < arrayList.size(); i++) {
        readingToken = (ReadingToken) arrayList.get(i);
        if (readingToken.getReaderType() == readerType && !readingToken.isOpen()) {
          // open the reader to get exclusive access to it
          try {
            readingToken.open();
          }
          catch (FileNotFoundException e) {
            throw new FileNotFoundException(e.getMessage() + "\ncurrentKey=" + currentKey);
          }
          // Found a closed reader, so reuse it
          // get the old key from the reader and rekey the reader in the hash map
          // with the current key
          String oldKey = readingToken.getKey();
          hashMap.remove(oldKey);
          readingToken.setKey(currentKey);
          hashMap.put(currentKey, readingToken);

        }
      }
      // Can't find a closed reader, so create a new one
      readingToken = newReadingToken(file, readerType);
      // open the reader to get exclusive access to it
      readingToken.open();
      // store the current key in the reader and store it in the array list and
      // hash map
      readingToken.setKey(currentKey);
      hashMap.put(currentKey, readingToken);
      arrayList.add(readingToken);
    }

    private ReadingToken newReadingToken(final File file, final ReaderType readerType) {
      if (readerType == ReaderType.READER) {
        return new Reader(file);
      }
      if (readerType == ReaderType.BIG_BUFFER_READER) {
        return new BigBufferReader(file);
      }
      return new ReadingToken(readerType);
    }
  }

  private static class ReadingToken {
    private final ReaderType readerType;

    private boolean open = true;
    private String key = null;

    private ReadingToken(final ReaderType readerType) {
      this.readerType = readerType;
    }

    private ReaderType getReaderType() {
      return readerType;
    }

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
      super(ReaderType.READER);
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

  private static final class BigBufferReader extends ReadingToken {
    private static final int SIZE = 4096;
    private final File file;

    private FileReader fileReader = null;
    private BufferedReader bufferedReader = null;

    BigBufferReader(File file) {
      super(ReaderType.BIG_BUFFER_READER);
      this.file = file;
    }

    void open() throws FileNotFoundException {
      if (fileReader == null) {
        fileReader = new FileReader(file.getAbsolutePath());
      }
      if (bufferedReader == null) {
        bufferedReader = new BufferedReader(fileReader, SIZE);
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

    /**
     * Searches for a string in the last characters in a file.  Finds the last characters
     * of the file equal to 2 times the length of lastLine and searches them for lastLine.
     * Returns true if lastLine found in this span of characters.
     * @param lastLine
     * @return
     * @throws IOException
     */
    boolean searchForLastLine(final String lastLine) throws IOException {
      // Increase the size to handle a final end of line (linux or windows).
      int lengthToScan = lastLine.length() * 2;
      if (lengthToScan > SIZE) {
        new IllegalArgumentException("String too large to scan for:" + lastLine)
            .printStackTrace();
        return false;
      }
      int nReadTemp;
      int nRead = -1;
      int nReadPrev = -1;
      // Not sure if read reallocates the char array. Avoid assigning one char array to
      // the other by using alternating reads
      boolean first = true;
      char[] firstCharArray = new char[SIZE];
      char[] secondCharArray = new char[SIZE];
      while ((nReadTemp = bufferedReader.read(first ? firstCharArray : secondCharArray)) != -1) {
        // Save the previous number of characters read
        nReadPrev = nRead;
        // Save the number of characters read this time
        nRead = nReadTemp;
        first = !first;
      }
      if (nRead == -1) {
        // Nothing was read
        return false;
      }
      // Assign the apropriate char array
      char[] charArray = null;
      char[] charArrayPrev = null;
      if (!first) {
        charArray = firstCharArray;
        charArrayPrev = secondCharArray;
      }
      else {
        charArray = secondCharArray;
        charArrayPrev = firstCharArray;
      }
      // Add the last characters equal to usefulLength to buffer.
      StringBuffer buffer = new StringBuffer();
      int usefulLength;
      if (nRead < lengthToScan && nReadPrev > 0) {
        // The last characters have been split in half
        usefulLength = Math.min(nReadPrev, lengthToScan - nRead);
        buffer.append(charArrayPrev, nReadPrev - usefulLength, usefulLength);
      }
      usefulLength = Math.min(nRead, lengthToScan);
      buffer.append(charArray, nRead - usefulLength, usefulLength);
      return buffer.toString().indexOf(lastLine) != -1;
    }
  }

  static class Id {
    private int id = NO_ID;

    void set(int input) {
      this.id = input;
    }

    int get() {
      return id;
    }

    private boolean equals(int input) {
      return id == input;
    }

    public boolean isEmpty() {
      return id == NO_ID;
    }

    public String toString() {
      return Integer.toString(get());
    }
  }

  private static final class FileId extends Id {
  }

  public static final class ReaderId extends Id {
  }

  public static final class BigBufferReaderId extends Id {
  }

  public static final class ReadingId extends Id {
  }

  static final class InputStreamId extends Id {
  }

  public static final class WriterId extends Id {
  }

  public static final class WritingId extends Id {
  }

  static final class OutputStreamId extends Id {
  }

  private static final class ReaderType {
    private static final ReaderType READING = new ReaderType();
    private static final ReaderType READER = new ReaderType();
    private static final ReaderType BIG_BUFFER_READER = new ReaderType();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.34  2011/06/28 02:34:00  sueh
 * <p> Bug# 1501 Corrected error message.
 * <p>
 * <p> Revision 1.33  2011/02/22 04:45:15  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.32  2010/11/13 16:05:03  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.31  2010/10/13 20:18:34  sueh
 * <p> bug# 1392 In getInstance making sure that the exception stack is printed.
 * <p>
 * <p> Revision 1.30  2010/05/12 21:13:44  sueh
 * <p> bug# 1358 Win7 Problem with backing up tomopitch.log.  Adding stack dumps.
 * <p>
 * <p> Revision 1.29  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.28  2010/01/13 21:54:04  sueh
 * <p> bug# 1298 Formatted.
 * <p>
 * <p> Revision 1.27  2009/09/01 03:18:06  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.26  2009/03/25 15:32:06  sueh
 * <p> In backup() printing the Lock.throwException state when there is a problem.
 * <p>
 * <p> Revision 1.25  2009/03/17 00:44:52  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.24  2009/02/04 23:06:33  sueh
 * <p> bug# 1158 Distinguishing between ids for different types of file opens by making a class for each type.  Helps avoid mysterious errors while developing.  Also simplified the @#!#$ exception class scheme.
 * <p>
 * <p> Revision 1.23  2008/10/27 18:07:26  sueh
 * <p> bug# 1141 Added more failure information to write().
 * <p>
 * <p> Revision 1.22  2008/09/11 20:33:04  sueh
 * <p> bug# 1139 Corrected a null pointer exception in closeReader.  Printing an
 * <p> exception if it happens again.
 * <p>
 * <p> Revision 1.21  2008/05/28 17:27:27  sueh
 * <p> bug# 1110 In move() base success on the return value of File.renameTo
 * <p> instead of the original file's disappearance.  It may take too long for the
 * <p> original file's disappearance to appear in the directory, especially for a
 * <p> big file.
 * <p>
 * <p> Revision 1.20  2008/02/26 01:38:46  sueh
 * <p> bug# 1087 Added more information to FileException.
 * <p>
 * <p> Revision 1.19  2008/01/31 20:22:14  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
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
