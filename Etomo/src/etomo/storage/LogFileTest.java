package etomo.storage;

import java.io.File;
import java.util.Properties;

import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;

import junit.framework.TestCase;

/**
 * <p>Description: Tests LogFile.  Always use WAIT_LIMIT to avoid deadlock.</p>
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
 * <p> Revision 1.6  2007/02/05 23:04:47  sueh
 * <p> bug# 962  Added testMove.
 * <p>
 * <p> Revision 1.5  2006/11/15 20:04:01  sueh
 * <p> bug# 872 Testing input and output streams.  Testing Id creation.  Testing the
 * <p> write lock.
 * <p>
 * <p> Revision 1.4  2006/10/16 22:47:42  sueh
 * <p> bug# 919  Changed touch(File) to touch(String absolutePath).
 * <p>
 * <p> Revision 1.3  2006/10/13 18:35:58  sueh
 * <p> bug# 931 Increasing sleep in createLog().
 * <p>
 * <p> Revision 1.2  2006/10/11 10:12:17  sueh
 * <p> bug# 931 Added delete functionality to LogFile - changed BackupException to
 * <p> FileException.
 * <p>
 * <p> Revision 1.1  2006/10/10 05:19:14  sueh
 * <p> bug# 931 Test class for LogFile.
 * <p> </p>
 */
public class LogFileTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final File testDir = new File(StorageTests.TEST_ROOT_DIR,
      "LogFile");
  //private static final long WAIT_LIMIT = 10;

  private final File log = new File(testDir, ProcessName.BLEND
      + DatasetFiles.LOG_EXT);
  private final File backupLog = new File(log.getAbsolutePath()
      + DatasetFiles.BACKUP_CHAR);

  public LogFileTest() {
    super();
  }
  
  public LogFileTest(String test){
    super(test);
  }
  
  protected void setUp() throws Exception {
    super.setUp();
    testDir.mkdirs();
    createLog();
    if (backupLog.exists()) {
      backupLog.delete();
    }
    assertFalse(backupLog.exists());
    LogFile.reset();
  }

  public void testGetInstance() {
    LogFile test = getInstance();
    LogFile dupTest = getInstance();
    //only one object is created when getInstance() is called with the same
    //parameter values
    assertTrue(test == dupTest);
    assertTrue(test.noLocks());
  }

  public void testOpenReader() throws LogFile.ReadException {
    LogFile test = getInstance();
    long readId1 = test.openReader();
    assertFalse("Should be able to open an unopened log file reader.",
        readId1 == LogFile.NO_ID);
    assertTrue("Should be able to close an open reader.", test
        .closeReader(readId1));
    long writeId = test.openForWriting();
    assertEquals("Ids should increment by one",readId1+1,writeId);
    long readId2 = test.openReader();
    assertFalse(
        "Should be able to open a log file reader when it is opened for writing.",
        readId2 == LogFile.NO_ID);
    assertEquals("Ids should increment by one",writeId+1,readId2);
    assertTrue(test.closeForWriting(writeId));
    assertTrue(test.closeReader(readId2));
    long readId3 = test.openReader();
    assertEquals("Ids should increment by one",readId2+1,readId3);
    long readId4 = test.openReader();
    assertEquals("Ids should increment by one",readId3+1,readId4);
    assertFalse(
        "Should be able to open a log file reader when a reader is is already opened.",
        readId4 == LogFile.NO_ID);
    assertTrue(
        "A successful openReader() call should open the log file reader.", test
            .isOpen(LogFile.LockType.READ, readId4));
    try {
      test.backup();
      fail("Can't backup a log file with an open reader.");
    }
    catch (LogFile.FileException e) {
    }
    assertFalse(
        "CloseReader should fail when passed an id that doesn't match the one returned by the open call.",
        test.closeReader(LogFile.NO_ID));
    assertTrue(test.closeReader(readId3));
    assertTrue(test.closeReader(readId4));
    log.delete();
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    try {
      test.openReader();
      fail("Should throw read exception when there is no file");
    }
    catch (LogFile.ReadException e) {
    }
    assertTrue(test.noLocks());
  }

  public void testReadLine() throws LogFile.WriteException,
      LogFile.ReadException {
    String line1 = "first line";
    LogFile test = getInstance();
    long writeId = test.openWriter();
    test.write(line1, writeId);
    test.newLine(writeId);
    assertTrue(test.closeWriter(writeId));
    long readId1 = test.openReader();
    assertTrue("Should return the first line of the file.", test.readLine(
        readId1).equals(line1));
    assertNull("Should return null.", test.readLine(readId1));
    long readId2 = test.openReader();
    assertTrue(
        "Should use a different reader to return the first line of the file.",
        test.readLine(readId2).equals(line1));
    assertNull("Should return null.", test.readLine(readId2));
    assertTrue(test.closeReader(readId1));
    assertTrue(test.closeReader(readId2));
    readId1 = test.openReader();
    assertTrue(
        "Should return the first line of the file when reusing a reader.", test
            .readLine(readId1).equals(line1));
    assertNull("Should return null.", test.readLine(readId1));
    assertTrue(test.closeReader(readId1));
    assertTrue(test.noLocks());
  }

  public void testCloseReader() throws LogFile.ReadException,
      LogFile.FileException {
    LogFile test = getInstance();
    assertFalse("Closing unnecessarily should fail.", test.closeReader(0));
    long readId = test.openReader();
    assertTrue("Should be able to close an opened log file.", test
        .closeReader(readId));
    assertFalse(
        "The log file should be closed after closeReader() is called with the read id.",
        test.isOpen(LogFile.LockType.READ, readId));
    assertTrue("Backup should succeed because the log file is closed.", test
        .backup());
    assertTrue(test.noLocks());
  }

  public void testOpenForWriting() throws LogFile.ReadException,
      LogFile.FileException {
    LogFile test = getInstance();
    long writeId1 = test.openForWriting();
    assertWriteLock(test, writeId1);
    assertFalse("Should be able to open an unopened log file for writing.",
        writeId1 == LogFile.NO_ID);
    assertTrue(
        "Should be able to call closeForWriting() on a log file where openForWriting() was used.",
        test.closeForWriting(writeId1));
    long readId1 = test.openReader();
    writeId1 = test.openForWriting();
    assertFalse(
        "Should be able to open an log file for writing when it is open for reading.",
        writeId1 == LogFile.NO_ID);
    assertTrue(
        "A successful openForWriting call should open log file for writing.",
        test.isOpen(LogFile.LockType.WRITE, writeId1));
    long writeId2 = LogFile.NO_ID;
    assertEquals(
        "Shouldn't be able to open a log file for writing when it is already open for writing.",
        writeId2 = test.openForWriting(), LogFile.NO_ID);
    try {
      test.backup();
      fail("Can't backup an open log file.");
    }
    catch (LogFile.FileException e) {
    }
    long readId2 = test.openReader();
    assertFalse(
        "Should be able to open a log file for reading when it is alrady open for writing.",
        readId2 == LogFile.NO_ID);
    assertFalse(
        "CloseForWriting should fail when passed an id that doesn't match the one returned by the open call.",
        test.closeForWriting(writeId2));
    assertTrue(test.closeForWriting(writeId1));
    assertTrue(test.closeReader(readId1));
    assertTrue(test.closeReader(readId2));
    assertTrue(test.noLocks());
  }

  public void testOpenWriter() throws LogFile.WriteException,
      LogFile.ReadException, LogFile.FileException {
    LogFile test = getInstance();
    long writeId1 = test.openWriter();
    assertWriteLock(test, writeId1);
    assertFalse("Should be able to open an unopened log file for writing.",
        writeId1 == LogFile.NO_ID);
    assertFalse(
        "Must close the writer with closeWriter() when it is opened with openWriter()",
        test.closeForWriting(writeId1));
    assertTrue(test.closeWriter(writeId1));
    long readId1 = test.openReader();
    writeId1 = test.openWriter();
    assertFalse(
        "Should be able to open an log file for writing when it is open for reading.",
        writeId1 == LogFile.NO_ID);
    assertTrue(
        "A successful openForWriting call should open log file for writing.",
        test.isOpen(LogFile.LockType.WRITE, writeId1));
    long writeId2 = LogFile.NO_ID;
    try {
      writeId2 = test.openWriter();
      fail("Shouldn't be able to open a log file writer when it is already has an open writer.");
    }
    catch (LogFile.WriteException e) {
    }
    try {
      test.backup();
      fail("Can't backup an open log file.");
    }
    catch (LogFile.FileException e) {
    }
    long readId2 = test.openReader();
    assertFalse(
        "Should be able to open a log file for reading when it is alrady open for writing.",
        readId2 == LogFile.NO_ID);
    assertFalse(
        "CloseForWriting should fail when passed an id that doesn't match the one returned by the open call.",
        test.closeForWriting(writeId2));
    assertTrue(test.closeWriter(writeId1));
    assertTrue(test.closeReader(readId1));
    assertTrue(test.closeReader(readId2));
    log.delete();
    assertTrue(test.noLocks());
  }

  public void testWrite() throws LogFile.WriteException, LogFile.ReadException {
    String line = "succeed";
    LogFile test = getInstance();
    try {
      test.write("fail", 0);
      fail("Shouldn't be able to write to a log file that doesn't isn't open for"
          + "writing.");
    }
    catch (LogFile.WriteException e) {
    }
    long writeId = test.openForWriting();
    try {
      test.write("fail", writeId);
      fail("Shouldn't be able to write to a log file that doesn't have an open"
          + "writer.");
    }
    catch (LogFile.WriteException e) {
    }
    assertTrue(test.closeForWriting(writeId));
    writeId = test.openWriter();
    test.write(line, writeId);
    assertTrue(test.closeWriter(writeId));
    long readId = test.openReader();
    assertTrue("Line should have been written to the file.", test.readLine(
        readId).equals(line));
    assertTrue(test.closeReader(readId));
    assertTrue(test.noLocks());
  }

  public void testNewLine() throws LogFile.WriteException,
      LogFile.ReadException {
    LogFile test = getInstance();
    try {
      test.newLine(0);
      fail("Shouldn't be able to write a new line to a log file that doesn't isn't open for"
          + "writing.");
    }
    catch (LogFile.WriteException e) {
    }
    long writeId = test.openForWriting();
    try {
      test.newLine(writeId);
      fail("Shouldn't be able to write a new line to a log file that doesn't have an open"
          + "writer.");
    }
    catch (LogFile.WriteException e) {
    }
    assertTrue(test.closeForWriting(writeId));
    writeId = test.openWriter();
    test.newLine(writeId);
    test.closeWriter(writeId);
    long readId = test.openReader();
    assertTrue("New line should have been written to the file.", test.readLine(
        readId).equals(""));
    assertTrue(test.closeReader(readId));
    assertTrue(test.noLocks());
  }

  public void testCloseForWriting() throws LogFile.FileException,
      LogFile.WriteException {
    LogFile test = getInstance();
    assertFalse("Closing unnecessarily should fail.", test.closeForWriting(0));
    long writeId = test.openForWriting();
    assertTrue("Should be able to close an opened log file.", test
        .closeForWriting(writeId));
    assertFalse(
        "The log file should be closed after closeForWriting() is called with the write id.",
        test.isOpen(LogFile.LockType.WRITE, writeId));
    assertTrue("Backup should succeed because the log file is closed.", test
        .backup());
    createLog();
    assertTrue(log.exists());
    writeId = test.openWriter();
    assertTrue("Opening the writer should not remove the file.", log.exists());
    assertTrue("Should be able to close an opened log file.", test
        .closeWriter(writeId));
    assertTrue("Opening the writer should not remove the file.", log.exists());
    assertFalse(
        "The log file should be closed after openWriter() is called with the write id.",
        test.isOpen(LogFile.LockType.WRITE, writeId));
    assertTrue("Backup should succeed because the log file is closed.", test
        .backup());
    assertTrue(test.noLocks());
  }

  public void testBackup() throws LogFile.FileException, LogFile.WriteException {
    LogFile test = getInstance();
    assertTrue("Should be able to backup an unopened log file.", test.backup());
    assertFalse("Backup should rename the log to .log~.", log.exists());
    assertTrue("Backup should rename the log to .log~.", backupLog.exists());
    assertFalse("Backup should fail when log has already been renamed.", test
        .backup());
    assertTrue(".log~ should not be deleted by a failed backup.", backupLog
        .exists());
    try {
      test.openReader();
      fail("Opening the file for reading should throw a file not found exception.");
    }
    catch (LogFile.ReadException e) {
    }
    long writeId = test.openForWriting();
    assertFalse(
        "Should be able to open the log file for writing after the backup is done, even though the file doesn't exist.",
        writeId == LogFile.NO_ID);
    assertTrue(test.closeForWriting(writeId));
    writeId = test.openWriter();
    assertFalse(
        "Should be able to open the log file writer after the backup is done, even though the file doesn't exist.",
        writeId == LogFile.NO_ID);
    assertTrue(test.closeWriter(writeId));
    assertTrue(test.noLocks());
  }

  public void testBackupOnce() throws LogFile.FileException {
    LogFile test = getInstance();
    assertTrue("Should be able to backup a log file.", test.backupOnce());
    createLog();
    assertFalse(
        "Should not be able to backup a log file more then once when it has been backed up with backupOnce.",
        test.backup());
    assertFalse(
        "Should not be able to backup a log file more then once when it has been backed up with backupOnce.",
        test.backupOnce());
    assertTrue(test.noLocks());
  }

  public void testDelete() throws LogFile.FileException, LogFile.ReadException,
      LogFile.WriteException {
    LogFile test = getInstance();
    long readId = test.openReader();
    try {
      test.delete();
      fail("Should throw an exception if a reader is open.");
    }
    catch (LogFile.FileException e) {
    }
    assertTrue(test.closeReader(readId));
    long writeId = LogFile.NO_ID;
    assertFalse((writeId = test.openForWriting()) == LogFile.NO_ID);
    try {
      test.delete();
      fail("Should throw an exception if open for writing.");
    }
    catch (LogFile.FileException e) {
    }
    assertTrue(test.closeForWriting(writeId));
    writeId = test.openWriter();
    try {
      test.delete();
      fail("Should throw an exception if the writer is open.");
    }
    catch (LogFile.FileException e) {
    }
    assertTrue(test.closeWriter(writeId));
    assertTrue(test.backup());
    createLog();
    assertTrue("Should be able to delete when it can get an exclusive lock.",
        test.delete());
    assertFalse("Should delete the file.", log.exists());
    assertFalse("Should return false when there is nothing to delete", test
        .delete());
    assertTrue(test.noLocks());
  }
  
  public void testMove() throws LogFile.FileException,LogFile.ReadException,LogFile.WriteException{
    LogFile test = getInstance();
    test.delete();
    assertTrue(test.noLocks());
    LogFile target = LogFile.getInstance(testDir.getAbsolutePath(),"target");
    target.delete();
    long writeId = test.openWriter();
    try {
      test.move(target);
      fail("Should not be able to move when a writer is open.");
    }
    catch (LogFile.FileException e) {
    }
    test.closeWriter(writeId);
    writeId = target.openWriter();
    try {
      test.move(target);
 //     fail("Should not be able to move when a writer is open on the target.");
    }
    catch (LogFile.FileException e) {
    }
    target.closeWriter(writeId);
    target.create();
    assertTrue(target.exists());
    long readId = target.openReader();
    try {
      test.move(target);
      fail("Should not be able to backup when a reader is open on the target.");
    }
    catch (LogFile.FileException e) {
    }
    assertTrue("Should not be able to backup when a reader is open on the target.",target.exists());
    target.closeReader(readId);
    test.create();
    assertTrue(test.exists());
    assertTrue(target.exists());
    test.move(target);
    assertFalse("Original file should not exist after backup.",test.exists());
    assertFalse("Move should return false when attempting to move a file that doesn't exist.", test.move(target));
    assertTrue("Attempting to move a file that doesn't exist should not cause a backup.", target.exists());
    assertTrue(test.noLocks());
    assertTrue(target.noLocks());
  }

  public void testCreate() throws LogFile.FileException, LogFile.ReadException {
    LogFile test = getInstance();
    long readId = test.openReader();
    assertFalse("When file exists, shouldn't check locks; just return false",
        test.create());
    assertTrue(test.closeReader(readId));
    log.delete();
    assertFalse(log.exists());
    long writeId = test.openForWriting();
    assertFalse(writeId == LogFile.NO_ID);
    try {
      test.create();
      fail("Should throw file exception when WRITE locked");
    }
    catch (LogFile.FileException e) {
    }
    assertTrue(test.closeForWriting(writeId));
    test.create();
    assertTrue("Create should create the log file", log.exists());
    assertTrue(test.noLocks());
  }

  public void testIsOpen() throws LogFile.ReadException, LogFile.WriteException {
    LogFile test = getInstance();
    long readId = LogFile.NO_ID;
    long writeId = LogFile.NO_ID;
    assertFalse("IsOpen should return false with nothing is opened.", test
        .isOpen(LogFile.LockType.WRITE, readId));
    assertFalse("IsOpen should return false with nothing is opened.", test
        .isOpen(LogFile.LockType.WRITE, writeId));
    readId = test.openReader();
    assertTrue("IsOpen should return true after openForReading() is called.",
        test.isOpen(LogFile.LockType.READ, readId));
    assertFalse("The LockType must match the type of open.", test.isOpen(
        LogFile.LockType.WRITE, readId));
    assertTrue(test.closeReader(readId));
    writeId = test.openForWriting();
    assertTrue("IsOpen should return true after openForWriting() is called.",
        test.isOpen(LogFile.LockType.WRITE, writeId));
    assertFalse("The LockType must match the type of open.", test.isOpen(
        LogFile.LockType.READ, writeId));
    readId = test.openReader();
    assertTrue("IsOpen should return true after openForReading() is called.",
        test.isOpen(LogFile.LockType.READ, readId));
    assertTrue("The log file should be open after openForWriting() is called.",
        test.isOpen(LogFile.LockType.WRITE, writeId));
    assertTrue(test.closeReader(readId));
    assertTrue(test.closeForWriting(writeId));
    assertFalse("IsOpen should return false when log file is closed.", test
        .isOpen(LogFile.LockType.READ, readId));
    assertFalse("IsOpen should return false when log file is closed.", test
        .isOpen(LogFile.LockType.WRITE, writeId));
    writeId = test.openWriter();
    assertTrue("The log file should be open after openWriter() is called.",
        test.isOpen(LogFile.LockType.WRITE, writeId));
    assertTrue(test.closeWriter(writeId));
    assertFalse("IsOpen should return false when log file is closed.", test
        .isOpen(LogFile.LockType.WRITE, writeId));
    assertTrue(test.noLocks());
  }

  public void testInputStream() throws LogFile.WriteException,
      LogFile.FileException, LogFile.ReadException {
    LogFile test = getInstance();
    long writeId = test.openWriter();
    String key = "key";
    String value = "1";
    test.write(key + '=' + value, writeId);
    test.newLine(writeId);
    test.closeWriter(writeId);
    writeId = test.openInputStream();
    assertFalse(
        "Should be able to open an input stream when there are no locks.",
        writeId == LogFile.NO_ID);
    assertWriteLock(test, writeId);
    Properties props = new Properties();
    test.load(props, writeId);
    assertTrue("Should be able to load with input stream open", props
        .getProperty(key).equals(value));
    assertFalse(
        "Should not be able to close for writing when the input stream is open",
        test.closeForWriting(writeId));
    assertFalse(
        "Should not be able to close output stream when the input stream is open",
        test.closeOutputStream(writeId));
    assertFalse(
        "Should not be able to close writer when the input stream is open",
        test.closeWriter(writeId));
    try {
      test.flush(writeId);
      fail("Should not be able to flush when the input stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    try {
      test.newLine(writeId);
      fail("Should not be able to write a new line when the input stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    try {
      test.store(props, writeId);
      fail("Should not be able to store when the input stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    try {
      test.write("string", writeId);
      fail("Should not be able to write when the input stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    assertTrue("Should be able to close an open input stream.", test
        .closeInputStream(writeId));
    assertFalse("Should be able to close a closed input stream.", test
        .closeInputStream(writeId));
    assertTrue(test.noLocks());
  }

  public void testOutputStream() throws LogFile.WriteException,
      LogFile.FileException, LogFile.ReadException {
    String key = "key";
    String value = "1";
    LogFile test = getInstance();
    long writeId = test.openOutputStream();
    assertFalse(
        "Should be able to open an output stream when there are no locks.",
        writeId == LogFile.NO_ID);
    assertWriteLock(test, writeId);
    try {
      test.load(new Properties(), writeId);
      fail("Should not be able to load with output stream open");
    }
    catch (LogFile.WriteException e) {
    }
    assertFalse(
        "Should not be able to close for writing when the output stream is open",
        test.closeForWriting(writeId));
    assertFalse(
        "Should not be able to close input stream when the output stream is open",
        test.closeInputStream(writeId));
    assertFalse(
        "Should not be able to close writer when the output stream is open",
        test.closeWriter(writeId));
    try {
      test.flush(writeId);
      fail("Should not be able to flush when the output stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    try {
      test.newLine(writeId);
      fail("Should not be able to write a new line when the output stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    Properties props = new Properties();
    props.setProperty(key, value);
    test.store(props, writeId);
    long readId = test.openReader();
    test.readLine(readId);
    String line = test.readLine(readId);
    assertTrue("Should be able to store when the output stream is open\nline="+line, line.equals(key + '=' + value));
    test.closeReader(readId);
    try {
      test.write("string", writeId);
      fail("Should not be able to write when the output stream is open");
    }
    catch (LogFile.WriteException e) {
    }
    assertTrue("Should be able to close an open output stream.", test
        .closeOutputStream(writeId));
    assertFalse("Should not be able to close a closed output stream.", test
        .closeOutputStream(writeId));
    assertTrue(test.noLocks());
  }
  
  public void testIds() throws LogFile.WriteException,LogFile.ReadException,LogFile.FileException{
    LogFile testa = getInstance();
    LogFile testb = LogFile.getInstance(log.getParent(), AxisID.ONLY,
        ProcessName.ALIGN);
    long id0a = testa.openInputStream();
    long id0b = testb.openForWriting();
    assertEquals("Ids in different instances do not affect each other",id0a,id0b);
    testa.closeInputStream(id0a);
    testb.closeForWriting(id0b);
    long id1 = testa.openOutputStream();
    assertEquals("Ids should increment by one",id0a+1,id1);
    testa.closeOutputStream(id1);
    long id2 = testa.openForWriting();
    assertEquals("Ids should increment by one",id1+1,id2);
    long id3 = testa.openReader();
    assertEquals("Ids should increment by one",id2+1,id3);
    testa.closeForWriting(id2);
    long id4 = testa.openWriter();
    assertEquals("Ids should increment by one",id3+1,id4);
    testa.closeReader(id3);
    testa.closeWriter(id4);
    testa.backup();
    long id5 = testa.openWriter();
    assertEquals("backing up should cause the Id to increment",id4+2,id5);
    testa.closeWriter(id5);
    testa.delete();
    assertTrue(testa.create());
    long id6 = testa.openReader();
    assertEquals("deleting and creating should cause the Id to increment",id5+3,id6);
    testa.closeReader(id6);
    assertTrue(testa.noLocks());
  }

  private void assertWriteLock(LogFile test, long testWriteId)
      throws LogFile.FileException, LogFile.ReadException {
    assertTrue("The WRITE lock should be set", test.isOpen(
        LogFile.LockType.WRITE, testWriteId));
    try {
      test.backup();
      fail("Should not be able to backup with WRITE lock set");
    }
    catch (LogFile.FileException e) {
    }
    try {
      test.backupOnce();
      fail("Should not be able to backup with WRITE lock set");
    }
    catch (LogFile.FileException e) {
    }
    try {
      test.delete();
      fail("Should not be able to delete with WRITE lock set");
    }
    catch (LogFile.FileException e) {
    }
    assertTrue("Should be able to run exists() with WRITE lock set", test
        .exists());
    assertTrue("Should be able to run getAbsolutePath() with WRITE lock set",
        test.getAbsolutePath().equals(log.getAbsolutePath()));
    assertTrue("Should be able to run getName() with WRITE lock set", test
        .getName().equals(log.getName()));
    assertEquals("Should be able to run lastModified() with WRITE lock set",
        test.lastModified(), log.lastModified());
    assertEquals("Should not be able to open for writing with WRITE lock set",
        test.openForWriting(), LogFile.NO_ID);
    try {
      test.openInputStream();
      fail("Should not be able to open input stream with WRITE lock set");
    }
    catch (LogFile.WriteException e) {
    }
    try {
      test.openOutputStream();
      fail("Should not be able to open output stream with WRITE lock set");
    }
    catch (LogFile.WriteException e) {
    }
    long readId = test.openReader();
    assertFalse("Should be able to open a reader with WRITE lock set",
        readId == LogFile.NO_ID);
    try {
      test.openWriter();
      fail("Should not be able to open writer with WRITE lock set");
    }
    catch (LogFile.WriteException e) {
    }
    //Should be able to read line with reader open and WRITE lock set
    test.readLine(readId);
    //Should be able to close reader with reader open and WRITE lock set
    test.closeReader(readId);
  }

  private LogFile getInstance() {
    LogFile logFile = LogFile.getInstance(testDir.getAbsolutePath(), AxisID.ONLY,
        ProcessName.BLEND);
    return logFile;
  }

  private void createLog() {
    if (!log.exists()) {
      EtomoDirector.INSTANCE.getCurrentManager().touch(
          log.getAbsolutePath());
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
  }
}
