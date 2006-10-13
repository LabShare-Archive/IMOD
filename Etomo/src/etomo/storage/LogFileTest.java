package etomo.storage;

import java.io.File;

import etomo.EtomoDirector;
import etomo.JUnitTests;
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

  protected void setUp() throws Exception {
    super.setUp();
    testDir.mkdirs();
    EtomoDirector.createInstance_test(JUnitTests.ETOMO_ARGUMENTS);
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
    readId1 = test.openReader();
    assertFalse(
        "Should be able to open a log file reader when it is opened for writing.",
        readId1 == LogFile.NO_ID);
    assertTrue(test.closeForWriting(writeId));
    assertTrue(test.closeReader(readId1));
    long readId2 = test.openReader();
    readId1 = test.openReader();
    assertFalse(
        "Should be able to open a log file reader when a reader is is already opened.",
        readId1 == LogFile.NO_ID);
    assertTrue(
        "A successful openReader() call should open the log file reader.", test
            .isOpen(LogFile.LockType.READ, readId1));
    try {
      test.backup();
      fail("Can't backup a log file with an open reader.");
    }
    catch (LogFile.FileException e) {
    }
    assertFalse(
        "CloseReader should fail when passed an id that doesn't match the one returned by the open call.",
        test.closeReader(LogFile.NO_ID));
    assertTrue(test.closeReader(readId1));
    assertTrue(test.closeReader(readId2));
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

  public void testOpenForWriting() throws LogFile.ReadException {
    LogFile test = getInstance();
    long writeId1 = test.openForWriting();
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
      LogFile.ReadException {
    LogFile test = getInstance();
    long writeId1 = test.openWriter();
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
    assertFalse("Should delete the file.",log.exists());
    assertFalse("Should return false when there is nothing to delete", test
        .delete());
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

  private LogFile getInstance() {
    LogFile logFile = LogFile.getInstance(log.getParent(), AxisID.ONLY,
        ProcessName.BLEND);
    return logFile;
  }

  private void createLog() {
    if (!log.exists()) {
      EtomoDirector.getInstance().getCurrentManager_test().touch(log);
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
  }
}
