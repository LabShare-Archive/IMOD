package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.BaseProcessManager;
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
 * <p> Revision 1.13  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.12  2009/10/23 22:24:41  sueh
 * <p> bug# 1275 No default manager.
 * <p>
 * <p> Revision 1.11  2009/03/17 00:45:12  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.10  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.9  2008/12/15 23:02:21  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 1.8  2008/01/31 20:22:25  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.7  2007/09/07 00:23:32  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
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

  private static final File testDir = new File(StorageTests.TEST_ROOT_DIR, "LogFile");
  //private static final int WAIT_LIMIT = 10;

  private final File log = new File(testDir, ProcessName.BLEND + DatasetFiles.LOG_EXT);
  private final File backupLog = new File(log.getAbsolutePath()
      + DatasetFiles.BACKUP_CHAR);
  private final BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();

  public LogFileTest() {
    super();
  }

  public LogFileTest(String test) {
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

  public void testGetInstance() throws LogFile.LockException {
    LogFile test = getInstance();
    LogFile dupTest = getInstance();
    //only one object is created when getInstance() is called with the same
    //parameter values
    assertTrue(test == dupTest);
    assertTrue(test.noLocks());
  }

  public void testOpenReader() throws LogFile.LockException, FileNotFoundException {
    LogFile test = getInstance();
    LogFile.ReaderId readerId1 = test.openReader();
    assertFalse("Should be able to open an unopened log file reader.", readerId1
        .isEmpty());
    assertTrue("Should be able to close an open reader.", test.closeRead(readerId1));
    LogFile.WritingId writingId = test.openForWriting();
    assertEquals("Ids should increment by one", readerId1.get() + 1, writingId.get());
    LogFile.ReaderId readerId2 = test.openReader();
    assertFalse(
        "Should be able to open a log file reader when it is opened for writing.",
        readerId2.isEmpty());
    assertEquals("Ids should increment by one", writingId.get() + 1, readerId2.get());
    assertTrue(test.closeForWriting(writingId));
    assertTrue(test.closeRead(readerId2));
    LogFile.ReaderId readerId3 = test.openReader();
    assertEquals("Ids should increment by one", readerId2.get() + 1, readerId3.get());
    LogFile.ReaderId readerId4 = test.openReader();
    assertEquals("Ids should increment by one", readerId3.get() + 1, readerId4.get());
    assertFalse(
        "Should be able to open a log file reader when a reader is is already opened.",
        readerId4.isEmpty());
    assertTrue("A successful openReader() call should open the log file reader.", test
        .isOpen(LogFile.LockType.READ, readerId4));
    try {
      test.backup();
      fail("Can't backup a log file with an open reader.");
    }
    catch (LogFile.LockException e) {
    }
    assertFalse(
        "CloseReader should fail when passed an id that doesn't match the one returned by the open call.",
        test.closeRead(new LogFile.ReaderId()));
    assertTrue(test.closeRead(readerId3));
    assertTrue(test.closeRead(readerId4));
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
    catch (FileNotFoundException e) {
    }
    assertTrue(test.noLocks());
  }

  public void testReadLine() throws LogFile.LockException, IOException {
    String line1 = "first line";
    LogFile test = getInstance();
    LogFile.WriterId writerId = test.openWriter();
    test.write(line1, writerId);
    test.newLine(writerId);
    assertTrue(test.closeWriter(writerId));
    LogFile.ReaderId readerId1 = test.openReader();
    assertTrue("Should return the first line of the file.", test.readLine(readerId1)
        .equals(line1));
    assertNull("Should return null.", test.readLine(readerId1));
    LogFile.ReaderId readerId2 = test.openReader();
    assertTrue("Should use a different reader to return the first line of the file.",
        test.readLine(readerId2).equals(line1));
    assertNull("Should return null.", test.readLine(readerId2));
    assertTrue(test.closeRead(readerId1));
    assertTrue(test.closeRead(readerId2));
    readerId1 = test.openReader();
    assertTrue("Should return the first line of the file when reusing a reader.", test
        .readLine(readerId1).equals(line1));
    assertNull("Should return null.", test.readLine(readerId1));
    assertTrue(test.closeRead(readerId1));
    assertTrue(test.noLocks());
  }

  public void testCloseReader() throws LogFile.LockException, FileNotFoundException {
    LogFile test = getInstance();
    LogFile.ReaderId readerId = new LogFile.ReaderId();
    readerId.set(0);
    assertFalse("Closing unnecessarily should fail.", test.closeRead(readerId));
    readerId = test.openReader();
    assertTrue("Should be able to close an opened log file.", test.closeRead(readerId));
    assertFalse(
        "The log file should be closed after closeReader() is called with the read id.",
        test.isOpen(LogFile.LockType.READ, readerId));
    assertTrue("Backup should succeed because the log file is closed.", test.backup());
    assertTrue(test.noLocks());
  }

  public void testOpenForWriting() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    LogFile.WritingId writingId1 = test.openForWriting();
    assertWriteLock(test, writingId1);
    assertFalse("Should be able to open an unopened log file for writing.", writingId1
        .isEmpty());
    assertTrue(
        "Should be able to call closeForWriting() on a log file where openForWriting() was used.",
        test.closeForWriting(writingId1));
    LogFile.ReaderId readerId1 = test.openReader();
    writingId1 = test.openForWriting();
    assertFalse(
        "Should be able to open an log file for writing when it is open for reading.",
        writingId1.isEmpty());
    assertTrue("A successful openForWriting call should open log file for writing.", test
        .isOpen(LogFile.LockType.WRITE, writingId1));
    LogFile.WritingId writingId2 = null;
    try {
      writingId2 = test.openForWriting();
      fail("Shouldn't be able to open a log file for writing when it is already open for writing.");
    }
    catch (LogFile.LockException e) {
    }
    try {
      test.backup();
      fail("Can't backup an open log file.");
    }
    catch (LogFile.LockException e) {
    }
    LogFile.ReaderId readerId2 = test.openReader();
    assertFalse(
        "Should be able to open a log file for reading when it is alrady open for writing.",
        readerId2.isEmpty());
    assertFalse(
        "CloseForWriting should fail when passed an id that doesn't match the one returned by the open call.",
        test.closeForWriting(writingId2));
    assertTrue(test.closeForWriting(writingId1));
    assertTrue(test.closeRead(readerId1));
    assertTrue(test.closeRead(readerId2));
    assertTrue(test.noLocks());
  }

  public void testOpenWriter() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    LogFile.WriterId writerId1 = test.openWriter();
    assertWriteLock(test, writerId1);
    assertFalse("Should be able to open an unopened log file for writing.", writerId1
        .isEmpty());
    assertTrue(test.closeWriter(writerId1));
    LogFile.ReaderId readerId1 = test.openReader();
    writerId1 = test.openWriter();
    assertFalse(
        "Should be able to open an log file for writing when it is open for reading.",
        writerId1.isEmpty());
    assertTrue("A successful openWriter call should open log file for writing.", test
        .isOpen(LogFile.LockType.WRITE, writerId1));
    LogFile.WriterId writerId2 = null;
    try {
      writerId2 = test.openWriter();
      fail("Shouldn't be able to open a log file writer when it is already has an open writer.");
    }
    catch (LogFile.LockException e) {
    }
    try {
      test.backup();
      fail("Can't backup an open log file.");
    }
    catch (LogFile.LockException e) {
    }
    LogFile.ReaderId readerId2 = test.openReader();
    assertFalse(
        "Should be able to open a log file for reading when it is alrady open for writing.",
        readerId2.isEmpty());
    assertTrue(test.closeWriter(writerId1));
    assertTrue(test.closeRead(readerId1));
    assertTrue(test.closeRead(readerId2));
    log.delete();
    assertTrue(test.noLocks());
  }

  public void testWrite() throws LogFile.LockException, FileNotFoundException,
      IOException {
    String line = "succeed";
    LogFile test = getInstance();
    LogFile.WriterId writerId = new LogFile.WriterId();
    writerId.set(0);
    try {
      test.write("fail", writerId);
      fail("Shouldn't be able to write to a log file that doesn't isn't open for"
          + "writing.");
    }
    catch (LogFile.LockException e) {
    }
    writerId = test.openWriter();
    test.write(line, writerId);
    assertTrue(test.closeWriter(writerId));
    LogFile.ReaderId readerId = test.openReader();
    assertTrue("Line should have been written to the file.", test.readLine(readerId)
        .equals(line));
    assertTrue(test.closeRead(readerId));
    assertTrue(test.noLocks());
  }

  public void testNewLine() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    LogFile.WriterId writerId = new LogFile.WriterId();
    writerId.set(0);
    try {
      test.newLine(writerId);
      fail("Shouldn't be able to write a new line to a log file that doesn't isn't open for"
          + "writing.");
    }
    catch (LogFile.LockException e) {
    }
    writerId = test.openWriter();
    test.newLine(writerId);
    test.closeWriter(writerId);
    LogFile.ReaderId readerId = test.openReader();
    assertTrue("New line should have been written to the file.", test.readLine(readerId)
        .equals(""));
    assertTrue(test.closeRead(readerId));
    assertTrue(test.noLocks());
  }

  public void testCloseForWriting() throws LogFile.LockException, IOException {
    LogFile test = getInstance();
    LogFile.WritingId writingId = new LogFile.WritingId();
    writingId.set(0);
    assertFalse("Closing unnecessarily should fail.", test.closeForWriting(writingId));
    writingId = test.openForWriting();
    assertTrue("Should be able to close an opened log file.", test
        .closeForWriting(writingId));
    assertFalse(
        "The log file should be closed after closeForWriting() is called with the write id.",
        test.isOpen(LogFile.LockType.WRITE, writingId));
    assertTrue("Backup should succeed because the log file is closed.", test.backup());
    createLog();
    assertTrue(log.exists());
    LogFile.WriterId writerId = test.openWriter();
    assertTrue("Opening the writer should not remove the file.", log.exists());
    assertTrue("Should be able to close an opened log file.", test.closeWriter(writerId));
    assertTrue("Opening the writer should not remove the file.", log.exists());
    assertFalse(
        "The log file should be closed after openWriter() is called with the write id.",
        test.isOpen(LogFile.LockType.WRITE, writerId));
    assertTrue("Backup should succeed because the log file is closed.", test.backup());
    assertTrue(test.noLocks());
  }

  public void testBackup() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    assertTrue("Should be able to backup an unopened log file.", test.backup());
    assertFalse("Backup should rename the log to .log~.", log.exists());
    assertTrue("Backup should rename the log to .log~.", backupLog.exists());
    assertFalse("Backup should fail when log has already been renamed.", test.backup());
    assertTrue(".log~ should not be deleted by a failed backup.", backupLog.exists());
    try {
      test.openReader();
      fail("Opening the file for reading should throw a file not found exception.");
    }
    catch (FileNotFoundException e) {
    }
    LogFile.WritingId writingId = test.openForWriting();
    assertFalse(
        "Should be able to open the log file for writing after the backup is done, even though the file doesn't exist.",
        writingId.isEmpty());
    assertTrue(test.closeForWriting(writingId));
    LogFile.WriterId writerId = test.openWriter();
    assertFalse(
        "Should be able to open the log file writer after the backup is done, even though the file doesn't exist.",
        writerId.isEmpty());
    assertTrue(test.closeWriter(writerId));
    assertTrue(test.noLocks());
  }

  public void testBackupOnce() throws LogFile.LockException {
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

  public void testDelete() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    LogFile.ReaderId readerId = test.openReader();
    try {
      test.delete();
      fail("Should throw an exception if a reader is open.");
    }
    catch (LogFile.LockException e) {
    }
    assertTrue(test.closeRead(readerId));
    LogFile.WritingId writingId = test.openForWriting();
    assertFalse(writingId.isEmpty());
    try {
      test.delete();
      fail("Should throw an exception if open for writing.");
    }
    catch (LogFile.LockException e) {
    }
    assertTrue(test.closeForWriting(writingId));
    LogFile.WriterId writerId = test.openWriter();
    try {
      test.delete();
      fail("Should throw an exception if the writer is open.");
    }
    catch (LogFile.LockException e) {
    }
    assertTrue(test.closeWriter(writerId));
    assertTrue(test.backup());
    createLog();
    assertTrue("Should be able to delete when it can get an exclusive lock.", test
        .delete());
    assertFalse("Should delete the file.", log.exists());
    assertFalse("Should return false when there is nothing to delete", test.delete());
    assertTrue(test.noLocks());
  }

  public void testMove() throws LogFile.LockException, FileNotFoundException, IOException {
    LogFile test = getInstance();
    test.delete();
    assertTrue(test.noLocks());
    LogFile target = LogFile.getInstance(testDir.getAbsolutePath(), "target");
    target.delete();
    LogFile.WriterId writerId = test.openWriter();
    try {
      test.move(target);
      fail("Should not be able to move when a writer is open.");
    }
    catch (LogFile.LockException e) {
    }
    test.closeWriter(writerId);
    writerId = target.openWriter();
    try {
      test.move(target);
      //     fail("Should not be able to move when a writer is open on the target.");
    }
    catch (LogFile.LockException e) {
    }
    target.closeWriter(writerId);
    target.create();
    assertTrue(target.exists());
    LogFile.ReaderId readerId = target.openReader();
    try {
      test.move(target);
      fail("Should not be able to backup when a reader is open on the target.");
    }
    catch (LogFile.LockException e) {
    }
    assertTrue("Should not be able to backup when a reader is open on the target.",
        target.exists());
    target.closeRead(readerId);
    test.create();
    assertTrue(test.exists());
    assertTrue(target.exists());
    test.move(target);
    assertFalse("Original file should not exist after backup.", test.exists());
    assertFalse(
        "Move should return false when attempting to move a file that doesn't exist.",
        test.move(target));
    assertTrue("Attempting to move a file that doesn't exist should not cause a backup.",
        target.exists());
    assertTrue(test.noLocks());
    assertTrue(target.noLocks());
  }

  public void testCreate() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    LogFile.ReaderId readerId = test.openReader();
    assertFalse("When file exists, shouldn't check locks; just return false", test
        .create());
    assertTrue(test.closeRead(readerId));
    log.delete();
    assertFalse(log.exists());
    LogFile.WritingId writingId = test.openForWriting();
    assertFalse(writingId.isEmpty());
    try {
      test.create();
      fail("Should throw file exception when WRITE locked");
    }
    catch (LogFile.LockException e) {
    }
    assertTrue(test.closeForWriting(writingId));
    test.create();
    assertTrue("Create should create the log file", log.exists());
    assertTrue(test.noLocks());
  }

  public void testIsOpen() throws LogFile.LockException, FileNotFoundException,
      IOException {
    LogFile test = getInstance();
    LogFile.ReaderId readerId = new LogFile.ReaderId();
    LogFile.WritingId writingId = new LogFile.WritingId();
    assertFalse("IsOpen should return false with nothing is opened.", test.isOpen(
        LogFile.LockType.WRITE, readerId));
    assertFalse("IsOpen should return false with nothing is opened.", test.isOpen(
        LogFile.LockType.WRITE, writingId));
    readerId = test.openReader();
    assertTrue("IsOpen should return true after openForReading() is called.", test
        .isOpen(LogFile.LockType.READ, readerId));
    assertFalse("The LockType must match the type of open.", test.isOpen(
        LogFile.LockType.WRITE, readerId));
    assertTrue(test.closeRead(readerId));
    writingId = test.openForWriting();
    assertTrue("IsOpen should return true after openForWriting() is called.", test
        .isOpen(LogFile.LockType.WRITE, writingId));
    assertFalse("The LockType must match the type of open.", test.isOpen(
        LogFile.LockType.READ, writingId));
    readerId = test.openReader();
    assertTrue("IsOpen should return true after openForReading() is called.", test
        .isOpen(LogFile.LockType.READ, readerId));
    assertTrue("The log file should be open after openForWriting() is called.", test
        .isOpen(LogFile.LockType.WRITE, writingId));
    assertTrue(test.closeRead(readerId));
    assertTrue(test.closeForWriting(writingId));
    assertFalse("IsOpen should return false when log file is closed.", test.isOpen(
        LogFile.LockType.READ, readerId));
    assertFalse("IsOpen should return false when log file is closed.", test.isOpen(
        LogFile.LockType.WRITE, writingId));
    LogFile.WriterId writerId = test.openWriter();
    assertTrue("The log file should be open after openWriter() is called.", test.isOpen(
        LogFile.LockType.WRITE, writerId));
    assertTrue(test.closeWriter(writerId));
    assertFalse("IsOpen should return false when log file is closed.", test.isOpen(
        LogFile.LockType.WRITE, writerId));
    assertTrue(test.noLocks());
  }

  public void testInputStream() throws LogFile.LockException, IOException {
    LogFile test = getInstance();
    LogFile.WriterId writerId = test.openWriter();
    String key = "key";
    String value = "1";
    test.write(key + '=' + value, writerId);
    test.newLine(writerId);
    test.closeWriter(writerId);
    LogFile.InputStreamId inputStreamId = test.openInputStream();
    assertFalse("Should be able to open an input stream when there are no locks.",
        inputStreamId.isEmpty());
    assertWriteLock(test, inputStreamId);
    Properties props = new Properties();
    test.load(props, inputStreamId);
    assertTrue("Should be able to load with input stream open", props.getProperty(key)
        .equals(value));
    assertTrue("Should be able to close an open input stream.", test
        .closeInputStream(inputStreamId));
    assertFalse("Should be able to close a closed input stream.", test
        .closeInputStream(inputStreamId));
    assertTrue(test.noLocks());
  }

  public void testOutputStream() throws LogFile.LockException, FileNotFoundException,
      IOException {
    String key = "key";
    String value = "1";
    LogFile test = getInstance();
    LogFile.OutputStreamId outputStreamId = test.openOutputStream();
    assertFalse("Should be able to open an output stream when there are no locks.",
        outputStreamId.isEmpty());
    assertWriteLock(test, outputStreamId);
    Properties props = new Properties();
    props.setProperty(key, value);
    test.store(props, outputStreamId);
    LogFile.ReaderId readerId = test.openReader();
    test.readLine(readerId);
    String line = test.readLine(readerId);
    assertTrue("Should be able to store when the output stream is open\nline=" + line,
        line.equals(key + '=' + value));
    test.closeRead(readerId);
    assertTrue("Should be able to close an open output stream.", test
        .closeOutputStream(outputStreamId));
    assertFalse("Should not be able to close a closed output stream.", test
        .closeOutputStream(outputStreamId));
    assertTrue(test.noLocks());
  }

  public void testIds() throws LogFile.LockException, FileNotFoundException, IOException {
    LogFile testa = getInstance();
    LogFile testb = LogFile.getInstance(log.getParent(), AxisID.ONLY, ProcessName.ALIGN);
    LogFile.InputStreamId id0a = testa.openInputStream();
    LogFile.WritingId id0b = testb.openForWriting();
    assertEquals("Ids in different instances do not affect each other", id0a.get(), id0b
        .get());
    testa.closeInputStream(id0a);
    testb.closeForWriting(id0b);
    LogFile.OutputStreamId id1 = testa.openOutputStream();
    assertEquals("Ids should increment by one", id0a.get() + 1, id1.get());
    testa.closeOutputStream(id1);
    LogFile.WritingId id2 = testa.openForWriting();
    assertEquals("Ids should increment by one", id1.get() + 1, id2.get());
    LogFile.ReaderId id3 = testa.openReader();
    assertEquals("Ids should increment by one", id2.get() + 1, id3.get());
    testa.closeForWriting(id2);
    LogFile.WriterId id4 = testa.openWriter();
    assertEquals("Ids should increment by one", id3.get() + 1, id4.get());
    testa.closeRead(id3);
    testa.closeWriter(id4);
    testa.backup();
    LogFile.WriterId id5 = testa.openWriter();
    assertEquals("backing up should cause the Id to increment", id4.get() + 2, id5.get());
    testa.closeWriter(id5);
    testa.delete();
    assertTrue(testa.create());
    LogFile.ReaderId id6 = testa.openReader();
    assertEquals("deleting and creating should cause the Id to increment", id5.get() + 3,
        id6.get());
    testa.closeRead(id6);
    assertTrue(testa.noLocks());
  }

  private void assertWriteLock(LogFile test, LogFile.Id testWriteId)
      throws LogFile.LockException, FileNotFoundException, IOException {
    assertTrue("The WRITE lock should be set", test.isOpen(LogFile.LockType.WRITE,
        testWriteId));
    try {
      test.backup();
      fail("Should not be able to backup with WRITE lock set");
    }
    catch (LogFile.LockException e) {
    }
    try {
      test.backupOnce();
      fail("Should not be able to backup with WRITE lock set");
    }
    catch (LogFile.LockException e) {
    }
    try {
      test.delete();
      fail("Should not be able to delete with WRITE lock set");
    }
    catch (LogFile.LockException e) {
    }
    assertTrue("Should be able to run exists() with WRITE lock set", test.exists());
    assertTrue("Should be able to run getAbsolutePath() with WRITE lock set", test
        .getAbsolutePath().equals(log.getAbsolutePath()));
    assertTrue("Should be able to run getName() with WRITE lock set", test.getName()
        .equals(log.getName()));
    assertEquals("Should be able to run lastModified() with WRITE lock set", test
        .lastModified(), log.lastModified());
    LogFile.WritingId writingId = null;
    try {
      writingId = test.openForWriting();
      fail("should throw exception when throwException is true and and WRITE lock is already set");
    }
    catch (LogFile.LockException e) {
    }
    try {
      test.openInputStream();
      fail("Should not be able to open input stream with WRITE lock set");
    }
    catch (LogFile.LockException e) {
    }
    try {
      test.openOutputStream();
      fail("Should not be able to open output stream with WRITE lock set");
    }
    catch (LogFile.LockException e) {
    }
    LogFile.ReaderId readerId = test.openReader();
    assertFalse("Should be able to open a reader with WRITE lock set", readerId.isEmpty());
    try {
      test.openWriter();
      fail("Should not be able to open writer with WRITE lock set");
    }
    catch (LogFile.LockException e) {
    }
    //Should be able to read line with reader open and WRITE lock set
    test.readLine(readerId);
    //Should be able to close reader with reader open and WRITE lock set
    test.closeRead(readerId);
  }

  private LogFile getInstance() throws LogFile.LockException {
    LogFile logFile = LogFile.getInstance(testDir.getAbsolutePath(), AxisID.ONLY,
        ProcessName.BLEND);
    return logFile;
  }

  private void createLog() {
    if (!log.exists()) {
      BaseProcessManager.touch(log.getAbsolutePath(), manager);
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
  }
}
