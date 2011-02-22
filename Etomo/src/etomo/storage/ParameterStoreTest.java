package etomo.storage;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

import junit.framework.TestCase;

/**
 * <p>Description: </p>
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
 * <p> Revision 1.8  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.7  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.6  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.5  2008/01/31 20:23:09  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.4  2007/09/07 00:24:27  sueh
 * <p> bug# 989 Do not have to create the EtomoDirector instance because
 * <p> EtomoDirector.main is being run by JUnitTest.
 * <p>
 * <p> Revision 1.3  2007/07/30 18:53:33  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.2  2006/11/18 01:16:05  sueh
 * <p> bug# 956 Temporarily not running problem bugs on Windows.
 * <p>
 * <p> Revision 1.1  2006/11/15 20:40:34  sueh
 * <p> bug# 872 Testing ParameterStore.
 * <p> </p>
 */
public final class ParameterStoreTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final File testDir = new File(StorageTests.TEST_ROOT_DIR,
      "ParameterStore");
  private static final File testFile = new File(testDir, "test-file");
  private static final File backupFile = new File(testFile.getAbsolutePath()
      + DatasetFiles.BACKUP_CHAR);
  private Data setupData = null;

  public ParameterStoreTest() {
    super();
  }

  public ParameterStoreTest(String test) {
    super(test);
  }

  protected void setUp() throws Exception {
    super.setUp();
    setupData = new Data(1, 2.3, "four", 5);
    //make test dir
    testDir.mkdirs();
    initFiles();
  }

  private void initFiles() throws LogFile.LockException, IOException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    //make test file
    if (testFile.exists()) {
      testFile.delete();
    }
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    assertFalse(testFile.exists());
    testFile.createNewFile();
    assertTrue(testFile.exists());
    Properties props = new Properties();
    setupData.store(props);
    FileOutputStream outputStream = new FileOutputStream(testFile);
    props.store(outputStream, null);
    //delete backup file
    if (backupFile.exists()) {
      backupFile.delete();
    }
    assertFalse(backupFile.exists());
  }

  public void testParameterStore() throws LogFile.LockException {
    ParameterStore psTest = ParameterStore.getInstance(testFile);
    LogFile.reset();
  }

  public void testLoadProperties() throws LogFile.LockException, IOException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    //test loading
    ParameterStore psTest = ParameterStore.getInstance(testFile);
    Data testData = new Data();
    psTest.load(testData);
    assertTrue("Should load the data stored in testFile:\ntestData=" + testData
        + ",setupData=" + setupData, testData.equals(setupData));
    //test not reloading
    Data newData = new Data(6, 7.8, "nine", 10);
    ParameterStore psControl = ParameterStore.getInstance(testFile);
    psControl.save(newData);
    psTest.load(testData);
    assertFalse(
        "Should only load the data once - ignores changes done by another class:\ntestData="
            + testData + ",newData=" + newData, testData.equals(newData));
    //test hand modification
    psTest = ParameterStore.getInstance(testFile);
    psTest.load(testData);
    assertTrue(
        "Creating a new instance of ParameterStore should allow reloading (simulates hand modification):\ntestData="
            + testData + ",newData=" + newData, testData.equals(newData));
    LogFile.reset();
  }

  public void testStoreProperties() throws LogFile.LockException, IOException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    //test backup
    ParameterStore psTest = ParameterStore.getInstance(testFile);
    Data testData = new Data(6, 7.8, "nine", 10);
    psTest.save(testData);
    ParameterStore psBackup = ParameterStore.getInstance(backupFile);
    Data backupData = new Data();
    psBackup.load(backupData);
    assertTrue("File should be backed up the first time it is modified.\nbackupData="
        + backupData + ",setupData=" + setupData, backupData.equals(setupData));
    //test backup only once per instance
    Data newData = new Data(11, 12.13, "fourteen", 15);
    psTest.save(newData);
    psBackup = ParameterStore.getInstance(backupFile);
    backupData = new Data();
    psBackup.load(backupData);
    assertTrue(
        "File should be backed up only the first time it is modified.\nbackupData="
            + backupData + ",setupData=" + setupData + "testData=" + testData, backupData
            .equals(setupData));
    //test storing data
    psTest = ParameterStore.getInstance(testFile);
    testData = new Data();
    psTest.load(testData);
    assertTrue("Data should be stored.\ntestData=" + testData + ",newData=" + newData,
        testData.equals(newData));
    LogFile.reset();
  }

  public void testLoad() throws LogFile.LockException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    ParameterStore psTest = ParameterStore.getInstance(testFile);
    Data testData = new Data();
    psTest.load(testData);
    assertTrue("Should be able to load the file.\ntestData=" + testData + ",setupData="
        + setupData, testData.equals(setupData));
    LogFile.reset();
  }

  public void testSave() throws LogFile.LockException, IOException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    ParameterStore psTest = ParameterStore.getInstance(testFile);
    Data testData = new Data(6, 7.8, "nine", 10);
    psTest.save(testData);
    Data loadedData = new Data();
    psTest.load(loadedData);
    assertTrue("The data should be saved in testFile.\ntestData=" + testData
        + ",loadedData=" + loadedData, testData.equals(loadedData));
    //test autoStore == false
    initFiles();
    psTest = ParameterStore.getInstance(testFile);
    psTest.setAutoStore(false);
    testData = new Data(11, 12.13, "fourteen", 15);
    psTest.save(testData);
    ParameterStore psReload = ParameterStore.getInstance(testFile);//load from file
    Data reloadData = new Data();
    psReload.load(reloadData);
    assertFalse(
        "(AutoStore == false) should prevent save() from writing the file.\ntestData="
            + testData + ",reloadData=" + reloadData, testData.equals(reloadData));
    String moreDataPrepend = "more";
    Data testMoreData = new Data(moreDataPrepend, 16, 17.18, "nineteen", 20);
    psTest.save(testMoreData);
    psReload = ParameterStore.getInstance(testFile);//load from file
    Data reloadMoreData = new Data(moreDataPrepend);
    psReload.load(reloadMoreData);
    assertFalse(
        "(AutoStore == false) should prevent save() from writing the file.\ntestMoreData="
            + testMoreData + ",reloadMoreData=" + reloadMoreData, testMoreData
            .equals(reloadMoreData));
    psTest.storeProperties();
    psReload = ParameterStore.getInstance(testFile);//load from file
    psReload.load(reloadData);
    assertTrue("StoreProperties should save to the file.\ntestData=" + testData
        + ",reloadData=" + reloadData, testData.equals(reloadData));
    psReload.load(reloadMoreData);
    assertTrue("StoreProperties should save to the file.\ntestMoreData=" + testMoreData
        + ",reloadMoreData=" + reloadMoreData, testMoreData.equals(reloadMoreData));
    LogFile.reset();
  }

  private final class Data implements Storable {
    private static final String I_KEY = "i";
    private static final String D_KEY = "d";
    private static final String STRING_KEY = "string";
    private static final int I_DEFAULT = Integer.MIN_VALUE;
    private static final double D_DEFAULT = Double.NaN;

    private int i = I_DEFAULT;
    private double d = Double.NaN;
    private String string = null;
    private EtomoNumber number = new EtomoNumber("number");
    private String prepend = null;

    Data() {
    }

    Data(String prepend) {
      this.prepend = prepend;
    }

    Data(int i, double d, String string, int number) {
      this.i = i;
      this.d = d;
      this.string = string;
      this.number.set(number);
    }

    Data(String prepend, int i, double d, String string, int number) {
      this(i, d, string, number);
      this.prepend = prepend;
    }

    void setI(int i) {
      this.i = i;
    }

    int getI() {
      return i;
    }

    double getD() {
      return d;
    }

    void setString(String string) {
      this.string = string;
    }

    String getString() {
      return string;
    }

    int getNumber() {
      return number.getInt();
    }

    public void load(Properties properties) {
      String prepend = this.prepend == null ? "" : this.prepend + '.';
      i = Integer.parseInt(properties.getProperty(prepend + I_KEY, String
          .valueOf(I_DEFAULT)));
      d = Double.parseDouble(properties.getProperty(prepend + D_KEY, String
          .valueOf(D_DEFAULT)));
      string = properties.getProperty(prepend + STRING_KEY);
      number.load(properties, this.prepend);
    }

    public void store(Properties properties) {
      String prepend = this.prepend == null ? "" : this.prepend + '.';
      if (i == I_DEFAULT) {
        properties.remove(prepend + I_KEY);
      }
      else {
        properties.setProperty(prepend + I_KEY, String.valueOf(i));
      }
      if (Double.isNaN(d)) {
        properties.remove(prepend + D_KEY);
      }
      else {
        properties.setProperty(prepend + D_KEY, String.valueOf(d));
      }
      if (string == null) {
        properties.remove(prepend + STRING_KEY);
      }
      else {
        properties.setProperty(prepend + STRING_KEY, string);
      }
      number.store(properties, this.prepend);
    }

    public boolean equals(Data data) {
      if (i != data.i) {
        return false;
      }
      if (d != data.d) {
        return false;
      }
      if (!string.equals(data.string)) {
        return false;
      }
      if (!number.equals(data.number)) {
        return false;
      }
      return true;
    }

    public String toString() {
      return "[i=" + i + ",d=" + d + ",string=" + string + ",number=" + number + "]";
    }
  }
}
