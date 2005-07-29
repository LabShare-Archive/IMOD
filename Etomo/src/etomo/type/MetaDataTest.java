package etomo.type;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.EtomoDirectorTestHarness;
import etomo.process.SystemProcessException;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.util.InvalidParameterException;
import etomo.util.TestUtilites;
import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.12  2004/12/09 04:58:13  sueh
 * <p> bug# 520 Removed unnecessary import.
 * <p>
 * <p> Revision 3.11  2004/12/08 21:31:41  sueh
 * <p> bug# 520 Setting the working directory in TestUtilities.checkoutVector().
 * <p> Also setting the fail message for SystemProcessException in
 * <p> TestUtilities.checkoutVector().
 * <p>
 * <p> Revision 3.10  2004/12/08 03:47:42  sueh
 * <p> bug# 520 improving failure message!!!!!!!!!!!#!@$@!#%#Q$^#%&#$
 * <p>
 * <p> Revision 3.9  2004/12/08 00:13:07  sueh
 * <p> bug# 520 Passing a relative vector to checkoutVector.
 * <p>
 * <p> Revision 3.8  2004/12/07 23:36:26  sueh
 * <p> bug# 520 Changing print statements.
 * <p>
 * <p> Revision 3.7  2004/12/06 23:35:47  sueh
 * <p> bug# 520 Added print statements.
 * <p>
 * <p> Revision 3.6  2004/11/30 18:03:37  sueh
 * <p> bug# 520  Fixing unit tests:  putting setup code in the SetUp() instead of
 * <p> constructor
 * <p>
 * <p> Revision 3.5  2004/11/24 22:15:47  sueh
 * <p> bug# 520 Initializing, creating, and testing testDir in the constructor and
 * <p> using it everywhere in MetaDataTest.
 * <p>
 * <p> Revision 3.4  2004/11/24 01:28:41  sueh
 * <p> bug# 520 MetaDataTest(): Getting the correct file path for error reporting.
 * <p>
 * <p> Revision 3.3  2004/11/23 00:27:38  sueh
 * <p> bug# 520 Using get and setPropertyUserDir instead of Property.  Don't
 * <p> use File.separator with propertyUserDir since it may end in "/".  Construct
 * <p> a new file with originalDirectory as the base directory and get the absolute
 * <p> file.
 * <p>
 * <p> Revision 3.2  2004/06/01 18:56:00  rickg
 * <p> Import fix for javadoc
 * <p>
 * <p> Revision 3.1  2004/04/06 03:24:31  rickg
 * <p> Create MetaDataTest
 * <p> </p>
 */

public class MetaDataTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final String testDirectory = "MetaData";
  private String[] edfList = {"setup_revision_1_2.edf",
      "setup_revision_1_3.edf", "setup_revision_1_4.edf",
      "setup_revision_1_5.edf"};
  
  private File testDir;
  private final ApplicationManager manager;
  
  public MetaDataTest() {
    EtomoDirector.createInstance(new String[] {"--test"});
    manager = (ApplicationManager) EtomoDirectorTestHarness.getCurrentManager();
  }

  protected void setUp() throws Exception {
    super.setUp();
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    File checkoutDirectory = new File(manager.getPropertyUserDir(),
        TypeTests.testRoot);
    testDir = new File(checkoutDirectory, testDirectory);
    if (!testDir.exists()) {
      assertTrue(testDir.mkdirs());
    }
    assertTrue(testDir.isDirectory() && testDir.canRead() && testDir.canWrite());
    
    //  Check out the test vectors from the CVS repository
    for (int i = 0; i < edfList.length; i++) {
      try {
        TestUtilites.checkoutVector(manager, checkoutDirectory
            .getAbsolutePath(), testDirectory, edfList[i]);
      }
      catch (SystemProcessException except) {
        System.err.println(except.getMessage());
        fail("Error checking out test vector:\n" + except.getMessage());
      }
    }
  }
  
  /*
   * Class to test for void store(Properties)
   */
  public void testStoreProperties() throws IOException {
    Storable[] storable = new Storable[1];
    
    for (int i = 0; i < edfList.length; i++) {
      MetaData origMetaData = new MetaData(manager);
      storable[0] = origMetaData;

      //  Load in the original etomo data file
      File origFile = new File(testDir, edfList[i]);
      ParameterStore paramStore = new ParameterStore(origFile);
      paramStore.load(storable);

      //  Create a new output file
      File newFile = new File(testDir, edfList[i] + "new");
      paramStore = new ParameterStore(newFile);

      //  Write out the meta data to the new file
      paramStore.save(storable);

      //  Load in the new file
      MetaData newMetaData = new MetaData(manager);
      storable[0] = newMetaData;
      paramStore.load(storable);

      //  Compare it to the old metadata object
      assertTrue("MetaData not invariant in : " + origFile.getAbsolutePath()
          + ", " + newFile.getAbsolutePath(), origMetaData.equals(newMetaData));
    }
  }

  /*
   * Class to test for void load(Properties)
   */
  public void testLoadProperties() throws IOException,
      InvalidParameterException {

    Storable[] storable = new Storable[1];
    for (int i = 0; i < edfList.length; i++) {
      MetaData metaData = new MetaData(manager);
      storable[0] = metaData;
      ParameterStore paramStore = new ParameterStore(new File(testDir, edfList[i]));
      paramStore.load(storable);
      //  Check some basic parameters to see if we actually loaded something
      if (metaData.getDatasetName().equals("")) {
        fail("Did not read dataset name in " + edfList[i]);
      }

      if (Float.isNaN(metaData.getImageRotation(AxisID.FIRST))) {
        fail("Axis rotation unspecified in A axis " + edfList[i]);
      }

      if (Float.isNaN(metaData.getImageRotation(AxisID.SECOND))) {
        fail("Axis rotation unspecified in B axis " + edfList[i]);
      }
    }
  }

}