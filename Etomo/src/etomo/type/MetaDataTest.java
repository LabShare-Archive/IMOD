package etomo.type;

import java.io.File;
import java.io.IOException;

import etomo.EtomoDirector;
import etomo.process.SystemProcessException;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.util.InvalidParameterException;
import etomo.util.TestUtilites;
import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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

  public MetaDataTest() throws IOException, InvalidParameterException {
    //  Check out the test vectors from the CVS repository

    //  Create the test directory
    TestUtilites.makeDirectories(new File(TypeTests.testRoot, testDirectory).getAbsolutePath());

    // Set the working directory to the current test directory for this package
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String originalDirectory = etomoDirector.getCurrentPropertyUserDir();
    etomoDirector.setCurrentPropertyUserDir(new File(originalDirectory,
        TypeTests.testRoot).getAbsolutePath());

    for (int i = 0; i < edfList.length; i++) {
      try {
        TestUtilites.checkoutVector(testDirectory, edfList[i]);
      }
      catch (SystemProcessException except) {
        etomoDirector.setCurrentPropertyUserDir(originalDirectory);
        System.err.println(except.getMessage());
        fail("Error checking out test vector: " + new File(testDirectory, edfList[i]).getAbsolutePath());
      }
    }

    // Switch back to the original working directory
    etomoDirector.setCurrentPropertyUserDir(originalDirectory);
  }

  /*
   * Class to test for void store(Properties)
   */
  public void testStoreProperties() throws IOException {
    String workingDirectory = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), TypeTests.testRoot + File.separator
        + testDirectory).getAbsolutePath();
    Storable[] storable = new Storable[1];

    for (int i = 0; i < edfList.length; i++) {
      MetaData origMetaData = new MetaData();
      storable[0] = origMetaData;

      //  Load in the original etomo data file
      File origFile = new File(workingDirectory, edfList[i]);
      ParameterStore paramStore = new ParameterStore(origFile);
      paramStore.load(storable);

      //  Create a new output file
      File newFile = new File(workingDirectory, edfList[i] + "new");
      paramStore = new ParameterStore(newFile);

      //  Write out the meta data to the new file
      paramStore.save(storable);

      //  Load in the new file
      MetaData newMetaData = new MetaData();
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
    File storeDir = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), TypeTests.testRoot + File.separator
        + testDirectory);
    for (int i = 0; i < edfList.length; i++) {
      MetaData metaData = new MetaData();
      storable[0] = metaData;
      ParameterStore paramStore = new ParameterStore(new File(storeDir, edfList[i]));
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