package etomo.storage;

import java.util.Map;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.util.TestUtilites;
import junit.framework.TestCase;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class ComFileTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private static final String COM_FILE_NAME = "align";
  private static final AxisID AXIS_ID = AxisID.FIRST;
  private static final String FIRST_PROGRAM_NAME = "tiltalign";
  private static final String SECOND_PROGRAM_NAME = "xfproduct";

  public void testComFile() {
    String unitTestDataFileName = TestUtilites.INSTANCE.getUnitTestData()
        .getAbsolutePath();
    BaseManager  manager = (BaseManager) EtomoDirector.INSTANCE.getCurrentManagerForTest();
    manager.setPropertyUserDir(unitTestDataFileName);

    ComFile comFile = new ComFile(manager, AXIS_ID);
    assertFalse(".com file name hasn't been set yet",
        comFile.equalsComFileName(COM_FILE_NAME));
    comFile.setComFileName(COM_FILE_NAME);
    assertTrue(".com file has been set", comFile.equalsComFileName(COM_FILE_NAME));
    assertFalse("program name hasn't been set yet",
        comFile.equalsProgramName(SECOND_PROGRAM_NAME));

    StringBuffer errmsg = new StringBuffer();
    Map<String, String> commandMap = comFile.getCommandMap(SECOND_PROGRAM_NAME, errmsg);
    assertNotNull("should find valid program name", commandMap);
    assertEquals("should find valid program name", 0, errmsg.length());
    assertEquals("gets all of the parameters", 3, commandMap.size());
    assertEquals("first parameter", "BBa.prexg", commandMap.get("InputFile1"));
    assertEquals("second parameter", "BBa.tltxf", commandMap.get("InputFile2"));
    assertEquals("third parameter", "BBa_fid.xf", commandMap.get("OutputFile"));
    assertNull("does not save the next program name", commandMap.get("$b3dcopy"));
    assertTrue(".com file is still open", comFile.equalsComFileName(COM_FILE_NAME));
    assertTrue("program name hasn't been changed",
        comFile.equalsProgramName(SECOND_PROGRAM_NAME));

    assertFalse("different program name", comFile.equalsProgramName(FIRST_PROGRAM_NAME));
    commandMap = comFile.getCommandMap(FIRST_PROGRAM_NAME, errmsg);
    assertNotNull("able to reopen file and find valid program name", commandMap);
    assertEquals("should find valid program name", 0, errmsg.length());
    assertNull("ignores comments", commandMap.get("#ImageSizeXandY"));
    assertNull("ignores comments", commandMap.get("ImageSizeXandY"));
    assertEquals("gets the whole value", "700, 700",
        commandMap.get("TargetPatchSizeXandY"));
    assertTrue("handles missing value", commandMap.containsKey("ShiftZFromOriginal"));
    assertNull("handles missing value", commandMap.get("ShiftZFromOriginal"));
    assertEquals("not confused by comments", "3.0",
        commandMap.get("ResidualReportCriterion"));
    assertEquals("not confused by empty lines", "7",
        commandMap.get("XStretchDefaultGrouping"));
    assertNull("does not save the program name", commandMap.get("$tiltalign"));

    commandMap = comFile.getCommandMap("fake", errmsg);
    assertNull("return null for a program name that doesn't exist", commandMap);
    assertEquals("missing program name is not an error", 0, errmsg.length());
    assertTrue("program name hasn't been changed", comFile.equalsProgramName("fake"));
    assertTrue(".com file hasn't been changed", comFile.equalsComFileName(COM_FILE_NAME));

    comFile.setComFileName("fake");
    commandMap = comFile.getCommandMap(FIRST_PROGRAM_NAME, errmsg);
    assertNull("missing file", commandMap);
    assertEquals("missing file is not an error", 0, errmsg.length());
  }
}
