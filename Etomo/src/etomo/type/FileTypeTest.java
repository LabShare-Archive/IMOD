package etomo.type;

import java.util.Iterator;

import etomo.EtomoDirector;
import etomo.FrontPageManager;

import junit.framework.TestCase;

/**
 * <p>Description: Testing FileType for file name collisions.</p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
public class FileTypeTest extends TestCase {
  public static final String rcsid = "$Id$";
  
  protected void setUp() throws Exception{
    EtomoDirector.INSTANCE.closeCurrentManager(AxisID.ONLY, true);
    EtomoDirector.INSTANCE.openFrontPage(true, AxisID.ONLY);
    super.setUp();
  }

  /**
   * Test the file types with name descriptions to make sure that none of them
   * are identical.
   */
  public void testForFileNameCollisions() {
    Iterator iterator = FileType.iterator();
    while (iterator.hasNext()) {
      Iterator comparisonIterator = FileType.iterator();
      while (comparisonIterator.hasNext()) {
        FileType fileType = (FileType) iterator.next();
        FileType comparisonFileType = (FileType) comparisonIterator.next();
        if (fileType != comparisonFileType) {
          assertFalse("File name collison: " + fileType + " is the same as "
              + comparisonFileType, fileType.equals(comparisonFileType));
        }
      }
    }
  }

  public void testGetInstanceFromNameDescription() {
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FIDUCIAL_3D_MODEL, FileType.getInstance(true, true, "",
            ".3dmod"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FLATTEN_TOOL_OUTPUT, FileType.getInstance(true, false, "",
            ".flat"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT, FileType.getInstance(true,
            true, "_3dfind", ".ali"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FLATTEN_WARP_INPUT_MODEL, FileType.getInstance(true, false,
            "_flat", ".mod"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FIND_BEADS_3D_COMSCRIPT, FileType.getInstance(false, true,
            "findbeads3d", ".com"));
  }

  public void testGetInstanceFromFileName() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.DUAL_AXIS);
    metaData.setName("BB");
    assertEquals(
        "getInstance(BaseManager,AxisID,boolean,boolean,string) failed - axisID "
            + "only should be translated to axisID first",
        FileType.FIDUCIAL_3D_MODEL, FileType.getInstance(manager, AxisID.ONLY,
            true, true, "BBa.3dmod"));
    assertEquals(
        "getInstance(BaseManager,AxisID,boolean,boolean,string) failed",
        FileType.FLATTEN_TOOL_OUTPUT, FileType.getInstance(manager,
            AxisID.FIRST, true, false, "BB.flat"));
    assertEquals(
        "getInstance(BaseManager,AxisID,boolean,boolean,string) failed",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT, FileType.getInstance(manager,
            AxisID.SECOND, true, true, "BBb_3dfind.ali"));
    assertEquals(
        "getInstance(BaseManager,AxisID,boolean,boolean,string) failed",
        FileType.FLATTEN_WARP_INPUT_MODEL, FileType.getInstance(manager,
            AxisID.FIRST, true, false, "BB_flat.mod"));
    metaData.setAxisType(AxisType.SINGLE_AXIS);
    metaData.setName("BBa");
    assertEquals(
        "getInstance(BaseManager,AxisID,boolean,boolean,string) failed - axisID "
            + "first should be translated to axisID only",
        FileType.FIND_BEADS_3D_COMSCRIPT, FileType.getInstance(manager,
            AxisID.FIRST, false, true, "findbeads3d.com"));
  }
}
