package etomo.logic;

import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DataFileType;
import junit.framework.TestCase;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public class DatasetToolTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private static final String ROOT = "root";
  private static final String DIFFERENT_ROOT = "diff";

  public void testCanShareWith_DataFileType_String_String() {
    // handle incorrect data file type
    assertFalse(
        "missing axis types => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), ROOT + DataFileType.RECON.extension));
    // Get the type of the existing data file
    assertTrue("null data file name => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, null, DIFFERENT_ROOT));
    assertTrue("no data file => yes", DatasetTool.canShareWith(DataFileType.JOIN,
        ROOT + ".notADataFile", DIFFERENT_ROOT));

    // ejf
    assertTrue(
        "ejf share with same file => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "ejf share with edf => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, DIFFERENT_ROOT, ROOT
            + DataFileType.RECON.extension));
    assertTrue(
        "ejf share with different ejf => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, DIFFERENT_ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "ejf share with epp => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, DIFFERENT_ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertTrue(
        "ejf share with epe => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, DIFFERENT_ROOT, ROOT
            + DataFileType.PEET.extension));
    assertFalse(
        "ejf share with ess => no",
        DatasetTool.canShareWith(DataFileType.JOIN, DIFFERENT_ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
    // epp
    assertTrue(
        "epp share with same file => yes",
        DatasetTool.canShareWith(DataFileType.PARALLEL, ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertTrue(
        "epp share with edf => yes",
        DatasetTool.canShareWith(DataFileType.PARALLEL, DIFFERENT_ROOT, ROOT
            + DataFileType.RECON.extension));
    assertTrue(
        "epp share with ejf => yes",
        DatasetTool.canShareWith(DataFileType.PARALLEL, DIFFERENT_ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "epp share with different epp => yes",
        DatasetTool.canShareWith(DataFileType.PARALLEL, DIFFERENT_ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertTrue(
        "epp share with epe => yes",
        DatasetTool.canShareWith(DataFileType.PARALLEL, DIFFERENT_ROOT, ROOT
            + DataFileType.PEET.extension));
    assertTrue(
        "epp share with ess => yes",
        DatasetTool.canShareWith(DataFileType.PARALLEL, DIFFERENT_ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
    // epe
    assertTrue(
        "epe share with same file => yes",
        DatasetTool.canShareWith(DataFileType.PEET, ROOT, ROOT
            + DataFileType.PEET.extension));
    assertTrue(
        "epe share with edf => yes",
        DatasetTool.canShareWith(DataFileType.PEET, DIFFERENT_ROOT, ROOT
            + DataFileType.RECON.extension));
    assertTrue(
        "epe share with ejf => yes",
        DatasetTool.canShareWith(DataFileType.PEET, DIFFERENT_ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "epe share with epp => yes",
        DatasetTool.canShareWith(DataFileType.PEET, DIFFERENT_ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertFalse(
        "epe share with different epe => no",
        DatasetTool.canShareWith(DataFileType.PEET, DIFFERENT_ROOT, ROOT
            + DataFileType.PEET.extension));
    assertFalse(
        "epe share with ess => no",
        DatasetTool.canShareWith(DataFileType.PEET, DIFFERENT_ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
    // ess
    assertTrue(
        "ess share with same file => yes",
        DatasetTool.canShareWith(DataFileType.SERIAL_SECTIONS, ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
    assertFalse(
        "ess share with edf => no",
        DatasetTool.canShareWith(DataFileType.SERIAL_SECTIONS, DIFFERENT_ROOT, ROOT
            + DataFileType.RECON.extension));
    assertFalse(
        "ess share with ejf => no",
        DatasetTool.canShareWith(DataFileType.SERIAL_SECTIONS, DIFFERENT_ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "ess share with epp => yes",
        DatasetTool.canShareWith(DataFileType.SERIAL_SECTIONS, DIFFERENT_ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertFalse(
        "ess share with epe => no",
        DatasetTool.canShareWith(DataFileType.SERIAL_SECTIONS, DIFFERENT_ROOT, ROOT
            + DataFileType.PEET.extension));
    assertFalse(
        "ess share with different ess => no",
        DatasetTool.canShareWith(DataFileType.SERIAL_SECTIONS, DIFFERENT_ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
    // tools - different root
    assertTrue(
        "tools share with edf (different root) => yes",
        DatasetTool.canShareWith(DataFileType.TOOLS, DIFFERENT_ROOT, ROOT
            + DataFileType.RECON.extension));
    assertTrue(
        "tools share with ejf (different root) => yes",
        DatasetTool.canShareWith(DataFileType.TOOLS, DIFFERENT_ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "tools share with epp (different root) => yes",
        DatasetTool.canShareWith(DataFileType.TOOLS, DIFFERENT_ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertTrue(
        "tools share with epe (different root) => yes",
        DatasetTool.canShareWith(DataFileType.TOOLS, DIFFERENT_ROOT, ROOT
            + DataFileType.PEET.extension));
    assertTrue(
        "tools share with ess (different root) => yes",
        DatasetTool.canShareWith(DataFileType.TOOLS, DIFFERENT_ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
    // tools - same root
    assertFalse(
        "tools share with edf (same root) => no",
        DatasetTool.canShareWith(DataFileType.TOOLS, ROOT, ROOT
            + DataFileType.RECON.extension));
    assertFalse(
        "tools share with ejf (same root) => no",
        DatasetTool.canShareWith(DataFileType.TOOLS, ROOT, ROOT
            + DataFileType.JOIN.extension));
    assertTrue(
        "tools share with epp (same root) => yes",
        DatasetTool.canShareWith(DataFileType.TOOLS, ROOT, ROOT
            + DataFileType.PARALLEL.extension));
    assertFalse(
        "tools share with epe (same root) => no",
        DatasetTool.canShareWith(DataFileType.TOOLS, ROOT, ROOT
            + DataFileType.PEET.extension));
    assertFalse(
        "tools share with ess (same root) => no",
        DatasetTool.canShareWith(DataFileType.TOOLS, ROOT, ROOT
            + DataFileType.SERIAL_SECTIONS.extension));
  }

  public void testCanShareWith_DataFileType_String_AxisType_String_AxisType() {
    // check input file
    assertFalse("no inputFile => no", DatasetTool.canShareWith(DataFileType.RECON,
        null, AxisType.SINGLE_AXIS, ROOT + DataFileType.RECON.extension,
        AxisType.SINGLE_AXIS));
    assertFalse(
        "no inputFile => no",
        DatasetTool.canShareWith(DataFileType.RECON, "", AxisType.SINGLE_AXIS, ROOT
            + DataFileType.RECON.extension, AxisType.SINGLE_AXIS));
    assertFalse(
        "no inputFile => no",
        DatasetTool.canShareWith(DataFileType.RECON, " ", AxisType.SINGLE_AXIS, ROOT
            + DataFileType.RECON.extension, AxisType.SINGLE_AXIS));
    // build thisRoot
    assertTrue(
        "roota share with root.edf/roota.st/rootb.st => yes",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS, ROOT
                + DataFileType.RECON.extension, AxisType.DUAL_AXIS));
    assertFalse(
        "empty string share with root.edf/roota.st/rootb.st => no",
        DatasetTool.canShareWith(DataFileType.RECON, "", AxisType.SINGLE_AXIS, ROOT
            + DataFileType.RECON.extension, AxisType.DUAL_AXIS));
    // handle incorrect data file types
    assertTrue(
        "ejf share with same file => yes",
        DatasetTool.canShareWith(DataFileType.JOIN, ROOT, AxisType.SINGLE_AXIS, ROOT
            + DataFileType.JOIN.extension, AxisType.SINGLE_AXIS));
    // Get the type of the existing data file
    assertTrue("no data file => yes", DatasetTool.canShareWith(DataFileType.RECON,
        ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS, null,
        AxisType.SINGLE_AXIS));
    assertTrue("not a data file => yes", DatasetTool.canShareWith(
        DataFileType.RECON, ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS,
        ROOT + ".foo", AxisType.SINGLE_AXIS));
    // Get existing data file root
    assertFalse("roota share with .edf/a.st/b.st => no", DatasetTool.canShareWith(
        DataFileType.RECON, ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS,
        DataFileType.RECON.extension, AxisType.DUAL_AXIS));
    // Can't share if the thisAxisType is missing
    assertFalse("null thisAxisType => no", DatasetTool.canShareWith(
        DataFileType.RECON, ROOT + AxisID.FIRST.getExtension(), null, ROOT
            + DataFileType.RECON.extension, AxisType.DUAL_AXIS));
    // check for sharing with another .edf file
    // Can't share if the dataFileAxisType is missing
    assertFalse(
        "null dataFileAxisType => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS, ROOT
                + DataFileType.RECON.extension, null));

    // Match the root without an axis letter
    // stripped
    assertFalse(
        "roota share with root.edf/root.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS, ROOT
                + DataFileType.RECON.extension, AxisType.SINGLE_AXIS));
    assertTrue(
        "roota share with root.edf/roota.st/rootb.st => yes",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS, ROOT
                + DataFileType.RECON.extension, AxisType.DUAL_AXIS));
    // not stripped
    assertFalse("root share with root.edf/roota.st/rootb.st => no",
        DatasetTool.canShareWith(DataFileType.RECON, ROOT, AxisType.SINGLE_AXIS,
            ROOT + DataFileType.RECON.extension, AxisType.DUAL_AXIS));
    assertTrue("root share with root.edf/root.st => yes", DatasetTool.canShareWith(
        DataFileType.RECON, ROOT, AxisType.SINGLE_AXIS, ROOT
            + DataFileType.RECON.extension, AxisType.SINGLE_AXIS));
    // Don't add an axis letter to a root that didn't originally have one
    assertFalse("root share with diff.edf/diff.st => no", DatasetTool.canShareWith(
        DataFileType.RECON, ROOT, AxisType.SINGLE_AXIS, DIFFERENT_ROOT
            + DataFileType.RECON.extension, AxisType.SINGLE_AXIS));
    // single axis can match the same single axis .edf file, or a dual axis file
    assertFalse("roota share with roota.edf/rootaa.st/rootab.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS,
            ROOT + AxisID.FIRST.getExtension() + DataFileType.RECON.extension,
            AxisType.DUAL_AXIS));
    assertFalse("rootb share with rootb.edf/rootba.st/rootbb.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.SECOND.getExtension(), AxisType.SINGLE_AXIS, ROOT
                + AxisID.SECOND.getExtension() + DataFileType.RECON.extension,
            AxisType.DUAL_AXIS));
    assertTrue("roota share with roota.edf/roota.st => yes",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS,
            ROOT + AxisID.FIRST.getExtension() + DataFileType.RECON.extension,
            AxisType.SINGLE_AXIS));
    assertTrue("rootb share with rootb.edf/rootb.st => yes",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.SECOND.getExtension(), AxisType.SINGLE_AXIS, ROOT
                + AxisID.SECOND.getExtension() + DataFileType.RECON.extension,
            AxisType.SINGLE_AXIS));
    assertFalse("roota share with diffa.edf/diffa.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.SINGLE_AXIS, DIFFERENT_ROOT
                + AxisID.FIRST.getExtension() + DataFileType.RECON.extension,
            AxisType.SINGLE_AXIS));
    // Dual axis can match the same dual axis file, or both single axis files
    assertFalse("roota/rootb share with roota.edf/rootaa.st/rootab.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.DUAL_AXIS,
            ROOT + AxisID.FIRST.getExtension() + DataFileType.RECON.extension,
            AxisType.DUAL_AXIS));
    assertTrue("roota/rootb share with roota.edf/roota.st => yes",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.DUAL_AXIS,
            ROOT + AxisID.FIRST.getExtension() + DataFileType.RECON.extension,
            AxisType.SINGLE_AXIS));
    assertFalse("roota/rootb share with rootb.edf/rootba.st/rootbb.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.SECOND.getExtension(), AxisType.DUAL_AXIS,
            ROOT + AxisID.SECOND.getExtension() + DataFileType.RECON.extension,
            AxisType.DUAL_AXIS));
    assertTrue("roota/rootb share with rootb.edf/rootb.st => yes",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.SECOND.getExtension(), AxisType.DUAL_AXIS,
            ROOT + AxisID.SECOND.getExtension() + DataFileType.RECON.extension,
            AxisType.SINGLE_AXIS));
    assertFalse("roota/rootb share with diffa.edf/diffa.st => no",
        DatasetTool.canShareWith(DataFileType.RECON,
            ROOT + AxisID.FIRST.getExtension(), AxisType.DUAL_AXIS, DIFFERENT_ROOT
                + AxisID.FIRST.getExtension() + DataFileType.RECON.extension,
            AxisType.SINGLE_AXIS));
    // other data file types
    assertTrue(
        "edf share with different ejf => yes",
        DatasetTool.canShareWith(DataFileType.RECON, DIFFERENT_ROOT, null, ROOT
            + DataFileType.JOIN.extension, null));
    assertTrue("edf share with epp => yes", DatasetTool.canShareWith(
        DataFileType.RECON, DIFFERENT_ROOT, AxisType.NOT_SET, ROOT
            + DataFileType.PARALLEL.extension, AxisType.NOT_SET));
    assertTrue("edf share with epe => yes", DatasetTool.canShareWith(
        DataFileType.RECON, DIFFERENT_ROOT, AxisType.SINGLE_AXIS, ROOT
            + DataFileType.PEET.extension, AxisType.SINGLE_AXIS));
    assertFalse(
        "edf share with ess => no",
        DatasetTool.canShareWith(DataFileType.RECON, DIFFERENT_ROOT, null, ROOT
            + DataFileType.SERIAL_SECTIONS.extension, null));
  }
}
