package etomo.storage;

import java.io.File;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.FileType;
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
public class DirectiveDescrFileTest extends TestCase {
  public static final String rcsid = "$Id:$";

  public void testIterator() {
    BaseManager manager = (BaseManager) EtomoDirector.INSTANCE.getCurrentManagerForTest();
    DirectiveDescrFile descrFile = DirectiveDescrFile.INSTANCE;
    AxisID axisID = AxisID.ONLY;
    descrFile.setFile(new File(TestUtilites.INSTANCE.getUnitTestData().getAbsolutePath(),
        FileType.DIRECTIVES_DESCR.getFileName(manager, axisID)));
    DirectiveDescrFile.Iterator iterator = descrFile.getIterator(manager, axisID);
    assertTrue("file title", iterator.hasNext());
    assertTrue("hasNext doesn't increment the iterator", iterator.hasNext());
    iterator.next();
    assertTrue("file title", iterator.isSection());
    assertFalse("file title", iterator.isDirective());
    assertEquals("file title", "Batch/Template Directives", iterator.getName());
    assertNull("file title", iterator.getDescription());
    assertEquals("file title", iterator.getValueType(), DirectiveValueType.UNKNOWN);
    assertFalse("file title", iterator.isBatch());
    assertFalse("file title", iterator.isTemplate());
    assertNull("file title", iterator.getEtomoColumn());

    // has next is not necessary because next increments the iterator
    iterator.next();
    assertTrue("title", iterator.isSection());
    assertFalse("title", iterator.isDirective());
    assertEquals("title", "Directives for Batch Processing and Template Files",
        iterator.getName());
    assertNull("title", iterator.getDescription());
    assertEquals("title", iterator.getValueType(), DirectiveValueType.UNKNOWN);
    assertFalse("title", iterator.isBatch());
    assertFalse("title", iterator.isTemplate());
    assertNull("title", iterator.getEtomoColumn());

    assertTrue("column header", iterator.hasNext());
    iterator.next();
    assertFalse("column header", iterator.isSection());
    assertTrue("column header", iterator.isDirective());
    assertEquals("column header", "Directive", iterator.getName());
    assertEquals("column header", "Definition", iterator.getDescription());
    assertEquals("column header", iterator.getValueType(), DirectiveValueType.UNKNOWN);
    assertFalse("column header", iterator.isBatch());
    assertFalse("column header", iterator.isTemplate());
    assertNull("column header", iterator.getEtomoColumn());

    assertTrue("blank", iterator.hasNext());
    iterator.next();
    assertFalse("blank", iterator.isSection());
    assertFalse("blank", iterator.isDirective());
    assertNull("blank", iterator.getName());
    assertNull("blank", iterator.getDescription());
    assertEquals("blank", iterator.getValueType(), DirectiveValueType.UNKNOWN);
    assertFalse("blank", iterator.isBatch());
    assertFalse("blank", iterator.isTemplate());
    assertNull("blank", iterator.getEtomoColumn());

    assertTrue("header - Arguments to copytomocoms", iterator.hasNext());
    iterator.next();
    assertTrue("header - Arguments to copytomocoms", iterator.isSection());
    assertFalse("header - Arguments to copytomocoms", iterator.isDirective());
    assertEquals("header - Arguments to copytomocoms", "Arguments to copytomocoms",
        iterator.getName());
    assertNull("header - Arguments to copytomocoms", iterator.getDescription());
    assertEquals("header - Arguments to copytomocoms", iterator.getValueType(),
        DirectiveValueType.UNKNOWN);
    assertFalse("header - Arguments to copytomocoms", iterator.isBatch());
    assertFalse("header - Arguments to copytomocoms", iterator.isTemplate());
    assertNull("header - Arguments to copytomocoms", iterator.getEtomoColumn());

    assertTrue("name", iterator.hasNext());
    iterator.next();
    assertFalse("name", iterator.isSection());
    assertTrue("name", iterator.isDirective());
    assertEquals("name", "setupset.copyarg.name", iterator.getName());
    assertEquals("name", "Root name of data set", iterator.getDescription());
    assertTrue("name", iterator.getValueType() == DirectiveValueType.STRING);
    assertTrue("name", iterator.isBatch());
    assertFalse("name", iterator.isTemplate());
    assertNull("name", iterator.getEtomoColumn());

    assertTrue("dual", iterator.hasNext());
    iterator.next();
    assertFalse("dual", iterator.isSection());
    assertTrue("dual", iterator.isDirective());
    assertEquals("dual", "setupset.copyarg.dual", iterator.getName());
    assertEquals("dual", "Dual-axis data set", iterator.getDescription());
    assertTrue("dual", iterator.getValueType() == DirectiveValueType.BOOLEAN);
    assertTrue("dual", iterator.isBatch());
    assertTrue("dual", iterator.isTemplate());
    assertTrue("dual", iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SO);

    assertTrue("montage", iterator.hasNext());
    iterator.next();
    assertFalse("montage", iterator.isSection());
    assertTrue("montage", iterator.isDirective());
    assertEquals("montage", "setupset.copyarg.montage", iterator.getName());
    assertEquals("montage", "Data are montaged", iterator.getDescription());
    assertTrue("montage", iterator.getValueType() == DirectiveValueType.BOOLEAN);
    assertTrue("montage", iterator.isBatch());
    assertTrue("montage", iterator.isTemplate());
    assertTrue("montage", iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SD);

    assertTrue("pixel", iterator.hasNext());
    iterator.next();
    assertFalse("pixel", iterator.isSection());
    assertTrue("pixel", iterator.isDirective());
    assertEquals("pixel", "setupset.copyarg.pixel", iterator.getName());
    assertEquals("pixel", "Pixel size of images in nanometers", iterator.getDescription());
    assertTrue("pixel", iterator.getValueType() == DirectiveValueType.FLOATING_POINT);
    assertTrue("pixel", iterator.isBatch());
    assertTrue("pixel", iterator.isTemplate());
    assertNull("pixel", iterator.getEtomoColumn());

    // gold
    iterator.next();
    // rotation
    iterator.next();
    // brotation
    iterator.next();

    assertTrue("firstinc", iterator.hasNext());
    iterator.next();
    assertFalse("firstinc", iterator.isSection());
    assertTrue("firstinc", iterator.isDirective());
    assertEquals("firstinc", "setupset.copyarg.firstinc", iterator.getName());
    assertEquals("firstinc", "First tilt angle and tilt angle increment",
        iterator.getDescription());
    assertTrue("firstinc",
        iterator.getValueType() == DirectiveValueType.FLOATING_POINT_PAIR);
    assertTrue("firstinc", iterator.isBatch());
    assertFalse("firstinc", iterator.isTemplate());
    assertNull("firstinc", iterator.getEtomoColumn());

    // bfirstinc
    iterator.next();
    // userawtlt
    iterator.next();
    // buserawtlt
    iterator.next();
    // extract
    iterator.next();
    // bextract
    iterator.next();

    assertTrue("skip", iterator.hasNext());
    iterator.next();
    assertFalse("skip", iterator.isSection());
    assertTrue("skip", iterator.isDirective());
    assertEquals("skip", "setupset.copyarg.skip", iterator.getName());
    assertEquals("skip", "List of views to exclude from processing for A or only axis",
        iterator.getDescription());
    assertTrue("skip", iterator.getValueType() == DirectiveValueType.LIST);
    assertTrue("skip", iterator.isBatch());
    assertFalse("skip", iterator.isTemplate());
    assertNull("skip", iterator.getEtomoColumn());

    // bskip
    iterator.next();
    // distort
    iterator.next();

    assertTrue("binning", iterator.hasNext());
    iterator.next();
    assertFalse("binning", iterator.isSection());
    assertTrue("binning", iterator.isDirective());
    assertEquals("binning", "setupset.copyarg.binning", iterator.getName());
    assertEquals("binning", "Binning of raw images", iterator.getDescription());
    assertTrue("binning", iterator.getValueType() == DirectiveValueType.INTEGER);
    assertTrue("binning", iterator.isBatch());
    assertTrue("binning", iterator.isTemplate());
    assertNull("binning", iterator.getEtomoColumn());

    // gradient
    iterator.next();
    // focus
    iterator.next();
    // bfocus
    iterator.next();
    // defocus
    iterator.next();
    // voltage
    iterator.next();

    assertTrue("Cs", iterator.hasNext());
    iterator.next();
    assertFalse("Cs", iterator.isSection());
    assertTrue("Cs", iterator.isDirective());
    assertEquals("Cs", "setupset.copyarg.Cs", iterator.getName());
    assertEquals("Cs", "Spherical aberration", iterator.getDescription());
    assertTrue("Cs", iterator.getValueType() == DirectiveValueType.FLOATING_POINT);
    assertTrue("Cs", iterator.isBatch());
    assertTrue("Cs", iterator.isTemplate());
    assertTrue("Cs", iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SD);

    // ctfnoise
    iterator.next();
    // blank
    iterator.next();
    // header - Other setup parameters
    iterator.next();

    assertTrue("scopeTemplate", iterator.hasNext());
    iterator.next();
    assertFalse("scopeTemplate", iterator.isSection());
    assertTrue("scopeTemplate", iterator.isDirective());
    assertEquals("scopeTemplate", "setupset.scopeTemplate", iterator.getName());
    assertEquals("scopeTemplate", "Name of scope template file to use",
        iterator.getDescription());
    assertTrue("scopeTemplate", iterator.getValueType() == DirectiveValueType.STRING);
    assertTrue("scopeTemplate", iterator.isBatch());
    assertFalse("scopeTemplate", iterator.isTemplate());
    assertNull("scopeTemplate", iterator.getEtomoColumn());

    // systemTemplate
    iterator.next();
    // userTemplate
    iterator.next();
    // scanHeader
    iterator.next();
    // datasetDirectory
    iterator.next();
    // blank
    iterator.next();
    // header - Preprocessing
    iterator.next();

    assertTrue("removeXrays", iterator.hasNext());
    iterator.next();
    assertFalse("removeXrays", iterator.isSection());
    assertTrue("removeXrays", iterator.isDirective());
    assertEquals("removeXrays", "runtime.Preprocessing.any.removeXrays",
        iterator.getName());
    assertEquals("removeXrays", "Run ccderaser to remove X rays",
        iterator.getDescription());
    assertTrue("removeXrays", iterator.getValueType() == DirectiveValueType.BOOLEAN);
    assertTrue("removeXrays", iterator.isBatch());
    assertFalse("removeXrays", iterator.isTemplate());
    assertTrue("removeXrays", iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.NE);

    assertTrue("PeakCriterion", iterator.hasNext());
    iterator.next();
    assertFalse("PeakCriterion", iterator.isSection());
    assertTrue("PeakCriterion", iterator.isDirective());
    assertEquals("PeakCriterion", "comparam.eraser.ccderaser.PeakCriterion",
        iterator.getName());
    assertEquals("PeakCriterion", "Peak criterion # of SDs", iterator.getDescription());
    assertTrue("PeakCriterion",
        iterator.getValueType() == DirectiveValueType.FLOATING_POINT);
    assertTrue("PeakCriterion", iterator.isBatch());
    assertTrue("PeakCriterion", iterator.isTemplate());
    assertTrue("PeakCriterion", iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SD);

    // DiffCriterion
    iterator.next();
    // MaximumRadius
    iterator.next();
    // ModelFile
    iterator.next();
    // LineObjects
    iterator.next();
    // BoundaryObjects
    iterator.next();
    // AllSectionObjects
    iterator.next();
    // blank
    iterator.next();
    // header - Coarse alignment
    iterator.next();
    // FilterRadius2
    iterator.next();
    // FilterSigma2
    iterator.next();
    // fiducialless
    iterator.next();

    assertTrue("newstack.BinByFactor", iterator.hasNext());
    iterator.next();
    assertFalse("newstack.BinByFactor", iterator.isSection());
    assertTrue("newstack.BinByFactor", iterator.isDirective());
    assertEquals("newstack.BinByFactor", "comparam.prenewst.newstack.BinByFactor",
        iterator.getName());
    assertEquals("newstack.BinByFactor", "Coarse aligned stack binning",
        iterator.getDescription());
    assertTrue("newstack.BinByFactor",
        iterator.getValueType() == DirectiveValueType.INTEGER);
    assertTrue("newstack.BinByFactor", iterator.isBatch());
    assertTrue("newstack.BinByFactor", iterator.isTemplate());
    assertTrue("newstack.BinByFactor",
        iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SD);

    // ModeToOutput
    iterator.next();

    assertTrue("blendmont.BinByFactor", iterator.hasNext());
    iterator.next();
    assertFalse("blendmont.BinByFactor", iterator.isSection());
    assertTrue("blendmont.BinByFactor", iterator.isDirective());
    assertEquals("blendmont.BinByFactor", "comparam.preblend.blendmont.BinByFactor",
        iterator.getName());
    assertEquals("blendmont.BinByFactor", "Coarse aligned stack binning",
        iterator.getDescription());
    assertTrue("blendmont.BinByFactor",
        iterator.getValueType() == DirectiveValueType.INTEGER);
    assertTrue("blendmont.BinByFactor", iterator.isBatch());
    assertTrue("blendmont.BinByFactor", iterator.isTemplate());
    assertTrue("blendmont.BinByFactor",
        iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SD);

    // blank
    iterator.next();
    // header - Tracking choices
    iterator.next();
    // trackingMethod
    iterator.next();
    // seedingMethod
    iterator.next();
    // blank
    iterator.next();
    // header - Beadtracking
    iterator.next();
    // LightBeads
    iterator.next();
    // LocalAreaTracking
    iterator.next();

    assertTrue("LocalAreaTargetSize", iterator.hasNext());
    iterator.next();
    assertFalse("LocalAreaTargetSize", iterator.isSection());
    assertTrue("LocalAreaTargetSize", iterator.isDirective());
    assertEquals("LocalAreaTargetSize", "comparam.track.beadtrack.LocalAreaTargetSize",
        iterator.getName());
    assertEquals("LocalAreaTargetSize", "Size of local areas", iterator.getDescription());
    assertTrue("LocalAreaTargetSize",
        iterator.getValueType() == DirectiveValueType.INTEGER_PAIR);
    assertTrue("LocalAreaTargetSize", iterator.isBatch());
    assertTrue("LocalAreaTargetSize", iterator.isTemplate());
    assertTrue("LocalAreaTargetSize",
        iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.SD);

    // SobelFilterCentering
    iterator.next();
    // KernelSigmaForSobel
    iterator.next();
    // RoundsOfTracking
    iterator.next();
    // NumberOfRuns
    iterator.next();
    // blank
    iterator.next();
    // header - Auto seed finding
    iterator.next();

    assertTrue("TwoSurfaces", iterator.hasNext());
    iterator.next();
    assertFalse("TwoSurfaces", iterator.isSection());
    assertTrue("TwoSurfaces", iterator.isDirective());
    assertEquals("TwoSurfaces", "comparam.autofidseed.autofidseed.TwoSurfaces",
        iterator.getName());
    assertEquals("TwoSurfaces", "Whether beads on 2 surfaces", iterator.getDescription());
    assertTrue("TwoSurfaces", iterator.getValueType() == DirectiveValueType.BOOLEAN);
    assertTrue("TwoSurfaces", iterator.isBatch());
    assertTrue("TwoSurfaces", iterator.isTemplate());
    assertTrue("TwoSurfaces", iterator.getEtomoColumn() == DirectiveDescrEtomoColumn.NES);

    DirectiveDescrFile.INSTANCE.releaseIterator(iterator);
  }
}
