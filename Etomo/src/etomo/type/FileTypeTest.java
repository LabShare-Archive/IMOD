package etomo.type;

import java.util.Iterator;

import etomo.EtomoDirector;
import etomo.FrontPageManager;
import etomo.process.ImodManager;

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
 * <p> $Log$
 * <p> Revision 1.3  2011/04/09 06:35:35  sueh
 * <p> bug# 1416 Added testGetFileName, testGetImodManagerKey, and testGetImodManagerKey2.
 * <p>
 * <p> Revision 1.2  2011/02/22 05:40:55  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/04/28 16:29:05  sueh
 * <p> bug# 1344 Makes sure that the front page manager is the current
 * <p> manager.  Tests for file name collisions.  Tests getting FileType instances
 * <p> from name descriptions and file names.
 * <p> </p>
 */
public class FileTypeTest extends TestCase {
  public static final String rcsid = "$Id$";

  protected void setUp() throws Exception {
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
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.DUAL_AXIS);
    while (iterator.hasNext()) {
      Iterator comparisonIterator = FileType.iterator();
      while (comparisonIterator.hasNext()) {
        FileType fileType = (FileType) iterator.next();
        FileType comparisonFileType = (FileType) comparisonIterator.next();
        if (fileType != comparisonFileType) {
          assertFalse("File name collison: " + fileType + " is the same as "
              + comparisonFileType, fileType.equals(manager, comparisonFileType));
        }
      }
    }
    metaData.setAxisType(AxisType.SINGLE_AXIS);
    while (iterator.hasNext()) {
      Iterator comparisonIterator = FileType.iterator();
      while (comparisonIterator.hasNext()) {
        FileType fileType = (FileType) iterator.next();
        FileType comparisonFileType = (FileType) comparisonIterator.next();
        if (fileType != comparisonFileType) {
          assertFalse("File name collison: " + fileType + " is the same as "
              + comparisonFileType, fileType.equals(manager, comparisonFileType));
        }
      }
    }
  }

  public void testGetInstanceFromNameDescription() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.DUAL_AXIS);
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FIDUCIAL_3D_MODEL,
        FileType.getInstance(manager, true, true, "", ".3dmod"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FLATTEN_TOOL_OUTPUT,
        FileType.getInstance(manager, true, false, "", ".flat"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT,
        FileType.getInstance(manager, true, true, "_3dfind", ".ali"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FLATTEN_WARP_INPUT_MODEL,
        FileType.getInstance(manager, true, false, "_flat", ".mod"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.FIND_BEADS_3D_COMSCRIPT,
        FileType.getInstance(manager, false, true, "findbeads3d", ".com"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.TILT_OUTPUT, FileType.getInstance(manager, true, true, "", ".rec"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, true, true, "", ".sint"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.SIRT_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, true, true, "", ".srec"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.TILT_FOR_SIRT_COMSCRIPT,
        FileType.getInstance(manager, false, true, "tilt", "_for_sirt.com"));
    metaData.setAxisType(AxisType.SINGLE_AXIS);
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.TILT_OUTPUT, FileType.getInstance(manager, true, true, "_full", ".rec"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, true, true, "_full", ".sint"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.SIRT_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, true, true, "_full", ".srec"));
    assertEquals("getInstance(boolean,boolean,string,string) failed",
        FileType.TILT_FOR_SIRT_COMSCRIPT,
        FileType.getInstance(manager, false, true, "tilt", "_for_sirt.com"));
    assertEquals("should find an imod file based on the description",
        FileType.SLOPPY_BLEND_COMSCRIPT,
        FileType.getInstance(manager, false, false, "sloppyblend", ".com"));
  }

  public void testGetInstanceFromFileName() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.DUAL_AXIS);
    metaData.setName("BB");
    assertEquals("getInstance failed", FileType.TILT_OUTPUT,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa.rec"));
    assertEquals("getInstance failed", FileType.TILT_OUTPUT,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb.rec"));
    assertEquals("getInstance failed", FileType.CCD_ERASER_BEADS_INPUT_MODEL,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa_erase.fid"));
    assertEquals("getInstance failed", FileType.CCD_ERASER_BEADS_INPUT_MODEL,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb_erase.fid"));
    assertEquals("getInstance failed", FileType.CROSS_CORRELATION_COMSCRIPT,
        FileType.getInstance(manager, AxisID.FIRST, false, true, "xcorra.com"));
    assertEquals("getInstance failed", FileType.CROSS_CORRELATION_COMSCRIPT,
        FileType.getInstance(manager, AxisID.SECOND, false, true, "xcorrb.com"));
    assertEquals("getInstance failed", FileType.DISTORTION_CORRECTED_STACK,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa.dcst"));
    assertEquals("getInstance failed", FileType.DISTORTION_CORRECTED_STACK,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb.dcst"));
    assertEquals("getInstance failed", FileType.FLATTEN_COMSCRIPT,
        FileType.getInstance(manager, AxisID.FIRST, false, false, "flatten.com"));
    assertEquals("getInstance failed", FileType.FLATTEN_COMSCRIPT,
        FileType.getInstance(manager, AxisID.SECOND, false, false, "flatten.com"));
    assertEquals("getInstance failed", FileType.FLATTEN_TOOL_COMSCRIPT,
        FileType.getInstance(manager, AxisID.FIRST, true, false, "BB_flatten.com"));
    assertEquals("getInstance failed", FileType.FLATTEN_TOOL_COMSCRIPT,
        FileType.getInstance(manager, AxisID.SECOND, true, false, "BB_flatten.com"));
    assertEquals("getInstance failed", FileType.TILT_FOR_SIRT_COMSCRIPT,
        FileType.getInstance(manager, AxisID.FIRST, false, true, "tilta_for_sirt.com"));
    assertEquals("getInstance failed", FileType.TILT_FOR_SIRT_COMSCRIPT,
        FileType.getInstance(manager, AxisID.SECOND, false, true, "tiltb_for_sirt.com"));
    assertEquals("getInstance failed", FileType.SIRT_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa.srec"));
    assertEquals("getInstance failed", FileType.SIRT_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb.srec"));
    assertEquals("getInstance failed", FileType.ANISOTROPIC_DIFFUSION_OUTPUT,
        FileType.getInstance(manager, AxisID.FIRST, true, false, "BB.nad"));
    assertEquals("getInstance failed", FileType.ANISOTROPIC_DIFFUSION_OUTPUT,
        FileType.getInstance(manager, AxisID.SECOND, true, false, "BB.nad"));
    assertEquals("getInstance failed", FileType.COMBINED_VOLUME,
        FileType.getInstance(manager, AxisID.FIRST, false, false, "sum.rec"));
    assertEquals("getInstance failed", FileType.COMBINED_VOLUME,
        FileType.getInstance(manager, AxisID.SECOND, false, false, "sum.rec"));
    assertEquals("getInstance failed", FileType.CTF_CORRECTED_STACK,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa_ctfcorr.ali"));
    assertEquals("getInstance failed", FileType.CTF_CORRECTED_STACK,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb_ctfcorr.ali"));
    assertEquals("getInstance failed", FileType.FIDUCIAL_3D_MODEL,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa.3dmod"));
    assertEquals("getInstance failed", FileType.FIDUCIAL_3D_MODEL,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb.3dmod"));
    assertEquals("getInstance failed", FileType.FLATTEN_TOOL_OUTPUT,
        FileType.getInstance(manager, AxisID.FIRST, true, false, "BB.flat"));
    assertEquals("getInstance failed", FileType.FLATTEN_TOOL_OUTPUT,
        FileType.getInstance(manager, AxisID.SECOND, true, false, "BB.flat"));
    assertEquals("getInstance failed", FileType.NAD_TEST_INPUT,
        FileType.getInstance(manager, AxisID.FIRST, false, false, "test.input"));
    assertEquals("getInstance failed", FileType.NAD_TEST_INPUT,
        FileType.getInstance(manager, AxisID.SECOND, false, false, "test.input"));
    assertEquals("getInstance failed", FileType.RAW_STACK,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa.st"));
    assertEquals("getInstance failed", FileType.RAW_STACK,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb.st"));
    assertEquals("getInstance failed", FileType.ALIGNED_STACK,
        FileType.getInstance(manager, AxisID.FIRST, true, true, "BBa.ali"));
    assertEquals("getInstance failed", FileType.ALIGNED_STACK,
        FileType.getInstance(manager, AxisID.SECOND, true, true, "BBb.ali"));

    metaData.setAxisType(AxisType.SINGLE_AXIS);
    metaData.setName("BBa");
    assertEquals("getInstance failed", FileType.TILT_OUTPUT,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa_full.rec"));
    assertEquals("getInstance failed", FileType.CCD_ERASER_BEADS_INPUT_MODEL,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa_erase.fid"));
    assertEquals("getInstance failed", FileType.CROSS_CORRELATION_COMSCRIPT,
        FileType.getInstance(manager, AxisID.ONLY, false, true, "xcorr.com"));
    assertEquals("getInstance failed", FileType.DISTORTION_CORRECTED_STACK,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa.dcst"));
    assertEquals("getInstance failed", FileType.FLATTEN_COMSCRIPT,
        FileType.getInstance(manager, AxisID.ONLY, false, false, "flatten.com"));
    assertEquals("getInstance failed", FileType.FLATTEN_TOOL_COMSCRIPT,
        FileType.getInstance(manager, AxisID.ONLY, true, false, "BBa_flatten.com"));
    assertEquals("getInstance failed", FileType.TILT_FOR_SIRT_COMSCRIPT,
        FileType.getInstance(manager, AxisID.ONLY, false, true, "tilt_for_sirt.com"));
    assertEquals("getInstance failed", FileType.SIRT_OUTPUT_TEMPLATE,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa_full.srec"));
    assertEquals("getInstance failed", FileType.ANISOTROPIC_DIFFUSION_OUTPUT,
        FileType.getInstance(manager, AxisID.ONLY, true, false, "BBa.nad"));
    assertEquals("getInstance failed", FileType.COMBINED_VOLUME,
        FileType.getInstance(manager, AxisID.ONLY, false, false, "sum.rec"));
    assertEquals("getInstance failed", FileType.CTF_CORRECTED_STACK,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa_ctfcorr.ali"));
    assertEquals("getInstance failed", FileType.FIDUCIAL_3D_MODEL,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa.3dmod"));
    assertEquals("getInstance failed", FileType.FLATTEN_TOOL_OUTPUT,
        FileType.getInstance(manager, AxisID.ONLY, true, false, "BBa.flat"));
    assertEquals("getInstance failed", FileType.NAD_TEST_INPUT,
        FileType.getInstance(manager, AxisID.ONLY, false, false, "test.input"));
    assertEquals("getInstance failed", FileType.RAW_STACK,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa.st"));
    assertEquals("getInstance failed", FileType.ALIGNED_STACK,
        FileType.getInstance(manager, AxisID.ONLY, true, true, "BBa.ali"));
  }

  public void testGetImodManagerKey() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.DUAL_AXIS);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FIDUCIAL_3D_MODEL.getImodManagerKey(manager),
        ImodManager.FIDUCIAL_MODEL_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.ALIGNED_STACK.getImodManagerKey(manager), ImodManager.FINE_ALIGNED_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.XCORR_BLEND_OUTPUT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.DISTORTION_CORRECTED_STACK.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.FIDUCIAL_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FLATTEN_TOOL_OUTPUT.getImodManagerKey(manager),
        ImodManager.FLATTEN_TOOL_OUTPUT_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.JOIN.getImodManagerKey(manager), ImodManager.JOIN_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getImodManagerKey(manager),
        ImodManager.ANISOTROPIC_DIFFUSION_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.PREALIGNED_STACK.getImodManagerKey(manager),
        ImodManager.COARSE_ALIGNED_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.RAW_TILT_ANGLES.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TRIM_VOL_OUTPUT.getImodManagerKey(manager),
        ImodManager.TRIMMED_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.JOIN_SAMPLE_AVERAGES.getImodManagerKey(manager),
        ImodManager.JOIN_SAMPLE_AVERAGES_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.JOIN_SAMPLE.getImodManagerKey(manager), ImodManager.JOIN_SAMPLES_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SQUEEZE_VOL_OUTPUT.getImodManagerKey(manager),
        ImodManager.SQUEEZED_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.RAW_STACK.getImodManagerKey(manager), ImodManager.RAW_STACK_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getImodManagerKey(manager),
        ImodManager.FINE_ALIGNED_3D_FIND_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TILT_3D_FIND_OUTPUT.getImodManagerKey(manager),
        ImodManager.FULL_VOLUME_3D_FIND_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getImodManagerKey(manager),
        ImodManager.SMOOTHING_ASSESSMENT_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.CTF_CORRECTED_STACK.getImodManagerKey(manager),
        ImodManager.CTF_CORRECTION_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.ERASED_BEADS_STACK.getImodManagerKey(manager),
        ImodManager.ERASED_FIDUCIALS_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.MTF_FILTERED_STACK.getImodManagerKey(manager),
        ImodManager.MTF_FILTER_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FIXED_XRAYS_STACK.getImodManagerKey(manager),
        ImodManager.ERASED_STACK_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.FLATTEN_WARP_INPUT_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FLATTEN_OUTPUT.getImodManagerKey(manager), ImodManager.FLAT_VOLUME_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.FLATTEN_TOOL_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.MODELED_JOIN.getImodManagerKey(manager), ImodManager.MODELED_JOIN_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.ORIGINAL_RAW_STACK.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TRANSFORMED_REFINING_MODEL.getImodManagerKey(manager),
        ImodManager.TRANSFORMED_MODEL_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TRIAL_JOIN.getImodManagerKey(manager), ImodManager.TRIAL_JOIN_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.CTF_CORRECTION_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.FIND_BEADS_3D_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.FLATTEN_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.MTF_FILTER_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.PATCH_VECTOR_MODEL.getImodManagerKey(manager),
        ImodManager.PATCH_VECTOR_MODEL_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.PATCH_VECTOR_CCC_MODEL.getImodManagerKey(manager),
        ImodManager.PATCH_VECTOR_CCC_MODEL_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.SIRTSETUP_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.COMBINED_VOLUME.getImodManagerKey(manager),
        ImodManager.COMBINED_TOMOGRAM_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.NAD_TEST_INPUT.getImodManagerKey(manager), ImodManager.TEST_VOLUME_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.TILT_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TILT_OUTPUT.getImodManagerKey(manager), ImodManager.FULL_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getImodManagerKey(manager),
        ImodManager.SIRT_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SIRT_OUTPUT_TEMPLATE.getImodManagerKey(manager), ImodManager.SIRT_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.CROSS_CORRELATION_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.PATCH_TRACKING_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getImodManagerKey(manager));

    metaData.setAxisType(AxisType.SINGLE_AXIS);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FIDUCIAL_3D_MODEL.getImodManagerKey(manager),
        ImodManager.FIDUCIAL_MODEL_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.ALIGNED_STACK.getImodManagerKey(manager), ImodManager.FINE_ALIGNED_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.XCORR_BLEND_OUTPUT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.DISTORTION_CORRECTED_STACK.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.FIDUCIAL_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FLATTEN_TOOL_OUTPUT.getImodManagerKey(manager),
        ImodManager.FLATTEN_TOOL_OUTPUT_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.JOIN.getImodManagerKey(manager), ImodManager.JOIN_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getImodManagerKey(manager),
        ImodManager.ANISOTROPIC_DIFFUSION_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.PREALIGNED_STACK.getImodManagerKey(manager),
        ImodManager.COARSE_ALIGNED_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.RAW_TILT_ANGLES.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TRIM_VOL_OUTPUT.getImodManagerKey(manager),
        ImodManager.TRIMMED_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.JOIN_SAMPLE_AVERAGES.getImodManagerKey(manager),
        ImodManager.JOIN_SAMPLE_AVERAGES_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.JOIN_SAMPLE.getImodManagerKey(manager), ImodManager.JOIN_SAMPLES_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SQUEEZE_VOL_OUTPUT.getImodManagerKey(manager),
        ImodManager.SQUEEZED_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.RAW_STACK.getImodManagerKey(manager), ImodManager.RAW_STACK_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getImodManagerKey(manager),
        ImodManager.FINE_ALIGNED_3D_FIND_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TILT_3D_FIND_OUTPUT.getImodManagerKey(manager),
        ImodManager.FULL_VOLUME_3D_FIND_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getImodManagerKey(manager),
        ImodManager.SMOOTHING_ASSESSMENT_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.CTF_CORRECTED_STACK.getImodManagerKey(manager),
        ImodManager.CTF_CORRECTION_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.ERASED_BEADS_STACK.getImodManagerKey(manager),
        ImodManager.ERASED_FIDUCIALS_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.MTF_FILTERED_STACK.getImodManagerKey(manager),
        ImodManager.MTF_FILTER_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FIXED_XRAYS_STACK.getImodManagerKey(manager),
        ImodManager.ERASED_STACK_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.FLATTEN_WARP_INPUT_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.FLATTEN_OUTPUT.getImodManagerKey(manager), ImodManager.FLAT_VOLUME_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.FLATTEN_TOOL_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.MODELED_JOIN.getImodManagerKey(manager), ImodManager.MODELED_JOIN_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.ORIGINAL_RAW_STACK.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TRANSFORMED_REFINING_MODEL.getImodManagerKey(manager),
        ImodManager.TRANSFORMED_MODEL_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TRIAL_JOIN.getImodManagerKey(manager), ImodManager.TRIAL_JOIN_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.CTF_CORRECTION_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.FIND_BEADS_3D_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.FLATTEN_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.MTF_FILTER_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.PATCH_VECTOR_MODEL.getImodManagerKey(manager),
        ImodManager.PATCH_VECTOR_MODEL_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.PATCH_VECTOR_CCC_MODEL.getImodManagerKey(manager),
        ImodManager.PATCH_VECTOR_CCC_MODEL_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.SIRTSETUP_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.COMBINED_VOLUME.getImodManagerKey(manager),
        ImodManager.COMBINED_TOMOGRAM_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.NAD_TEST_INPUT.getImodManagerKey(manager), ImodManager.TEST_VOLUME_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.TILT_COMSCRIPT.getImodManagerKey(manager));
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.TILT_OUTPUT.getImodManagerKey(manager), ImodManager.FULL_VOLUME_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getImodManagerKey(manager),
        ImodManager.SIRT_KEY);
    assertSame("getImodManagerKey did not return the correct ImodManager key",
        FileType.SIRT_OUTPUT_TEMPLATE.getImodManagerKey(manager), ImodManager.SIRT_KEY);
    assertNull("getImodManagerKey did not return null",
        FileType.CROSS_CORRELATION_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.PATCH_TRACKING_COMSCRIPT.getImodManagerKey(manager));
    assertNull("getImodManagerKey did not return null",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getImodManagerKey(manager));
  }

  public void testGetImodManagerKey2() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.DUAL_AXIS);
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIDUCIAL_3D_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ALIGNED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.XCORR_BLEND_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.DISTORTION_CORRECTED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIDUCIAL_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_TOOL_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.JOIN.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PREALIGNED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.RAW_TILT_ANGLES.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TRIM_VOL_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.JOIN_SAMPLE_AVERAGES.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.JOIN_SAMPLE.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SQUEEZE_VOL_OUTPUT.getImodManagerKey2(manager));
    assertSame("getImodManagerKey2 did not return the correct ImodManager key",
        FileType.RAW_STACK.getImodManagerKey2(manager), ImodManager.PREVIEW_KEY);
    assertNull("getImodManagerKey2 did not return null",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_3D_FIND_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CTF_CORRECTED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ERASED_BEADS_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.MTF_FILTERED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIXED_XRAYS_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_WARP_INPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_TOOL_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.MODELED_JOIN.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ORIGINAL_RAW_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TRANSFORMED_REFINING_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TRIAL_JOIN.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CTF_CORRECTION_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIND_BEADS_3D_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.MTF_FILTER_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_VECTOR_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_VECTOR_CCC_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SIRTSETUP_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.COMBINED_VOLUME.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.NAD_TEST_INPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SIRT_OUTPUT_TEMPLATE.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CROSS_CORRELATION_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_TRACKING_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getImodManagerKey2(manager));

    metaData.setAxisType(AxisType.SINGLE_AXIS);
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIDUCIAL_3D_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ALIGNED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.XCORR_BLEND_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.DISTORTION_CORRECTED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIDUCIAL_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_TOOL_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.JOIN.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PREALIGNED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.RAW_TILT_ANGLES.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TRIM_VOL_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.JOIN_SAMPLE_AVERAGES.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.JOIN_SAMPLE.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SQUEEZE_VOL_OUTPUT.getImodManagerKey2(manager));
    assertSame("getImodManagerKey2 did not return the correct ImodManager key",
        FileType.RAW_STACK.getImodManagerKey2(manager), ImodManager.PREVIEW_KEY);
    assertNull("getImodManagerKey2 did not return null",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_3D_FIND_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CTF_CORRECTED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ERASED_BEADS_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.MTF_FILTERED_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIXED_XRAYS_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_WARP_INPUT_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_TOOL_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.MODELED_JOIN.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.ORIGINAL_RAW_STACK.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TRANSFORMED_REFINING_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TRIAL_JOIN.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CTF_CORRECTION_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FIND_BEADS_3D_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.FLATTEN_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.MTF_FILTER_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_VECTOR_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_VECTOR_CCC_MODEL.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SIRTSETUP_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.COMBINED_VOLUME.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.NAD_TEST_INPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_OUTPUT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.SIRT_OUTPUT_TEMPLATE.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.CROSS_CORRELATION_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.PATCH_TRACKING_COMSCRIPT.getImodManagerKey2(manager));
    assertNull("getImodManagerKey2 did not return null",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getImodManagerKey2(manager));
  }

  public void testGetFileName() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    // test dual axis
    metaData.setAxisType(AxisType.DUAL_AXIS);
    metaData.setName("BB");
    assertEquals("file name is wrong",
        FileType.FIDUCIAL_3D_MODEL.getFileName(manager, AxisID.FIRST), "BBa.3dmod");
    assertEquals("file name is wrong",
        FileType.FIDUCIAL_3D_MODEL.getFileName(manager, AxisID.SECOND), "BBb.3dmod");
    assertEquals("file name is wrong",
        FileType.ALIGNED_STACK.getFileName(manager, AxisID.FIRST), "BBa.ali");
    assertEquals("file name is wrong",
        FileType.ALIGNED_STACK.getFileName(manager, AxisID.SECOND), "BBb.ali");
    assertEquals("file name is wrong",
        FileType.XCORR_BLEND_OUTPUT.getFileName(manager, AxisID.FIRST), "BBa.bl");
    assertEquals("file name is wrong",
        FileType.XCORR_BLEND_OUTPUT.getFileName(manager, AxisID.SECOND), "BBb.bl");
    assertEquals("file name is wrong",
        FileType.DISTORTION_CORRECTED_STACK.getFileName(manager, AxisID.FIRST),
        "BBa.dcst");
    assertEquals("file name is wrong",
        FileType.DISTORTION_CORRECTED_STACK.getFileName(manager, AxisID.SECOND),
        "BBb.dcst");
    assertEquals("file name is wrong",
        FileType.FIDUCIAL_MODEL.getFileName(manager, AxisID.FIRST), "BBa.fid");
    assertEquals("file name is wrong",
        FileType.FIDUCIAL_MODEL.getFileName(manager, AxisID.SECOND), "BBb.fid");
    assertEquals("file name is wrong",
        FileType.FLATTEN_TOOL_OUTPUT.getFileName(manager, AxisID.FIRST), "BB.flat");
    assertEquals("file name is wrong",
        FileType.FLATTEN_TOOL_OUTPUT.getFileName(manager, AxisID.SECOND), "BB.flat");
    assertEquals("file name is wrong", FileType.JOIN.getFileName(manager, AxisID.FIRST),
        "BB.join");
    assertEquals("file name is wrong", FileType.JOIN.getFileName(manager, AxisID.SECOND),
        "BB.join");
    assertEquals("file name is wrong",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getFileName(manager, AxisID.FIRST),
        "BB.nad");
    assertEquals("file name is wrong",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getFileName(manager, AxisID.SECOND),
        "BB.nad");
    assertEquals("file name is wrong", "BBa.preali",
        FileType.PREALIGNED_STACK.getFileName(manager, AxisID.FIRST));
    assertEquals("file name is wrong",
        FileType.PREALIGNED_STACK.getFileName(manager, AxisID.SECOND), "BBb.preali");
    assertEquals("file name is wrong",
        FileType.RAW_TILT_ANGLES.getFileName(manager, AxisID.FIRST), "BBa.rawtlt");
    assertEquals("file name is wrong",
        FileType.RAW_TILT_ANGLES.getFileName(manager, AxisID.SECOND), "BBb.rawtlt");
    assertEquals("file name is wrong",
        FileType.TRIM_VOL_OUTPUT.getFileName(manager, AxisID.FIRST), "BB.rec");
    assertEquals("file name is wrong",
        FileType.TRIM_VOL_OUTPUT.getFileName(manager, AxisID.SECOND), "BB.rec");
    assertEquals("file name is wrong",
        FileType.JOIN_SAMPLE_AVERAGES.getFileName(manager, AxisID.FIRST), "BB.sampavg");
    assertEquals("file name is wrong",
        FileType.JOIN_SAMPLE_AVERAGES.getFileName(manager, AxisID.SECOND), "BB.sampavg");
    assertEquals("file name is wrong",
        FileType.JOIN_SAMPLE.getFileName(manager, AxisID.FIRST), "BB.sample");
    assertEquals("file name is wrong",
        FileType.JOIN_SAMPLE.getFileName(manager, AxisID.SECOND), "BB.sample");
    assertEquals("file name is wrong",
        FileType.SQUEEZE_VOL_OUTPUT.getFileName(manager, AxisID.FIRST), "BB.sqz");
    assertEquals("file name is wrong",
        FileType.SQUEEZE_VOL_OUTPUT.getFileName(manager, AxisID.SECOND), "BB.sqz");
    assertEquals("file name is wrong",
        FileType.RAW_STACK.getFileName(manager, AxisID.FIRST), "BBa.st");
    assertEquals("file name is wrong",
        FileType.RAW_STACK.getFileName(manager, AxisID.SECOND), "BBb.st");
    assertEquals("file name is wrong",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager, AxisID.FIRST),
        "BBa_3dfind.ali");
    assertEquals("file name is wrong",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager, AxisID.SECOND),
        "BBb_3dfind.ali");
    assertEquals("file name is wrong",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getFileName(manager, AxisID.FIRST),
        "BBa_3dfind.mod");
    assertEquals("file name is wrong",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getFileName(manager, AxisID.SECOND),
        "BBb_3dfind.mod");
    assertEquals("file name is wrong",
        FileType.TILT_3D_FIND_OUTPUT.getFileName(manager, AxisID.FIRST), "BBa_3dfind.rec");
    assertEquals("file name is wrong",
        FileType.TILT_3D_FIND_OUTPUT.getFileName(manager, AxisID.SECOND),
        "BBb_3dfind.rec");
    assertEquals("file name is wrong",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getFileName(manager, AxisID.FIRST),
        "BBa_checkflat.mod");
    assertEquals("file name is wrong",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getFileName(manager, AxisID.SECOND),
        "BBb_checkflat.mod");
    assertEquals("file name is wrong", "BBa_ctfcorr.ali",
        FileType.CTF_CORRECTED_STACK.getFileName(manager, AxisID.FIRST));
    assertEquals("file name is wrong",
        FileType.CTF_CORRECTED_STACK.getFileName(manager, AxisID.SECOND),
        "BBb_ctfcorr.ali");
    assertEquals("file name is wrong",
        FileType.ERASED_BEADS_STACK.getFileName(manager, AxisID.FIRST), "BBa_erase.ali");
    assertEquals("file name is wrong",
        FileType.ERASED_BEADS_STACK.getFileName(manager, AxisID.SECOND), "BBb_erase.ali");
    assertEquals("file name is wrong",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(manager, AxisID.FIRST),
        "BBa_erase.fid");
    assertEquals("file name is wrong",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(manager, AxisID.SECOND),
        "BBb_erase.fid");
    assertEquals("file name is wrong",
        FileType.MTF_FILTERED_STACK.getFileName(manager, AxisID.FIRST), "BBa_filt.ali");
    assertEquals("file name is wrong",
        FileType.MTF_FILTERED_STACK.getFileName(manager, AxisID.SECOND), "BBb_filt.ali");
    assertEquals("file name is wrong",
        FileType.FIXED_XRAYS_STACK.getFileName(manager, AxisID.FIRST), "BBa_fixed.st");
    assertEquals("file name is wrong",
        FileType.FIXED_XRAYS_STACK.getFileName(manager, AxisID.SECOND), "BBb_fixed.st");
    assertEquals("file name is wrong",
        FileType.FLATTEN_WARP_INPUT_MODEL.getFileName(manager, AxisID.FIRST),
        "BB_flat.mod");
    assertEquals("file name is wrong",
        FileType.FLATTEN_WARP_INPUT_MODEL.getFileName(manager, AxisID.SECOND),
        "BB_flat.mod");
    assertEquals("file name is wrong",
        FileType.FLATTEN_OUTPUT.getFileName(manager, AxisID.FIRST), "BB_flat.rec");
    assertEquals("file name is wrong",
        FileType.FLATTEN_OUTPUT.getFileName(manager, AxisID.SECOND), "BB_flat.rec");
    assertEquals("file name is wrong",
        FileType.FLATTEN_TOOL_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "BB_flatten.com");
    assertEquals("file name is wrong",
        FileType.FLATTEN_TOOL_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "BB_flatten.com");
    assertEquals("file name is wrong",
        FileType.MODELED_JOIN.getFileName(manager, AxisID.FIRST), "BB_modeled.join");
    assertEquals("file name is wrong",
        FileType.MODELED_JOIN.getFileName(manager, AxisID.SECOND), "BB_modeled.join");
    assertEquals("file name is wrong",
        FileType.ORIGINAL_RAW_STACK.getFileName(manager, AxisID.FIRST), "BBa_orig.st");
    assertEquals("file name is wrong",
        FileType.ORIGINAL_RAW_STACK.getFileName(manager, AxisID.SECOND), "BBb_orig.st");
    assertEquals("file name is wrong",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getFileName(manager, AxisID.FIRST),
        "BBa_ptbound.mod");
    assertEquals("file name is wrong",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getFileName(manager, AxisID.SECOND),
        "BBb_ptbound.mod");
    assertEquals("file name is wrong",
        FileType.TRANSFORMED_REFINING_MODEL.getFileName(manager, AxisID.FIRST),
        "BB_refine.alimod");
    assertEquals("file name is wrong",
        FileType.TRANSFORMED_REFINING_MODEL.getFileName(manager, AxisID.SECOND),
        "BB_refine.alimod");
    assertEquals("file name is wrong",
        FileType.TRIAL_JOIN.getFileName(manager, AxisID.FIRST), "BB_trial.join");
    assertEquals("file name is wrong",
        FileType.TRIAL_JOIN.getFileName(manager, AxisID.SECOND), "BB_trial.join");
    assertEquals("file name is wrong",
        FileType.CTF_CORRECTION_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "ctfcorrectiona.com");
    assertEquals("file name is wrong",
        FileType.CTF_CORRECTION_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "ctfcorrectionb.com");
    assertEquals("file name is wrong",
        FileType.FIND_BEADS_3D_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "findbeads3da.com");
    assertEquals("file name is wrong",
        FileType.FIND_BEADS_3D_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "findbeads3db.com");
    assertEquals("file name is wrong",
        FileType.FLATTEN_COMSCRIPT.getFileName(manager, AxisID.FIRST), "flatten.com");
    assertEquals("file name is wrong",
        FileType.FLATTEN_COMSCRIPT.getFileName(manager, AxisID.SECOND), "flatten.com");
    assertEquals("file name is wrong",
        FileType.MTF_FILTER_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "mtffiltera.com");
    assertEquals("file name is wrong",
        FileType.MTF_FILTER_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "mtffilterb.com");
    assertEquals("file name is wrong",
        FileType.PATCH_VECTOR_MODEL.getFileName(manager, AxisID.FIRST),
        "patch_vector.mod");
    assertEquals("file name is wrong",
        FileType.PATCH_VECTOR_MODEL.getFileName(manager, AxisID.SECOND),
        "patch_vector.mod");
    assertEquals("file name is wrong",
        FileType.PATCH_VECTOR_CCC_MODEL.getFileName(manager, AxisID.FIRST),
        "patch_vector_ccc.mod");
    assertEquals("file name is wrong",
        FileType.PATCH_VECTOR_CCC_MODEL.getFileName(manager, AxisID.SECOND),
        "patch_vector_ccc.mod");
    assertEquals("file name is wrong",
        FileType.SIRTSETUP_COMSCRIPT.getFileName(manager, AxisID.FIRST), "sirtsetupa.com");
    assertEquals("file name is wrong",
        FileType.SIRTSETUP_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "sirtsetupb.com");
    assertEquals("file name is wrong",
        FileType.COMBINED_VOLUME.getFileName(manager, AxisID.FIRST), "sum.rec");
    assertEquals("file name is wrong",
        FileType.COMBINED_VOLUME.getFileName(manager, AxisID.SECOND), "sum.rec");
    assertEquals("file name is wrong",
        FileType.NAD_TEST_INPUT.getFileName(manager, AxisID.FIRST), "test.input");
    assertEquals("file name is wrong",
        FileType.NAD_TEST_INPUT.getFileName(manager, AxisID.SECOND), "test.input");
    assertEquals("file name is wrong",
        FileType.TILT_COMSCRIPT.getFileName(manager, AxisID.FIRST), "tilta.com");
    assertEquals("file name is wrong",
        FileType.TILT_COMSCRIPT.getFileName(manager, AxisID.SECOND), "tiltb.com");
    assertEquals("file name is wrong",
        FileType.TILT_OUTPUT.getFileName(manager, AxisID.FIRST), "BBa.rec");
    assertEquals("file name is wrong",
        FileType.TILT_OUTPUT.getFileName(manager, AxisID.SECOND), "BBb.rec");
    assertEquals("file name is wrong",
        FileType.TILT_OUTPUT.getFileName(manager, AxisID.FIRST), "BBa.rec");
    assertEquals("file name is wrong",
        FileType.TILT_OUTPUT.getFileName(manager, AxisID.SECOND), "BBb.rec");
    assertEquals("file name is wrong",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getFileName(manager, AxisID.FIRST),
        "BBa.sint");
    assertEquals("file name is wrong",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getFileName(manager, AxisID.SECOND),
        "BBb.sint");
    assertEquals("file name is wrong",
        FileType.SIRT_OUTPUT_TEMPLATE.getFileName(manager, AxisID.FIRST), "BBa.srec");
    assertEquals("file name is wrong",
        FileType.SIRT_OUTPUT_TEMPLATE.getFileName(manager, AxisID.SECOND), "BBb.srec");
    assertEquals("file name is wrong",
        FileType.TRACK_COMSCRIPT.getFileName(manager, AxisID.FIRST), "tracka.com");
    assertEquals("file name is wrong",
        FileType.TRACK_COMSCRIPT.getFileName(manager, AxisID.SECOND), "trackb.com");
    assertEquals("file name is wrong",
        FileType.CROSS_CORRELATION_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "xcorra.com");
    assertEquals("file name is wrong",
        FileType.CROSS_CORRELATION_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "xcorrb.com");
    assertEquals("file name is wrong",
        FileType.PATCH_TRACKING_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "xcorr_pta.com");
    assertEquals("file name is wrong",
        FileType.PATCH_TRACKING_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "xcorr_ptb.com");
    assertEquals("file name is wrong",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getFileName(manager, AxisID.FIRST),
        "tilta_for_sirt.com");
    assertEquals("file name is wrong",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getFileName(manager, AxisID.SECOND),
        "tiltb_for_sirt.com");

    // test single axis
    metaData.setAxisType(AxisType.SINGLE_AXIS);
    assertEquals("file name is wrong",
        FileType.FIDUCIAL_3D_MODEL.getFileName(manager, AxisID.ONLY), "BB.3dmod");
    assertEquals("file name is wrong",
        FileType.ALIGNED_STACK.getFileName(manager, AxisID.ONLY), "BB.ali");
    assertEquals("file name is wrong",
        FileType.XCORR_BLEND_OUTPUT.getFileName(manager, AxisID.ONLY), "BB.bl");
    assertEquals("file name is wrong",
        FileType.DISTORTION_CORRECTED_STACK.getFileName(manager, AxisID.ONLY), "BB.dcst");
    assertEquals("file name is wrong",
        FileType.FIDUCIAL_MODEL.getFileName(manager, AxisID.ONLY), "BB.fid");
    assertEquals("file name is wrong",
        FileType.FLATTEN_TOOL_OUTPUT.getFileName(manager, AxisID.ONLY), "BB.flat");
    assertEquals("file name is wrong", FileType.JOIN.getFileName(manager, AxisID.ONLY),
        "BB.join");
    assertEquals("file name is wrong",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.getFileName(manager, AxisID.ONLY), "BB.nad");
    assertEquals("file name is wrong",
        FileType.PREALIGNED_STACK.getFileName(manager, AxisID.ONLY), "BB.preali");
    assertEquals("file name is wrong",
        FileType.RAW_TILT_ANGLES.getFileName(manager, AxisID.ONLY), "BB.rawtlt");
    assertEquals("file name is wrong",
        FileType.TRIM_VOL_OUTPUT.getFileName(manager, AxisID.ONLY), "BB.rec");
    assertEquals("file name is wrong",
        FileType.JOIN_SAMPLE_AVERAGES.getFileName(manager, AxisID.ONLY), "BB.sampavg");
    assertEquals("file name is wrong",
        FileType.JOIN_SAMPLE.getFileName(manager, AxisID.ONLY), "BB.sample");
    assertEquals("file name is wrong", "BB.sqz",
        FileType.SQUEEZE_VOL_OUTPUT.getFileName(manager, AxisID.ONLY));
    assertEquals("file name is wrong",
        FileType.RAW_STACK.getFileName(manager, AxisID.ONLY), "BB.st");
    assertEquals("file name is wrong",
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager, AxisID.ONLY),
        "BB_3dfind.ali");
    assertEquals("file name is wrong",
        FileType.FIND_BEADS_3D_OUTPUT_MODEL.getFileName(manager, AxisID.ONLY),
        "BB_3dfind.mod");
    assertEquals("file name is wrong",
        FileType.TILT_3D_FIND_OUTPUT.getFileName(manager, AxisID.ONLY), "BB_3dfind.rec");
    assertEquals("file name is wrong",
        FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getFileName(manager, AxisID.ONLY),
        "BB_checkflat.mod");
    assertEquals("file name is wrong", "BB_ctfcorr.ali",
        FileType.CTF_CORRECTED_STACK.getFileName(manager, AxisID.ONLY));
    assertEquals("file name is wrong",
        FileType.ERASED_BEADS_STACK.getFileName(manager, AxisID.ONLY), "BB_erase.ali");
    assertEquals("file name is wrong",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(manager, AxisID.ONLY),
        "BB_erase.fid");
    assertEquals("file name is wrong",
        FileType.MTF_FILTERED_STACK.getFileName(manager, AxisID.ONLY), "BB_filt.ali");
    assertEquals("file name is wrong",
        FileType.FIXED_XRAYS_STACK.getFileName(manager, AxisID.ONLY), "BB_fixed.st");
    assertEquals("file name is wrong",
        FileType.FLATTEN_WARP_INPUT_MODEL.getFileName(manager, AxisID.ONLY),
        "BB_flat.mod");
    assertEquals("file name is wrong",
        FileType.FLATTEN_OUTPUT.getFileName(manager, AxisID.ONLY), "BB_flat.rec");
    assertEquals("file name is wrong",
        FileType.FLATTEN_TOOL_COMSCRIPT.getFileName(manager, AxisID.ONLY),
        "BB_flatten.com");
    assertEquals("file name is wrong",
        FileType.MODELED_JOIN.getFileName(manager, AxisID.ONLY), "BB_modeled.join");
    assertEquals("file name is wrong",
        FileType.ORIGINAL_RAW_STACK.getFileName(manager, AxisID.ONLY), "BB_orig.st");
    assertEquals("file name is wrong",
        FileType.PATCH_TRACKING_BOUNDARY_MODEL.getFileName(manager, AxisID.ONLY),
        "BB_ptbound.mod");
    assertEquals("file name is wrong",
        FileType.TRANSFORMED_REFINING_MODEL.getFileName(manager, AxisID.ONLY),
        "BB_refine.alimod");
    assertEquals("file name is wrong",
        FileType.TRIAL_JOIN.getFileName(manager, AxisID.ONLY), "BB_trial.join");
    assertEquals("file name is wrong",
        FileType.CTF_CORRECTION_COMSCRIPT.getFileName(manager, AxisID.ONLY),
        "ctfcorrection.com");
    assertEquals("file name is wrong",
        FileType.FIND_BEADS_3D_COMSCRIPT.getFileName(manager, AxisID.ONLY),
        "findbeads3d.com");
    assertEquals("file name is wrong",
        FileType.FLATTEN_COMSCRIPT.getFileName(manager, AxisID.ONLY), "flatten.com");
    assertEquals("file name is wrong",
        FileType.MTF_FILTER_COMSCRIPT.getFileName(manager, AxisID.ONLY), "mtffilter.com");
    assertEquals("file name is wrong",
        FileType.PATCH_VECTOR_MODEL.getFileName(manager, AxisID.ONLY), "patch_vector.mod");
    assertEquals("file name is wrong",
        FileType.PATCH_VECTOR_CCC_MODEL.getFileName(manager, AxisID.ONLY),
        "patch_vector_ccc.mod");
    assertEquals("file name is wrong",
        FileType.SIRTSETUP_COMSCRIPT.getFileName(manager, AxisID.ONLY), "sirtsetup.com");
    assertEquals("file name is wrong",
        FileType.COMBINED_VOLUME.getFileName(manager, AxisID.ONLY), "sum.rec");
    assertEquals("file name is wrong",
        FileType.NAD_TEST_INPUT.getFileName(manager, AxisID.ONLY), "test.input");
    assertEquals("file name is wrong", "tilt.com",
        FileType.TILT_COMSCRIPT.getFileName(manager, AxisID.ONLY));
    assertEquals("file name is wrong",
        FileType.TILT_OUTPUT.getFileName(manager, AxisID.ONLY), "BB_full.rec");
    assertEquals("file name is wrong",
        FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getFileName(manager, AxisID.ONLY),
        "BB_full.sint");
    assertEquals("file name is wrong",
        FileType.SIRT_OUTPUT_TEMPLATE.getFileName(manager, AxisID.ONLY), "BB_full.srec");
    assertEquals("file name is wrong",
        FileType.TRACK_COMSCRIPT.getFileName(manager, AxisID.ONLY), "track.com");
    assertEquals("file name is wrong",
        FileType.CROSS_CORRELATION_COMSCRIPT.getFileName(manager, AxisID.ONLY),
        "xcorr.com");
    assertEquals("file name is wrong",
        FileType.PATCH_TRACKING_COMSCRIPT.getFileName(manager, AxisID.ONLY),
        "xcorr_pt.com");
    assertEquals("file name is wrong",
        FileType.TILT_FOR_SIRT_COMSCRIPT.getFileName(manager, AxisID.ONLY),
        "tilt_for_sirt.com");
  }

  public void testIsInSubdirectory() {
    FrontPageManager manager = (FrontPageManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    FrontPageMetaData metaData = manager.getMetaData();
    metaData.setAxisType(AxisType.SINGLE_AXIS);
    assertFalse("In subdirectory", FileType.TILT_OUTPUT.isInSubdirectory());
    assertFalse("In subdirectory",
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.isInSubdirectory());
    assertFalse("In subdirectory", FileType.SIRT_OUTPUT_TEMPLATE.isInSubdirectory());
    assertFalse("In subdirectory",
        FileType.ANISOTROPIC_DIFFUSION_OUTPUT.isInSubdirectory());
    assertTrue("Not in subdirectory", FileType.NAD_TEST_INPUT.isInSubdirectory());
    assertFalse("In subdirectory", FileType.RAW_STACK.isInSubdirectory());
    assertFalse("In subdirectory", FileType.ALIGNED_STACK.isInSubdirectory());
    assertFalse("In subdirectory", FileType.AVERAGED_VOLUMES.isInSubdirectory());
    assertTrue("Not in subdirectory",
        FileType.NAD_TEST_VARYING_ITERATIONS.isInSubdirectory());
  }
}
