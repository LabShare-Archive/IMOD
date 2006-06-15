package etomo.storage.autodoc;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.type.AxisID;
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
 */
public class AutodocTest extends TestCase {
  public static final String rcsid = "$Id$";

  public AutodocTest(String name) {
    super(name);
  }

  protected void setUp() throws Exception {
    super.setUp();

  }

  protected void tearDown() throws Exception {

    super.tearDown();
  }

  /*
   public void testUITest() throws FileNotFoundException, IOException{
   //Autodoc.setInternalTest(true);
   Autodoc.setTest(true);
   Autodoc autodoc = Autodoc.getInstance(Autodoc.UITEST, AxisID.ONLY);
   assertFalse(autodoc.isError());
   Autodoc.setTest(false);
   //Autodoc.setInternalTest(false);
   }
   
   public void testUITestAxis_testa() throws FileNotFoundException, IOException{
   Autodoc.setTest(true);
   Autodoc autodoc = Autodoc.getInstance("testa", Autodoc.UITEST_AXIS, AxisID.ONLY);
   assertFalse(autodoc.isError());
   Autodoc.setTest(false);
   }
   
   public void testUITestAxis_testb() throws FileNotFoundException, IOException{
   Autodoc.setTest(true);
   Autodoc autodoc = Autodoc.getInstance("testb", Autodoc.UITEST_AXIS, AxisID.ONLY);
   assertFalse(autodoc.isError());
   Autodoc.setTest(false);
   }
   
   public void testUITestAxis_setup_recon() throws FileNotFoundException, IOException{
   Autodoc.setTest(true);
   Autodoc autodoc = Autodoc.getInstance("setup-recon", Autodoc.UITEST_AXIS, AxisID.ONLY);
   assertFalse(autodoc.isError());
   Autodoc.setTest(false);
   }
   */
  public void testBeadtrack() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.BEADTRACK, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testCcderaser() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.CCDERASER, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testCombineFft() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.COMBINE_FFT, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testCpu() throws IOException {
    try {
      Autodoc autodoc = Autodoc.getInstance(Autodoc.CPU, AxisID.ONLY);
      assertFalse(autodoc.isError());
    }
    catch (FileNotFoundException e) {
    }
  }

  public void testDensmatch() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.DENS_MATCH, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testMtfFilter() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.MTF_FILTER, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testSolvematch() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.SOLVEMATCH, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testTiltalign() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.TILTALIGN, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }

  public void testTiltxcorr() throws FileNotFoundException, IOException {
    Autodoc autodoc = Autodoc.getInstance(Autodoc.TILTXCORR, AxisID.ONLY);
    assertFalse(autodoc.isError());
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2006/06/14 16:26:04  sueh
 * <p> bug# 852 Fixed problem with tests
 * <p>
 * <p> Revision 1.2  2006/06/14 00:31:25  sueh
 * <p> bug# 852 Added a test for setup-recon.adoc.
 * <p>
 * <p> Revision 1.1  2006/06/14 00:23:02  sueh
 * <p> bug# 852 Tests for Autodoc.  Parses autodocs and then checks the parser for an
 * <p> error.
 * <p> </p>
 */
