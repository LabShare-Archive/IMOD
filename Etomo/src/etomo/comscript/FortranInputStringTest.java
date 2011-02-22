/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.3  2010/04/28 15:55:56  sueh
 * <p> bug# 1344 Removed unnecessary tearDown function override.
 * <p>
 * <p> Revision 1.2  2004/06/01 18:54:21  rickg
 * <p> Comment fixes
 * <p>
 * <p> Revision 1.1  2004/02/12 04:35:59  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.comscript;

import junit.framework.TestCase;

public class FortranInputStringTest extends TestCase {
  public static final String rcsid = "$Id$";

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /**
   * Test some basic tuples
   */
  public void testSimpleTuple() {
    FortranInputString fisPair = new FortranInputString(2);
    String pair = "1.0,-3.0";
    try {
      fisPair.validateAndSet(pair);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect simple pair toString", pair, fisPair.toString());

    FortranInputString fisTriple = new FortranInputString(3);
    String triple = "-2.1,-3.0,3.1415";
    try {
      fisTriple.validateAndSet(triple);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect simple triple toString", triple, fisTriple.toString());

    FortranInputString fisQuad = new FortranInputString(4);
    String quad = "1.0,2.4,-3.5,1999.9";
    try {
      fisQuad.validateAndSet(quad);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect simple quad toString", quad, fisQuad.toString());
  }

  /**
   * Test some integer and mixed tuples
   */
  public void testIntegerTuple() {
    FortranInputString fisPair = new FortranInputString(2);
    boolean[] intFlag2 = { true, true };
    fisPair.setIntegerType(intFlag2);
    String pair = "1,-3";
    try {
      fisPair.validateAndSet(pair);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect integer pair toString", pair, fisPair.toString());

    FortranInputString fisTriple = new FortranInputString(3);
    boolean[] intFlag3 = { true, true, false };
    fisTriple.setIntegerType(intFlag3);
    String triple = "-2,3,3.1415";
    try {
      fisTriple.validateAndSet(triple);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect mixed triple toString", triple, fisTriple.toString());

    FortranInputString fisQuad = new FortranInputString(4);
    boolean[] intFlag4 = { false, true, false, true };
    fisQuad.setIntegerType(intFlag4);
    String quad = "1.0,2,-3.5,1999";
    try {
      fisQuad.validateAndSet(quad);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect mixed quad toString", quad, fisQuad.toString());
  }

  /**
   * Test the various default specifier patterns
   */
  public void testDefaultSpecifier() {
    FortranInputString all = new FortranInputString(3);
    String allDefSpec = "/";
    try {
      all.validateAndSet(allDefSpec);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect all default specifier toString", allDefSpec, all.toString());

    FortranInputString restOfArgs = new FortranInputString(3);
    boolean[] intFlag3 = { true, true, false };
    restOfArgs.setIntegerType(intFlag3);
    String restOfSpec = "1,1024/";
    try {
      restOfArgs.validateAndSet(restOfSpec);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect rest of default specifier toString", restOfSpec, restOfArgs
        .toString());

    FortranInputString firstDefault = new FortranInputString(3);
    String firstDefSpec = ",3.2,-1.0";
    try {
      firstDefault.validateAndSet(firstDefSpec);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect first default specifier toString", firstDefSpec, firstDefault
        .toString());

    FortranInputString middleDefault = new FortranInputString(3);
    String middleDefSpec = "3.2,,-1.0";
    try {
      middleDefault.validateAndSet(middleDefSpec);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage());
    }
    assertEquals("Incorrect middle default specifier toString", middleDefSpec,
        middleDefault.toString());

    /*  Is this allowed????
    FortranInputString endDefault = new FortranInputString(3);
    String endDefSpec = "3.2,-1.0,,";
    try {
      endDefault.validateAndSet(endDefSpec);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": "
        + e.getMessage());
    }
    assertEquals("Incorrect end default specifier toString", endDefSpec, 
    endDefault.toString()); */
  }
}
