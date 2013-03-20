package etomo.storage;

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
public class DirectiveNameTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private static final String SETUP_SET_COPY_ARG_NAME = "setupset.copyarg.voltage";
  private static final String SETUP_SET_NAME = "setupset.datasetDirectory";

  private static final String RUNTIME_NAME = "runtime.Preprocessing.any.removeXrays";
  private static final String RUNTIME_A_NAME = "runtime.Preprocessing.a.removeXrays";
  private static final String RUNTIME_B_NAME = "runtime.Preprocessing.b.removeXrays";

  private static final String COM_PARAM_NAME = "comparam.eraser.ccderaser.PeakCriterion";
  private static final String COM_PARAM_A_NAME = "comparam.erasera.ccderaser.PeakCriterion";
  private static final String COM_PARAM_B_NAME = "comparam.eraserb.ccderaser.PeakCriterion";

  private final DirectiveName directiveName = new DirectiveName();

  public DirectiveNameTest(String name) {
    super(name);
  }

  protected void setUp() throws Exception {
    super.setUp();
  }

  protected void tearDown() throws Exception {
    super.tearDown();
  }

  public void testEquals() {
    assertFalse("returns false when nothing is passed", DirectiveName.equals(null, null));
    assertFalse("returns false when no value",
        DirectiveName.equals(null, DirectiveType.COM_PARAM));
    assertFalse("returns false when no type",
        DirectiveName.equals(SETUP_SET_COPY_ARG_NAME, null));

    assertTrue("recognizes setupset",
        DirectiveName.equals(SETUP_SET_COPY_ARG_NAME, DirectiveType.SETUP_SET));
    assertTrue("recognizes setupset",
        DirectiveName.equals(SETUP_SET_NAME, DirectiveType.SETUP_SET));
    assertTrue("recognizes runtime",
        DirectiveName.equals(RUNTIME_NAME, DirectiveType.RUNTIME));
    assertTrue("recognizes comparam",
        DirectiveName.equals(COM_PARAM_NAME, DirectiveType.COM_PARAM));

    assertFalse("returns false when nothing is passed", directiveName.equals(null));
    assertFalse("returns false when no value",
        directiveName.equals(DirectiveType.COM_PARAM));
    directiveName.set((String) null);
    assertFalse("returns false when nothing is passed", directiveName.equals(null));
    assertFalse("returns false when no value",
        directiveName.equals(DirectiveType.COM_PARAM));

    directiveName.set(SETUP_SET_COPY_ARG_NAME);
    assertFalse("returns false when no type", directiveName.equals(null));
    directiveName.set(SETUP_SET_COPY_ARG_NAME);
    assertTrue("recognizes setupset", directiveName.equals(DirectiveType.SETUP_SET));
    directiveName.set(SETUP_SET_NAME);
    assertTrue("recognizes setupset", directiveName.equals(DirectiveType.SETUP_SET));
    directiveName.set(RUNTIME_NAME);
    assertTrue("recognizes runtime", directiveName.equals(DirectiveType.RUNTIME));
    directiveName.set(COM_PARAM_NAME);
    assertTrue("recognizes comparam", directiveName.equals(DirectiveType.COM_PARAM));
  }

  public void testGetComFile() {
    assertNull("returns null with empty data", directiveName.getComFileName());
    directiveName.set((String) null);
    assertNull("returns null with empty data", directiveName.getComFileName());
    directiveName.set(RUNTIME_NAME);
    assertNull("returns null when not a comparam", directiveName.getComFileName());
    directiveName.set("comparam");
    assertNull("returns null when name doesn't contain a com file name",
        directiveName.getComFileName());

    directiveName.set(COM_PARAM_NAME);
    assertEquals("returns com file name", "eraser", directiveName.getComFileName());
    directiveName.set(COM_PARAM_A_NAME);
    assertEquals("strips the axis letter", "eraser", directiveName.getComFileName());
    directiveName.set(COM_PARAM_B_NAME);
    assertEquals("strips the axis letter", "eraser", directiveName.getComFileName());
  }
  
  public void testGetProgramName() {
    assertNull("returns null with empty data", directiveName.getProgramName());
    directiveName.set((String) null);
    assertNull("returns null with empty data", directiveName.getProgramName());
    directiveName.set(RUNTIME_NAME);
    assertNull("returns null when not a comparam", directiveName.getProgramName());
    directiveName.set("comparam.eraser");
    assertNull("returns null when name doesn't contain a com file name",
        directiveName.getProgramName());

    directiveName.set(COM_PARAM_NAME);
    assertEquals("returns com file name", "ccderaser", directiveName.getProgramName());
    directiveName.set(COM_PARAM_A_NAME);
    assertEquals("strips the axis letter", "ccderaser", directiveName.getProgramName());
    directiveName.set(COM_PARAM_B_NAME);
    assertEquals("strips the axis letter", "ccderaser", directiveName.getProgramName());
  }

  public void testRemoveAxisID() {
    // Setting a null string does not cause an exception
    directiveName.set((String) null);
    // Setting an empty string does not cause an exception
    directiveName.set("");

    String name = "setupset.erasera.ccderaser.PeakCriterion";
    directiveName.set(name);
    assertEquals("setupset names are never changed", directiveName.get(), name);
    name = "setupset.Preprocessing.b.removeXrays";
    directiveName.set(name);
    assertEquals("setupset names are never changed", directiveName.get(), name);

    directiveName.set(RUNTIME_NAME);
    assertEquals("runtime name without an axis is unchanged", directiveName.get(),
        RUNTIME_NAME);
    name = "runtime.a.Preprocessing.removeXrays";
    directiveName.set(name);
    assertEquals("only an axis letter in correct place is removed", directiveName.get(),
        name);
    directiveName.set(RUNTIME_A_NAME);
    assertEquals("axis a is changed to 'any'", directiveName.get(), RUNTIME_NAME);
    directiveName.set(RUNTIME_B_NAME);
    assertEquals("axis b is changed to 'any'", directiveName.get(), RUNTIME_NAME);

    directiveName.set(COM_PARAM_NAME);
    assertEquals("comparam name without an axis is unchanged", directiveName.get(),
        COM_PARAM_NAME);
    name = "comparam.eraser.ccderaserb.PeakCriterion";
    directiveName.set(name);
    assertEquals("only an axis letter in correct place is removed", directiveName.get(),
        name);
    directiveName.set(COM_PARAM_A_NAME);
    assertEquals("axis a is removed", directiveName.get(), COM_PARAM_NAME);
    directiveName.set(COM_PARAM_B_NAME);
    assertEquals("axis b is removed", directiveName.get(), COM_PARAM_NAME);

    assertEquals("axis b is changed to 'any'",
        DirectiveName.removeAxisID(RUNTIME_B_NAME), RUNTIME_NAME);
    assertEquals("axis a is removed", COM_PARAM_NAME,DirectiveName.removeAxisID(COM_PARAM_A_NAME));
  }
}
