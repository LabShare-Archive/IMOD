package etomo.logic;

import java.io.File;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

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

public class TextFieldStateTest extends TestCase {
  public static final String rcsid = "$Id:$";

  /**
   * @param name
   */
  public TextFieldStateTest(String name) {
    super(name);
  }

  /* (non-Javadoc)
   * @see junit.framework.TestCase#setUp() */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /* (non-Javadoc)
   * @see junit.framework.TestCase#tearDown() */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  public void testExpandFieldText() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    File file = new File(new File(new File("c"), "d"), "e");
    assertEquals("contracted field displays file name", file.getName(),
        testInstance.expandFieldText(false, file.getPath()));
    assertEquals("expanded field displays file path", file.getPath(),
        testInstance.expandFieldText(true, file.getName()));
  }

  public void textApplyExpandedToFieldText() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    File file = new File(new File(new File("c"), "d"), "e");
    testInstance.expandFieldText(false, file.getPath());
    assertEquals("contracted field displays file name", file.getName(),
        testInstance.applyExpandedToFieldText(file.getPath()));

    testInstance.expandFieldText(true, file.getName());
    assertEquals("expanded field displays file path", file.getPath(),
        testInstance.applyExpandedToFieldText(file.getName()));
  }

  public void testConvertToFieldText() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    File file = new File(new File(new File("c"), "d"), "e");
    testInstance.expandFieldText(false, file.getPath());
    assertEquals("contracted field displays file name", file.getName(),
        testInstance.convertToFieldText(file));
    assertEquals("contracted field displays file name", file.getName(),
        testInstance.convertToFieldText(file.getPath()));

    testInstance.expandFieldText(true, file.getName());
    assertEquals("expanded field displays file path", file.getAbsolutePath(),
        testInstance.convertToFieldText(file));
    assertEquals("expanded field displays file path", file.getPath(),
        testInstance.convertToFieldText(file.getPath()));

    File rootDir = new File(new File(new File("a"), "b"), "c");
    testInstance = new TextFieldState(false, null, rootDir.getAbsolutePath());
    testInstance.expandFieldText(false, file.getPath());
    assertEquals("contracted field displays file name", file.getName(),
        testInstance.convertToFieldText(file));
    assertEquals("contracted field displays file name", file.getName(),
        testInstance.convertToFieldText(file.getPath()));

    testInstance.expandFieldText(true, file.getName());
    assertEquals("expanded field displays relative file path", ".." + File.separator
        + ".." + File.separator + ".." + File.separator + "c" + File.separator + "d"
        + File.separator + "e", testInstance.convertToFieldText(file));
    assertEquals("expanded field displays relative file path", file.getPath(),
        testInstance.convertToFieldText(file.getPath()));
  }

  public void testConvertRangeToFieldText() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    assertEquals("builds range", "1 - 10", testInstance.convertRangeToFieldText(1, 10));
    assertEquals("negative number work", "-10 - -1",
        testInstance.convertRangeToFieldText(-10, -1));

  }

  public void testConvertToContractedString() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    File file = new File(new File(new File("c"), "d"), "e");
    testInstance.expandFieldText(false, file.getPath());
    assertEquals("contracted field - return the field text", file.getName(),
        testInstance.convertToContractedString(file.getName()));

    testInstance.expandFieldText(true, file.getName());
    assertEquals("expanded field - return the file name", file.getName(),
        testInstance.convertToContractedString(file.getPath()));
  }

  public void testConvertToExpandedString() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    File file = new File(new File(new File("c"), "d"), "e");
    testInstance.expandFieldText(false, file.getPath());
    assertEquals("empty - return empty sting", "",
        testInstance.convertToExpandedString(null));
    assertEquals("empty - return empty sting", "",
        testInstance.convertToExpandedString("   "));

    testInstance.expandFieldText(true, file.getName());
    assertEquals("expanded field - return field text", file.getPath(),
        testInstance.convertToExpandedString(file.getPath()));

    testInstance.expandFieldText(false, file.getPath());
    File newfile = new File(new File(new File("x"), "y"), "e");
    assertEquals("contracted field - if pass in a path, return the path",
        newfile.getPath(), testInstance.convertToExpandedString(newfile.getPath()));
    assertEquals("contracted field - return parent + name", file.getPath(),
        testInstance.convertToExpandedString(file.getPath()));
  }

  public void testExtractEndValue() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    assertEquals("empty input - return null value", EtomoNumber.INTEGER_NULL_VALUE,
        testInstance.extractEndValue(null));
    assertEquals("empty input - return null value", EtomoNumber.INTEGER_NULL_VALUE,
        testInstance.extractEndValue(""));
    assertEquals("empty input - return null value", EtomoNumber.INTEGER_NULL_VALUE,
        testInstance.extractEndValue("   "));
    assertEquals("not a range - return null value", EtomoNumber.INTEGER_NULL_VALUE,
        testInstance.extractEndValue("not a range"));
    assertEquals("return end value", 10, testInstance.extractEndValue(" 1 - 10 "));
    assertEquals("negative values work", -1, testInstance.extractEndValue(" -10 - -1 "));
  }

  public void testConvertToEtomoNumber() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    ConstEtomoNumber n = testInstance.convertToEtomoNumber(null);
    assertTrue("null number - return empty etomo number", n.isNull());

    n = testInstance.convertToEtomoNumber("not a number");
    assertTrue("bad number - return empty etomo number", n.isNull());

    n = testInstance.convertToEtomoNumber("1");
    assertTrue("return number", n.equals(1));

    n = testInstance.convertToEtomoNumber(EtomoNumber.Type.DOUBLE, "1.5");
    assertTrue("return number", n.equals(1.5));
  }

  public void testConvertToInt() {
    TextFieldState testInstance = new TextFieldState(false, null, null);
    assertEquals("null number - return 0", 0, testInstance.convertToInt(null));
    assertEquals("bad number - return 0", 0, testInstance.convertToInt("not a number"));
    assertEquals("return number", 1, testInstance.convertToInt("1"));
  }
}
