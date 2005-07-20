package etomo.type;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import etomo.EtomoDirector;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class ConstEtomoNumberTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final double bigDouble = 999999999999999999999999999999999999999.3421D;
  private static final double smallDouble = -999999999999999999999999999999999999999.8793D;
  private static final float bigFloat = 99999999999999999999999999999999999999.903F;
  private static final float smallFloat = -99999999999999999999999999999999999999.0893F;
  private static final long bigLong = 999999999999999999L;
  private static final long smallLong = -999999999999999999L;
  private static final int bigInteger = 999999999;
  private static final int smallInteger = -999999999;

  private File testDir = new File(new File(EtomoDirector.getInstance()
      .getCurrentPropertyUserDir(), TypeTests.testRoot), "ConstEtomoNumber");
  private File propertiesFile = new File(testDir, "properties");

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  /**
   * Constructor for ConstEtomoNumberTest.
   * @param arg0
   */
  public ConstEtomoNumberTest(String arg0) {
    super(arg0);
    testDir.mkdirs();
    propertiesFile.delete();
  }

  final public void testConstEtomoNumber() {
    EtomoNumber test = new EtomoNumber();
    test.selfTestInvariants();
  }

  final public void testConstEtomoNumber_String() {
    String name = "ConstEtomoNumberTest";
    EtomoNumber test = new EtomoNumber(name);
    test.selfTestInvariants();
    //test name
    assertTrue(name.equals(test.getName()));
    //test description
    assertTrue(name.equals(test.getDescription()));
  }

  final public void testConstEtomoNumber_int() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    test.selfTestInvariants();
  }

  final public void testConstEtomoNumber_int_String() {
    //double
    String name = "ConstEtomoNumberTest";
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, name);
    test.selfTestInvariants();
    ///test: name
    assertTrue("name test failed: name=" + name + ",test.getName()="
        + test.getName(), name.equals(test.getName()));
    ///test: description
    assertTrue(name.equals(test.getDescription()));
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE, name);
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE, name);
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE, name);
    test.selfTestInvariants();
  }

  final public void testConstEtomoNumber_ConstEtomoNumber() {
    String name = "ConstEtomoNumberTest";
    String description = "testConstEtomoNumber_ConstEtomoNumber";
    int currentValue = 1;
    int displayValue = 2;
    EtomoNumber test = new EtomoNumber(name);
    test.setDescription(description);
    test.set(currentValue);
    test.setDisplayValue(displayValue);
    EtomoNumber copy = new EtomoNumber(test);
    //test deep copy
    assertFalse(test == copy);
    //test name copy
    assertTrue(copy.getName().equals(name));
    //test description copy
    assertTrue(copy.getDescription().equals(description));
    //test currentValue copy
    assertEquals(copy.getInteger(), currentValue);
    //test displayValue copy
    assertEquals(copy.getDisplayInteger(), displayValue);
    test.selfTestInvariants();
    copy.selfTestInvariants();
  }

  public final void testValidateReturnTypeInteger() {
    int displayValue = 2;
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    ///test corruption prevention
    try {
      test.getDisplayInteger();
      fail("A double can't be returned in an integer");
    }
    catch (IllegalStateException e) {
    }
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test corruption prevention
    try {
      test.getDisplayInteger();
      fail("A float can't be returned in an integer");
    }
    catch (IllegalStateException e) {
    }
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    ///test no exception thrown
    test.getDisplayInteger();
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test corruption prevention
    try {
      test.getDisplayInteger();
      fail("A long can't be returned in an integer");
    }
    catch (IllegalStateException e) {
    }
    test.selfTestInvariants();
  }

  public final void testGetDisplayInteger() {
    //test: validateReturnTypeInteger() is being called
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    ///test corruption prevention
    try {
      test.getDisplayInteger();
      fail("validateReturnTypeInteger() is not being called");
    }
    catch (IllegalStateException e) {
    }
    test.selfTestInvariants();
  }

  public final void testSetInvalidReason() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    //test nullIsValid == true
    test.setInvalidReason();
    assertTrue(test.isValid());
    //test nullIsValid == false
    test.setNullIsValid(false);
    ///null
    test.setInvalidReason();
    assertFalse(test.isValid());
    ///not null
    test.resetInvalidReason();
    test.set(1);
    test.setInvalidReason();
    assertTrue(test.isValid());
    //test validValues
    test.resetInvalidReason();
    test.setValidValues(new int[] { 2, 3 });
    test.setInvalidReason();
    assertFalse(test.isValid());
    test.selfTestInvariants();
  }

  public final void testApplyCeilingValue() {
    int lowValue = 2;
    int ceilingValue = 3;
    int highValue = 4;
    EtomoNumber test = new EtomoNumber();
    test.setCeiling(ceilingValue);
    //test (value < ceiling) => value
    assertEquals(test.applyCeilingValue(new Integer(lowValue)).intValue(),
        lowValue);
    //test (value == ceiling) => ceiling
    assertEquals(test.applyCeilingValue(new Integer(ceilingValue)).intValue(),
        ceilingValue);
    //test (value > ceiling) => ceiling
    assertEquals(test.applyCeilingValue(new Integer(highValue)).intValue(),
        ceilingValue);
    test.selfTestInvariants();
  }

  public final void testApplyFloorValue() {
    int lowValue = 2;
    int floorValue = 3;
    int highValue = 4;
    EtomoNumber test = new EtomoNumber();
    test.setFloor(floorValue);
    //test (value > floor) => value
    assertEquals(test.applyFloorValue(new Integer(highValue)).intValue(),
        highValue);
    //test (value == floor) => floor
    assertEquals(test.applyFloorValue(new Integer(floorValue)).intValue(),
        floorValue);
    //test (value < floor) => floor
    assertEquals(test.applyFloorValue(new Integer(lowValue)).intValue(),
        floorValue);
    test.selfTestInvariants();
  }

  public final void testValidate_String_String_AxisID()
      throws InvalidEtomoNumberException {
    String errorTitle = "testValidate_String_String_AxisID";
    EtomoNumber test = new EtomoNumber();
    //test valid
    test.validate(errorTitle, "test valid failed", AxisID.FIRST);
    //test invalid
    test.setNullIsValid(false);
    test.setInvalidReason();
    try {
      test.validate(errorTitle, "test invalid succeeded", AxisID.FIRST);
      fail("invalid ConstEomoNumber failed to cause exception to be thrown");
    }
    catch (InvalidEtomoNumberException e) {
    }
    test.selfTestInvariants();
  }

  public final void testIsValid_boolean_String_String_AxisID() {
    String errorTitle = "testIsValid_boolean_String_String_AxisID";
    EtomoNumber test = new EtomoNumber();
    //test valid with print
    assertTrue(test
        .isValid(true, errorTitle, "test valid failed", AxisID.FIRST));
    //test valid without print
    assertTrue(test.isValid(false, errorTitle, "test valid failed",
        AxisID.FIRST));
    //test invalid
    test.setNullIsValid(false);
    test.setInvalidReason();
    ///with print
    assertFalse(test.isValid(true, errorTitle, "test valid succeeded",
        AxisID.FIRST));
    //without print
    assertFalse(test.isValid(false, errorTitle, "test valid succeeded",
        AxisID.FIRST));
    test.selfTestInvariants();
  }

  public final void testGetInvalidReason() {
    EtomoNumber test = new EtomoNumber();
    test.setNullIsValid(false);
    test.setInvalidReason();
    test.setDescription("invalid reason format test");
    //test invalid reason format
    assertTrue("invalid reason format test failed: test.getInvalidReason()="
        + test.getInvalidReason() + ",test.getDescription()="
        + test.getDescription(), test.getInvalidReason().indexOf(
        test.getDescription()) != -1);
    test.selfTestInvariants();
  }

  public final void testValidateFloorAndCeiling() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    //test: floor > ceiling throws IllegalStateException
    test.setFloor(bigInteger);
    try {
      test.setCeiling(smallInteger);
      fail("floor > ceiling did not throw IllegalStateException");
    }
    catch (IllegalStateException e) {
    }
    //test: floor == ceiling is valid
    test.setFloor(smallInteger);
    //test: floor < ceiling is valid
    test.setCeiling(bigInteger);
    test.selfTestInvariants();
  }

  public final void testSetCeiling() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    //test: validateFloorAndCeiling() was called
    test.setFloor(bigInteger);
    try {
      test.setCeiling(smallInteger);
      fail("validateFloorAndCeiling() was not called");
    }
    catch (IllegalStateException e) {
    }
  }

  public final void testSetFloor() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    //test: validateFloorAndCeiling() was called
    test.setCeiling(smallInteger);
    try {
      test.setFloor(bigInteger);
      fail("validateFloorAndCeiling() was not called");
    }
    catch (IllegalStateException e) {
    }
  }

  public final void testSetDescription() {
    String name = "ConstEtomoNumberTest";
    String description = "testSetDescription";
    EtomoNumber test = new EtomoNumber(name);
    //test set == get
    test.setDescription(description);
    assertTrue(test.getDescription().equals(description));
    //test (set null) => name
    test.setDescription(null);
    assertTrue(test.getDescription().equals(name));
    test.selfTestInvariants();
  }

  public final void testSetValidValues_intArray() {
    int validNumber = 3;
    EtomoNumber test = new EtomoNumber();
    test.setValidValues(new int[] { 1, EtomoNumber.INTEGER_NULL_VALUE,
        validNumber, 5 });
    //test nulls are ignored in valid value
    test.setInvalidReason();
    assertTrue(test.isValid());
    //test valid value
    test.set(validNumber);
    test.setInvalidReason();
    assertTrue(test.isValid());
    //test invalid value
    test.set(7);
    test.setInvalidReason();
    assertFalse(test.isValid());
    test.selfTestInvariants();
  }

  public final void testStore_Properties() throws IOException {
    String name = "TestStore_PropertiesName";
    EtomoNumber test = new EtomoNumber(name);
    test.set(smallInteger);
    ParameterStore properties = new ParameterStore(propertiesFile);
    Storable storable[] = new Storable[1];
    storable[0] = test;
    //test: no IOException thrown on save
    properties.save(storable);
    //test: write parameter to file
    BufferedReader logFileReader = new BufferedReader(new FileReader(
        propertiesFile));
    String line;
    while ((line = logFileReader.readLine()) != null) {
      if (line.trim().equals(name + "=" + smallInteger)) {
        return;
      }
    }
    fail("write parameter to file test failed");
  }

  public final void testStore_Properties_String() {
  }

  public final void testRemove_Properties() {
  }

  public final void testRemove_Properties_prepend() {
  }

  public final void testToString() {
  }

  public final void testGetInteger() {
  }

  public final void testIs() {
    EtomoNumber test = new EtomoNumber();
    //test null false
    assertFalse(test.is());
    //test negatives are true
    test.set(-590);
    assertTrue(test.is());
    //test positives are true
    test.set(174809);
    assertTrue(test.is());
    //test 0 is false
    test.set(0);
    assertFalse(test.is());
    test.selfTestInvariants();
  }

  public final void testIsPositive() {
    EtomoNumber test = new EtomoNumber();
    //test null is not positive
    assertFalse(test.isPositive());
    //test negative returns false
    test.set(-590);
    assertFalse(test.isPositive());
    //test positive returns true
    test.set(174809);
    assertTrue(test.isPositive());
    //test 0 is not positive
    test.set(0);
    assertFalse(test.isPositive());
    test.selfTestInvariants();
  }

  public final void testIsNegative() {
    EtomoNumber test = new EtomoNumber();
    //test null is not negative
    assertFalse(test.isNegative());
    //test negative returns true
    test.set(-590);
    assertTrue(test.isNegative());
    //test positive returns false
    test.set(174809);
    assertFalse(test.isNegative());
    //test 0 is not negative
    test.set(0);
    assertFalse(test.isNegative());
    test.selfTestInvariants();
  }

  public final void testValidateReturnTypeLong() {
  }

  public final void testGetLong() {
  }

  public final void validateReturnTypeDouble() {
  }

  public final void testGetDouble() {
  }

  public final void testGetNumber() {
  }

  public final void testEquals_Number() {
  }

  public final void testEquals_String() {
  }

  public final void testIsNamed_String() {
  }

  public final void testGetValue() {
    int displayValue = 2;
    int currentValue = 3;
    EtomoNumber test = new EtomoNumber();
    //test returns null value
    assertTrue(test.getValue().intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    //test returns non-null display value when current value is null
    test.setDisplayValue(displayValue);
    assertTrue(test.getValue().intValue() == displayValue);
    //test returns non-null current value when display value is set
    test.set(currentValue);
    assertTrue(test.getValue().intValue() == currentValue);
    test.selfTestInvariants();
  }

  public final void toString_Number() {
    Integer nullValue = new Integer(EtomoNumber.INTEGER_NULL_VALUE);
    Integer value = new Integer(3);
    EtomoNumber test = new EtomoNumber();
    //test returns empty string for a null value
    assertTrue(test.toString(nullValue).equals(""));
    //test returns string version of value
    assertTrue(test.toString(value).equals(value.toString()));
    test.selfTestInvariants();
  }

  public final void testToString_Vector() {
  }

  public final void testAddInvalidReason() {
    String msg1 = "message 1";
    String msg2 = "message 2";
    EtomoNumber test = new EtomoNumber();
    //test: save message
    test.addInvalidReason(msg1);
    assertTrue("save message test failed: test.getInvalidReason()="
        + test.getInvalidReason(), test.getInvalidReason().indexOf(msg1) != -1);
    //test: add to existing message
    test.addInvalidReason(msg2);
    String invalidReason = test.getInvalidReason();
    assertTrue(invalidReason.indexOf(msg1) != -1
        && invalidReason.indexOf(msg2) != -1);
    test.selfTestInvariants();
  }

  public final void testNewNumber() {
    //test: returns null value
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(new Double(test.newNumber().doubleValue()).isNaN());
    test.selfTestInvariants();
    ///float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    assertTrue(new Float(test.newNumber().floatValue()).isNaN());
    test.selfTestInvariants();
    ///integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    assertTrue(test.newNumber().intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    test.selfTestInvariants();
    ///long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    assertTrue(test.newNumber().longValue() == EtomoNumber.LONG_NULL_VALUE);
    test.selfTestInvariants();
  }

  public final void testValidateInputType_Number() {
  }

  public final void testNewNumber_Number() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    ///test: null returns null value
    assertTrue(new Double(test.newNumber(null).doubleValue()).isNaN());
    ///test: convert Double to Double
    assertTrue(test.newNumber(new Double(bigDouble)).doubleValue() == bigDouble);
    ///test: convert Float to Double
    assertTrue(test.newNumber(new Float(bigFloat)).doubleValue() == bigFloat);
    ///test: convert Integer to Double
    assertTrue(test.newNumber(new Integer(bigInteger)).doubleValue() == bigInteger);
    ///test: convert Long to Double
    assertTrue(test.newNumber(new Long(bigLong)).doubleValue() == bigLong);
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test: null returns null value
    assertTrue(new Float(test.newNumber(null).floatValue()).isNaN());
    ///test: validateInputType(Number) is being called
    try {
      test.newNumber(new Double(bigDouble));
      fail("validateInputType(Number) was not called");
    }
    catch (IllegalStateException e) {
    }
    ///test: convert Float to Float
    assertTrue(test.newNumber(new Float(bigFloat)).floatValue() == bigFloat);
    ///test: convert Integer to Float
    assertTrue(test.newNumber(new Integer(bigInteger)).floatValue() == bigInteger);
    ///test: convert Long to Float - see testValidateInputType_Number
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    ///test: null returns null value
    assertTrue(test.newNumber(null).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    ///test: convert Double to Integer - see testValidateInputType_Number
    ///test: convert Float to Integer - see testValidateInputType_Number
    ///test: convert Integer to Integer
    assertTrue(test.newNumber(new Integer(bigInteger)).intValue() == bigInteger);
    ///test: convert Long to Integer - see testValidateInputType_Number
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: null returns null value
    assertTrue(test.newNumber(null).longValue() == EtomoNumber.LONG_NULL_VALUE);
    ///test: convert Double to Long - see testValidateInputType_Number
    ///test: convert Float to Long - see testValidateInputType_Number
    ///test: convert Integer to Long
    assertTrue(test.newNumber(new Integer(bigInteger)).longValue() == bigInteger);
    ///test: convert Long to Long
    assertTrue(test.newNumber(new Long(bigLong)).longValue() == bigLong);
    test.selfTestInvariants();
  }

  public final void testNewNumber_String_StringBuffer() {
    StringBuffer invalidBuffer = new StringBuffer();
    String goodInteger = new Integer(smallInteger).toString();
    String badInteger = "asfdj23";
    EtomoNumber test = new EtomoNumber();
    //test: null returns null value
    assertTrue(test.newNumber(null, invalidBuffer).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(invalidBuffer.length() == 0);
    //test: empty string returns null value
    assertTrue(test.newNumber("", invalidBuffer).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(invalidBuffer.length() == 0);
    //test: string with spaces returns null value
    assertTrue(test.newNumber("         ", invalidBuffer).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(invalidBuffer.length() == 0);
    //test: string with spaces returns null value
    assertTrue(test.newNumber("         ", invalidBuffer).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(invalidBuffer.length() == 0);
    //test: integer returns integer
    assertTrue(test.newNumber(goodInteger, invalidBuffer).intValue() == Integer
        .parseInt(goodInteger));
    assertTrue(invalidBuffer.length() == 0);
    //test: non numeric string fills invalidBuffer and return null value
    assertTrue(test.newNumber(badInteger, invalidBuffer).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(invalidBuffer.length() != 0);
    test.selfTestInvariants();
  }

  public final void testValidateInputType_int() {
    //nothing to do
  }

  public final void testNewNumber_int() {
    //test: validateInputType(int) is called - nothing to do
    //test: convert int to Number
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(smallInteger).doubleValue() == smallInteger);
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    assertTrue(test.newNumber(smallInteger).floatValue() == smallInteger);
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    assertTrue(test.newNumber(smallInteger).intValue() == smallInteger);
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    assertTrue(test.newNumber(smallInteger).longValue() == smallInteger);
    test.selfTestInvariants();
  }

  public final void testNewNumber_boolean() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(true).doubleValue() == 1);
    assertTrue(test.newNumber(false).doubleValue() == 0);
    test.selfTestInvariants();
  }

  public final void testValidateInputType_double() {
  }

  public final void testNewNumber_double() {
    //double
    ///test: convert double to Double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(smallDouble).doubleValue() == smallDouble);
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test:  - validateInputType(double) is being called
    try {
      test.newNumber(smallDouble);
      fail("validateInputType(double) was not called");
    }
    catch (IllegalStateException e) {
    }
    test.selfTestInvariants();
    //integer
    ///test: convert double to Integer - see testValidateInputType_double
    //long
    ///test: convert double to Long - see testValidateInputType_double
  }

  public final void testValidateInputType_long() {
  }

  public final void testNewNumber_long() {
    //double
    ///test: convert long to Double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(smallLong).doubleValue() == smallLong);
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test:  - validateInputType(long) is being called
    try {
      test.newNumber(smallLong);
      fail("validateInputType(long) was not called");
    }
    catch (IllegalStateException e) {
    }
    test.selfTestInvariants();
    //integer
    ///test: convert long to Integer - see testValidateInputType_long
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: convert long to Long
    assertTrue(test.newNumber(smallLong).longValue() == smallLong);
    test.selfTestInvariants();
  }

  public final void testIsNull_Number() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    //test: null
    ///test: NaN is always null
    assertTrue(test.isNull(new Double(Double.NaN)));
    assertTrue(test.isNull(new Double(Float.NaN)));
    assertTrue(test.isNull(new Float(Double.NaN)));
    assertTrue(test.isNull(new Float(Float.NaN)));
    ///test other null values
    assertTrue(test.isNull(new Long(EtomoNumber.LONG_NULL_VALUE)));
    assertTrue(test.isNull(new Integer(EtomoNumber.INTEGER_NULL_VALUE)));
    assertTrue(test.isNull(new Short(Short.MIN_VALUE)));
    assertTrue(test.isNull(new Byte(Byte.MIN_VALUE)));
    //test: other null values are relative to their type
    ///double
    assertFalse(test.isNull(new Double(EtomoNumber.LONG_NULL_VALUE)));
    assertFalse(test.isNull(new Double(EtomoNumber.INTEGER_NULL_VALUE)));
    assertFalse(test.isNull(new Double(Short.MIN_VALUE)));
    assertFalse(test.isNull(new Double(Byte.MIN_VALUE)));
    ///float
    assertFalse(test.isNull(new Float(EtomoNumber.LONG_NULL_VALUE)));
    assertFalse(test.isNull(new Float(EtomoNumber.INTEGER_NULL_VALUE)));
    assertFalse(test.isNull(new Float(Short.MIN_VALUE)));
    assertFalse(test.isNull(new Float(Byte.MIN_VALUE)));
    ///long
    assertFalse(test.isNull(new Long(EtomoNumber.INTEGER_NULL_VALUE)));
    assertFalse(test.isNull(new Long(Short.MIN_VALUE)));
    assertFalse(test.isNull(new Long(Byte.MIN_VALUE)));
    ///integer
    assertFalse(test.isNull(new Integer(Short.MIN_VALUE)));
    assertFalse(test.isNull(new Integer(Byte.MIN_VALUE)));
    ///short
    assertFalse(test.isNull(new Short(Byte.MIN_VALUE)));
    test.selfTestInvariants();
  }

  public final void testIsNull_int() {
    //test: the int null value is always null when its type is int
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.isNull(EtomoNumber.INTEGER_NULL_VALUE));
    test.selfTestInvariants();
    ///float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    assertTrue(test.isNull(EtomoNumber.INTEGER_NULL_VALUE));
    test.selfTestInvariants();
    ///long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    assertTrue(test.isNull(EtomoNumber.INTEGER_NULL_VALUE));
    test.selfTestInvariants();
    ///integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    assertTrue(test.isNull(EtomoNumber.INTEGER_NULL_VALUE));
    test.selfTestInvariants();
  }

  public final void testGt_Number_Number() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    ///test: gt() is equivalent to ">"
    assertTrue(test.gt(new Double(bigDouble), new Double(smallDouble)));
    assertFalse(test.gt(new Double(bigDouble), new Double(bigDouble)));
    assertFalse(test.gt(new Double(smallDouble), new Double(bigDouble)));
    ///test: double type handles float
    assertTrue(test.gt(new Float(bigFloat), new Float(smallFloat)));
    assertFalse(test.gt(new Float(smallFloat), new Float(smallFloat)));
    assertFalse(test.gt(new Float(smallFloat), new Float(bigFloat)));
    ///test: double type handles long
    assertTrue(test.gt(new Long(bigLong), new Long(smallLong)));
    assertFalse(test.gt(new Long(bigLong), new Long(bigLong)));
    assertFalse(test.gt(new Long(smallLong), new Long(bigLong)));
    ///test: double type handle integer
    assertTrue(test.gt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test: validateInputType(Number) is being called against the first
    ////parameter
    try {
      test.gt(new Double(bigDouble), new Float(smallFloat));
      fail("validateInputType(Number) is not being called against the first parameter");
    }
    catch (Exception e) {
    }
    ///test: validateInputType(Number) is being called against the second
    ////parameter
    try {
      test.gt(new Float(bigFloat), new Double(smallDouble));
      fail("validateInputType(Number) is not being called against the second parameter");
    }
    catch (Exception e) {
    }
    ///test: gt() is equivalent to ">"
    assertTrue(test.gt(new Float(bigFloat), new Float(smallFloat)));
    assertFalse(test.gt(new Float(smallFloat), new Float(smallFloat)));
    assertFalse(test.gt(new Float(smallFloat), new Float(bigFloat)));
    ///test: float type handle integer
    assertTrue(test.gt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: gt() is equivalent to ">"
    assertTrue(test.gt(new Long(bigLong), new Long(smallLong)));
    assertFalse(test.gt(new Long(bigLong), new Long(bigLong)));
    assertFalse(test.gt(new Long(smallLong), new Long(bigLong)));
    ///test: long type handle integer
    assertTrue(test.gt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: gt() is equivalent to ">"
    assertTrue(test.gt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
  }

  public final void testLt_Number_Number() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    ///test: lt() is equivalent to "<"
    assertFalse(test.lt(new Double(bigDouble), new Double(smallDouble)));
    assertFalse(test.lt(new Double(bigDouble), new Double(bigDouble)));
    assertTrue(test.lt(new Double(smallDouble), new Double(bigDouble)));
    ///test: double type handles float
    assertFalse(test.lt(new Float(bigFloat), new Float(smallFloat)));
    assertFalse(test.lt(new Float(smallFloat), new Float(smallFloat)));
    assertTrue(test.lt(new Float(smallFloat), new Float(bigFloat)));
    ///test: double type handles long
    assertFalse(test.lt(new Long(bigLong), new Long(smallLong)));
    assertFalse(test.lt(new Long(bigLong), new Long(bigLong)));
    assertTrue(test.lt(new Long(smallLong), new Long(bigLong)));
    ///test: double type handle integer
    assertFalse(test.lt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.lt(new Integer(smallInteger), new Integer(smallInteger)));
    assertTrue(test.lt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test: validateInputType(Number) is being called against the first
    ////parameter
    try {
      test.lt(new Double(bigDouble), new Float(smallFloat));
      fail("validateInputType(Number) is not being called against the first parameter");
    }
    catch (Exception e) {
    }
    ///test: validateInputType(Number) is being called against the second
    ////parameter
    try {
      test.lt(new Float(bigFloat), new Double(smallDouble));
      fail("validateInputType(Number) is not being called against the second parameter");
    }
    catch (Exception e) {
    }
    ///test: lt() is equivalent to "<"
    assertFalse(test.lt(new Float(bigFloat), new Float(smallFloat)));
    assertFalse(test.lt(new Float(smallFloat), new Float(smallFloat)));
    assertTrue(test.lt(new Float(smallFloat), new Float(bigFloat)));
    ///test: float type handle integer
    assertFalse(test.lt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.lt(new Integer(smallInteger), new Integer(smallInteger)));
    assertTrue(test.lt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: lt() is equivalent to "<"
    assertFalse(test.lt(new Long(bigLong), new Long(smallLong)));
    assertFalse(test.lt(new Long(bigLong), new Long(bigLong)));
    assertTrue(test.lt(new Long(smallLong), new Long(bigLong)));
    ///test: long type handle integer
    assertFalse(test.lt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.lt(new Integer(smallInteger), new Integer(smallInteger)));
    assertTrue(test.lt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
    //integer
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: lt() is equivalent to "<"
    assertFalse(test.lt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.lt(new Integer(smallInteger), new Integer(smallInteger)));
    assertTrue(test.lt(new Integer(smallInteger), new Integer(bigInteger)));
    test.selfTestInvariants();
  }

  public final void testEquals_Number_Number() {
  }

  public final void testEquals_Number_int() {
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.7  2005/07/19 21:20:56  sueh
 * <p> bug# 692 fixed testStore_Properties()
 * <p>
 * <p> Revision 1.6  2005/07/18 22:03:18  sueh
 * <p> bug# 692 Added tests for store(Properties).
 * <p>
 * <p> Revision 1.5  2005/06/22 23:36:05  sueh
 * <p> bug# 692 Add empty tests to fill in later.  Test private functions separately.
 * <p>
 * <p> Revision 1.4  2005/06/21 16:34:09  sueh
 * <p> bug# 692 Make constants member variables.  Don't test validate
 * <p> functions twice.  Test to make sure that the validate function is called
 * <p> and then test the fuctionality of the validate function in testValidate...
 * <p> function.
 * <p>
 * <p> Revision 1.3  2005/06/20 16:53:59  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.selfTest() to selfTestInvariants().
 * <p>
 * <p> Revision 1.2  2005/06/17 17:48:47  sueh
 * <p> bug# 692 Added test for isNull(Number).
 * <p>
 * <p> Revision 1.1  2005/06/16 20:00:36  sueh
 * <p> bug# 692 Added unit tests for ConstEtomoNumber.
 * <p> </p>
 */
