package etomo.type;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

import etomo.storage.LogFile;
import etomo.storage.ParameterStore;
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
public final class ConstEtomoNumberTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final double bigDouble = 999999999999999999999999999999999999999.3421D;
  private static final double smallDouble = -999999999999999999999999999999999999999.8793D;
  private static final float bigFloat = 99999999999999999999999999999999999999.903F;
  private static final float smallFloat = -99999999999999999999999999999999999999.0893F;
  private static final long bigLong = 999999999999999999L;
  private static final long smallLong = -999999999999999999L;
  private static final int bigInteger = 999999999;
  private static final int smallInteger = -999999999;
  private static final double easyDouble = 1D;
  private static final float easyFloat = 1F;
  private static final long easyLong = 1L;
  private static final double easyInteger = 1;

  private static final File testDir = new File(TypeTests.TEST_ROOT_DIR,
      "ConstEtomoNumber");
  private static final File propertiesFile = new File(testDir, "properties");

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
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

  public void testConstEtomoNumber() {
    EtomoNumber test = new EtomoNumber();
    assertEquals(test.getType(), EtomoNumber.Type.INTEGER);
    assertTrue(test.getName().equals(test.getDescription()));
    assertTrue(test.isValid());
    test.internalTest();
  }

  public void testInitialize_Number() {
    EtomoNumber test = new EtomoNumber();
    assertEquals(test.getDisplayInteger(), EtomoNumber.INTEGER_NULL_VALUE);
    assertEquals(test.getInt(), EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(test.isValid());
    test.internalTest();
  }

  public void testConstEtomoNumber_String() {
    String name = "name";
    EtomoNumber test = new EtomoNumber(name);
    assertEquals(test.getType(), EtomoNumber.Type.INTEGER);
    assertTrue(name.equals(test.getName()));
    assertTrue(test.getName().equals(test.getDescription()));
    assertTrue(test.isValid());
    test.internalTest();
  }

  public void testConstEtomoNumber_int() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.INTEGER);
    assertEquals(test.getType(), EtomoNumber.Type.INTEGER);
    assertTrue(test.getName().equals(test.getDescription()));
    test.internalTest();
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    assertEquals(test.getType(), EtomoNumber.Type.LONG);
    assertTrue(test.getName().equals(test.getDescription()));
    test.internalTest();
    test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    assertEquals(test.getType(), EtomoNumber.Type.DOUBLE);
    assertTrue(test.getName().equals(test.getDescription()));
    assertTrue(test.isValid());
    test.internalTest();
  }

  public void testConstEtomoNumber_int_String() {
    String name = "name";
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.INTEGER, name);
    assertEquals(test.getType(), EtomoNumber.Type.INTEGER);
    assertTrue(name.equals(test.getName()));
    assertTrue(test.getName().equals(test.getDescription()));
    test.internalTest();
    test = new EtomoNumber(EtomoNumber.Type.LONG, name);
    assertEquals(test.getType(), EtomoNumber.Type.LONG);
    assertTrue(name.equals(test.getName()));
    assertTrue(test.getName().equals(test.getDescription()));
    test.internalTest();
    test = new EtomoNumber(EtomoNumber.Type.DOUBLE, name);
    assertEquals(test.getType(), EtomoNumber.Type.DOUBLE);
    assertTrue(name.equals(test.getName()));
    assertTrue(test.getName().equals(test.getDescription()));
    assertTrue(test.isValid());
    test.internalTest();
  }

  public void testConstEtomoNumber_ConstEtomoNumber() {
    //test null parameter
    EtomoNumber test = null;
    EtomoNumber copy = new EtomoNumber(test);
    assertEquals(copy.getType(), EtomoNumber.Type.INTEGER);
    assertTrue(copy.getName().equals(copy.getDescription()));
    assertTrue(copy.isValid());
    copy.internalTest();
    //make copy
    int floor = 0;
    int validFloor = 1;
    int displayValue = 2;
    int validValue = 3;
    int ceiling = 4;
    int currentValue = 5;
    test = new EtomoNumber("name");
    test.setDescription("description");
    test.set(currentValue);
    test.setDisplayValue(displayValue);
    test.setCeiling(ceiling);
    test.setFloor(floor);
    test.setNullIsValid(false);
    test.setValidFloor(validFloor);
    test.setValidValues(new int[] { displayValue, validValue });
    copy = new EtomoNumber(test);
    //test copy
    assertTrue(test.equals(copy));
    assertEquals(test.getType(), copy.getType());
    assertEquals(test.getName(), copy.getName());
    assertEquals(test.getDescription(), copy.getDescription());
    assertEquals(test.getInt(), copy.getInt());
    assertEquals(test.getDisplayInteger(), copy.getDisplayInteger());
    assertFalse(test.isValid());
    assertFalse(copy.isValid());
    //test deep copy
    assertNotSame(test, copy);
    copy.internalTestDeepCopy(test);
    copy.setDescription("different description");
    assertFalse(test.getDescription().equals(copy.getDescription()));
    ///make copy valid
    copy.set(displayValue);
    assertFalse(test.equals(copy));
    assertFalse(test.isValid());
    assertTrue(copy.isValid());
    ///make test valid and copy invalid
    test.setValidValues(null);
    copy.set(ceiling);
    assertTrue(test.isValid());
    assertFalse(copy.isValid());
    ///make test invalid and copy valid
    test.set(floor);
    copy.setDisplayValue(null);
    copy.reset();
    copy.setNullIsValid(true);
    assertFalse(test.isValid());
    assertTrue(copy.isValid());
    //internal tests
    test.internalTest();
    copy.internalTest();
    int defaultValue = 1;
    test = new EtomoNumber();
    test.setDefault(defaultValue);
    copy = new EtomoNumber(test);
    copy.internalTestDeepCopy(test);
    assertTrue(defaultValue == copy.getDefaultedDouble());
  }

  public void testSetInvalidReason() {
    EtomoNumber test = new EtomoNumber();
    //Pass when there are no validation settings
    test.setInvalidReason();
    assertTrue(test.isValid());
    //Catch illegal null values
    test.setNullIsValid(false);
    assertFalse(test.isValid());
    assertTrue(test.getInvalidReason().indexOf("This field cannot be empty") != -1);
    test.set(1);
    assertTrue(test.isValid());
    assertEquals(test.getInvalidReason().length(), 0);
    //Validate against validValues, overrides validFloor
    test.setValidValues(new int[] { 2, 3 });
    assertFalse(test.isValid());
    test.setValidFloor(1);
    assertFalse(test.isValid());
    assertTrue(test.getInvalidReason().indexOf("Valid values are") != -1);
    //If validValues is not set, validate against validFloor
    test.setValidValues(null);
    assertTrue(test.isValid());
    assertEquals(test.getInvalidReason().length(), 0);
    test.set(0);
    assertFalse(test.isValid());
    assertTrue(test.getInvalidReason().indexOf("Valid values are greater or equal to") != -1);
    test.set(2);
    assertTrue(test.isValid());
    assertEquals(test.getInvalidReason().length(), 0);
    test.internalTest();
  }

  public void testApplyCeilingValue_Number() {
    int lowValue = 2;
    int ceilingValue = 3;
    int highValue = 4;
    EtomoNumber test = new EtomoNumber();
    test.setCeiling(ceilingValue);
    //test (value < ceiling) => value
    assertEquals(test.applyCeilingValue(new Integer(lowValue)).intValue(), lowValue);
    //test (value == ceiling) => value
    assertEquals(test.applyCeilingValue(new Integer(ceilingValue)).intValue(),
        ceilingValue);
    //test (value > ceiling) => ceiling
    assertEquals(test.applyCeilingValue(new Integer(highValue)).intValue(), ceilingValue);
    test.internalTest();
  }

  public void testApplyFloorValue_Number() {
    int lowValue = 2;
    int floorValue = 3;
    int highValue = 4;
    EtomoNumber test = new EtomoNumber();
    test.setFloor(floorValue);
    //test (value > floor) => value
    assertEquals(test.applyFloorValue(new Integer(highValue)).intValue(), highValue);
    //test (value == floor) => value
    assertEquals(test.applyFloorValue(new Integer(floorValue)).intValue(), floorValue);
    //test (value < floor) => floor
    assertEquals(test.applyFloorValue(new Integer(lowValue)).intValue(), floorValue);
    test.internalTest();
  }

  public void testIsValid() {
    EtomoNumber test = new EtomoNumber();
    assertTrue(test.isValid());
    test.setNullIsValid(false);
    assertFalse(test.isValid());
    test.set(1);
    assertTrue(test.isValid());
    test.setNullIsValid(true);
    test.set("nan");
    assertFalse(test.isValid());
    test.set("  ");
    assertTrue(test.isValid());
  }

  public final void testValidate_String_String_AxisID()
      throws InvalidEtomoNumberException {
    String errorTitle = "testValidate_String_String_AxisID";
    EtomoNumber test = new EtomoNumber();
    //test valid
    String errorMessage = test.validate("test valid failed");
    if (errorMessage != null) {
      throw new InvalidEtomoNumberException(errorMessage);
    }
    //test invalid
    test.setNullIsValid(false);
    test.setInvalidReason();
    try {
      errorMessage = test.validate("test invalid succeeded");
      if (errorMessage != null) {
        throw new InvalidEtomoNumberException("test invalid succeeded");
      }
      fail("invalid ConstEomoNumber failed to cause exception to be thrown");
    }
    catch (InvalidEtomoNumberException e) {
    }
    test.internalTest();
  }

  public final void testIsValid_boolean_String_String_AxisID() {
    String errorTitle = "testIsValid_boolean_String_String_AxisID";
    EtomoNumber test = new EtomoNumber();
    //test valid with print
    assertNull(test.validate("test valid failed"));
    //test valid without print
    assertNull(test.validate("test valid failed"));
    //test invalid
    test.setNullIsValid(false);
    test.setInvalidReason();
    ///with print
    assertNotNull(test.validate("test valid succeeded"));
    //without print
    assertNotNull(test.validate("test valid succeeded"));
    test.internalTest();
  }

  public final void testSetCeiling_int() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.LONG);
    //test: validateFloorAndCeiling() was called
    test.setFloor(bigInteger);
    try {
      test.setCeiling(smallInteger);
      fail("validateFloorAndCeiling() was not called");
    }
    catch (IllegalStateException e) {
    }
    //test: setCeiling can modify current value
    test = new EtomoNumber();
    test.set(bigInteger);
    test.setCeiling(smallInteger);
    assertEquals(test.getInt(), smallInteger);
    test.internalTest();
  }

  public final void testSetFloor_int() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    //test: validateFloorAndCeiling() was called
    test.setCeiling(smallInteger);
    try {
      test.setFloor(bigInteger);
      fail("validateFloorAndCeiling() was not called");
    }
    catch (IllegalStateException e) {
    }
    //test: setFloor does not change a larger currentValue
    test = new EtomoNumber();
    test.set(3);
    test.setFloor(-50);
    assertEquals(test.getInt(), 3);
    //test: setFloor does change a smaller currentValue
    test = new EtomoNumber();
    test.set(3);
    test.setFloor(50);
    assertEquals(test.getInt(), 50);
    test.internalTest();
  }

  public void testSetDisplayValue_int() {
    int testValue = 1;
    EtomoNumber test = new EtomoNumber();
    test.setDisplayValue(testValue);
    assertEquals(testValue, test.getDisplayInteger());
    assertEquals(testValue, test.getInt());
    assertTrue(String.valueOf(testValue).equals(test.toString()));
    assertTrue(testValue == test.getDouble());
    assertTrue(testValue == test.getLong());
    assertEquals(testValue, test.getNumber().intValue());
    assertEquals(testValue, test.getValue().intValue());
    test.internalTest();
  }

  public void testSetDisplayValue_boolean() {
    boolean testValue = true;
    int intTestValue = 1;
    EtomoNumber test = new EtomoNumber();
    test.setDisplayValue(testValue);
    assertEquals(intTestValue, test.getDisplayInteger());
    assertEquals(intTestValue, test.getInt());
    assertTrue(String.valueOf(intTestValue).equals(test.toString()));
    assertTrue(intTestValue == test.getDouble());
    assertTrue(intTestValue == test.getLong());
    assertEquals(intTestValue, test.getNumber().intValue());
    assertEquals(intTestValue, test.getValue().intValue());
    test.internalTest();
  }

  public void setDisplayValue_Number() {
    Number testValue = new Integer(1);
    EtomoNumber test = new EtomoNumber();
    test.setDisplayValue(testValue);
    assertEquals(testValue.intValue(), test.getDisplayInteger());
    assertEquals(testValue.intValue(), test.getInt());
    assertTrue(String.valueOf(testValue).equals(test.toString()));
    assertTrue(testValue.intValue() == test.getDouble());
    assertTrue(testValue.intValue() == test.getLong());
    assertEquals(testValue.intValue(), test.getNumber().intValue());
    assertEquals(testValue.intValue(), test.getValue().intValue());
    test.internalTest();
  }

  public void testSetDisplayValue_double() {
    double testValue = 1;
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    test.setDisplayValue(testValue);
    assertTrue(testValue == test.getDouble());
    assertTrue(String.valueOf(testValue).equals(test.toString()));
    assertTrue(testValue == test.getDouble());
    assertTrue(testValue == test.getDouble());
    assertTrue(testValue == test.getNumber().doubleValue());
    assertTrue(testValue == test.getValue().doubleValue());
    test.internalTest();
  }

  public void testSetDisplayValue_long() {
    long testValue = 1;
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.LONG);
    test.setDisplayValue(testValue);
    assertTrue(testValue == test.getLong());
    assertTrue(String.valueOf(testValue).equals(test.toString()));
    assertTrue(testValue == test.getLong());
    assertTrue(testValue == test.getLong());
    assertTrue(testValue == test.getNumber().longValue());
    assertTrue(testValue == test.getValue().longValue());
    test.internalTest();
  }

  public final void testValidateReturnTypeInteger() {
    int displayValue = 2;
    //integer
    EtomoNumber  test = new EtomoNumber(EtomoNumber.Type.INTEGER);
    ///test no exception thrown
    test.getDisplayInteger();
    test.internalTest();
  }

  public final void testApplyFloorValue() {
    int lowValue = 2;
    int floorValue = 3;
    int highValue = 4;
    EtomoNumber test = new EtomoNumber();
    test.setFloor(floorValue);
    //test (value > floor) => value
    assertEquals(test.applyFloorValue(new Integer(highValue)).intValue(), highValue);
    //test (value == floor) => floor
    assertEquals(test.applyFloorValue(new Integer(floorValue)).intValue(), floorValue);
    //test (value < floor) => floor
    assertEquals(test.applyFloorValue(new Integer(lowValue)).intValue(), floorValue);
    test.internalTest();
  }

  public final void testValidateFloorAndCeiling() {
    EtomoNumber test = new EtomoNumber();
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
    test.internalTest();
  }

  public final void testSetNullIsValid_boolean() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.LONG);
    //test: setNullIsValid sets invalidReason
    test.setNullIsValid(false);
    assertFalse(test.isValid());
    test.internalTest();
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
    test.internalTest();
  }

  public final void testSetValidValues_intArray() {
    int validNumber = 3;
    EtomoNumber test = new EtomoNumber();
    test.setValidValues(new int[] { 1, EtomoNumber.INTEGER_NULL_VALUE, validNumber, 5 });
    //test nulls are ignored in valid value
    assertTrue(test.isValid());
    //test valid value
    test.set(validNumber);
    assertTrue(test.isValid());
    //test invalid value
    test.set(7);
    assertFalse(test.isValid());
    test.internalTest();
  }

  public final void testSetValidFloor_int() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    test.set(smallInteger);
    test.setValidFloor(bigInteger);
    assertFalse(test.isValid());
    test.internalTest();
  }

  public final void testStore_Properties() throws LogFile.LockException,
      FileNotFoundException, IOException {
    String name = "TestStore_PropertiesName";
    EtomoNumber test = new EtomoNumber(name);
    test.set(smallInteger);
    ParameterStore properties = ParameterStore.getInstance(propertiesFile);
    //test: no IOException thrown on save
    properties.save(test);
    //test: write parameter to file
    BufferedReader logFileReader = new BufferedReader(new FileReader(propertiesFile));
    String line;
    while ((line = logFileReader.readLine()) != null) {
      if (line.trim().equals(name + "=" + smallInteger)) {
        logFileReader.close();
        return;
      }
    }
    if (logFileReader!=null) {
      logFileReader.close();
    }
    if (logFileReader!=null) {
      logFileReader.close();
    }
    logFileReader.close();
    fail("write parameter to file test failed");
    test.internalTest();
  }

  public final void testStore_Properties_String() {
    String name = "test";
    String prepend = "prepend";
    String value = "42";
    Properties props = new Properties();
    EtomoNumber test = new EtomoNumber(name).set(value);
    test.store(props, prepend);
    assertTrue(props.containsKey(prepend + "." + name));
    assertTrue(props.containsValue(value));
    test.set("");
    test.store(props, prepend);
    assertFalse(props.containsKey(prepend + "." + name));
    assertFalse(props.containsValue(value));
    test.internalTest();
  }

  public final void testStore_EtomoNumber_String_Properties_String() {
    String name = "test";
    String prepend = "prepend";
    String value = "42";
    Properties props = new Properties();
    String key = prepend + '.' + name;
    props.setProperty(key, value);
    assertTrue(props.getProperty(key).equals(value));
    EtomoNumber.store(null, name, props, prepend);
    assertNull(props.getProperty(key));
    EtomoNumber test = new EtomoNumber(name);
    test.set(value);
    EtomoNumber.store(test, name, props, prepend);
    assertTrue(props.getProperty(key).equals(value));
    test.internalTest();
  }

  public final void testRemove_Properties() {
    String name = "test";
    String value = "42";
    Properties props = new Properties();
    props.setProperty(name, value);
    EtomoNumber test = new EtomoNumber(name);
    test.remove(props);
    assertFalse(props.containsKey(name));
    assertFalse(props.containsValue(value));
    test.internalTest();
  }

  public final void testRemove_Properties_prepend() {
    String name = "test";
    String prepend = "prepend";
    String value = "42";
    Properties props = new Properties();
    props.setProperty(name, prepend + "." + value);
    EtomoNumber test = new EtomoNumber(name);
    test.remove(props, prepend);
    assertFalse(props.containsKey(prepend + "." + name));
    assertFalse(props.containsValue(value));
    test.internalTest();
  }

  public final void testToString() {
    String name = "test";
    String prepend = "prepend";
    String value = "42";
    EtomoNumber test = new EtomoNumber(name).set("000" + value);
    assertTrue(test.toString().equals(value));
    test.internalTest();
  }

  public final void testGetInt() {
    //int should succeed
    EtomoNumber test = new EtomoNumber();
    test.set(1);
    test.getInt();
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
    test.internalTest();
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
    test.internalTest();
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
    test.internalTest();
  }

  public final void testGetLong() {
    EtomoNumber test = new EtomoNumber();
    test.getLong();
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    test.getLong();
    test.internalTest();
  }

  public final void testGetDouble() {
    EtomoNumber test = new EtomoNumber();
    test.getDouble();
    test.internalTest();
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    test.getDouble();
    test.internalTest();
    test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    test.getDouble();
    test.internalTest();
  }

  public final void testGetNumber() {
    testGetValue();
    testNewNumber_Number();
  }

  public final void testEquals_Number() {
    Double d = new Double(9);
    EtomoNumber test = new EtomoNumber();
    test.set(9);
    assertTrue("Values should be equal", test.equals(d));
    Integer i = new Integer(9);
    //this is an unfortunate side-effect of storing things in Number
    assertTrue("Values should be equal", test.equals(i));
    test.set(4);
    assertFalse("Values should be unequal", test.equals(i));
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
    test.internalTest();
  }

  public final void toString_Number() {
    Integer nullValue = new Integer(EtomoNumber.INTEGER_NULL_VALUE);
    Integer value = new Integer(3);
    EtomoNumber test = new EtomoNumber();
    //test returns empty string for a null value
    assertTrue(test.toString(nullValue).equals(""));
    //test returns string version of value
    assertTrue(test.toString(value).equals(value.toString()));
    test.internalTest();
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
    assertTrue(invalidReason.indexOf(msg1) != -1 && invalidReason.indexOf(msg2) != -1);
    test.internalTest();
  }

  public final void testNewNumber() {
    //test: returns null value
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    assertTrue(new Double(test.newNumber().doubleValue()).isNaN());
    test.internalTest();
    ///integer
    test = new EtomoNumber(EtomoNumber.Type.INTEGER);
    assertTrue(test.newNumber().intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    test.internalTest();
    ///long
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    assertTrue(test.newNumber().longValue() == EtomoNumber.LONG_NULL_VALUE);
    test.internalTest();
  }

  public final void testNewNumber_Number() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
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
    test.internalTest();
    //integer
    test = new EtomoNumber(EtomoNumber.Type.INTEGER);
    ///test: null returns null value
    assertTrue(test.newNumber(null).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    ///test: convert Double to Integer - see testValidateInputType_Number
    ///test: convert Float to Integer - see testValidateInputType_Number
    ///test: convert Integer to Integer
    assertTrue(test.newNumber(new Integer(bigInteger)).intValue() == bigInteger);
    ///test: convert Long to Integer - see testValidateInputType_Number
    test.internalTest();
    //long
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    ///test: null returns null value
    assertTrue(test.newNumber(null).longValue() == EtomoNumber.LONG_NULL_VALUE);
    ///test: convert Double to Long - see testValidateInputType_Number
    ///test: convert Float to Long - see testValidateInputType_Number
    ///test: convert Integer to Long
    assertTrue(test.newNumber(new Integer(bigInteger)).longValue() == bigInteger);
    ///test: convert Long to Long
    assertTrue(test.newNumber(new Long(bigLong)).longValue() == bigLong);
    test.internalTest();
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
    test.internalTest();
  }

  public final void testValidateInputType_int() {
    //nothing to do
  }

  public final void testNewNumber_int() {
    //test: validateInputType(int) is called - nothing to do
    //test: convert int to Number
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    assertTrue(test.newNumber(smallInteger).doubleValue() == smallInteger);
    test.internalTest();
    //integer
    test = new EtomoNumber(EtomoNumber.Type.INTEGER);
    assertTrue(test.newNumber(smallInteger).intValue() == smallInteger);
    test.internalTest();
    //long
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    assertTrue(test.newNumber(smallInteger).longValue() == smallInteger);
    test.internalTest();
  }

  public final void testNewNumber_boolean() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    assertTrue(test.newNumber(true).doubleValue() == 1);
    assertTrue(test.newNumber(false).doubleValue() == 0);
    test.internalTest();
  }

  public final void testNewNumber_double() {
    //double
    ///test: convert double to Double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    assertTrue(test.newNumber(smallDouble).doubleValue() == smallDouble);
    test.internalTest();
  }

  public final void testNewNumber_long() {
    //double
    ///test: convert long to Double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    assertTrue(test.newNumber(smallLong).doubleValue() == smallLong);
    test.internalTest();
    //integer
    ///test: convert long to Integer - see testValidateInputType_long
    //long
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    ///test: convert long to Long
    assertTrue(test.newNumber(smallLong).longValue() == smallLong);
    test.internalTest();
  }

  public final void testIsNull_Number() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.LONG);
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
    test.internalTest();
  }

  public final void testIsNull() {
    EtomoNumber test = new EtomoNumber();
    assertTrue("A newly created instance should always be null.", test.isNull());
    test.setDisplayValue(0);
    assertTrue("The display value should not affect isNull().", test.isNull());
    test.set(EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue(
        "IsNull() should return true after the NULL_VALUE for the current Type is set.",
        test.isNull());
    test.set(1);
    assertFalse("IsNull() should return false after a non-null number has been set.",
        test.isNull());
    test.reset();
    assertTrue(
        "IsNull() should return true after a reset() is called, even when display value is set.",
        test.isNull());
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    test.set(EtomoNumber.INTEGER_NULL_VALUE);
    assertFalse(
        "IsNull() should return false after a NULL_VALUE for another Type is set.  Otherwise the full range of values would not be valid.",
        test.isNull());
  }

  public final void testIsNull_int() {
    //static isNull() is true only when the parameter is equal to EtomoNumber.INTEGER_NULL_VALUE
    assertTrue(EtomoNumber.isNull(EtomoNumber.INTEGER_NULL_VALUE));
  }

  public final void testGt_Number_Number() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
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
    test.internalTest();
    //long
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    ///test: gt() is equivalent to ">"
    assertTrue(test.gt(new Long(bigLong), new Long(smallLong)));
    assertFalse(test.gt(new Long(bigLong), new Long(bigLong)));
    assertFalse(test.gt(new Long(smallLong), new Long(bigLong)));
    ///test: long type handle integer
    assertTrue(test.gt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(bigInteger)));
    test.internalTest();
    //integer
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    ///test: gt() is equivalent to ">"
    assertTrue(test.gt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(smallInteger)));
    assertFalse(test.gt(new Integer(smallInteger), new Integer(bigInteger)));
    test.internalTest();
  }

  public final void testLt_Number_Number() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.Type.DOUBLE);
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
    test.internalTest();
    //long
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    ///test: lt() is equivalent to "<"
    assertFalse(test.lt(new Long(bigLong), new Long(smallLong)));
    assertFalse(test.lt(new Long(bigLong), new Long(bigLong)));
    assertTrue(test.lt(new Long(smallLong), new Long(bigLong)));
    ///test: long type handle integer
    assertFalse(test.lt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.lt(new Integer(smallInteger), new Integer(smallInteger)));
    assertTrue(test.lt(new Integer(smallInteger), new Integer(bigInteger)));
    test.internalTest();
    //integer
    test = new EtomoNumber(EtomoNumber.Type.LONG);
    ///test: lt() is equivalent to "<"
    assertFalse(test.lt(new Integer(bigInteger), new Integer(smallInteger)));
    assertFalse(test.lt(new Integer(smallInteger), new Integer(smallInteger)));
    assertTrue(test.lt(new Integer(smallInteger), new Integer(bigInteger)));
    test.internalTest();
  }


  public void testSetDefault_int() {
    EtomoNumber test = new EtomoNumber();
    int defaultValue = 1;
    assertTrue("DefaultValue is null when not set.",
        test.getDefaultedDouble() == EtomoNumber.INTEGER_NULL_VALUE);
    test.setDefault(EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue("Setting defaultValue to null is the same as not setting it.", test
        .getDefaultedDouble() == EtomoNumber.INTEGER_NULL_VALUE);
    test.setDefault(defaultValue);
    assertTrue("setDefault sets defaultValue.", test.getDefaultedDouble() == defaultValue);
    test.internalTest();
    test.setDefault(EtomoNumber.INTEGER_NULL_VALUE);
    assertTrue("Setting defaultValue to null is the same as unsetting it.", test
        .getDefaultedDouble() == EtomoNumber.INTEGER_NULL_VALUE);
    test.internalTest();
  }

  public void testIsDefault_Number() {
    EtomoNumber test = new EtomoNumber();
    int defaultValue = 1;
    Number defaultNumber = new Integer(defaultValue);
    assertFalse("Should return false when defaultValue is not set.", test
        .isDefault(defaultNumber));
    test.setDefault(EtomoNumber.INTEGER_NULL_VALUE);
    assertFalse("Null is not a valid value for defaultValue.", test
        .isDefault(new Integer(EtomoNumber.INTEGER_NULL_VALUE)));
    test.setDefault(defaultValue);
    assertFalse("Should only return true when it matches the defaultValue.", test
        .isDefault(new Integer(EtomoNumber.INTEGER_NULL_VALUE)));
    assertFalse("Should only return true when it matches the defaultValue.", test
        .isDefault(new Integer(2)));
    assertTrue("Should return true when it matches the defaultValue.", test
        .isDefault(defaultNumber));
    test.internalTest();
  }

  public void testUseDefaultAsDisplayValue() {
    EtomoNumber test = new EtomoNumber();
    test.useDefaultAsDisplayValue();
    assertEquals("An unset display value is null.", test.getDisplayInteger(),
        EtomoNumber.INTEGER_NULL_VALUE);
    test.useDefaultAsDisplayValue();
    assertEquals(
        "Calling useDefaultAsDisplayValue when defaultValue isn't set has no effect.",
        test.getDisplayInteger(), EtomoNumber.INTEGER_NULL_VALUE);
    int defaultValue = 1;
    test.setDefault(defaultValue);
    assertEquals(
        "Default should not affect the displayValue, unless useDefaultAsDisplay is called.",
        test.getDisplayInteger(), EtomoNumber.INTEGER_NULL_VALUE);
    test.useDefaultAsDisplayValue();
    assertEquals(
        "Calling useDefaultAsDisplayValue when defaultValue is set should change the display value.",
        test.getDisplayInteger(), defaultValue);
  }

  public void testGetDefaultedValue() {
    EtomoNumber test = new EtomoNumber();
    assertEquals(
        "When default value isn't set, the result of getDefaultedValue() should be the same as that of getValue().",
        test.getDefaultedValue().intValue(), test.getValue().intValue());
    test.setDisplayValue(2);
    assertEquals(
        "When default value isn't set, the result of getDefaultedValue() should be the same as that of getValue().",
        test.getDefaultedValue().intValue(), test.getValue().intValue());
    test.set(3);
    assertEquals(
        "When default value isn't set, the result of getDefaultedValue() should be the same as that of getValue().",
        test.getDefaultedValue().intValue(), test.getValue().intValue());
    test = new EtomoNumber();
    int defaultValue = 1;
    test.setDefault(defaultValue);
    assertEquals(
        "When default value is set, the result of getDefaultedValue() should be defaultValue.",
        test.getDefaultedValue().intValue(), defaultValue);
    test.setDisplayValue(2);
    assertEquals(
        "When default value and display value are both set, the result of getDefaultedValue() should be defaultValue.",
        test.getDefaultedValue().intValue(), defaultValue);
    int currentValue = 3;
    test.set(currentValue);
    assertEquals(
        "When current value, default value and display value are all set, the result of getDefaultedValue() should be currentValue.",
        test.getDefaultedValue().intValue(), currentValue);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.51  2010/04/28 16:23:27  sueh
 * <p> bug# 1344 Removed unnecessary tearDown function.
 * <p>
 * <p> Revision 1.50  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.49  2009/09/01 02:33:37  sueh
 * <p> bug# 1222 Fixed comparisons between different types.
 * <p>
 * <p> Revision 1.48  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.47  2009/02/04 23:30:30  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.46  2007/07/30 18:53:44  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.45  2007/05/18 23:53:01  sueh
 * <p> bug# 987 Made isNull(int) static.
 * <p>
 * <p> Revision 1.44  2007/03/26 18:37:39  sueh
 * <p> bug# 964 Changed getDouble(boolean defaultIfNull) to getDefaultDouble() so that
 * <p> the functionality will be remembered and used.  Also changed getValue(boolean).
 * <p>
 * <p> Revision 1.43  2007/02/05 23:09:04  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.42  2006/11/30 17:25:37  sueh
 * <p> bug# 692 Added tests for equals(Number)
 * <p>
 * <p> Revision 1.41  2006/11/15 20:44:47  sueh
 * <p> bug# 872 testStore_Properties:  throw LogFile exceptions because properties is
 * <p> using LogFile.
 * <p>
 * <p> Revision 1.40  2006/10/17 20:07:40  sueh
 * <p> bug# 939  Added testGetDouble_boolean(), testGetValue_boolean(), testIsDefault_Number(), testSetDefault_int(), testUseDefaultAsDisplayValue().
 * <p>
 * <p> Revision 1.39  2006/09/21 16:36:23  sueh
 * <p> bug# 680 Modified testIsValid() to test assumptions in
 * <p> MRCHeader.parsePixelSpacing().
 * <p>
 * <p> Revision 1.38  2006/09/14 22:41:56  sueh
 * <p> bug#  692 Added testSetDisplayValue_long().
 * <p>
 * <p> Revision 1.37  2006/09/14 15:19:01  sueh
 * <p> bug# 692 Fixed testSetDisplayValue_double
 * <p>
 * <p> Revision 1.36  2006/09/13 23:34:04  sueh
 * <p> bug# 692 Added testSetDisplayValue_double().  Bug# 920 added testGetFLoat()
 * <p> and testValidateReturnTypeFloat().
 * <p>
 * <p> Revision 1.35  2006/09/05 17:36:55  sueh
 * <p> bug# 692 Added setDisplayValue_Number
 * <p>
 * <p> Revision 1.34  2006/08/30 19:07:25  sueh
 * <p> bug# 692 Add testSetDisplayValue_boolean
 * <p>
 * <p> Revision 1.33  2006/08/29 22:04:32  sueh
 * <p> bug# 692 test store(EtomoNumber, String, Properties, String)
 * <p>
 * <p> Revision 1.32  2006/08/29 20:05:36  sueh
 * <p> bug# 924 Expanded testSetDisplayValue_int
 * <p>
 * <p> Revision 1.31  2006/08/18 00:16:51  sueh
 * <p> bug# 692
 * <p>
 * <p> Revision 1.30  2006/08/17 20:04:13  sueh
 * <p> bug 692 testing isValid
 * <p>
 * <p> Revision 1.29  2006/06/27 18:31:10  sueh
 * <p> bug# 692 Added testApplyFloorValue_Number
 * <p>
 * <p> Revision 1.28  2006/06/22 21:14:41  sueh
 * <p> bug# 692 Adding to test applyCeilingValue(Number).
 * <p>
 * <p> Revision 1.27  2006/06/21 16:50:37  sueh
 * <p> bug# 692 Modified testSetInvalidReason().
 * <p>
 * <p> Revision 1.26  2006/06/16 17:01:10  sueh
 * <p> bug# #692 Changed testGetDisplayInteger() so that it just calls testValidateReturnType, which reflects what is happening in the tested code and
 * <p> prevents duplicate test code.
 * <p>
 * <p> Revision 1.25  2006/06/09 21:54:01  sueh
 * <p> bug# 692 Reimplemented testConstEtomoNumber_ConstEtomoNumber().
 * <p>
 * <p> Revision 1.24  2006/06/09 17:25:33  sueh
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 1.23  2006/06/08 19:46:45  sueh
 * <p> bug# 692 added tests
 * <p>
 * <p> Revision 1.22  2006/06/07 23:51:53  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.selfTest to internalTest.  Changed
 * <p> ConstEtomoNumber.setTestCopy to internalTestDeepCopy.  Calling internalTest
 * <p> whenever a ConstEtomoNumber instance is created.  Calling
 * <p> internalTestDeepCopy from testConstEtomoNumber_ConstEtomoNumber.
 * <p>
 * <p> Revision 1.21  2006/05/23 21:20:51  sueh
 * <p> bug# 692 Implemented testGetDouble().
 * <p>
 * <p> Revision 1.20  2006/05/22 23:06:45  sueh
 * <p> bug# 692 Implemented getGetLong().
 * <p>
 * <p> Revision 1.19  2006/05/19 22:56:52  sueh
 * <p> bug# 692 Added testGenInt().
 * <p>
 * <p> Revision 1.18  2006/04/11 14:07:05  sueh
 * <p> bug# 692 Tested remove(Properties, String)
 * <p>
 * <p> Revision 1.17  2006/04/10 19:31:51  sueh
 * <p> bug# 692 Tested toString().
 * <p>
 * <p> Revision 1.16  2006/04/06 23:40:21  sueh
 * <p> bug# 692 Tested remove(Properties).
 * <p>
 * <p> Revision 1.15  2006/04/06 22:18:39  sueh
 * <p> bug# 692 Tested store(Properties, String).
 * <p>
 * <p> Revision 1.14  2005/11/10 18:07:18  sueh
 * <p> bug# 758 Placed the root test directory in a File object in JUnitTests.  It is
 * <p> instanciated once so there won't be a problem if the working directory is
 * <p> changed.  Added a root test directory File object to each of the suites,
 * <p> which is based on the JUnitTests root test directory.
 * <p>
 * <p> Revision 1.13  2005/10/27 00:31:54  sueh
 * <p> bug# 725 Added testNewNumber_float().
 * <p>
 * <p> Revision 1.12  2005/07/29 19:46:37  sueh
 * <p> bug# 692 Added tests.  Changed ConstEtomoNumber.getInteger() to
 * <p> getInt.
 * <p>
 * <p> Revision 1.11  2005/07/29 00:53:09  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.10  2005/07/26 23:00:21  sueh
 * <p> bug# 692
 * <p>
 * <p> Revision 1.9  2005/07/21 22:01:11  sueh
 * <p> bug# 532 Added validFloor.  Keepting tests up to date
 * <p>
 * <p> Revision 1.8  2005/07/20 17:53:17  sueh
 * <p> bug# 706 Fix testStore_Properties().  Make all the test directories.
 * <p>
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
