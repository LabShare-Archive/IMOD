package etomo.type;

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
  public static  final String  rcsid =  "$Id$";

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
  }
  
  final public void testConstEtomoNumber() {
    EtomoNumber test = new EtomoNumber();
    test.selfTest();
  }
  
  final public void testConstEtomoNumber_String() {
    String name = "ConstEtomoNumberTest";
    EtomoNumber test = new EtomoNumber(name);
    test.selfTest();
    //test name
    assertTrue(name.equals(test.getName()));
    //test description
    assertTrue(name.equals(test.getDescription()));
  }
  
  final public void testConstEtomoNumber_int() {
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    test.selfTest();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    test.selfTest();
  }
  
  final public void testConstEtomoNumber_int_String() {
    //double
    String name = "ConstEtomoNumberTest";
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, name);
    test.selfTest();
    ///test: name
    assertTrue("name test failed: name=" + name + ",test.getName()="
        + test.getName(), name.equals(test.getName()));
    ///test: description
    assertTrue(name.equals(test.getDescription()));
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE, name);
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE, name);
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE, name);
    test.selfTest();
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
    test.selfTest();
    copy.selfTest();
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
    test.setValidValues(new int[] {2,3});
    test.setInvalidReason();
    assertFalse(test.isValid());
    test.selfTest();
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
    test.selfTest();
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
    test.selfTest();
  }
  
  public final void testValidate_String_String_AxisID() throws InvalidEtomoNumberException {
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
    test.selfTest();
  }
  
  public final void testIsValid_boolean_String_String_AxisID() {
    String errorTitle = "testIsValid_boolean_String_String_AxisID";
    EtomoNumber test = new EtomoNumber();
    //test valid with print
    assertTrue(test.isValid(true, errorTitle, "test valid failed", AxisID.FIRST));
    //test valid without print
    assertTrue(test.isValid(false, errorTitle, "test valid failed", AxisID.FIRST));
    //test invalid
    test.setNullIsValid(false);
    test.setInvalidReason();
    ///with print
    assertFalse(test.isValid(true, errorTitle, "test valid succeeded", AxisID.FIRST));
    //without print
    assertFalse(test.isValid(false, errorTitle, "test valid succeeded", AxisID.FIRST));
    test.selfTest();
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
    test.selfTest();
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
    test.selfTest();
  }

  public final void testSetValidValues_intArray() {
    int validNumber = 3;
    EtomoNumber test = new EtomoNumber();
    test.setValidValues(new int[] { 1, EtomoNumber.INTEGER_NULL_VALUE, validNumber, 5 });
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
    test.selfTest();
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
    test.selfTest();
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
    test.selfTest();
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
    test.selfTest();
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
    test.selfTest();
  }
  
  public final void toString_Number() {
    Integer nullValue = new Integer(EtomoNumber.INTEGER_NULL_VALUE);
    Integer value = new Integer(3);
    EtomoNumber test = new EtomoNumber();
    //test returns empty string for a null value
    assertTrue(test.toString(nullValue).equals(""));
    //test returns string version of value
    assertTrue(test.toString(value).equals(value.toString()));
    test.selfTest();
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
    test.selfTest();
  }
  
  public final void testNewNumber() {
    //test: returns null value
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(new Double(test.newNumber().doubleValue()).isNaN());
    test.selfTest();
    ///float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    assertTrue(new Float(test.newNumber().floatValue()).isNaN());
    test.selfTest();
    ///integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    assertTrue(test.newNumber().intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    test.selfTest();
    ///long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    assertTrue(test.newNumber().longValue() == EtomoNumber.LONG_NULL_VALUE);
    test.selfTest();
  }
  
  public final void testNewNumber_Number() {
    double doubleValue = 999999999999999999999999999999999999999.99D;
    float floatValue = 99999999999999999999999999999999999999.99F;
    int integerValue = 999999999;
    long longValue = 999999999999999999L;
    //double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    ///test: null returns null value
    assertTrue(new Double(test.newNumber(null).doubleValue()).isNaN());
    ///test: convert Double to Double
    assertTrue(test.newNumber(new Double(doubleValue)).doubleValue() == doubleValue);
    ///test: convert Float to Double
    assertTrue(test.newNumber(new Float(floatValue)).doubleValue() == floatValue);
    ///test: convert Integer to Double
    assertTrue(test.newNumber(new Integer(integerValue)).doubleValue() == integerValue);
    ///test: convert Long to Double
    assertTrue(test.newNumber(new Long(longValue)).doubleValue() == longValue);
    test.selfTest();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test: null returns null value
    assertTrue(new Float(test.newNumber(null).floatValue()).isNaN());
    ///test: convert Double to Float should cause exception
    try {
      test.newNumber(new Double(doubleValue));
      fail("convert Double to Float should cause exception");
    }
    catch (IllegalStateException e) {
    }
    ///test: convert Float to Float
    assertTrue(test.newNumber(new Float(floatValue)).floatValue() == floatValue);
    ///test: convert Integer to Float
    assertTrue(test.newNumber(new Integer(integerValue)).floatValue() == integerValue);
    ///test: convert Long to Float should cause exception
    try {
      test.newNumber(new Long(longValue));
      fail("convert Long to Float should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    ///test: null returns null value
    assertTrue(test.newNumber(null).intValue() == EtomoNumber.INTEGER_NULL_VALUE);
    ///test: convert Double to Integer should cause exception
    try {
      test.newNumber(new Double(doubleValue));
      fail("convert Double to Integer should cause exception");
    }
    catch (IllegalStateException e) {
    }
    ///test: convert Float to Integer should cause exception
    try {
      test.newNumber(new Float(floatValue));
      fail("convert Float to Integer should cause exception");
    }
    catch (IllegalStateException e) {
    }
    ///test: convert Integer to Integer
    assertTrue(test.newNumber(new Integer(integerValue)).intValue() == integerValue);
    ///test: convert Long to Integer should cause exception
    try {
      test.newNumber(new Long(longValue));
      fail("convert Long to Integer should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: null returns null value
    assertTrue(test.newNumber(null).longValue() == EtomoNumber.LONG_NULL_VALUE);
    ///test: convert Double to Long should cause exception
    try {
      test.newNumber(new Double(doubleValue));
      fail("convert Double to Long should cause exception");
    }
    catch (IllegalStateException e) {
    }
    ///test: convert Float to Long should cause exception
    try {
      test.newNumber(new Float(floatValue));
      fail("convert Float to Long should cause exception");
    }
    catch (IllegalStateException e) {
    }
    ///test: convert Integer to Long
    assertTrue(test.newNumber(new Integer(integerValue)).longValue() == integerValue);
    ///test: convert Long to Long
    assertTrue(test.newNumber(new Long(longValue)).longValue() == longValue);
    test.selfTest();
  }
  
  public final void testNewNumber_String_StringBuffer() {
    StringBuffer invalidBuffer = new StringBuffer();
    String goodInteger = "-999999999";
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
    test.selfTest();
  }
  
  public final void testNewNumber_int() {
    int integerValue = -999999999;
    //test: convert int to Number
    ///double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(integerValue).doubleValue() == integerValue);
    test.selfTest();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    assertTrue(test.newNumber(integerValue).floatValue() == integerValue);
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    assertTrue(test.newNumber(integerValue).intValue() == integerValue);
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    assertTrue(test.newNumber(integerValue).longValue() == integerValue);
    test.selfTest();
  }
  
  public final void newNumber_boolean() {
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(true).doubleValue() == 1);
    assertTrue(test.newNumber(false).doubleValue() == 0);
    test.selfTest();
  }
  
  public final void newNumber_double() {
    double doubleValue = -999999999999999999999999999999999999999.99D;
    //double
    ///test: convert double to Double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(doubleValue).doubleValue() == doubleValue);
    test.selfTest();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test: convert double to Float should cause exception
    try {
      test.newNumber(doubleValue);
      fail("convert double to Float should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    ///test: convert double to Integer should cause exception
    try {
      test.newNumber(doubleValue);
      fail("convert double to Integer should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: convert double to Long should cause exception
    try {
      test.newNumber(doubleValue);
      fail("convert double to Long should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
  }

  public final void newNumber_long() {
    long longValue = -999999999999999999L;
    //double
    ///test: convert long to Double
    EtomoNumber test = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    assertTrue(test.newNumber(longValue).doubleValue() == longValue);
    test.selfTest();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test: convert long to Float should cause exception
    try {
      test.newNumber(longValue);
      fail("convert long to Float should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    ///test: convert long to Integer should cause exception
    try {
      test.newNumber(longValue);
      fail("convert long to Integer should cause exception");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test: convert long to Long
    assertTrue(test.newNumber(longValue).longValue() == longValue);
    test.selfTest();
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
    test.selfTest();
    //float
    test = new EtomoNumber(EtomoNumber.FLOAT_TYPE);
    ///test corruption prevention
    try {
      test.getDisplayInteger();
      fail("A float can't be returned in an integer");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
    //integer
    test = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    ///test no exception thrown
    test.getDisplayInteger();
    test.selfTest();
    //long
    test = new EtomoNumber(EtomoNumber.LONG_TYPE);
    ///test corruption prevention
    try {
      test.getDisplayInteger();
      fail("A long can't be returned in an integer");
    }
    catch (IllegalStateException e) {
    }
    test.selfTest();
  }

}
/**
* <p> $Log$ </p>
*/
