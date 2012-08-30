package etomo.logic;

import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
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
public class FieldValidatorTest extends TestCase {
  public static final String rcsid = "$Id:$";

  // ints and floats
  private static final String INT = " 12 ";
  private static final String FLOAT = " 1.2";
  private static final String EMPTY = " ";
  private static final String INVALID_NUMBER = "abc ";

  // pairs
  private static final String PAIR_EMPTY = " , , , ";
  private static final String INT_PAIR_EMPTY0 = " , 12";
  private static final String INT_PAIR_EMPTY1 = "12 , , ";
  private static final String FLOAT_PAIR_EMPTY1 = " 1.2 , , ";
  private static final String INT_PAIR = " 12,34";
  private static final String FLOAT_PAIR = "1.2,3.4 ";
  private static final String INT_PAIR_INVALID_NUMBER0 = " abc,12 ";
  private static final String FLOAT_PAIR_INVALID_NUMBER1 = " 1.2,abc";

  // triples
  private static final String INT_TRIPLE_EMPTY = " ,,,, ";
  private static final String INT_TRIPLE_EMPTY02 = " ,12,,";
  private static final String INT_TRIPLE_EMPTY1 = "12,,34 ";
  private static final String INT_TRIPLE_EMPTY2 = " 12,34,, ";
  private static final String INT_TRIPLE = " 12,34 -56";
  private static final String FLOAT_TRIPLE = "1.2,3.4, 5.6 ";
  private static final String FLOAT_TRIPLE_INT1 = " 1.2, 34,5.6 ";
  private static final String INT_TRIPLE_EMPTY_INVALID = " ,,12,,";
  private static final String FLOAT_TRIPLE_INVALID_NUMBER0 = " abc,1.2, -3.4";

  // arrays
  private static final String FLOAT_ARRAY = " 1.2, 3.4 5.6,, 78 ";
  private static final String FLOAT_ARRAY_IGNORED_SYNTAX_ERROR = " 1.2 3.4 5.6,,, 7.8,";
  private static final String FLOAT_ARRAY_INVALID_NUMBER = "1.2 abc ,3.4 ";

  // lists
  private static final String INT_LIST = " , ,12-34 56, 78 - -91, 23---45,, , ";
  private static final String INT_LIST_INVALID_FLOAT = " , ,12-34 56, 7.8 - -91, 23---45,, , ";
  private static final String INT_LIST_INVALID_NUMBER = " , ,12-34 56, 78 - -91, 23---abc,, , ";

  // error messages
  private static final String ERR_NO_VALIDATION_EXPECTED = " - text field should not change";
  private static final String ERR_VALIDATION_EXPECTED = " - text field should be trimmed";
  private static final String EXCEPTION_EXPECTED = " - should throw an exception";

  public void testValidateText() {
    String failMessage = null;
    try {
      // String field type
      FieldType fieldType = FieldType.STRING;
      failMessage = "string field type cannot be validated";
      assertEquals(failMessage + ERR_NO_VALIDATION_EXPECTED, INVALID_NUMBER,
          FieldValidator.validateText(INVALID_NUMBER, fieldType, null, null));
      // Integer field type
      fieldType = FieldType.INTEGER;
      failMessage = "null text field cannot be validated";
      assertEquals(failMessage + ERR_NO_VALIDATION_EXPECTED, null,
          FieldValidator.validateText(null, fieldType, null, null));
      failMessage = "valid int";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT.trim(),
          FieldValidator.validateText(INT, fieldType, null, null));
      try {
        FieldValidator.validateText(FLOAT, fieldType, null, null);
        fail("float when expecting an int" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      failMessage = "spaces";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT.trim(),
          FieldValidator.validateText(EMPTY, fieldType, null, null));
      try {
        FieldValidator.validateText(INVALID_NUMBER, fieldType, null, null);
        fail("invalid number" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      try {
        FieldValidator.validateText(INT_PAIR, fieldType, null, null);
        fail("collect which expecting a single number" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      // floating point
      fieldType = FieldType.FLOATING_POINT;
      failMessage = "int is valid in a float";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT.trim(),
          FieldValidator.validateText(INT, fieldType, null, null));
      failMessage = "valid float";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, FLOAT.trim(),
          FieldValidator.validateText(FLOAT, fieldType, null, null));
      failMessage = "spaces";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT.trim(),
          FieldValidator.validateText(EMPTY, fieldType, null, null));
      try {
        FieldValidator.validateText(INVALID_NUMBER, fieldType, null, null);
        fail("invalid number" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      try {
        FieldValidator.validateText(FLOAT_PAIR, fieldType, null, null);
        fail("collect which expecting a single number" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      //int pair
      fieldType = FieldType.INTEGER_PAIR;
      try {
        FieldValidator.validateText(INT, fieldType, null, null);
        fail("Too few numbers" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      failMessage = "empty pair is valid";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, PAIR_EMPTY.trim(),
          FieldValidator.validateText(PAIR_EMPTY, fieldType, null, null));
      failMessage = "valid int pair";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT_PAIR_EMPTY0.trim(),
          FieldValidator.validateText(INT_PAIR_EMPTY0, fieldType, null, null));
      failMessage = "valid int pair";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT_PAIR_EMPTY1.trim(),
          FieldValidator.validateText(INT_PAIR_EMPTY1, fieldType, null, null));
      failMessage = "valid int pair";
      assertEquals(failMessage + ERR_VALIDATION_EXPECTED, INT_PAIR.trim(),
          FieldValidator.validateText(INT_PAIR, fieldType, null, null));
      try {
        FieldValidator.validateText(FLOAT_PAIR, fieldType, null, null);
        fail("float not valid in int pair" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      try {
        FieldValidator.validateText(INT_PAIR_INVALID_NUMBER0, fieldType, null, null);
        fail("int pair with an invalid number" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      try {
        FieldValidator.validateText(INT_TRIPLE_EMPTY02, fieldType, null, null);
        fail("too many elements" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
      try {
        FieldValidator.validateText(INT_TRIPLE, fieldType, null, null);
        fail("too many elements" + EXCEPTION_EXPECTED);
      }
      catch (FieldValidationFailedException e) {
      }
    }
    catch (FieldValidationFailedException e) {
      fail(failMessage);
    }
  }
}
