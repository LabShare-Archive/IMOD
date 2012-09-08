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

  private static final String EMPTY = "";
  private static final String SPACES = "   ";
  private static final String INVALID_NUMBER = "abc ";

  // int
  private static final String INT = " 12 ";

  // float
  private static final String FLOAT = " 1.2 ";

  // pair
  private static final String PAIR_EMPTY = " , , , ";

  // int pair
  private static final String INT_PAIR_EMPTY0 = " , 12 ";
  private static final String INT_PAIR_EMPTY1 = " 12 , , ";
  private static final String INT_PAIR = " 12,34 ";
  private static final String INT_PAIR_INVALID_NUMBER0 = " abc,12 ";

  // float pair
  private static final String FLOAT_PAIR_EMPTY1 = " 1.2 , , ";
  private static final String FLOAT_PAIR = " 1.2,3.4 ";
  private static final String FLOAT_PAIR_INVALID_NUMBER1 = " 1.2,abc ";

  // triple
  private static final String INT_TRIPLE_EMPTY = " ,,,, ";

  // int triple
  private static final String INT_TRIPLE_EMPTY02 = " ,12,,";
  private static final String INT_TRIPLE_EMPTY1 = "12,,34 ";
  private static final String INT_TRIPLE_EMPTY2 = " 12,34,, ";
  private static final String INT_TRIPLE = " 12,34 -56";
  private static final String INT_TRIPLE_EMPTY_INVALID = " ,,12,,";

  // float triple
  private static final String FLOAT_TRIPLE_EMPTY02 = " ,1.2,,";
  private static final String FLOAT_TRIPLE = "1.2,3.4, 5.6 ";
  private static final String FLOAT_TRIPLE_INT1 = " 1.2, 34,5.6 ";
  private static final String FLOAT_TRIPLE_INVALID_NUMBER0 = " abc,1.2, -3.4";

  // arrray/list
  private static final String FLOAT_ARRAY_EMPTY = " ,,,, , ,,, ,, , ";

  // float array
  private static final String FLOAT_ARRAY = " 1.2, 3.4 5.6,, 78 ";
  private static final String FLOAT_ARRAY_IGNORED_SYNTAX_ERROR = " 1.2 3.4 5.6,,, 7.8,";
  private static final String FLOAT_ARRAY_INVALID_NUMBER = "1.2 abc ,3.4 ";

  // int list
  private static final String INT_LIST = " , ,12-34 56, 78 - -91, 23---45,, ,";
  private static final String INT_LIST_INVALID_FLOAT = ", ,12-34 56, 7.8 - -91, 23---45,, , ";
  private static final String INT_LIST_INVALID_NUMBER = " , ,12-34 56, 78 - -91, 23---abc,, , ";

  // error messages
  private static final String ERR_NO_VALIDATION_EXPECTED = " - text field should not change";
  private static final String ERR_VALIDATION_EXPECTED = " - text field should be trimmed";
  private static final String EXCEPTION_EXPECTED = " - should throw an exception";

  public void testValidateTextString() {
    FieldType fieldType = FieldType.STRING;
    String msg = "no validation for string field";
    testValidateText(null, fieldType, true, null, msg);
    testValidateText(EMPTY, fieldType, true, EMPTY, msg);
    testValidateText(SPACES, fieldType, true, SPACES, msg);
    testValidateText(INVALID_NUMBER, fieldType, true, INVALID_NUMBER, msg);
    testValidateText(INT, fieldType, true, INT, msg);
  }

  public void testValidateTextInteger() {
    FieldType fieldType = FieldType.INTEGER;
    testValidateText(null, fieldType, true, null, "no validation for null string");
    testValidateText(EMPTY, fieldType, true, EMPTY, "no validation for empty string");
    testValidateText(SPACES, fieldType, true, SPACES.trim(),
        "no validation for empty string");
    testValidateText(INVALID_NUMBER, fieldType, false, null, "invalid number");
    testValidateText(INT, fieldType, true, INT.trim(), "valid int");
    testValidateText(FLOAT, fieldType, false, null, "float not compatible with int");
    testValidateText(INT_PAIR, fieldType, false, null,
        "pair not compatible with single int");
  }

  public void testValidateTextFloatingPoint() {
    FieldType fieldType = FieldType.FLOATING_POINT;
    testValidateText(null, fieldType, true, null, "no validation for null string");
    testValidateText(EMPTY, fieldType, true, EMPTY, "no validation for empty string");
    testValidateText(SPACES, fieldType, true, SPACES.trim(),
        "no validation for empty string");
    testValidateText(INVALID_NUMBER, fieldType, false, null, "invalid number");
    testValidateText(INT, fieldType, true, INT.trim(), "int compatible with float");
    testValidateText(FLOAT, fieldType, true, FLOAT.trim(), "valid float");
    testValidateText(FLOAT_PAIR, fieldType, false, null,
        "pair not compatible with single float");
  }

  public void testValidateTextIntegerPair() {
    FieldType fieldType = FieldType.INTEGER_PAIR;
    testValidateText(null, fieldType, true, null, "no validation for null string");
    testValidateText(EMPTY, fieldType, true, EMPTY, "no validation for empty string");
    testValidateText(SPACES, fieldType, true, SPACES.trim(),
        "no validation for empty string");
    testValidateText(INT, fieldType, false, null, "must have two elements");
    testValidateText(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), "valid pair");
    testValidateText(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0.trim(),
        "valid int pair");
    testValidateText(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1.trim(),
        "valid int pair");
    testValidateText(INT_PAIR, fieldType, true, INT_PAIR.trim(), "valid int pair");
    testValidateText(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, "invalid number");
    testValidateText(FLOAT_PAIR, fieldType, false, null, "float not compatible with int");
    testValidateText(INT_TRIPLE, fieldType, false, null,
        "triple not compatible with pair");
  }

  public void testValidateTextFloatingPointPair() {
    FieldType fieldType = FieldType.FLOATING_POINT_PAIR;
    testValidateText(null, fieldType, true, null, "no validation for null string");
    testValidateText(EMPTY, fieldType, true, EMPTY, "no validation for empty string");
    testValidateText(SPACES, fieldType, true, SPACES.trim(),
        "no validation for empty string");
    testValidateText(FLOAT, fieldType, false, null, "must have two elements");
    testValidateText(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), "valid pair");
    testValidateText(INT_PAIR, fieldType, true, INT_PAIR.trim(),
        "int is compatible with float");
    testValidateText(FLOAT_PAIR_EMPTY1, fieldType, true, FLOAT_PAIR_EMPTY1.trim(),
        "valid float pair");
    testValidateText(FLOAT_PAIR, fieldType, true, FLOAT_PAIR.trim(), "valid float pair");
    testValidateText(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, "invalid number");
    testValidateText(FLOAT_TRIPLE, fieldType, false, null,
        "triple not compatible with pair");
  }

  private void testValidateText(final String testString, final FieldType fieldType,
      final boolean shouldSucceed, final String outputString, final String msg) {
    try {
      if (shouldSucceed) {
        assertEquals(msg, outputString,
            FieldValidator.validateText(testString, fieldType, null, null));
      }
      else {
        FieldValidator.validateText(testString, fieldType, null, null);
      }
    }
    catch (FieldValidationFailedException e) {
      if (shouldSucceed) {
        fail(msg);
      }
    }
  }
}
