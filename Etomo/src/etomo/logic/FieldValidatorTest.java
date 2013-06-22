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
  private static final String TRIPLE_EMPTY = " ,,,, ";

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

  // arrray
  private static final String ARRAY_EMPTY = " ,,,, , ,,, ,, , ";

  // float array
  private static final String FLOAT_ARRAY = " 1.2, 3.4 5.6,, 78 ";
  private static final String FLOAT_ARRAY_IGNORED_SYNTAX_ERROR = " 1.2 3.4 5.6,,, 7.8,";
  private static final String FLOAT_ARRAY_INVALID_NUMBER = "1.2 abc ,3.4 ";

  // int array
  private static final String INT_ARRAY = " 12, 34 56,, 78 ";
  private static final String INT_ARRAY_IGNORED_SYNTAX_ERROR = " 12 34 56,,, 78,";
  private static final String INT_ARRAY_INVALID_FLOAT = " 12, 3.4 56,, 78 ";
  private static final String INT_ARRAY_INVALID_NUMBER = "12 abc ,34 ";

  // list
  private static final String LIST_EMPTY = " ,-,,---, , ,-,,- ,, ,- ";

  // int list
  private static final String INT_LIST = " , ,12-34 56, 78 - -91, 23---45,, ,";
  private static final String INT_LIST_INVALID_FLOAT = ", ,12-34 56, 7.8 - -91, 23---45,, , ";
  private static final String INT_LIST_INVALID_NUMBER = " , ,12-34 56, 78 - -91, 23---abc,, , ";

  // messages
  private static final String NULL_STRING_MSG = "no validation for null string";
  private static final String EMPTY_STRING_MSG = "no validation for empty string";
  private static final String INVALID_NUMBER_MSG = "invalid number";
  private static final String SINGLE_NUMBER_MSG = "array/list not compatible with single number";
  private static final String FLOAT_NOT_COMPATIBLE_MSG = "float not compatible with int";
  private static final String INT_COMPATIBLE_MSG = "int compatible with float";
  private static final String VALID_PAIR_MSG = "valid pair";
  private static final String PAIR_ELEMENTS_MSG = "pair must have exactly two elements";
  private static final String VALID_TRIPLE_MSG = "valid triple";
  private static final String TRIPLE_ELEMENTS_MSG = "triple must have exactly three elements";
  private static final String VALID_ARRAY_MSG = "valid array";
  private static final String IGNORED_SYNTAX_ERROR_MSG = "syntax error ignored";
  private static final String ARRAY_SYNTAX_MSG = "arrays do not take dashes";
  private static final String VALID_LIST_MSG = "valid list";
  private static final String LIST_SYNTAX_MSG = "too many dashes";
  private static final String REQUIRED_EMPTY_MSG = "required field cannot be empty";
  private static final String REQUIRED_EMPTY_WHITESPACE_MSG = "required field cannot be empty - whitespace only is considered empty";

  public void testValidateString() {
    FieldType fieldType = FieldType.STRING;
    String msg = "no validation(except for required) for string field - no trimming done";
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(INVALID_NUMBER, fieldType, true, INVALID_NUMBER, msg);
    testValidate(null, fieldType, true, null, msg);
    testValidate(EMPTY, fieldType, true, EMPTY, msg);
    testValidate(SPACES, fieldType, true, SPACES, msg);
    testValidate(INVALID_NUMBER, fieldType, true, INVALID_NUMBER, msg);
    testValidate(INT, fieldType, true, INT, msg);
    testValidate(FLOAT, fieldType, true, FLOAT, msg);
    testValidate(PAIR_EMPTY, fieldType, true, PAIR_EMPTY, msg);
    testValidate(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0, msg);
    testValidate(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1, msg);
    testValidate(INT_PAIR, fieldType, true, INT_PAIR, msg);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, true, INT_PAIR_INVALID_NUMBER0, msg);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, true, FLOAT_PAIR_EMPTY1, msg);
    testValidate(FLOAT_PAIR, fieldType, true, FLOAT_PAIR, msg);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, true, FLOAT_PAIR_INVALID_NUMBER1,
        msg);
    testValidate(TRIPLE_EMPTY, fieldType, true, TRIPLE_EMPTY, msg);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, true, INT_TRIPLE_EMPTY02, msg);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, true, INT_TRIPLE_EMPTY1, msg);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, true, INT_TRIPLE_EMPTY2, msg);
    testValidate(INT_TRIPLE, fieldType, true, INT_TRIPLE, msg);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, true, INT_TRIPLE_EMPTY_INVALID, msg);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, true, FLOAT_TRIPLE_EMPTY02, msg);
    testValidate(FLOAT_TRIPLE, fieldType, true, FLOAT_TRIPLE, msg);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, true, FLOAT_TRIPLE_INT1, msg);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, true,
        FLOAT_TRIPLE_INVALID_NUMBER0, msg);
    testValidate(ARRAY_EMPTY, fieldType, true, ARRAY_EMPTY, msg);
    testValidate(FLOAT_ARRAY, fieldType, true, FLOAT_ARRAY, msg);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, true,
        FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, msg);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, true, FLOAT_ARRAY_INVALID_NUMBER,
        msg);
    testValidate(INT_ARRAY, fieldType, true, INT_ARRAY, msg);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, true,
        INT_ARRAY_IGNORED_SYNTAX_ERROR, msg);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, true, INT_ARRAY_INVALID_FLOAT, msg);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, true, INT_ARRAY_INVALID_NUMBER, msg);
    testValidate(LIST_EMPTY, fieldType, true, LIST_EMPTY, msg);
    testValidate(INT_LIST, fieldType, true, INT_LIST, msg);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, true, INT_LIST_INVALID_FLOAT, msg);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, true, INT_LIST_INVALID_NUMBER, msg);
  }

  public void testValidateInteger() {
    FieldType fieldType = FieldType.INTEGER;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, EMPTY, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, SPACES, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(INT, fieldType, true, INT.trim(), "valid int");
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INVALID_NUMBER, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(INT, fieldType, true, INT.trim(), "valid int");
    testValidate(FLOAT, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(PAIR_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_PAIR, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(ARRAY_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        SINGLE_NUMBER_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_LIST, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, SINGLE_NUMBER_MSG);
  }

  public void testValidateFloatingPoint() {
    FieldType fieldType = FieldType.FLOATING_POINT;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(INT, fieldType, true, INT.trim(), INT_COMPATIBLE_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INVALID_NUMBER, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(INT, fieldType, true, INT.trim(), INT_COMPATIBLE_MSG);
    testValidate(FLOAT, fieldType, true, FLOAT.trim(), "valid float");
    testValidate(PAIR_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_PAIR, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(ARRAY_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        SINGLE_NUMBER_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_LIST, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, SINGLE_NUMBER_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, SINGLE_NUMBER_MSG);
  }

  public void testValidateIntegerPair() {
    FieldType fieldType = FieldType.INTEGER_PAIR;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_PAIR_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR, fieldType, true, INT_PAIR.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(ARRAY_EMPTY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_LIST, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, PAIR_ELEMENTS_MSG);
  }

  public void testValidateFloatingPointPair() {
    FieldType fieldType = FieldType.FLOATING_POINT_PAIR;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_PAIR_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR, fieldType, true, INT_PAIR.trim(), INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, true, FLOAT_PAIR_EMPTY1.trim(),
        VALID_PAIR_MSG);
    testValidate(FLOAT_PAIR, fieldType, true, FLOAT_PAIR.trim(), VALID_PAIR_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(ARRAY_EMPTY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        PAIR_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_LIST, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, PAIR_ELEMENTS_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, PAIR_ELEMENTS_MSG);
  }

  // TEMP 1614
  public void testValidateIntegerTriple() {
    FieldType fieldType = FieldType.INTEGER_TRIPLE;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(TRIPLE_EMPTY, fieldType, true, TRIPLE_EMPTY.trim(),
        VALID_TRIPLE_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INT, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(FLOAT, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(PAIR_EMPTY, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_PAIR, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, true, TRIPLE_EMPTY.trim(), VALID_TRIPLE_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, true, INT_TRIPLE_EMPTY02.trim(),
        VALID_TRIPLE_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, true, INT_TRIPLE_EMPTY1.trim(),
        VALID_TRIPLE_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, true, INT_TRIPLE_EMPTY2.trim(),
        VALID_TRIPLE_MSG);
    testValidate(INT_TRIPLE, fieldType, true, INT_TRIPLE.trim(), VALID_TRIPLE_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(ARRAY_EMPTY, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        TRIPLE_ELEMENTS_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_ARRAY, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        TRIPLE_ELEMENTS_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_LIST, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, TRIPLE_ELEMENTS_MSG);
  }

  public void testValidateIntegerArray() {
    FieldType fieldType = FieldType.INTEGER_ARRAY;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(INT, fieldType, true, INT.trim(), VALID_ARRAY_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INT, fieldType, true, INT.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR, fieldType, true, INT_PAIR.trim(), INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null,
        FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, true, TRIPLE_EMPTY.trim(), VALID_ARRAY_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, true, INT_TRIPLE_EMPTY02.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, true, INT_TRIPLE_EMPTY1.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, true, INT_TRIPLE_EMPTY2.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE, fieldType, true, INT_TRIPLE.trim(), INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null,
        FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(ARRAY_EMPTY, fieldType, true, ARRAY_EMPTY.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false, null,
        FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null,
        FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(INT_ARRAY, fieldType, true, INT_ARRAY.trim(), VALID_ARRAY_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, true,
        INT_ARRAY_IGNORED_SYNTAX_ERROR.trim(), IGNORED_SYNTAX_ERROR_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false, null,
        FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, ARRAY_SYNTAX_MSG);
    testValidate(INT_LIST, fieldType, false, null, ARRAY_SYNTAX_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, ARRAY_SYNTAX_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, ARRAY_SYNTAX_MSG);
  }

  public void testValidateFloatingPointArray() {
    FieldType fieldType = FieldType.FLOATING_POINT_ARRAY;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(INT, fieldType, true, INT.trim(), VALID_ARRAY_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INT, fieldType, true, INT.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT, fieldType, true, FLOAT.trim(), VALID_ARRAY_MSG);
    testValidate(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_PAIR_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR, fieldType, true, INT_PAIR.trim(), INT_COMPATIBLE_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, true, FLOAT_PAIR_EMPTY1.trim(),
        VALID_ARRAY_MSG);
    testValidate(FLOAT_PAIR, fieldType, true, FLOAT_PAIR.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, true, TRIPLE_EMPTY.trim(), VALID_ARRAY_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, true, INT_TRIPLE_EMPTY02.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, true, INT_TRIPLE_EMPTY1.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, true, INT_TRIPLE_EMPTY2.trim(),
        INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE, fieldType, true, INT_TRIPLE.trim(), INT_COMPATIBLE_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, true, FLOAT_TRIPLE_EMPTY02.trim(),
        VALID_ARRAY_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, true, FLOAT_TRIPLE.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, true, FLOAT_TRIPLE_INT1.trim(),
        VALID_ARRAY_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(ARRAY_EMPTY, fieldType, true, ARRAY_EMPTY.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT_ARRAY, fieldType, true, FLOAT_ARRAY.trim(), VALID_ARRAY_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, true,
        FLOAT_ARRAY_IGNORED_SYNTAX_ERROR.trim(), IGNORED_SYNTAX_ERROR_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(INT_ARRAY, fieldType, true, INT_ARRAY.trim(), VALID_ARRAY_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, true,
        INT_ARRAY_IGNORED_SYNTAX_ERROR.trim(), IGNORED_SYNTAX_ERROR_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, true,
        INT_ARRAY_INVALID_FLOAT.trim(), VALID_ARRAY_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(LIST_EMPTY, fieldType, false, null, ARRAY_SYNTAX_MSG);
    testValidate(INT_LIST, fieldType, false, null, ARRAY_SYNTAX_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, ARRAY_SYNTAX_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, ARRAY_SYNTAX_MSG);
  }

  public void testValidateIntegerList() {
    FieldType fieldType = FieldType.INTEGER_LIST;
    testValidateRequired(null, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(EMPTY, fieldType, false, null, REQUIRED_EMPTY_MSG);
    testValidateRequired(SPACES, fieldType, false, null, REQUIRED_EMPTY_WHITESPACE_MSG);
    testValidateRequired(INT, fieldType, true, INT.trim(), VALID_ARRAY_MSG);
    testValidate(null, fieldType, true, null, NULL_STRING_MSG);
    testValidate(EMPTY, fieldType, true, EMPTY, EMPTY_STRING_MSG);
    testValidate(SPACES, fieldType, true, SPACES.trim(), EMPTY_STRING_MSG);
    testValidate(INT, fieldType, true, INT.trim(), VALID_LIST_MSG);
    testValidate(FLOAT, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(PAIR_EMPTY, fieldType, true, PAIR_EMPTY.trim(), VALID_LIST_MSG);
    testValidate(INT_PAIR_EMPTY0, fieldType, true, INT_PAIR_EMPTY0.trim(), VALID_LIST_MSG);
    testValidate(INT_PAIR_EMPTY1, fieldType, true, INT_PAIR_EMPTY1.trim(), VALID_LIST_MSG);
    testValidate(INT_PAIR, fieldType, true, INT_PAIR.trim(), VALID_LIST_MSG);
    testValidate(INT_PAIR_INVALID_NUMBER0, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(FLOAT_PAIR_EMPTY1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_PAIR_INVALID_NUMBER1, fieldType, false, null,
        FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(TRIPLE_EMPTY, fieldType, true, TRIPLE_EMPTY.trim(), VALID_LIST_MSG);
    testValidate(INT_TRIPLE_EMPTY02, fieldType, true, INT_TRIPLE_EMPTY02.trim(),
        VALID_LIST_MSG);
    testValidate(INT_TRIPLE_EMPTY1, fieldType, true, INT_TRIPLE_EMPTY1.trim(),
        VALID_LIST_MSG);
    testValidate(INT_TRIPLE_EMPTY2, fieldType, true, INT_TRIPLE_EMPTY2.trim(),
        VALID_LIST_MSG);
    testValidate(INT_TRIPLE, fieldType, true, INT_TRIPLE.trim(), VALID_LIST_MSG);
    testValidate(INT_TRIPLE_EMPTY_INVALID, fieldType, true,
        INT_TRIPLE_EMPTY_INVALID.trim(), VALID_LIST_MSG);
    testValidate(FLOAT_TRIPLE_EMPTY02, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE_INT1, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_TRIPLE_INVALID_NUMBER0, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(ARRAY_EMPTY, fieldType, true, ARRAY_EMPTY.trim(), VALID_LIST_MSG);
    testValidate(FLOAT_ARRAY, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, false,
     null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(FLOAT_ARRAY_INVALID_NUMBER, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(INT_ARRAY, fieldType, true, INT_ARRAY.trim(), VALID_LIST_MSG);
    testValidate(INT_ARRAY_IGNORED_SYNTAX_ERROR, fieldType, true,
        INT_ARRAY_IGNORED_SYNTAX_ERROR.trim(), IGNORED_SYNTAX_ERROR_MSG);
    testValidate(INT_ARRAY_INVALID_FLOAT, fieldType, false,
      null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(INT_ARRAY_INVALID_NUMBER, fieldType, false, null, INVALID_NUMBER_MSG);
    testValidate(LIST_EMPTY, fieldType, true, LIST_EMPTY.trim(), VALID_LIST_MSG);
    testValidate(INT_LIST, fieldType, true, INT_LIST.trim(), VALID_LIST_MSG);
    testValidate(INT_LIST_INVALID_FLOAT, fieldType, false, null, FLOAT_NOT_COMPATIBLE_MSG);
    testValidate(INT_LIST_INVALID_NUMBER, fieldType, false, null, LIST_SYNTAX_MSG);
  }

  private void testValidate(final String testString, final FieldType fieldType,
      final boolean shouldSucceed, final String outputString, final String msg) {
    try {
      if (shouldSucceed) {
        assertEquals(msg, outputString,
            FieldValidator.validateText(testString, fieldType, null, null, false));
      }
      else {
        FieldValidator.validateText(testString, fieldType, null, null, false);
      }
    }
    catch (FieldValidationFailedException e) {
      if (shouldSucceed) {
        fail(msg);
      }
    }
  }

  private void testValidateRequired(final String testString, final FieldType fieldType,
      final boolean shouldSucceed, final String outputString, final String msg) {
    try {
      if (shouldSucceed) {
        assertEquals(msg, outputString,
            FieldValidator.validateText(testString, fieldType, null, null, true));
      }
      else {
        FieldValidator.validateText(testString, fieldType, null, null, true);
      }
    }
    catch (FieldValidationFailedException e) {
      if (shouldSucceed) {
        fail(msg);
      }
    }
  }
}
