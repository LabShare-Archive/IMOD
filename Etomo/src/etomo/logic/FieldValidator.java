package etomo.logic;

import java.io.File;

import etomo.EtomoDirector;
import etomo.type.EtomoNumber;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.ui.UIComponent;
import etomo.ui.swing.UIHarness;

/**
* <p>Description: Validator for numeric text fields.</p>
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
public final class FieldValidator {
  public static final String rcsid = "$Id:$";

  private static final String TITLE = "Field Validation Failed";

  /**
   * Validates field text based on field type.  Pops up an error message and throws an
   * exception if the validation fails.  Does number format validation on individual
   * numbers and on arrays and lists.  Validates the number of elements in pairs and
   * triples.  Does minimal validatation of the syntax of arrays and lists.  If required
   * is true, validation fails when the field is empty or contains nothing but whitespace.
   * @param fieldText
   * @param fieldType
   * @param component
   * @param descr
   * @param required
   * @return fieldText, trimmed if validation is possible on fieldType
   * @throws FieldValidationFailedException if the validation fails
   */
  public static String validateText(final String fieldText, final FieldType fieldType,
      final UIComponent component, final String descr, final boolean required)
      throws FieldValidationFailedException {
    if (required && (fieldText == null || fieldText.matches("\\s*"))) {
      UIHarness.INSTANCE.openMessageDialog(component, descr + " is a required field.",
          TITLE);
      FieldValidationFailedException fe = new FieldValidationFailedException(descr
          + ":required field:" + "fieldText:" + fieldText);
      if (EtomoDirector.INSTANCE.getArguments().isDebug()
          || EtomoDirector.INSTANCE.getArguments().isTest()) {
        fe.printStackTrace();
      }
      throw fe;

    }
    if (!fieldType.validationType.canValidate || fieldText == null) {
      return fieldText;
    }
    // Trim because Number.parse... will fail on external spaces.
    String text = fieldText.trim();
    if (fieldType.isCollection()) {
      // Empty fields are valid
      if (text.equals("")) {
        return text;
      }
      // Validate arrays and lists.
      ElementList elementList = new ElementList(fieldType, text);
      if (fieldType.hasRequiredSize()) {
        // Validate the number of elements
        if (!elementList.equalsNElements(fieldType.requiredSize)) {
          // Wrong number of elements
          UIHarness.INSTANCE.openMessageDialog(component, "The value in " + descr
              + " should have " + fieldType.requiredSize + " elements.", TITLE);
          FieldValidationFailedException fe = new FieldValidationFailedException(descr
              + ":wrong number of elements:" + "fieldText:" + fieldText + ",nElements:"
              + elementList.getNElements() + ",requiredSize:" + fieldType.requiredSize);
          if (EtomoDirector.INSTANCE.getArguments().isDebug()
              || EtomoDirector.INSTANCE.getArguments().isTest()) {
            fe.printStackTrace();
          }
          throw fe;
        }
      }
      // Validate integers or floating point numbers in the array or list.
      ElementListIterator iterator = elementList.iterator();
      while (iterator.hasNext()) {
        validateText(iterator.next(), fieldType.validationType, component, descr);
      }
    }
    else {
      // Validate integers and floating point numbers.
      validateText(text, fieldType.validationType, component, descr);
    }
    // Validation succeeded - return original trimmed field text.
    return fieldText.trim();
  }

  /**
   * Validate text based on validation type.  Pops up an error message and throws an
   * exception if the validation fails.  Does number format validation on individual
   * numbers
   * @param text
   * @param validationType
   * @param component
   * @param descr
   * @throws FieldValidationFailedException if the validation fails
   */
  private static void validateText(final String text,
      final FieldType.ValidationType validationType, final UIComponent component,
      final String descr) throws FieldValidationFailedException {
    // Empty fields are valid. External spaces should already have been removed
    if (text.equals("")) {
      return;
    }
    try {
      if (validationType == FieldType.ValidationType.INTEGER) {
        Integer.parseInt(text);
      }
      else if (validationType == FieldType.ValidationType.FLOATING_POINT) {
        Double.parseDouble(text);
      }
    }
    catch (NumberFormatException e) {
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        e.printStackTrace();
      }
      UIHarness.INSTANCE.openMessageDialog(component, "The value in " + descr
          + " is not " + validationType + ".\n" + e.getMessage(), TITLE);
      FieldValidationFailedException fe = new FieldValidationFailedException(descr
          + ":text" + text + ",validationType+:" + validationType);
      if (EtomoDirector.INSTANCE.getArguments().isDebug()
          || EtomoDirector.INSTANCE.getArguments().isTest()) {
        fe.printStackTrace();
      }
      throw fe;
    }
  }

  public static boolean equals(final FieldType fieldType, String fieldText1,
      String fieldText2) {
    if (fieldText1 == null || fieldText2 == null) {
      if (fieldText1 == null && fieldText2 == null) {
        return true;
      }
      return false;
    }
    if (fieldType == FieldType.INTEGER) {
      EtomoNumber number1 = new EtomoNumber();
      EtomoNumber number2 = new EtomoNumber();
      number1.set(fieldText1);
      number2.set(fieldText2);
      return number1.equals(number2);
    }
    if (fieldType == FieldType.FLOATING_POINT) {
      EtomoNumber number1 = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      EtomoNumber number2 = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      number1.set(fieldText1);
      number2.set(fieldText2);
      return number1.equals(number2);
    }
    if (fieldType == null || fieldType == FieldType.STRING) {
      return fieldText1.trim().equals(fieldText2.trim());
    }
    if (fieldType == FieldType.FILE) {
      return new File(fieldText1).equals(new File(fieldText2));
    }
    if (fieldType == FieldType.INTEGER_LIST) {
      // Lists are too complicated to parse (they contain things like "1 - 3"). Remove all
      // spaces and compare as strings.
      return fieldText1.replaceAll("\\s+", "").equals(fieldText2.replaceAll("\\s+", ""));
    }
    // Handle arrays
    FieldType numberFieldType = null;
    if (fieldType == FieldType.INTEGER_ARRAY || fieldType == FieldType.INTEGER_PAIR
        || fieldType == FieldType.INTEGER_TRIPLE) {
      numberFieldType = FieldType.INTEGER;
    }
    if (fieldType == FieldType.FLOATING_POINT_ARRAY
        || fieldType == FieldType.FLOATING_POINT_PAIR) {
      numberFieldType = FieldType.FLOATING_POINT;
    }
    ElementList elementList1 = new ElementList(fieldType, fieldText1);
    ElementList elementList2 = new ElementList(fieldType, fieldText2);
    if (elementList1.getNElements() != elementList2.getNElements()) {
      return false;
    }
    ElementListIterator iterator1 = elementList1.iterator();
    ElementListIterator iterator2 = elementList2.iterator();
    while (iterator1.hasNext()) {
      // Call this function with the field type for either float or integer.
      if (!equals(numberFieldType, iterator1.next(), iterator2.next())) {
        return false;
      }
    }
    return true;
  }

  private static final class ElementList {
    private final FieldType fieldType;
    private final String fieldText;

    private String[] list = null;
    private int nElements = -1;

    private ElementList(final FieldType fieldType, final String fieldText) {
      this.fieldType = fieldType;
      this.fieldText = fieldText;
    }

    private boolean equalsNElements(final int input) {
      return getNElements() == input;
    }

    private ElementListIterator iterator() {
      if (list == null) {
        split();
      }
      return new ElementListIterator(list);
    }

    private int getNElements() {
      if (list == null) {
        split();
      }
      if (nElements == -1) {
        countElements();
      }
      return nElements;
    }

    private void split() {
      list = fieldText.split(fieldType.getSplitter());
    }

    private void countElements() {
      nElements = 0;
      if (fieldText == null) {
        return;
      }
      String text = fieldText;
      // The commas at the end of the collection count as elements. Splitting will
      // eliminate these commas, so they have to be counted.
      if (text.endsWith(",")) {
        // Remove all whitespace
        text = text.replaceAll("\\s+", "");
        // Count ending commas. The last comma doesn't count because an extra comma is
        // necessary to add an element onto the end.
        for (int i = text.length() - 2; i >= 0; i--) {
          if (text.charAt(i) == ',') {
            nElements++;
          }
          else {
            break;
          }
        }
      }
      if (list != null) {
        nElements += list.length;
      }
    }
  }

  private static final class ElementListIterator {
    private final String[] list;

    private int index = 0;

    private ElementListIterator(final String[] list) {
      this.list = list;
    }

    private boolean hasNext() {
      if (list == null || index < 0 || index >= list.length) {
        return false;
      }
      return true;
    }

    private String next() {
      if (list == null || index < 0 || index >= list.length) {
        return null;
      }
      return list[index++];
    }
  }
}
