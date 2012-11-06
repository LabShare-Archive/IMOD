package etomo.logic;

import etomo.EtomoDirector;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.ui.swing.UIComponent;
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
    if (required && (fieldText == null || fieldText.matches("\\s+"))) {
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
      String[] elementList = text.split(fieldType.getSplitter());
      if (fieldType.hasRequiredSize()) {
        // Count elements
        int nElements = 0;
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
        if (elementList != null) {
          nElements += elementList.length;
        }
        // Validate the number of elements
        if (nElements != fieldType.requiredSize) {
          // Wrong number of elements
          UIHarness.INSTANCE.openMessageDialog(component, "The value in " + descr
              + " should have " + fieldType.requiredSize + " elements.", TITLE);
          FieldValidationFailedException fe = new FieldValidationFailedException(descr
              + ":wrong number of elements:" + "fieldText:" + fieldText
              + ",elementList.length:" + elementList.length + ",requiredSize:"
              + fieldType.requiredSize);
          if (EtomoDirector.INSTANCE.getArguments().isDebug()
              || EtomoDirector.INSTANCE.getArguments().isTest()) {
            fe.printStackTrace();
          }
          throw fe;
        }
      }
      // Validate integers or floating point numbers in the array or list.
      if (elementList != null) {
        for (int i = 0; i < elementList.length; i++) {
          validateText(elementList[i], fieldType.validationType, component, descr);
        }
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
}
