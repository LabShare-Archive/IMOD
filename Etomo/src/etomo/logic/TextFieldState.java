package etomo.logic;

import java.io.File;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.ParsedElementType;
import etomo.util.FilePath;

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
public final class TextFieldState {
  public static final String rcsid = "$Id:$";

  private final boolean editableField;
  private final ParsedElementType parsedElementType;
  private final String rootDir;

  private boolean expanded = true;
  private File parent = null;

  public TextFieldState(final boolean editableField,
      final ParsedElementType parsedElementType, final String rootDir) {
    this.editableField = editableField;
    this.parsedElementType = parsedElementType;
    this.rootDir = rootDir;
  }

  public TextFieldState(final TextFieldState textFieldState) {
    this.editableField = textFieldState.editableField;
    this.parsedElementType = textFieldState.parsedElementType;
    this.rootDir = textFieldState.rootDir;
    this.expanded = textFieldState.expanded;
    this.parent = textFieldState.parent;
  }

  public boolean isEditableField() {
    return editableField;
  }

  /**
   * Applies the expand state to the text and returns it.
   * @param expand
   * @param text
   * @return
   */
  public String expandFieldText(final boolean expand, final String text) {
    if (expanded == expand) {
      return text;
    }
    expanded = expand;
    return applyExpandedToFieldText(text);
  }

  /**
   * When expand is true, return the file with its path.  When expand is false, return the
   * file name.  The only change that needs to be handled is if a path was added to the
   * text while it was contracted.  Paths entered by hand are not modified.  Paths entered
   * using the button are made relative to the rootDir.
   * @param expand
   */
  public String applyExpandedToFieldText(final String text) {
    if (!expanded) {
      // Contract
      // Save the path when contracting.
      parent = FilePath.getFileParent(text);
      // Set the contracted form of the text
      return FilePath.getFileName(text);
    }
    // Expand
    if (!FilePath.isPath(text) && parent != null) {
      // The text has changed, but is doesn't include a path, so replace the old file
      // name with the new one in the path.
      return new File(parent, text).getPath();
    }
    // Nothing is saved while the field is expanded.
    parent = null;
    return text;
  }

  public void msgResettingFieldText() {
    parent = null;
  }

  /**
   * Sets the absolute path of the file if the root directory is empty, otherwise sets the
   * relative path from the root directory to the file.
   * @param file
   * @return
   */
  public String convertToFieldText(final File file) {
    if (rootDir == null) {
      return convertToFieldText(file.getAbsolutePath());
    }
    return convertToFieldText(FilePath.getRelativePath(rootDir, file));
  }

  /**
   * Returns string, except when sring is a file path and the state is
   * contracted. In that case it returns the string's file name.  When the
   * instance is contracted the strings parent path is stored in the parent member
   * variable.
   * @param value
   */
  public String convertToFieldText(final String string) {
    if (expanded || !FilePath.isPath(string)) {
      // Set value as is, unless the field is contracted and its a file path.
      parent = null;
      return string;
    }
    // Set a contracted file path.
    parent = new File(string).getParentFile();
    return FilePath.getFileName(string);
  }

  public String convertRangeToFieldText(int start, int end) {
    return convertToFieldText(new Integer(start).toString() + " - "
        + new Integer(end).toString());
  }

  public String convertToContractedString(final String text) {
    if (!expanded) {
      return text;
    }
    return FilePath.getFileName(text);
  }

  /**
   * Return text.  Or if text is contracted and is not a path, and the parent
   * exists, return the parent plus the text.
   * @return
   */
  public String convertToExpandedString(final String text) {
    if (text == null || text.matches("\\s*")) {
      parent = null;
      return "";
    }
    if (expanded || parent == null) {
      return text;
    }
    if (FilePath.isPath(text)) {
      return text;
    }
    return new File(parent, text).getPath();
  }

  /**
   * Parse and return the second number in an "n - m" string.  Return the null value from
   * EtomoNumber if the format is wrong.
   * @return
   */
  public int extractEndValue(String text) {
    if (text == null) {
      return EtomoNumber.INTEGER_NULL_VALUE;
    }
    text = text.trim();
    if (text.length() <= 1 || !text.matches("\\S+\\s*-\\s*\\S+")) {
      return EtomoNumber.INTEGER_NULL_VALUE;
    }
    EtomoNumber endValue = new EtomoNumber();
    // Avoid getting the "-" from a negative number by searching from the second
    // character.
    endValue.set(text.substring(text.indexOf('-', 1) + 1));
    // Returns null if the string set was not a valid integer.
    return endValue.getInt();
  }

  public ConstEtomoNumber convertToEtomoNumber(final String text) {
    EtomoNumber number = new EtomoNumber();
    number.set(text);
    return number;
  }

  public ConstEtomoNumber convertToEtomoNumber(EtomoNumber.Type type, final String text) {
    EtomoNumber number = new EtomoNumber(type);
    number.set(text);
    return number;
  }

  public int convertToInt(final String text) {
    try {
      return new Integer(text).intValue();
    }
    catch (NumberFormatException e) {
      return 0;
    }
  }
}
