package etomo.storage;

import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class DirectiveName {
  public static final String rcsid = "$Id:$";

  private static final int TYPE_INDEX = 0;
  private static final int COM_FILE_NAME_INDEX = 1;
  private static final int PROGRAM_INDEX = 2;
  private static final int PARAMETER_NAME_INDEX = 3;

  private String[] array = null;
  private boolean initialized = false;
  private String name = null;
  private DirectiveType type = null;

  public DirectiveName() {
  }

  static boolean equals(final String name, final DirectiveType input) {
    if (name == null || input == null) {
      return false;
    }
    return name.startsWith(input.toString() + AutodocTokenizer.SEPARATOR_CHAR);
  }

  static String removeAxisID(final String name) {
    if (mayContainAxisID(name)) {
      String[] array = name.split("\\" + AutodocTokenizer.SEPARATOR_CHAR);
      if (array != null && array.length > 0) {
        return removeAxisID(name, array, DirectiveType.getInstance(array[0]));
      }
    }
    return name;
  }

  boolean equals(final DirectiveType input) {
    if (initialized) {
      return type == input;
    }
    return equals(name, input);
  }

  public DirectiveType getType() {
    if (isNull()) {
      return null;
    }
    init();
    return type;
  }

  public String getComFileName() {
    if (isNull()) {
      return null;
    }
    init();
    // Only comparam directives have a comfile name. Missing comfile name.
    if (type != DirectiveType.COM_PARAM || array == null
        || array.length <= COM_FILE_NAME_INDEX) {
      return null;
    }
    return array[COM_FILE_NAME_INDEX];
  }

  public String getProgramName() {
    if (isNull()) {
      return null;
    }
    init();
    // Only comparam directives have a program name. Missing program name.
    if (type != DirectiveType.COM_PARAM || array == null || array.length <= PROGRAM_INDEX) {
      return null;
    }
    return array[PROGRAM_INDEX];
  }

  public String getParameterName() {
    if (isNull()) {
      return null;
    }
    init();
    // Only comparam directives have a parameter name. Missing parameter name.
    if (type != DirectiveType.COM_PARAM || array == null
        || array.length <= PARAMETER_NAME_INDEX) {
      return null;
    }
    return array[PARAMETER_NAME_INDEX];
  }

  public String get() {
    return name;
  }

  void set(DirectivesDescriptionFile.DirectiveDescription descr) {
    // The a and b axes are not included for comparam and runtime directives in the
    // directive.csv file, so not need to remove them.
    name = descr.getName();
    initialized = false;
    array = null;
    type = null;
  }

  public void set(String input) {
    name = input;
    initialized = false;
    array = null;
    type = null;
    removeAxisID();
  }

  private void init() {
    if (!initialized) {
      initialized = true;
      if (name != null) {
        array = name.split("\\" + AutodocTokenizer.SEPARATOR_CHAR);
        if (array != null && array.length > TYPE_INDEX) {
          type = DirectiveType.getInstance(array[TYPE_INDEX]);
        }
      }
    }
  }

  private boolean isNull() {
    return name == null || name.matches("\\s*");
  }

  private static boolean mayContainAxisID(final String name) {
    return name != null
        && (name.startsWith(DirectiveType.RUNTIME.toString()
            + AutodocTokenizer.SEPARATOR_CHAR) || name.startsWith(DirectiveType.COM_PARAM
            .toString() + AutodocTokenizer.SEPARATOR_CHAR))
        && (name.indexOf(AxisID.FIRST.getExtension() + AutodocTokenizer.SEPARATOR_CHAR) != -1 || name
            .indexOf(AxisID.SECOND.getExtension() + AutodocTokenizer.SEPARATOR_CHAR) != -1);
  }

  private void removeAxisID() {
    if (mayContainAxisID(name)) {
      init();
      name = removeAxisID(name, array, type);
      // array contains the old version of the name
      initialized = false;
      array = null;
    }
  }

  /**
  * Convert name into a directive name for both axes.
  * @param name - must not be empty, should contain "a." or "b."
  * @param array - must be from name.split
  * @param type - the type found in array[0]
  * @return
  */
  private static String removeAxisID(final String name, final String[] array,
      final DirectiveType type) {
    if (type == null || array == null) {
      return name;
    }
    int index;
    if (type == DirectiveType.COM_PARAM) {
      index = COM_FILE_NAME_INDEX;
      if (array.length > index
          && array[index] != null
          && (array[index].endsWith(AxisID.FIRST.getExtension()) || array[index]
              .endsWith(AxisID.SECOND.getExtension()))) {
        // Remove the a or b at the end of the com file name
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < array.length; i++) {
          if (i == index) {
            buffer.append(AutodocTokenizer.SEPARATOR_CHAR
                + array[index].substring(0, array[index].length() - 1));
          }
          else {
            buffer.append((i == 0 ? "" : AutodocTokenizer.SEPARATOR_CHAR) + array[i]);
          }
        }
        return buffer.toString();
      }
    }
    else if (type == DirectiveType.RUNTIME) {
      index = 2;
      if (array.length > index
          && array[index] != null
          && (array[index].equals(AxisID.FIRST.getExtension()) || array[index]
              .equals(AxisID.SECOND.getExtension()))) {
        // Replace .a. or .b. with .any.
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < array.length; i++) {
          if (i == index) {

            buffer.append(AutodocTokenizer.SEPARATOR_CHAR + DirectiveFile.ANY_AXIS_NAME);
          }
          else {
            buffer.append((i == 0 ? "" : AutodocTokenizer.SEPARATOR_CHAR) + array[i]);
          }
        }
        return buffer.toString();
      }
    }
    return name;
  }
}
