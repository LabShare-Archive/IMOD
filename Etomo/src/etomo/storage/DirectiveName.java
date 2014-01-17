package etomo.storage;

import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;

/**
* <p>Description: Handles the left side of a directive set.
* Directive set:
* - 1 directive with no axisID information or
* - 3 directives: A, B, and both axes.
* This class can return a key, which is identical to the directive name for both axes.  It
* can also return a directive name for a specific axisID.</p>
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
  private static final int RUNTIME_AXIS_INDEX = 2;

  private String[] key = null;
  private DirectiveType type = null;

  public String toString() {
    StringBuffer buffer = new StringBuffer();
    buffer.append("[key:");
    if (key != null) {
      for (int i = 0; i < key.length; i++) {
        buffer.append(key[i] + " ");
      }
    }
    return buffer + ",type:" + type + "]";
  }

  public DirectiveName() {
  }

  static boolean equals(final String key, final DirectiveType input) {
    if (key == null || input == null) {
      return false;
    }
    return key.startsWith(input.toString() + AutodocTokenizer.SEPARATOR_CHAR);
  }

  boolean equals(final DirectiveType input) {
    if (isNull()) {
      return false;
    }
    return type == input;
  }

  /**
   * @return the name with no axis ID
   */
  public String getKey() {
    if (isNull()) {
      return null;
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < key.length; i++) {
      buffer.append((i > 0 ? "." : "") + key[i]);
    }
    return buffer.toString();
  }

  public String getKeyDescription() {
    String key = getKey();
    if (type == DirectiveType.RUNTIME) {
      return key.replace(AutodocTokenizer.SEPARATOR_CHAR + DirectiveFile.ANY_AXIS_NAME,
          "");
    }
    return key;
  }

  public String getComFileName() {
    if (isNull()) {
      return null;
    }
    // Only comparam directives have a comfile name. Missing comfile name.
    if (type != DirectiveType.COM_PARAM || key.length <= COM_FILE_NAME_INDEX) {
      return null;
    }
    return key[COM_FILE_NAME_INDEX];
  }

  public String getParameterName() {
    if (isNull()) {
      return null;
    }
    if (type == DirectiveType.SETUP_SET) {
      if (key.length < 2) {
        return null;
      }
      boolean copyarg = key[1].equals(DirectiveFile.COPY_ARG_NAME);
      if (copyarg) {
        if (key.length > 2) {
          return key[2];
        }
      }
      else {
        return key[1];
      }
    }
    else if ((type == DirectiveType.COM_PARAM || type == DirectiveType.RUNTIME)
        && key.length > PARAMETER_NAME_INDEX) {
      return key[PARAMETER_NAME_INDEX];
    }
    return null;
  }

  public String getProgramName() {
    if (isNull()) {
      return null;
    }
    // Only comparam directives have a program name. Missing program name.
    if (type != DirectiveType.COM_PARAM || key.length <= PROGRAM_INDEX) {
      return null;
    }
    return key[PROGRAM_INDEX];
  }

  public String getTitle() {
    if (isNull()) {
      return null;
    }
    if (type == DirectiveType.COM_PARAM) {
      return getComFileName() + "." + getParameterName();
    }
    else {
      return getParameterName();
    }
  }

  public DirectiveType getType() {
    return type;
  }

  boolean isCopyArg() {
    return type == DirectiveType.SETUP_SET && key.length > 1 && key[1].equals("copyarg");
  }

  public boolean isValid() {
    if (isNull()) {
      return false;
    }
    return type != null && key.length > 1;
  }

  /**
   * Copies the data rather then pointers to mutable objects in the parameter.
   * @param directiveName
   */
  void deepCopy(final DirectiveName directiveName) {
    if (directiveName.key == null) {
      key = null;
    }
    else {
      key = new String[directiveName.key.length];
      for (int i = 0; i < key.length; i++) {
        key[i] = directiveName.key[i];
      }
    }
    type = directiveName.type;
  }

  void setKey(DirectiveDescr descr) {
    // The a and b axes are not included for comparam and runtime directives in the
    // directive.csv file, so no need to remove them.
    splitKey(descr.getName());
  }

  private void splitKey(String input) {
    key = null;
    type = null;
    if (input != null && !input.matches("\\s*")) {
      key = input.split("\\" + AutodocTokenizer.SEPARATOR_CHAR);
      if (key != null && key.length > TYPE_INDEX) {
        type = DirectiveType.getInstance(key[TYPE_INDEX]);
      }
    }
  }

  /**
   * Strips axis information and saves a key containing the "any" form of the directive
   * name.  For a directive with no axis information or an "any" directive name, the key
   * is the same as the input string, and null is returned.
   * @param input
   * @return the axisID that was removed from the name (or null for "any").
   */
  public AxisID setKey(String input) {
    splitKey(input);
    if (type != DirectiveType.COM_PARAM && type != DirectiveType.RUNTIME) {
      return null;
    }
    // Remove axisID from the name to create a key. Standardize the key to the Any form of
    // the directive name, and return the axisID that was found.
    AxisID axisID = null;
    for (int i = 0; i < key.length; i++) {
      if (type == DirectiveType.COM_PARAM
          && i == COM_FILE_NAME_INDEX
          && key[i] != null
          && (key[i].endsWith(AxisID.FIRST.getExtension()) || key[i]
              .endsWith(AxisID.SECOND.getExtension()))) {
        if (key[i].endsWith(AxisID.FIRST.getExtension())) {
          axisID = AxisID.FIRST;
        }
        else {
          axisID = AxisID.SECOND;
        }
        // Strip off the a or b
        key[i] = key[i].substring(0, key[i].length() - 1);
      }
      else if (type == DirectiveType.RUNTIME
          && i == RUNTIME_AXIS_INDEX
          && key[i] != null
          && (key[i].equals(AxisID.FIRST.getExtension()) || key[i].equals(AxisID.SECOND
              .getExtension()))) {
        if (key[i].equals(AxisID.FIRST.getExtension())) {
          axisID = AxisID.FIRST;
        }
        else {
          axisID = AxisID.SECOND;
        }
        // Replace with "any".
        key[i] = DirectiveFile.ANY_AXIS_NAME;
      }
    }
    return axisID;
  }

  private boolean isNull() {
    return key == null || key.length == 0;
  }

  private static boolean mayContainAxisID(final String name) {
    return name != null
        && (name.startsWith(DirectiveType.RUNTIME.toString()
            + AutodocTokenizer.SEPARATOR_CHAR) || name.startsWith(DirectiveType.COM_PARAM
            .toString() + AutodocTokenizer.SEPARATOR_CHAR))
        && (name.indexOf(AxisID.FIRST.getExtension() + AutodocTokenizer.SEPARATOR_CHAR) != -1 || name
            .indexOf(AxisID.SECOND.getExtension() + AutodocTokenizer.SEPARATOR_CHAR) != -1);
  }

  /**
   * Returns the directive name for the axisID specified.
   * @param axisID
   * @return
   */
  public String getName() {
    if (key == null) {
      return null;
    }
    // Set ext to the correct form
    String ext = "";
    if (type == DirectiveType.RUNTIME) {
      ext = DirectiveFile.ANY_AXIS_NAME;
    }
    // Create a string version of the directive name with the correct axisID string
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < key.length; i++) {
      if (type == DirectiveType.COM_PARAM && i == COM_FILE_NAME_INDEX) {
        buffer.append((i > 0 ? AutodocTokenizer.SEPARATOR_CHAR : "") + key[i] + ext);
      }
      else if (type == DirectiveType.RUNTIME && i == RUNTIME_AXIS_INDEX) {
        buffer.append((i > 0 ? AutodocTokenizer.SEPARATOR_CHAR : "") + ext);
      }
      else {
        buffer.append((i > 0 ? AutodocTokenizer.SEPARATOR_CHAR : "") + key[i]);
      }
    }
    return buffer.toString();
  }
}
