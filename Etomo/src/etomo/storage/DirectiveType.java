package etomo.storage;

import etomo.storage.autodoc.AutodocTokenizer;

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
public final class DirectiveType {
  public static final String rcsid = "$Id:$";

  public static final DirectiveType SETUP_SET = new DirectiveType(
      DirectiveFile.SETUP_SET_NAME);
  public static final DirectiveType RUNTIME = new DirectiveType(
      DirectiveFile.RUNTIME_NAME);
  public static final DirectiveType COM_PARAM = new DirectiveType("comparam");

  private final String tag;

  private DirectiveType(final String tag) {
    this.tag = tag;
  }

  /**
   * @param input - directive name or first part of the name
   * @return the instance matching the first section of input
   */
  static DirectiveType getInstance(String input) {
    if (input == null) {
      return null;
    }
    if (input.indexOf('.') != -1) {
      String[] array = input.split("\\" + AutodocTokenizer.SEPARATOR_CHAR);
      if (array == null || array.length < 1 || array[0] == null) {
        return null;
      }
      input = array[0];
    }
    input = input.trim();
    if (SETUP_SET.equals(input)) {
      return SETUP_SET;
    }
    if (RUNTIME.equals(input)) {
      return RUNTIME;
    }
    if (COM_PARAM.equals(input)) {
      return COM_PARAM;
    }
    return null;
  }

  /**
   * @param input - directive name or first part of the name
   * @return true if this instance matches the first section of input
   */
  public boolean equals(String input) {
    if (input == null) {
      return false;
    }
    if (input.indexOf('.') != -1) {
      String[] array = input.split("\\" + AutodocTokenizer.SEPARATOR_CHAR);
      if (array == null || array.length < 1 || array[0] == null) {
        return false;
      }
      input = array[0];
    }
    input = input.trim();
    return input.equals(this.tag);
  }

  public String toString() {
    return tag;
  }
}