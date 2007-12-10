package etomo.type;

import java.util.Properties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * @notthreadsafe
 * 
 * <p> $Log$
 * <p> Revision 1.2  2007/05/16 22:59:31  sueh
 * <p> bug# 964 Added set(StringProperty).
 * <p>
 * <p> Revision 1.1  2007/04/09 21:12:14  sueh
 * <p> bug# 964 A class to make storing, loading, and removing strings from Properties
 * <p> easier.
 * <p> </p>
 */
final class StringProperty {
  public static final String rcsid = "$Id$";

  private final String key;

  private String string = "";

  StringProperty(final String key) {
    this.key = key;
  }

  public String toString() {
    return string;
  }

  /**
   * sets this.string to string.  If string is null, empty, or contains only whitespace,
   * sets this.string to "".
   * @param string
   */
  void set(final String input) {
    if (input == null || input.matches("\\s*")) {
      reset();
    }
    else {
      string = input;
    }
  }

  void set(final StringProperty input) {
    string = input.string;
  }

  boolean isEmpty() {
    return string == null || string.matches("\\s*");
  }

  boolean equals(final String string) {
    return this.string.equals(string);
  }

  boolean equals(final StringProperty stringProperty) {
    return string.equals(stringProperty.string);
  }

  void load(Properties props, String prepend) {
    if (props == null) {
      reset();
    }
    else {
      string = props.getProperty(createKey(prepend), "");
    }
  }

  void loadFromOtherKey(Properties props, String prepend, String key) {
    if (props == null) {
      reset();
    }
    else {
      string = props.getProperty(createKey(prepend, key), "");
    }
  }

  void store(Properties props, String prepend) {
    if (props == null) {
      return;
    }
    String currentKey = createKey(prepend);
    if (isEmpty()) {
      props.remove(currentKey);
    }
    else {
      props.setProperty(currentKey, string);
    }
  }

  void reset() {
    string = "";
  }

  private String createKey(String prepend) {
    return createKey(prepend, key);
  }

  private String createKey(String prepend, String key) {
    if (prepend == null || prepend.matches("\\s*")) {
      return key;
    }
    return prepend + "." + key;
  }
}
