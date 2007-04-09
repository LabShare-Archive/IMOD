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
 * <p> $Log$ </p>
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
  void set(final String string) {
    if (string == null || string.matches("\\s*")) {
      reset();
    }
    else {
      this.string = string;
    }
  }

  /**
   * assumes that the string can not be null and can not contain only whitespace
   * @return
   */
  boolean isEmpty() {
    return string.equals("");
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
      string = props.getProperty(createKey(prepend),"");
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
      props.setProperty(currentKey,string);
    }
  }
  
  void reset() {
    string ="";
  }

  private String createKey(String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      return key;
    }
    return prepend + "." + key;
  }
}
