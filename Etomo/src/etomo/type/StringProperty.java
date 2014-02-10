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
 * <p> Revision 1.6  2009/04/13 22:55:22  sueh
 * <p> bug# 1207 Fixed equals(String).
 * <p>
 * <p> Revision 1.5  2008/11/20 01:39:55  sueh
 * <p> bug# 1149 Simplified StringProperty - set string to null when it is empty.
 * <p>
 * <p> Revision 1.4  2008/01/14 22:03:11  sueh
 * <p> bug# 1050 Made the class public.
 * <p>
 * <p> Revision 1.3  2007/12/10 22:39:19  sueh
 * <p> bug# 1041 Added loadFromOtherKey to load value with a key that is different
 * <p> from the one saved in the instance.  Useful for backwards compatibility.
 * <p>
 * <p> Revision 1.2  2007/05/16 22:59:31  sueh
 * <p> bug# 964 Added set(StringProperty).
 * <p>
 * <p> Revision 1.1  2007/04/09 21:12:14  sueh
 * <p> bug# 964 A class to make storing, loading, and removing strings from Properties
 * <p> easier.
 * <p> </p>
 */
public final class StringProperty implements ConstStringProperty {
  public static final String rcsid = "$Id$";

  private final String key;
  private final boolean returnNullWhenEmpty;

  private String string = null;

  public StringProperty() {
    this.key = null;
    this.returnNullWhenEmpty = false;
  }
  
  public StringProperty(final String key) {
    this.key = key;
    this.returnNullWhenEmpty = false;
  }

  public StringProperty(final String key, final boolean returnNullWhenEmpty) {
    this.key = key;
    this.returnNullWhenEmpty = returnNullWhenEmpty;
  }

  public String toString() {
    if (returnNullWhenEmpty && isEmpty()) {
      return null;
    }
    if (string == null) {
      return "";
    }
    return string;
  }

  /**
   * Sets string to input.  If string is null, empty, or contains only
   * whitespace, sets string to null.
   * @param input
   */
  public void set(final String input) {
    if (isEmpty(input)) {
      reset();
    }
    else {
      string = input;
    }
  }
  
  int length() {
    if (isEmpty()) {
      return 0;
    }
    return string.length();
  }

  void set(final StringProperty input) {
    string = input.string;
  }

  public boolean isEmpty() {
    return string == null;
  }

  private boolean isEmpty(String string) {
    return string == null || string.matches("\\s*");
  }

  public boolean equals(final String string) {
    if (string == null || string.matches("\\*")) {
      return isEmpty(this.string);
    }
    if (isEmpty(this.string)) {
      return false;
    }
    return this.string.equals(string);
  }

  boolean equals(final StringProperty stringProperty) {
    if (string == null) {
      return isEmpty(stringProperty.string);
    }
    return string.equals(stringProperty.string);
  }

  public void load(Properties props, String prepend) {
    if (props == null) {
      reset();
    }
    else {
      string = props.getProperty(createKey(prepend));
    }
  }

  void loadFromOtherKey(Properties props, String prepend, String key) {
    if (props == null) {
      reset();
    }
    else {
      string = props.getProperty(createKey(prepend, key));
    }
  }

  public void store(Properties props, String prepend) {
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

  public void remove(Properties props, String prepend) {
    if (props == null) {
      return;
    }
    String currentKey = createKey(prepend);
    props.remove(currentKey);
  }

  public void reset() {
    string = null;
  }

  private String createKey(String prepend) {
    return createKey(prepend, key);
  }

  private String createKey(String prepend, String key) {
    if (isEmpty(prepend)) {
      return key;
    }
    return prepend + "." + key;
  }
}
