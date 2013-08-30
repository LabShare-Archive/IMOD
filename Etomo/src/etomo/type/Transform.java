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
 * <p> $Log$
 * <p> Revision 1.1  2007/02/05 23:32:21  sueh
 * <p> bug# 962 Enum containing tranformation information.
 * <p> </p>
 */
public class Transform {
  public static final String rcsid = "$Id$";

  public static final Transform FULL_LINEAR_TRANSFORMATION = new Transform(
      "FullLinearTransformation", "0");
  public static final Transform ROTATION_TRANSLATION_MAGNIFICATION = new Transform(
      "RotationTranslationMagnification", "4");
  public static final Transform ROTATION_TRANSLATION = new Transform(
      "RotationTranslation", "3");
  public static final Transform TRANSLATION = new Transform("Translation", "2");
  public static final Transform SKIP_SEARCH = new Transform("SkipSearch", "-1");

  public static final Transform DEFAULT = FULL_LINEAR_TRANSFORMATION;

  private static final String KEY = "Transform";

  private final String name;
  private final String value;

  private Transform(final String name, final String value) {
    this.name = name;
    this.value = value;
  }

  public static void store(Transform transform, Properties props, String prepend,
      String key) {
    prepend = createPrepend(prepend);
    if (transform == null) {
      props.remove(prepend + '.' + key);
    }
    props.setProperty(prepend + '.' + key, transform.toString());
  }

  public static void remove(Properties props, String prepend, String key) {
    prepend = createPrepend(prepend);
    props.remove(prepend + '.' + key);
  }

  public static Transform load(Properties props, String prepend, String key,
      Transform defaultTransform) {
    prepend = createPrepend(prepend);
    String value = props.getProperty(prepend + '.' + key);
    if (value == null) {
      return defaultTransform;
    }
    Transform instance = getInstance(value);
    if (instance == null) {
      return defaultTransform;
    }
    return instance;
  }

  public static Transform getInstance(String name) {
    if (name.equals(FULL_LINEAR_TRANSFORMATION.name)) {
      return FULL_LINEAR_TRANSFORMATION;
    }
    if (name.equals(ROTATION_TRANSLATION_MAGNIFICATION.name)) {
      return ROTATION_TRANSLATION_MAGNIFICATION;
    }
    if (name.equals(ROTATION_TRANSLATION.name)) {
      return ROTATION_TRANSLATION;
    }
    if (name.equals(TRANSLATION.name)) {
      return TRANSLATION;
    }
    if (name.equals(SKIP_SEARCH.name)) {
      return SKIP_SEARCH;
    }
    return null;
  }

  public String toString() {
    return name;
  }
  
  public String getValue() {
    return value;
  }

  private static String createPrepend(String prepend) {
    if (prepend.equals("")) {
      return KEY;
    }
    return prepend + '.' + KEY;
  }
}
