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
 * <p> $Log$ </p>
 */
public class Transform {
  public static final String rcsid = "$Id$";

  private static final String FULL_LINEAR_TRANSFORMATION_VALUE = "FullLinearTransformation";
  private static final String ROTATION_TRANSLATION_MAGNIFICATION_VALUE = "RotationTranslationMagnification";
  private static final String ROTATION_TRANSLATION_VALUE = "RotationTranslation";
  private static final String TRANSLATION_VALUE = "Translation";

  public static final Transform FULL_LINEAR_TRANSFORMATION = new Transform(
      FULL_LINEAR_TRANSFORMATION_VALUE);
  public static final Transform ROTATION_TRANSLATION_MAGNIFICATION = new Transform(
      ROTATION_TRANSLATION_MAGNIFICATION_VALUE);
  public static final Transform ROTATION_TRANSLATION = new Transform(
      ROTATION_TRANSLATION_VALUE);
  public static final Transform TRANSLATION = new Transform(TRANSLATION_VALUE);

  private static final String KEY = "Transform";
  private final String value;

  public static void store(Transform transform, Properties props,
      String prepend, String key) {
    prepend = createPrepend(prepend);
    if (transform == null) {
      props.remove(prepend + '.' + key);
    }
    props.setProperty(prepend + '.' + key, transform.toString());
  }

  public static Transform load(Properties props,
      String prepend, String key, Transform defaultTransform) {
    prepend = createPrepend(prepend);
    String value = props.getProperty(prepend+'.'+key);
    if (value ==null) {
      return defaultTransform;
    }
    Transform instance= getInstance(value);
    if (instance==null) {
      return defaultTransform;
    }
    return instance;
  }
  
  public static Transform getInstance(String value) {
    if (value.equals(FULL_LINEAR_TRANSFORMATION.value)) {
      return FULL_LINEAR_TRANSFORMATION;
    }
    if (value.equals(ROTATION_TRANSLATION_MAGNIFICATION.value)) {
      return ROTATION_TRANSLATION_MAGNIFICATION;
    }
    if (value.equals(ROTATION_TRANSLATION.value)) {
      return ROTATION_TRANSLATION;
    }
    if (value.equals(TRANSLATION.value)) {
      return TRANSLATION;
    }
    return null;
  }

  public String toString() {
    return value;
  }

  private Transform(String string) {
    this.value = string;
  }

  private static String createPrepend(String prepend) {
    if (prepend.equals("")) {
      return KEY;
    }
    return prepend + '.' + KEY;
  }
}
