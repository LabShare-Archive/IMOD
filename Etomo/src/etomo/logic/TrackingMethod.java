package etomo.logic;

import etomo.type.ConstEtomoNumber;
import etomo.type.EnumeratedType;
import etomo.type.EtomoNumber;

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
public final class TrackingMethod implements EnumeratedType {
  public static final String rcsid = "$Id:$";

  public static final TrackingMethod SEED = new TrackingMethod(true, 0, "Seed");
  public static final TrackingMethod PATCH_TRACKING = new TrackingMethod(false, 1,
      "PatchTracking");
  public static final TrackingMethod RAPTOR = new TrackingMethod(false, 2, "Raptor");

  public static final int NUM = 3;

  private final boolean isDefault;
  private final EtomoNumber value = new EtomoNumber();
  private final String string;

  private TrackingMethod(final boolean isDefault, final int value, final String string) {
    this.isDefault = isDefault;
    this.value.set(value);
    this.string = string;

  }

  public static TrackingMethod getInstance(final String string) {
    if (string == null) {
      return null;
    }
    if (SEED.string.equals(string)) {
      return SEED;
    }
    if (PATCH_TRACKING.string.equals(string)) {
      return PATCH_TRACKING;
    }
    if (RAPTOR.string.equals(string)) {
      return RAPTOR;
    }
    return null;
  }

  public static String toMetaDataValue(final String fromDirectiveValue) {
    if (fromDirectiveValue == null) {
      return null;
    }
    if (fromDirectiveValue.equals(SEED.value.toString())) {
      return SEED.string;
    }
    if (fromDirectiveValue.equals(PATCH_TRACKING.value.toString())) {
      return PATCH_TRACKING.string;
    }
    if (fromDirectiveValue.equals(RAPTOR.value.toString())) {
      return RAPTOR.string;
    }
    return null;
  }

  public static ConstEtomoNumber toDirectiveValue(final String fromMetaDataValue) {
    if (fromMetaDataValue == null) {
      return null;
    }
    if (fromMetaDataValue.equals(SEED.string)) {
      return SEED.value;
    }
    if (fromMetaDataValue.equals(PATCH_TRACKING.string)) {
      return PATCH_TRACKING.value;
    }
    if (fromMetaDataValue.equals(RAPTOR.string)) {
      return RAPTOR.value;
    }
    return null;
  }

  public boolean isDefault() {
    return isDefault;
  }

  public String getLabel() {
    return null;
  }

  public ConstEtomoNumber getValue() {
    return value;
  }

  public String toString() {
    return string;
  }
}
