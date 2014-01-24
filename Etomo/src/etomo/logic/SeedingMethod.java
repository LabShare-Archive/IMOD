package etomo.logic;

import etomo.type.AxisID;
import etomo.type.MetaData;

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
public final class SeedingMethod {
  public static final String rcsid = "$Id:$";

  public static final SeedingMethod MANUAL = new SeedingMethod("0");
  public static final SeedingMethod AUTO_FID_SEED = new SeedingMethod("1");
  public static final SeedingMethod TRANSFER_FID = new SeedingMethod("2");
  public static final SeedingMethod BOTH = new SeedingMethod("3");

  private final String value;

  private SeedingMethod(final String value) {
    this.value = value;
  }

  public static SeedingMethod getInstance(String value) {
    if (value == null) {
      return null;
    }
    value = value.trim();
    if (value.equals(MANUAL.value)) {
      return MANUAL;
    }
    if (value.equals(AUTO_FID_SEED.value)) {
      return AUTO_FID_SEED;
    }
    if (value.equals(TRANSFER_FID.value)) {
      return TRANSFER_FID;
    }
    if (value.equals(BOTH.value)) {
      return BOTH;
    }
    return null;
  }
  
  public String getValue() {
    return value;
  }

  public static String toDirectiveValue(final MetaData metaData, final AxisID axisID) {
    if (metaData.isTrackSeedModelManual(axisID)) {
      return MANUAL.value;
    }
    if (metaData.isTrackSeedModelAuto(axisID)) {
      return AUTO_FID_SEED.value;
    }
    if (metaData.isTrackSeedModelTransfer(axisID)) {
      return TRANSFER_FID.value;
    }
    return null;
  }
}
