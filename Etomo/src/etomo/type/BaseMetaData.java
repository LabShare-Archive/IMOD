package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;
import etomo.ui.LogProperties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.8  2011/02/22 05:30:49  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.7  2007/12/10 22:33:43  sueh
 * <p> bug# 1041 Added current processchunks root name and subdir name.
 * <p>
 * <p> Revision 1.6  2007/03/26 23:34:22  sueh
 * <p> bug# 964 Reduced visibility of inherited fields.
 * <p>
 * <p> Revision 1.5  2007/02/05 23:09:42  sueh
 * <p> bug# 962 Made revisionNumber a EtomoVersion, so it can be compared.
 * <p>
 * <p> Revision 1.4  2005/12/14 01:28:07  sueh
 * <p> bug# 782 Updated toString().
 * <p>
 * <p> Revision 1.3  2005/05/17 19:15:31  sueh
 * <p> bug# 663 Allowing isValid() to be called from the base class.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:31:33  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/29 01:18:18  sueh
 * <p> bug# 520 Removing unecessary abstract isValid functions.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/15 00:16:21  sueh
 * <p> bug# 520 Added toString().
 * <p>
 * <p> Revision 1.1.2.2  2004/10/01 19:44:27  sueh
 * <p> bug# 520 provide a standard way to get the identifier of a meta data file.
 * <p> Add a file extension static, since there are two meta data file extensions.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 19:15:08  sueh
 * <p> bug# 520 Added base class for ConstMetaData and ConstJoinMetaData.
 * <p> Implements Storable with abstract class.  Implements store(Properties),
 * <p> since this function is generic and suitable for a const class.
 * <p> </p>
 */
public abstract class BaseMetaData implements Storable {
  public static final String rcsid = "$Id$";

  private static final String CURRENT_PROCESSCHUNKS_ROOT_NAME = "CurrentProcesschunksRootName";
  private static final String CURRENT_PROCESSCHUNKS_SUBDIR_NAME = "CurrentProcesschunksSubdirName";

  static final String revisionNumberString = "RevisionNumber";

  String fileExtension;

  // revisionNumber should be set only by load()
  EtomoVersion revisionNumber = EtomoVersion.getEmptyInstance(revisionNumberString);
  AxisType axisType = AxisType.NOT_SET;
  String invalidReason = "";

  private final StringProperty currentProcesschunksRootNameA = new StringProperty("A."
      + CURRENT_PROCESSCHUNKS_ROOT_NAME);
  private final StringProperty currentProcesschunksRootNameB = new StringProperty("B."
      + CURRENT_PROCESSCHUNKS_ROOT_NAME);
  private final StringProperty currentProcesschunksSubdirNameA = new StringProperty("A."
      + CURRENT_PROCESSCHUNKS_SUBDIR_NAME);
  private final StringProperty currentProcesschunksSubdirNameB = new StringProperty("B."
      + CURRENT_PROCESSCHUNKS_SUBDIR_NAME);

  private final LogProperties logProperties;

  public abstract String getMetaDataFileName();

  public abstract String getName();

  public abstract String getDatasetName();

  public abstract boolean isValid();

  abstract String getGroupKey();

  BaseMetaData(final LogProperties logProperties) {
    this.logProperties = logProperties;
  }

  public String toString() {
    return "[fileExtension:" + fileExtension + ",revisionNumber:" + revisionNumber
        + ",\naxisType:" + axisType + ",invalidReason:" + invalidReason + "]";
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void load(Properties props) {
    load(props, "");
  }

  String createPrepend(final String prepend) {
    if (prepend.equals("")) {
      return getGroupKey();
    }
    return prepend + "." + getGroupKey();
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    currentProcesschunksRootNameA.store(props, prepend);
    currentProcesschunksRootNameB.store(props, prepend);
    currentProcesschunksSubdirNameA.store(props, prepend);
    currentProcesschunksSubdirNameB.store(props, prepend);
    if (logProperties != null) {
      logProperties.store(props, prepend);
    }
  }

  public void load(Properties props, String prepend) {
    // reset
    currentProcesschunksRootNameA.reset();
    currentProcesschunksRootNameB.reset();
    currentProcesschunksSubdirNameA.reset();
    currentProcesschunksSubdirNameB.reset();
    // load
    prepend = createPrepend(prepend);
    currentProcesschunksRootNameA.load(props, prepend);
    currentProcesschunksRootNameB.load(props, prepend);
    currentProcesschunksSubdirNameA.load(props, prepend);
    currentProcesschunksSubdirNameB.load(props, prepend);
    if (logProperties != null) {
      logProperties.load(props, prepend);
    }
  }

  public EtomoVersion getRevisionNumber() {
    return revisionNumber;
  }

  public AxisType getAxisType() {
    return axisType;
  }

  public String getInvalidReason() {
    return invalidReason;
  }

  public String getFileExtension() {
    return fileExtension;
  }

  public boolean equals(BaseMetaData input) {
    if (!currentProcesschunksRootNameA.equals(input.currentProcesschunksRootNameA)) {
      return false;
    }
    if (!currentProcesschunksRootNameB.equals(input.currentProcesschunksRootNameB)) {
      return false;
    }
    if (axisType != input.axisType) {
      return false;
    }
    return true;
  }

  public boolean equals(Object object) {
    if (!(object instanceof BaseMetaData))
      return false;
    return equals((BaseMetaData) object);
  }

  public String getCurrentProcesschunksRootName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return currentProcesschunksRootNameB.toString();
    }
    return currentProcesschunksRootNameA.toString();
  }

  public String getCurrentProcesschunksSubdirName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return currentProcesschunksSubdirNameB.toString();
    }
    return currentProcesschunksSubdirNameA.toString();
  }

  public boolean isCurrentProcesschunksSubdirNameSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !currentProcesschunksSubdirNameB.isEmpty();
    }
    return !currentProcesschunksSubdirNameA.isEmpty();
  }

  public void setCurrentProcesschunksRootName(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      currentProcesschunksRootNameB.set(input);
    }
    else {
      currentProcesschunksRootNameA.set(input);
    }
  }

  public void setCurrentProcesschunksSubdirName(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      currentProcesschunksSubdirNameB.set(input);
    }
    else {
      currentProcesschunksSubdirNameA.set(input);
    }
  }

  public void resetCurrentProcesschunksRootName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      currentProcesschunksRootNameB.reset();
    }
    else {
      currentProcesschunksRootNameA.reset();
    }
  }

  public void resetCurrentProcesschunksSubdirName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      currentProcesschunksSubdirNameB.reset();
    }
    else {
      currentProcesschunksSubdirNameA.reset();
    }
  }
}