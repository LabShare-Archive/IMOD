package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.logic.SerialSectionsStartupData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class SerialSectionsMetaData extends BaseMetaData implements
    ConstSerialSectionsMetaData {
  public static final String rcsid = "$Id:$";

  public static final String NEW_TITLE = "Serial Sections";
  private static final EtomoVersion CURRENT_VERSION = EtomoVersion.getInstance(
      BaseMetaData.revisionNumberString, "1.0");

  private final StringProperty rootName = new StringProperty("RootName");
  private final AutoAlignmentMetaData autoAlignmentMetaData = new AutoAlignmentMetaData();
  private final StringProperty stack = new StringProperty("Stack");
  private final StringProperty viewType = new StringProperty("ViewType");
  private final StringProperty distortionField = new StringProperty("DistortionField");
  private final ScriptParameter imagesAreBinned = new ScriptParameter("ImagesAreBinned");

  public SerialSectionsMetaData() {
    fileExtension = DataFileType.SERIAL_SECTIONS.extension;
    axisType = AxisType.SINGLE_AXIS;
  }

  public void setName(final String rootName) {
    this.rootName.set(rootName);
  }

  String getGroupKey() {
    return "SerialSections";
  }

  public String getDatasetName() {
    return rootName.toString();
  }

  public String getMetaDataFileName() {
    if (rootName.equals("")) {
      return null;
    }
    return rootName + fileExtension;
  }

  public void load(final Properties props, String prepend) {
    // reset
    rootName.reset();
    stack.reset();
    viewType.reset();
    distortionField.reset();
    imagesAreBinned.reset();
    // load
    prepend = createPrepend(prepend);
    autoAlignmentMetaData.load(props, prepend);
    rootName.load(props, prepend);
    stack.load(props, prepend);
    viewType.load(props, prepend);
    distortionField.load(props, prepend);
    imagesAreBinned.load(props, prepend);
  }

  public void store(final Properties props, String prepend) {
    prepend = createPrepend(prepend);
    CURRENT_VERSION.store(props, prepend);
    autoAlignmentMetaData.store(props, prepend);
    rootName.store(props, prepend);
    stack.store(props, prepend);
    viewType.store(props, prepend);
    distortionField.store(props, prepend);
    imagesAreBinned.store(props, prepend);
  }

  public void setStartupData(final SerialSectionsStartupData startupData) {
    setName(startupData.getRootName());
    stack.set(startupData.getStack().getAbsolutePath());
    viewType.set(startupData.getViewType().getParamValue());
    File file = startupData.getDistortionField();
    if (file == null) {
      distortionField.reset();
    }
    else {
      distortionField.set(file.getAbsolutePath());
    }
    imagesAreBinned.set(startupData.getImagesAreBinned());
  }

  public String getName() {
    if (rootName.toString() == null || rootName.toString().matches("\\s*")) {
      return NEW_TITLE;
    }
    return rootName.toString();
  }

  public boolean isValid() {
    return true;
  }

  public AutoAlignmentMetaData getAutoAlignmentMetaData() {
    return autoAlignmentMetaData;
  }

  public String getStack() {
    return stack.toString();
  }
  
  public String getDistortionField() {
    return distortionField.toString();
  }

  public String getViewType() {
    return viewType.toString();
  }
}
