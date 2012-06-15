package etomo.type;

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
public final class SerialSectionsMetaData extends BaseMetaData {
  public static final String rcsid = "$Id:$";

  public static final String NEW_TITLE = "Serial Sections";

  private final StringProperty rootName = new StringProperty("RootName");
  private final AutoAlignmentMetaData autoAlignmentMetaData = new AutoAlignmentMetaData();

  public SerialSectionsMetaData() {
    fileExtension = DataFileType.SERIAL_SECTIONS.extension;
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
}
