package etomo.logic;

import java.io.File;
import java.util.SortedMap;
import java.util.TreeMap;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.AutodocFilter;
import etomo.type.UserConfiguration;

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
public final class ConfigTool {
  public static final String rcsid = "$Id:$";
  private static final String SYSTEM_TEMPLATE_DIR = "templates";

  /**
   * Returns a map of the system template files.  Files in ImodCalib override files in
   * IMOD_DIR.
   * @return
   */
  public static File[] getSystemTemplateFiles() {
    File[] fileArray = new File(BaseManager.getIMODBinPath(), SYSTEM_TEMPLATE_DIR)
        .listFiles(new AutodocFilter());
    if (fileArray == null) {
      return null;
    }
    SortedMap<String, File> map = new TreeMap<String, File>();
    for (int i = 0; i < fileArray.length; i++) {
      map.put(fileArray[i].getName(), fileArray[i]);
    }
    fileArray = new File(EtomoDirector.INSTANCE.getIMODCalibDirectory(),
        SYSTEM_TEMPLATE_DIR).listFiles(new AutodocFilter());
    for (int i = 0; i < fileArray.length; i++) {
      String key = fileArray[i].getName();
      if (map.containsKey(key)) {
        map.remove(key);
      }
      map.put(key, fileArray[i]);
    }
    int size = map.size();
    if (size == 0) {
      return null;
    }
    if (size == 1) {
      return new File[] { map.get(map.firstKey()) };
    }
    return map.values().toArray(new File[size]);
  }

  /**
   * Returns a map of the user template files.  User template files are store either in
   * .etomotemplates, or in a directory specificed in the Settings dialog.
   * @return
   */
  public static File[] getUserTemplateFiles() {
    UserConfiguration userConfig = EtomoDirector.INSTANCE.getUserConfiguration();
    String userTemplateDirPath = userConfig.getUserTemplateDir();
    File dir = null;
    if (userTemplateDirPath != null) {
      dir = new File(userTemplateDirPath);
    }
    else {
      String homeDirectory = System.getProperty("user.home");
      if (homeDirectory == null) {
        return null;
      }
      dir = new File(homeDirectory, ".etomotemplates");
    }
    File[] fileArray = dir.listFiles(new AutodocFilter());
    if (fileArray == null) {
      return null;
    }
    SortedMap<String, File> map = new TreeMap<String, File>();
    for (int i = 0; i < fileArray.length; i++) {
      map.put(fileArray[i].getName(), fileArray[i]);
    }
    int size = map.size();
    if (size == 0) {
      return null;
    }
    if (size == 1) {
      return new File[] { map.get(map.firstKey()) };
    }
    return map.values().toArray(new File[size]);
  }
}
