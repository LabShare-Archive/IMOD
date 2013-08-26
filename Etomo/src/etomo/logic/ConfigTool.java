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

  private static final String DEFAULT_SYSTEM_TEMPLATE_DIR = "SystemTemplate";

  /**
   * Returns a sorted list of the scope template files.
   * @return
   */
  public static File[] getScopeTemplateFiles() {
    SortedMap<String, File> map = new TreeMap<String, File>();
    File[] fileArray = new File(EtomoDirector.INSTANCE.getIMODCalibDirectory(),
        "ScopeTemplate").listFiles(new AutodocFilter());
    if (fileArray == null) {
      return null;
    }
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

  /**
   * Returns a sorted list of the system template files.  File names in ImodCalib override
   * files in IMOD_DIR.
   * @return
   */
  public static File[] getSystemTemplateFiles() {
    File[] fileArray = new File(EtomoDirector.INSTANCE.getIMODDirectory(),
        DEFAULT_SYSTEM_TEMPLATE_DIR).listFiles(new AutodocFilter());
    SortedMap<String, File> map = null;
    if (fileArray != null) {
      map = new TreeMap<String, File>();
      for (int i = 0; i < fileArray.length; i++) {
        map.put(fileArray[i].getName(), fileArray[i]);
      }
    }
    fileArray = new File(EtomoDirector.INSTANCE.getIMODCalibDirectory(),
        DEFAULT_SYSTEM_TEMPLATE_DIR).listFiles(new AutodocFilter());
    if (fileArray != null) {
      if (map == null) {
        map = new TreeMap<String, File>();
      }
      for (int i = 0; i < fileArray.length; i++) {
        String key = fileArray[i].getName();
        if (map.containsKey(key)) {
          map.remove(key);
        }
        map.put(key, fileArray[i]);
      }
    }
    if (map == null) {
      return null;
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
   * Returns the user's .etomotemplate directive, or null if the user's home dirctive is
   * not known.
   * @return
   */
  public static File getDefaultUserTemplateDir() {
    String homeDirectory = System.getProperty("user.home");
    if (homeDirectory == null) {
      return null;
    }
    return new File(homeDirectory, ".etomotemplate");
  }

  /**
   * Returns a sorted list of the user template files.  User template files are stored
   * either in .etomotemplate, or in a directory specified in the Settings dialog.
   * @param newUserTemplateDir overrides the user template director from User Configuration
   * @return
   */
  public static File[] getUserTemplateFiles(final File newUserTemplateDir) {
    UserConfiguration userConfig = EtomoDirector.INSTANCE.getUserConfiguration();
    String userTemplateDirPath = null;
    if (newUserTemplateDir != null) {
      userTemplateDirPath = newUserTemplateDir.getAbsolutePath();
    }
    else {
      userTemplateDirPath = userConfig.getUserTemplateDir();
    }
    File dir = null;
    if (userTemplateDirPath != null) {
      dir = new File(userTemplateDirPath);
    }
    else {
      String homeDirectory = System.getProperty("user.home");
      if (homeDirectory == null) {
        return null;
      }
      dir = getDefaultUserTemplateDir();
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

  /**
   * Returns either the parent of currentDistortionFile, the distortion directory in
   * ImodCalib, or the propery user directory.
   * @param manager
   * @param currentDistortionFile
   * @return
   */
  public static String getDistortionDir(final BaseManager manager,
      final File curtDistortionFile) {
    File dir = null;
    if (curtDistortionFile != null) {
      dir = curtDistortionFile.getParentFile();
      if (dir != null && dir.exists() && dir.isDirectory() && dir.canRead()) {
        return dir.getAbsolutePath();
      }
    }
    dir = new File(EtomoDirector.INSTANCE.getIMODCalibDirectory().getAbsolutePath(),
        "Distortion");
    if (dir.exists() && dir.isDirectory() && dir.canRead()) {
      return dir.getAbsolutePath();
    }
    return manager.getPropertyUserDir();
  }
}
