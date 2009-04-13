package etomo.type;

import java.util.Properties;

/**
 * <p>
 * Description:
 * </p>
 * 
 * <p>
 * Copyright: Copyright 2008
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3-Dimensional Electron Microscopy of
 * Cells (BL3DEMC), University of Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * </p>
 */
public final class OSType {
  public static final String rcsid = "$Id$";

  public static final String KEY = "OSType";

  public static final OSType LINUX = new OSType("Linux");

  public static final OSType WINDOWS = new OSType("Windows");

  public static final OSType MAC = new OSType("Mac");

  public static final OSType DEFAULT = LINUX;

  private final String description;

  private OSType(String description) {
    this.description = description;
  }

  public static OSType getInstance() {
    String osName = System.getProperty("os.name").toLowerCase();
    if (osName.indexOf("windows") != -1) {
      return WINDOWS;
    }
    if (osName.indexOf("mac") != -1) {
      return MAC;
    }
    return LINUX;
  }

  public static OSType getInstance(Properties props, String prepend) {
    String description = props.getProperty(prepend + "." + KEY);
    if (description == null || description.matches("\\*")) {
      return DEFAULT;
    }
    if (description.equals(LINUX.description)) {
      return LINUX;
    }
    if (description.equals(WINDOWS.description)) {
      return WINDOWS;
    }
    if (description.equals(MAC.description)) {
      return MAC;
    }
    return DEFAULT;
  }

  public void store(Properties props, String prepend) {
    props.setProperty(prepend + "." + KEY, description);
  }

}
