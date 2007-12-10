package etomo.type;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import etomo.storage.Storable;

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
 * <p> Revision 1.4  2007/06/11 17:20:54  sueh
 * <p> bug# 1014 Must pass a String to String.compareTo().
 * <p>
 * <p> Revision 1.3  2007/06/08 22:16:47  sueh
 * <p> bug# 1014 isNull() was not working when empty section where found,
 * <p> since isNull was testing the size of the array and not looking inside it.
 * <p> Made sectionList a List instead of an array so it can contain only non-
 * <p> empty strings.
 * <p>
 * <p> Revision 1.2  2007/02/05 23:26:52  sueh
 * <p> bug# 962 Added comparison functions.
 * <p>
 * <p> Revision 1.1  2005/01/10 23:49:41  sueh
 * <p> bug# 578 A class to parse and compare version numbers.
 * <p> </p>
 */
public final class EtomoVersion implements ConstEtomoVersion, Storable {
  public static final String rcsid = "$Id$";

  public static final String DEFAULT_KEY = "Version";
  private final List sectionList = new ArrayList();
  private String key;
  private boolean debug = false;

  public static EtomoVersion getDefaultInstance() {
    return new EtomoVersion();
  }

  public static EtomoVersion getDefaultInstance(String version) {
    EtomoVersion instance = new EtomoVersion();
    instance.set(version);
    return instance;
  }

  public static EtomoVersion getEmptyInstance(String key) {
    EtomoVersion instance = new EtomoVersion();
    instance.key = key;
    return instance;
  }

  public static EtomoVersion getInstance(String key, String version) {
    EtomoVersion instance = getEmptyInstance(key);
    instance.set(version);
    return instance;
  }

  public String getKey() {
    return key;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  private EtomoVersion() {
    key = DEFAULT_KEY;
  }

  private boolean equals(EtomoVersion version) {
    //treat null as the earliest version
    if ((version == null || version.isNull()) && isNull()) {
      return true;
    }
    if (version == null || version.isNull() || isNull()) {
      return false;
    }
    if (sectionList.size() != version.sectionList.size()) {
      return false;
    }
    for (int i = 0; i < sectionList.size(); i++) {
      if (!((String) sectionList.get(i)).equals((String) version.sectionList
          .get(i))) {
        return false;
      }
    }
    return true;
  }

  /**
   * @param version
   * @return true if this is less then version
   */
  public boolean lt(EtomoVersion version) {
    //treat null as the earliest version
    if (isNull() && (version != null && !version.isNull())) {
      return true;
    }
    if (version == null || version.isNull() || isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    //loop until a section is not equal then corresponding version section
    for (int i = 0; i < length; i++) {
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) > 0) {
        return false;
      }
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) < 0) {
        return true;
      }
    }
    //equal so far - shorter one is less then
    if (sectionList.size() < version.sectionList.size()) {
      return true;
    }
    return false;
  }

  /**
   * @param version
   * @return true if this is less then or equal to version
   */
  public boolean le(EtomoVersion version) {
    //treat null as the earliest version
    if (isNull()) {
      return true;
    }
    if (version == null || version.isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    //loop until a section is not equal then corresponding version section
    for (int i = 0; i < length; i++) {
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) > 0) {
        return false;
      }
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) < 0) {
        return true;
      }
    }
    //equal so far - shorter one is less then
    if (sectionList.size() <= version.sectionList.size()) {
      return true;
    }
    return false;
  }

  public boolean gt(EtomoVersion version) {
    //treat null as the earliest version
    if ((version == null || version.isNull()) && !isNull()) {
      return true;
    }
    if (version == null || version.isNull() || isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    //loop until a section is not equal then corresponding version section
    for (int i = 0; i < length; i++) {
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) > 0) {
        return true;
      }
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) < 0) {
        return false;
      }
    }
    //equal so far - longer one is greater then
    if (sectionList.size() > version.sectionList.size()) {
      return true;
    }
    return false;
  }

  /**
   * @param version
   * @return true if this is greater then or equal to version
   */
  public boolean ge(EtomoVersion version) {
    //treat null as the earliest version
    if (version == null || version.isNull()) {
      return true;
    }
    if (isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    //loop until a section is not equal then corresponding version section
    for (int i = 0; i < length; i++) {
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) > 0) {
        return true;
      }
      if (((String) sectionList.get(i)).compareTo((String) version.sectionList
          .get(i)) < 0) {
        return false;
      }
    }
    //equal so far - longer one is greater then
    if (sectionList.size() >= version.sectionList.size()) {
      return true;
    }
    return false;
  }

  public boolean ge(ConstEtomoVersion version) {
    return ge((EtomoVersion) version);
  }

  public String toString() {
    if (isNull()) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(((String) sectionList.get(0)));
    for (int i = 1; i < sectionList.size(); i++) {
      buffer.append("." + sectionList.get(i));
    }
    return buffer.toString();
  }

  public void store(Properties props) {
    if (isNull()) {
      props.remove(key);
    }
    else {
      props.setProperty(key, toString());
    }
  }

  public void store(Properties props, String prepend) {
    if (debug) {
      System.err.println("store:prepend=" + prepend + ",key=" + key
          + ",toString()=" + toString());
    }
    if (isNull()) {
      if (debug) {
        System.err.println("isNull");
      }
      props.remove(prepend + '.' + key);
    }
    else {
      props.setProperty(prepend + "." + key, toString());
    }
    if (debug) {
      System.err.println("props:" + props.getProperty(prepend + "." + key));
    }
  }

  boolean isNull() {
    return sectionList.size() == 0;
  }

  /**
   * Split the parameter version into a list by ".".  Ignore any empty sections.
   * @param version
   */
  public void set(String version) {
    sectionList.clear();
    if (version == null || version.matches("\\s*")) {
      return;
    }
    String[] stringList = version.trim().split("\\.");
    if (stringList == null || stringList.length == 0) {
      return;
    }
    for (int i = 0; i < stringList.length; i++) {
      String section = stringList[i].trim();
      //Ignore empty sections
      if (section != null && !section.equals("") && !section.matches("\\s+")) {
        sectionList.add(section);
      }
    }
  }

  public void set(EtomoVersion that) {
    sectionList.clear();
    if (that.isNull()) {
      return;
    }
    for (int i = 0; i < that.sectionList.size(); i++) {
      sectionList.add(that.sectionList.get(i));
    }
  }

  public void load(Properties props) {
    set(props.getProperty(key));
  }

  public void load(Properties props, String prepend) {
    set(props.getProperty(prepend + "." + key));
  }

  public void reset() {
    sectionList.clear();
  }
}
