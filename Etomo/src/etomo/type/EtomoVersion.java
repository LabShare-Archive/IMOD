package etomo.type;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import etomo.storage.Storable;

/**
 * <p>Description:  Version of an object or file.  Used when necessary.  Treats
 * null as the earliest version.</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2008</p>
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
 * <p> Revision 1.10  2010/03/18 22:44:28  sueh
 * <p> bug# 1323 In load(Properties,String) and store(Properties,String) fixed the
 * <p> key that was used with Properties.
 * <p>
 * <p> Revision 1.9  2009/09/01 02:59:42  sueh
 * <p> bug# 1222 Corrected version comparison.
 * <p>
 * <p> Revision 1.8  2009/02/19 21:37:59  sueh
 * <p> bug# 1180
 * <p>
 * <p> Revision 1.7  2009/02/19 21:31:51  sueh
 * <p> bug# 1180 Ported change from branch IMOD_3-13, Revision 1.6.2.1.
 * <p>
 * <p> Revision 1.6.2.1  2009/02/19 21:28:24  sueh
 * <p> bug# 1180 Added get(int).
 * <p>
 * <p> Revision 1.6  2008/10/16 20:57:40  sueh
 * <p> bug# 1141 Added description.
 * <p>
 * <p> Revision 1.5  2007/12/10 22:36:12  sueh
 * <p> bug# 1041 Changed getInstance(String key) to getEmptyInstance(String key) for
 * <p> clarity.
 * <p>
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
public final class EtomoVersion implements Storable, ConstEtomoVersion {
  public static final String rcsid = "$Id$";

  public static final String DEFAULT_KEY = "Version";

  private final SectionList sectionList = new SectionList();
  private String key;
  private boolean debug = false;
  private String status = null;
  private boolean beta = false;
  private int year = -1;
  private int month = -1;
  private int day = -1;
  private int hour = -1;
  private int minute = -1;

  public static EtomoVersion getDefaultInstance() {
    return new EtomoVersion();
  }

  public static EtomoVersion getDefaultInstance(String version) {
    EtomoVersion instance = new EtomoVersion();
    instance.set(version);
    return instance;
  }

  public static EtomoVersion getEmptyInstance(final String key) {
    EtomoVersion instance = new EtomoVersion();
    instance.key = key;
    return instance;
  }

  public static EtomoVersion getInstance(final String key, final String version) {
    EtomoVersion instance = getEmptyInstance(key);
    instance.set(version);
    return instance;
  }

  public String getKey() {
    return key;
  }

  public void setDebug(final boolean debug) {
    this.debug = debug;
  }

  private EtomoVersion() {
    key = DEFAULT_KEY;
  }

  public int hashCode() {
    return sectionList.hashCode();
  }

  /**
   * Returns true if equal to the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean equals(final EtomoVersion version) {
    // treat null as the earliest version
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
      if (!sectionList.equals(version.sectionList, i)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns true if less then the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean lt(final String version) {
    return lt(getDefaultInstance(version));
  }

  /**
   * Returns true if less then the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean lt(final EtomoVersion version) {
    // treat null as the earliest version
    if (isNull() && (version != null && !version.isNull())) {
      return true;
    }
    if (version == null || version.isNull() || isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    // loop until a section is not equal to corresponding version section
    for (int i = 0; i < length; i++) {
      if (sectionList.gt(version.sectionList, i)) {
        return false;
      }
      if (sectionList.lt(version.sectionList, i)) {
        return true;
      }
    }
    // equal so far - shorter one is less then
    if (sectionList.size() < version.sectionList.size()) {
      return true;
    }
    return false;
  }

  public boolean isNumeric() {
    return sectionList.isNumeric();
  }
  
  public boolean le(final String version) {
    return le(EtomoVersion.getDefaultInstance(version));
  }

  /**
   * Returns true if less then or equal to the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean le(final EtomoVersion version) {
    // treat null as the earliest version
    if (isNull()) {
      return true;
    }
    if (version == null || version.isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    // loop until a section is not equal to corresponding version section
    for (int i = 0; i < length; i++) {
      if (sectionList.gt(version.sectionList, i)) {
        return false;
      }
      if (sectionList.lt(version.sectionList, i)) {
        return true;
      }
    }
    // equal so far - shorter one is less then
    if (sectionList.size() <= version.sectionList.size()) {
      return true;
    }
    return false;
  }

  /**
   * Returns true if greater then the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean gt(final EtomoVersion version) {
    // treat null as the earliest version
    if ((version == null || version.isNull()) && !isNull()) {
      return true;
    }
    if (version == null || version.isNull() || isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    // loop until a section is not equal then corresponding version section
    for (int i = 0; i < length; i++) {
      if (sectionList.gt(version.sectionList, i)) {
        return true;
      }
      if (sectionList.lt(version.sectionList, i)) {
        return false;
      }
    }
    // equal so far - longer one is greater then
    if (sectionList.size() > version.sectionList.size()) {
      return true;
    }
    return false;
  }

  /**
   * Returns true if greater or equal to the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean ge(final String version) {
    return ge(getDefaultInstance(version));
  }

  /**
   * Returns true if greater or equal to the parameter.  Ignores status, beta, and date.
   * @param version
   * @return
   */
  public boolean ge(final EtomoVersion version) {
    // treat null as the earliest version
    if (version == null || version.isNull()) {
      return true;
    }
    if (isNull()) {
      return false;
    }
    int length = Math.min(sectionList.size(), version.sectionList.size());
    // loop until a section is not equal then corresponding version section
    for (int i = 0; i < length; i++) {
      if (sectionList.gt(version.sectionList, i)) {
        return true;
      }
      if (sectionList.lt(version.sectionList, i)) {
        return false;
      }
    }
    // equal so far - longer one is greater then
    if (sectionList.size() >= version.sectionList.size()) {
      return true;
    }
    return false;
  }

  /**
   * Returns the parseable format: v.v... [Beta] M/D/YYYY 24H:Min
   */
  public String toString() {
    if (isNull()) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(((String) sectionList.get(0)));
    for (int i = 1; i < sectionList.size(); i++) {
      buffer.append("." + sectionList.get(i));
    }
    if (status != null) {
      buffer.append(" " + status);
    }
    if (year != -1 && month != -1 && day != -1) {
      buffer.append(" " + month + "/" + day + "/" + year);
    }
    if (hour != -1 && minute != -1) {
      buffer.append(" " + hour + ":" + minute);
    }
    return buffer.toString();
  }

  public void store(final Properties props) {
    if (isNull()) {
      props.remove(key);
    }
    else {
      props.setProperty(key, toString());
    }
  }

  public void store(final Properties props, final String prepend) {
    String propsKey;
    if (prepend == null || prepend.matches("\\s*")) {
      propsKey = key;
    }
    else if (prepend.endsWith(".")) {
      propsKey = prepend + key;
    }
    else {
      propsKey = prepend + "." + key;
    }
    if (debug) {
      System.err.println("store:prepend=" + prepend + ",key=" + key + ",toString()="
          + toString());
    }
    if (isNull()) {
      if (debug) {
        System.err.println("isNull");
      }
      props.remove(propsKey);
    }
    else {
      props.setProperty(propsKey, toString());
    }
    if (debug) {
      System.err.println("props:" + props.getProperty(propsKey));
    }
  }

  boolean isNull() {
    return sectionList.size() == 0;
  }

  /**
   * Gets a single section.
   * Example:  If the version is 3.12.5, get(1) returns 12.
   * @param sectionIndex
   * @return
   */
  public String get(final int sectionIndex) {
    if (sectionIndex < sectionList.size()) {
      return sectionList.get(sectionIndex);
    }
    return null;
  }

  /**
   * Split the parameter version into a list by ".".  Ignore any empty sections.  Then
   * parses status, beta, date, and time.
   * @param version
   */
  public void set(final String version) {
    reset();
    if (version == null) {
      return;
    }
    String[] versionArray = version.trim().split("\\s+");
    if (versionArray == null || versionArray.length == 0) {
      return;
    }
    int versionIndex = 0;
    // Actual version number is first and required - sections are divided by ".".
    String[] stringList = versionArray[versionIndex].split("\\.");
    if (stringList == null || stringList.length == 0) {
      return;
    }
    for (int i = 0; i < stringList.length; i++) {
      sectionList.add(stringList[i].trim());
    }
    versionIndex++;
    if (versionArray.length == versionIndex) {
      return;
    }
    // Optional status - cannot contain "/" or ":"
    if (versionArray[versionIndex].indexOf("/") == -1
        && versionArray[versionIndex].indexOf(":") == -1) {
      status = versionArray[versionIndex];
      if (status.equalsIgnoreCase("beta")) {
        beta = true;
      }
      versionIndex++;
    }
    if (versionArray.length == versionIndex) {
      return;
    }
    // Optional date and time
    String warning = "WARNING: Bad version ";
    String format = "format.  Should be: v.v... [Beta] [M/D/YYYY] [24H:Min]";
    // Optional date
    if (versionArray[versionIndex].indexOf("/") != -1) {
      EtomoNumber enYear = null;
      EtomoNumber enMonth = null;
      EtomoNumber enDay = null;
      stringList = versionArray[versionIndex].split("/");
      if (stringList != null && stringList.length == 3) {
        enYear = new EtomoNumber();
        enYear.set(stringList[2]);
        enMonth = new EtomoNumber();
        enMonth.set(stringList[0]);
        enDay = new EtomoNumber();
        enDay.set(stringList[1]);
        if (enYear.isValid() && !enYear.isNull() && enMonth.isValid()
            && !enMonth.isNull() && enDay.isValid() && !enDay.isNull()) {
          year = enYear.getInt();
          month = enMonth.getInt();
          day = enDay.getInt();
        }
        else {
          System.err.println(warning + "- date " + format);
        }
      }
      else {
        System.err.println(warning + "- date " + format);
      }
      versionIndex++;
    }
    if (versionArray.length == versionIndex) {
      return;
    }
    // Set optional time
    EtomoNumber enHour = null;
    EtomoNumber enMinute = null;
    if (versionArray[versionIndex].indexOf(":") != -1) {
      stringList = versionArray[versionIndex].split(":");
      if (stringList != null && stringList.length == 2) {
        enHour = new EtomoNumber();
        enHour.set(stringList[0]);
        enMinute = new EtomoNumber();
        enMinute.set(stringList[1]);
        if (enHour.isValid() && !enHour.isNull() && enMinute.isValid()
            && !enMinute.isNull()) {
          // Avoid setting an invalid time
          hour = enHour.getInt();
          minute = enMinute.getInt();
        }
        else {
          System.err.println(warning + "- time " + format);
        }
      }
      else {
        System.err.println(warning + "- time " + format);
      }
      versionIndex++;
    }
    if (versionArray.length == versionIndex) {
      return;
    }
    // Nothing more to parse
    System.err.println(warning + format);
  }

  public void set(final EtomoVersion etomoVersion) {
    reset();
    if (etomoVersion == null || etomoVersion.isNull()) {
      return;
    }
    sectionList.add(etomoVersion.sectionList);
    status = etomoVersion.status;
    beta = etomoVersion.beta;
    year = etomoVersion.year;
    month = etomoVersion.month;
    day = etomoVersion.day;
    hour = etomoVersion.hour;
    minute = etomoVersion.minute;
  }

  public void load(final Properties props) {
    set(props.getProperty(key));
  }

  public void load(final Properties props, final String prepend) {
    String propsKey;
    if (prepend == null || prepend.matches("\\s*")) {
      propsKey = key;
    }
    else if (prepend.endsWith(".")) {
      propsKey = prepend + key;
    }
    else {
      propsKey = prepend + "." + key;
    }
    set(props.getProperty(propsKey));
  }

  public void reset() {
    sectionList.reset();
    status = null;
    beta = false;
    year = -1;
    month = -1;
    day = -1;
    hour = -1;
    minute = -1;
  }

  private static final class SectionList {
    private final List list = new ArrayList();

    private int size() {
      return list.size();
    }

    private void reset() {
      list.clear();
    }

    /**
     * Returns true if list[index] equals sectionList.list[index].
     * @param sectionList
     * @param index
     * @return
     */
    private boolean equals(final SectionList sectionList, final int index) {
      Object section = list.get(index);
      Object otherSection = sectionList.list.get(index);
      if (section instanceof EtomoNumber) {
        // Do a numeric comparison.
        EtomoNumber numericSection = (EtomoNumber) section;
        if (otherSection instanceof EtomoNumber) {
          EtomoNumber numericOtherSection = (EtomoNumber) otherSection;
          return numericSection.equals(numericOtherSection);
        }
        else {
          // One is numeric and the other non-numeric; can't be equal.
          return false;
        }
      }
      // Non-numeric - do a string comparison.
      return ( section.toString()).equals( otherSection.toString());
    }

    /**
     * Returns true if list[index] is greater then sectionList.list[index].
     * Does a numeric comparison if both elements are numeric.
     * @param sectionList
     * @param index
     * @return
     */
    private boolean gt(final SectionList sectionList, final int index) {
      Object section = list.get(index);
      Object otherSection = sectionList.list.get(index);
      if (section instanceof EtomoNumber) {
        // Do a numeric comparison.
        EtomoNumber numericSection = (EtomoNumber) section;
        if (otherSection instanceof EtomoNumber) {
          EtomoNumber numericOtherSection = (EtomoNumber) otherSection;
          return numericSection.gt(numericOtherSection);
        }
      }
      // One or both are non-numeric - do a string comparison.
      return (section.toString()).compareTo(otherSection.toString()) > 0;
    }

    /**
     * Returns true if list[index] is less then sectionList.list[index].
     * Does a numeric comparison if both elements are numeric.
     * @param sectionList
     * @param index
     * @return
     */
    private boolean lt(final SectionList sectionList, final int index) {
      Object section = list.get(index);
      Object otherSection = sectionList.list.get(index);
      if (section instanceof EtomoNumber) {
        // Do a numeric comparison.
        EtomoNumber numericSection = (EtomoNumber) section;
        if (otherSection instanceof EtomoNumber) {
          EtomoNumber numericOtherSection = (EtomoNumber) otherSection;
          return numericSection.lt(numericOtherSection);
        }
      }
      // One or both are non-numeric - do a string comparison.
      return ( section.toString()).compareTo( otherSection.toString()) < 0;
    }

    /**
     * Adds section to list.  Doesn't add empty sections.  Adds numeric sections
     * as EtomoNumbers.
     * @param section
     */
    private void add(final String section) {
      // Ignore empty sections
      if (section == null || section.matches("\\s*")) {
        return;
      }
      // Try to store a numeric section
      EtomoNumber numericSection = new EtomoNumber();
      numericSection.set(section);
      if (numericSection.isValid() && !numericSection.isNull()) {
        // Store a numeric section.
        list.add(numericSection);
      }
      else {
        // Store a non-numeric section.
        list.add(section);
      }
    }

    /**
     * Return true if each element in list is an EtomoNumber.
     * @return
     */
    private boolean isNumeric() {
      if (list.size() == 0) {
        return false;
      }
      Iterator i = list.iterator();
      while (i.hasNext()) {
        if (!(i.next() instanceof EtomoNumber)) {
          return false;
        }
      }
      return true;
    }

    /**
     * Add the sections from one sectionList to another.
     * @param sectionList
     */
    private void add(final SectionList sectionList) {
      for (int i = 0; i < sectionList.size(); i++) {
        list.add(sectionList.get(i));
      }
    }

    private String get(final int index) {
      Object section = list.get(index);
      if (section instanceof EtomoNumber) {
        return ((EtomoNumber) section).toString();
      }
      return (String) section;
    }
  }
}
