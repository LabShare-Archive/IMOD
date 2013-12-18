package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import etomo.BaseManager;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.storage.autodoc.SectionLocation;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.util.EnvironmentVariable;

/**
 * <p>Description:</p>
 * <p>Represents the cpu.adoc file except the mount rules, which
 * are handled by RemotePath.</p>
 * 
 * <p>Assumptions:</p>
 * <p>The Computer section names in cpu.adoc must be unique.</p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * @threadsafe
 * @immutable
 * @singleton
 */
public class CpuAdoc {
  public static final String rcsid = "$Id$";

  public static final String COMPUTER_SECTION_TYPE = "Computer";
  public static final String QUEUE_SECTION_TYPE = "Queue";
  static final String GPU_KEY = "gpu";
  private static final String SPEED_KEY = "speed";
  private static final String MEMORY_KEY = "memory";
  private static final int MIN_NICE_DEFAULT = 0;

  public static final CpuAdoc INSTANCE = new CpuAdoc();

  private final List computerList = new ArrayList();
  private final Map computerMap = new Hashtable();
  private final List queueList = new ArrayList();
  private final Map queueMap = new Hashtable();
  private final EtomoNumber minNice = new EtomoNumber();
  private final EtomoNumber maxTilt = new EtomoNumber();
  private final EtomoNumber maxVolcombine = new EtomoNumber();

  private boolean separateChunks = false;
  private boolean usersColumn = false;
  private String speedUnits = "";
  private String memoryUnits = "";
  private String gpuSpeedUnits = "";// Not in use - has not been added to cpuadoc man page
  private String gpuMemoryUnits = "";// Not in use - has not been added to cpuadoc man
                                     // page
  private String[] loadUnits = new String[0];
  private boolean envVar = false;
  private boolean userConfig = false;
  private boolean loaded = false;
  private boolean exists = false;

  private CpuAdoc() {
    minNice.setDisplayValue(MIN_NICE_DEFAULT);
    minNice.setDefault(MIN_NICE_DEFAULT);
  }

  public boolean isSeparateChunks(BaseManager manager, AxisID axisID,
      String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return separateChunks;
  }

  public boolean isUsersColumn(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return usersColumn;
  }

  public int getMinNice(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return minNice.getInt();
  }

  public String getSpeedUnits(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return speedUnits;
  }

  public String getMemoryUnits(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return memoryUnits;
  }

  public String[] getLoadUnits(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return loadUnits;
  }

  public ConstEtomoNumber getMaxTilt(BaseManager manager, AxisID axisID,
      String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return maxTilt;
  }

  public ConstEtomoNumber getMaxVolcombine(BaseManager manager, AxisID axisID,
      String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return maxVolcombine;
  }

  /**
   * Returns true if there is no computerList entries, or false if any of the computerList
   * entries have a non-local-only GPU.
   * @param manager
   * @param axisID
   * @param propertyUserDir
   * @param ignoredNode if this node is non-null and present in the list, the list is still considered empty.  Parameter may be null
   * @return
   */
  boolean isGpuComputerListEmpty(BaseManager manager, AxisID axisID,
      String propertyUserDir, final Node ignoredNode) {
    load(manager, axisID, propertyUserDir);
    if (computerList.isEmpty()) {
      return true;
    }
    for (int i = 0; i < computerList.size(); i++) {
      Node node = (Node) computerMap.get(computerList.get(i));
      if ((ignoredNode == null || node != ignoredNode) && node.isGpu()) {
        return false;
      }
    }
    return true;
  }

  boolean isComputerListEmpty(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return computerList.isEmpty();
  }

  boolean isQueueListEmpty(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return queueList.isEmpty();
  }

  /**
   * Return computerMap.get(computerList[index]) or null.
   * @param index
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  Node getComputer(BaseManager manager, int index, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return (Node) computerMap.get(computerList.get(index));
  }

  /**
   * Get the Computer section for the current computer.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  Node getLocalHostComputer(BaseManager manager, AxisID axisID, String propertyUserDir) {
    // Search for "localhost":
    Node localHost = getComputer(manager, Node.LOCAL_HOST_NAME, axisID, propertyUserDir);
    if (localHost != null) {
      return localHost;
    }
    // Search for the computer name:
    String localHostName = Network.getLocalHostName(manager, axisID, propertyUserDir);
    if (localHostName == null) {
      return null;
    }
    localHost = getComputer(manager, localHostName, axisID, propertyUserDir);
    if (localHost != null) {
      return localHost;
    }
    // Local host not found. Try removing everything from the local host name
    // starting with the first ".".
    int index = localHostName.indexOf('.');
    if (index != -1) {
      localHostName = localHostName.substring(0, index);
      localHost = getComputer(manager, localHostName, axisID, propertyUserDir);
    }
    return localHost;
  }

  Node getQueue(BaseManager manager, int index, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return (Node) queueMap.get(queueList.get(index));
  }

  /**
   * Get queue from queueMap by name.  Return null if not found.
   * @param name
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  Node getQueue(BaseManager manager, String name, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return (Node) queueMap.get(name);
  }

  /**
   * Get computer from computerMap by name.  Return null if not found.
   * @param name
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  Node getComputer(BaseManager manager, String name, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    if (name != null) {
      return (Node) computerMap.get(name);
    }
    return null;
  }

  int getComputerListSize(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return computerList.size();
  }

  int getQueueListSize(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return queueList.size();
  }

  boolean exists(BaseManager manager, AxisID axisID, String propertyUserDir) {
    load(manager, axisID, propertyUserDir);
    return exists;
  }

  private synchronized void load(BaseManager manager, AxisID axisID,
      String propertyUserDir) {
    if (loaded) {
      return;
    }
    loaded = true;
    ReadOnlyAutodoc autodoc = getAutodoc(manager, axisID);
    if (autodoc != null && autodoc.exists()) {
      exists = true;
      separateChunks = loadBooleanAttribute(autodoc, "separate-chunks");
      loadAttribute(minNice, autodoc, "min", "nice");
      usersColumn = loadBooleanAttribute(autodoc, "users-column");
      ReadOnlyAttribute attrib = autodoc.getAttribute("max");
      if (attrib != null) {
        loadAttribute(maxTilt, attrib, "tilt");
        loadAttribute(maxVolcombine, attrib, "volcombine");
      }
      attrib = autodoc.getAttribute("units");
      if (attrib != null) {
        speedUnits = loadStringAttribute(attrib, SPEED_KEY);
        memoryUnits = loadStringAttribute(attrib, MEMORY_KEY);
        loadUnits = loadStringListAttribute(attrib, "load");
        attrib = attrib.getAttribute(GPU_KEY);
        if (attrib != null) {
          gpuSpeedUnits = loadStringAttribute(attrib, SPEED_KEY);
          gpuMemoryUnits = loadStringAttribute(attrib, MEMORY_KEY);
        }
      }
      loadComputers(autodoc);
      loadQueues(autodoc);
    }
  }

  private String loadStringAttribute(ReadOnlyAttribute attrib, String key) {
    if (attrib == null) {
      return "";
    }
    attrib = attrib.getAttribute(key);
    if (attrib == null) {
      return "";
    }
    return attrib.getValue();
  }

  private void loadAttribute(EtomoNumber number, ReadOnlyAttribute attrib, String key) {
    number.reset();
    if (attrib == null) {
      return;
    }
    attrib = attrib.getAttribute(key);
    if (attrib == null) {
      return;
    }
    number.set(attrib.getValue());
  }

  private String[] loadStringListAttribute(ReadOnlyAttribute attrib, String key) {
    if (attrib == null) {
      return new String[0];
    }
    attrib = attrib.getAttribute(key);
    if (attrib == null) {
      return new String[0];
    }
    String list = attrib.getValue();
    if (list == null) {
      return new String[0];
    }
    return list.split("\\s*,\\s*");
  }

  private ReadOnlyAutodoc getAutodoc(BaseManager manager, AxisID axisID) {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.CPU, axisID);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    if (autodoc == null) {
      System.err.println("Missing $" + EnvironmentVariable.CALIB_DIR
          + "/cpu.adoc file.\n" + "Parallel processing cannot be used.\n"
          + "See $IMOD_DIR/autodoc/cpu.adoc.");
    }
    return autodoc;
  }

  private void loadComputers(ReadOnlyAutodoc autodoc) {
    SectionLocation location = autodoc.getSectionLocation(COMPUTER_SECTION_TYPE);
    if (location == null) {
      return;
    }
    ReadOnlySection section = null;
    while ((section = autodoc.nextSection(location)) != null) {
      Node computer = Node.getComputerInstance();
      computer.load(section);
      String name = section.getName();
      if (computer != null) {
        computerList.add(name);
        computerMap.put(name, computer);
      }
    }
  }

  private void loadQueues(ReadOnlyAutodoc autodoc) {
    SectionLocation location = autodoc.getSectionLocation(QUEUE_SECTION_TYPE);
    if (location == null) {
      return;
    }
    ReadOnlySection section = null;
    while ((section = autodoc.nextSection(location)) != null) {
      Node queue = Node.getQueueInstance();
      queue.load(section);
      String name = section.getName();
      if (queue != null) {
        queueList.add(name);
        queueMap.put(name, queue);
      }
    }
  }

  private boolean loadBooleanAttribute(ReadOnlyAutodoc autodoc, String key) {
    ReadOnlyAttribute attrib = autodoc.getAttribute(key);
    if (attrib != null && (attrib.getValue() == null || !attrib.getValue().equals("0"))) {
      return true;
    }
    return false;
  }

  private void loadAttribute(EtomoNumber number, ReadOnlyAutodoc autodoc, String key1,
      String key2) {
    number.reset();
    ReadOnlyAttribute attrib = autodoc.getAttribute(key1);
    if (attrib == null) {
      return;
    }
    attrib = attrib.getAttribute(key2);
    if (attrib == null) {
      return;
    }
    number.set(attrib.getValue());
  }

  private String loadStringAttribute(ReadOnlyAutodoc autodoc, String key1, String key2) {
    ReadOnlyAttribute attrib = autodoc.getAttribute(key1);
    if (attrib == null) {
      return "";
    }
    attrib = attrib.getAttribute(key2);
    if (attrib == null) {
      return "";
    }
    return attrib.getValue();
  }

  private String[] loadStringListAttribute(ReadOnlyAutodoc autodoc, String key1,
      String key2) {
    ReadOnlyAttribute attrib = autodoc.getAttribute(key1);
    if (attrib == null) {
      return new String[0];
    }
    attrib = attrib.getAttribute(key2);
    if (attrib == null) {
      return new String[0];
    }
    String list = attrib.getValue();
    if (list == null) {
      return new String[0];
    }
    return list.split("\\s*,\\s*");
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.23  2011/06/28 21:09:37  sueh
 * <p> Bug# 1480 Added isGpuComputerListEmpty.
 * <p>
 * <p> Revision 1.22  2011/06/23 14:59:11  sueh
 * <p> Bug# 1495 In getLocalHostComputer added a search for "localhost".
 * <p>
 * <p> Revision 1.21  2011/04/04 16:56:54  sueh
 * <p> bug# 1469 Added/modified GPU_KEY, gpuMemoryUnits, gpuSpeedUnits, MEMORY_KEY, load (refactored),
 * <p> loadAttribute, loadStringAttribute, loadStringListAttribute.  Removed UNITS_KEY.
 * <p>
 * <p> Revision 1.20  2011/02/22 04:32:04  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.19  2010/11/04 21:02:25  sueh
 * <p> bug# 1415 Changed MIN_NICE_DEFAULT to 0.
 * <p>
 * <p> Revision 1.18  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.17  2010/01/11 23:56:47  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.
 * <p>
 * <p> Revision 1.16  2009/03/17 00:44:22  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.15  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.14  2008/07/19 00:28:49  sueh
 * <p> bug# 1125 Get rid of isSetByUser and isValid because they where always
 * <p> used together.  Have file, envVar, and userConfig booleans.  IsAvailable
 * <p> is true when one of these booleans is true.  SettingsDialog can also check
 * <p> whether file or envVar is true.  Added setting userConfig and the number
 * <p> of CPUs.
 * <p>
 * <p> Revision 1.13  2008/01/25 22:23:44  sueh
 * <p> bug# 1070 Added setByUser which is true when the user sets the cpu data with
 * <p> cpu.adoc or IMOD_PROCESSORS.
 * <p>
 * <p> Revision 1.12  2007/11/06 19:28:32  sueh
 * <p> bug# 1047 Allow parallel processing even when that is no cpu.adoc and
 * <p> IMOD_PROCESSORS is not set.
 * <p>
 * <p> Revision 1.11  2007/09/27 20:30:38  sueh
 * <p> bug# 1044 Retrieving Queue sections.
 * <p>
 * <p> Revision 1.10  2007/08/29 22:42:32  sueh
 * <p> bug# 1040 Fixing a bug in load(AxisID, BaseManager) where the value max
 * <p> volcombine value was loaded into maxTilt.
 * <p>
 * <p> Revision 1.9  2007/07/17 21:25:47  sueh
 * <p> bug# 1018 Move data reading and storage to CpuAdoc, except mount
 * <p> rules.
 * <p>
 * <p> Revision 1.8  2007/05/22 21:07:57  sueh
 * <p> bug# 999 Added class Computer, to hold section level data.  Added users.
 * <p>
 * <p> Revision 1.7  2007/05/21 22:29:13  sueh
 * <p> bug# 1000 Added excludeInterface and loadExcludeInterfaceMap().
 * <p>
 * <p> Revision 1.6  2007/05/21 18:16:27  sueh
 * <p> bug# 992 Fixed a bug in loadBoolean(); wasn't handle attrib.getValue()
 * <p> returning null.
 * <p>
 * <p> Revision 1.5  2007/05/21 18:10:27  sueh
 * <p> bug# 964 Added usersColumn.
 * <p>
 * <p> Revision 1.4  2007/05/18 23:52:22  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.3  2007/03/21 18:10:49  sueh
 * <p> bug# 964 Moved Adoc classes out of the autodoc package because
 * <p> they not part of the autodoc.
 * <p>
 * <p> Revision 1.5  2007/03/15 21:46:20  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.4  2007/03/01 01:19:30  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.3  2006/07/21 22:11:49  sueh
 * <p> bug# 901 Getting the calibration directory environment variable name from
 * <p> EnvironmentVariable.
 * <p>
 * <p> Revision 1.2  2006/06/30 17:02:15  sueh
 * <p> Improved warning about missing cpu.adoc.
 * <p>
 * <p> Revision 1.1  2006/06/14 00:33:47  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 1.1  2006/06/08 19:04:38  sueh
 * <p> bug# 867 Class to read the cpu autodoc.
 * <p> </p>
 */
