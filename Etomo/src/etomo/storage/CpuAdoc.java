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
import etomo.type.InterfaceType;
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

  public static final String LOCAL_HOST = "localhost";
  public static final String COMPUTER_SECTION_TYPE = "Computer";
  public static final String QUEUE_SECTION_TYPE = "Queue";
  public static final String UNITS_KEY = "units";

  private static final int MIN_NICE_DEFAULT = 4;

  private static CpuAdoc INSTANCE = null;

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
  private String[] loadUnits = new String[0];

  private CpuAdoc() {
    minNice.setDisplayValue(MIN_NICE_DEFAULT);
    minNice.setDefault(MIN_NICE_DEFAULT);
  }

  public static CpuAdoc getInstance(AxisID axisID, BaseManager manager) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    return createInstance(axisID, manager);
  }

  private static synchronized CpuAdoc createInstance(AxisID axisID,
      BaseManager manager) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    INSTANCE = new CpuAdoc();
    INSTANCE.load(axisID, manager);
    return INSTANCE;
  }

  public boolean isSeparateChunks() {
    return separateChunks;
  }

  public boolean isUsersColumn() {
    return usersColumn;
  }

  public int getMinNice() {
    return minNice.getInt();
  }

  public String getSpeedUnits() {
    return speedUnits;
  }

  public String getMemoryUnits() {
    return memoryUnits;
  }

  public String[] getLoadUnits() {
    return loadUnits;
  }

  public ConstEtomoNumber getMaxTilt() {
    return maxTilt;
  }

  public ConstEtomoNumber getMaxVolcombine() {
    return maxVolcombine;
  }

  public String getComputerName(int index) {
    String name = (String) computerList.get(index);
    if (name == null) {
      return "";
    }
    return name;
  }

  public String getQueueName(int index) {
    String name = (String) queueList.get(index);
    if (name == null) {
      return "";
    }
    return name;
  }

  public Section getComputer(int index) {
    Section computer = (Section) computerMap.get(getComputerName(index));
    if (computer == null) {
      return Section.getComputerInstance();
    }
    return computer;
  }

  public boolean hasQueues() {
    return queueMap.size() >= 1;
  }

  public boolean hasComputers() {
    return computerMap.size() >= 1;
  }

  public Section getQueue(int index) {
    Section queue = (Section) queueMap.get(getQueueName(index));
    if (queue == null) {
      return Section.getQueueInstance();
    }
    return queue;
  }

  public Section getQueue(String name) {
    Section queue = (Section) queueMap.get(name);
    if (queue == null) {
      return Section.getQueueInstance();
    }
    return queue;
  }

  public int getNumComputers() {
    return computerList.size();
  }

  public int getNumQueues() {
    return queueList.size();
  }

  private void load(AxisID axisID, BaseManager manager) {
    ReadOnlyAutodoc autodoc = getAutodoc(axisID);
    if (autodoc == null) {
      loadImodProcessors(axisID, manager);
    }
    else {
      separateChunks = loadBooleanAttribute(autodoc, "separate-chunks");
      loadAttribute(minNice, autodoc, "min", "nice");
      usersColumn = loadBooleanAttribute(autodoc, "users-column");
      speedUnits = loadStringAttribute(autodoc, UNITS_KEY, "speed");
      memoryUnits = loadStringAttribute(autodoc, UNITS_KEY, "memory");
      loadAttribute(maxTilt, autodoc, "max", "tilt");
      loadAttribute(maxVolcombine, autodoc, "max", "volcombine");
      loadUnits = loadStringListAttribute(autodoc, UNITS_KEY, "load");
      loadComputers(autodoc);
      loadQueues(autodoc);
      if (computerList.size() == 0) {
        loadImodProcessors(axisID, manager);
      }
    }
  }

  private void loadImodProcessors(AxisID axisID, BaseManager manager) {
    EtomoNumber imodProcessors = new EtomoNumber();
    imodProcessors.set(EnvironmentVariable.INSTANCE.getValue(manager
        .getPropertyUserDir(), "IMOD_PROCESSORS", axisID));
    if (imodProcessors.isNull()) {
      return;
    }
    loadComputers(imodProcessors);
  }

  public boolean isValid() {
    return computerList.size() > 0 || queueList.size() > 0;
  }

  private ReadOnlyAutodoc getAutodoc(AxisID axisID) {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.CPU, axisID);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.ReadException e) {
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
    SectionLocation location = autodoc
        .getSectionLocation(COMPUTER_SECTION_TYPE);
    if (location == null) {
      return;
    }
    ReadOnlySection section = null;
    while ((section = autodoc.nextSection(location)) != null) {
      Section computer = Section.getComputerInstance(section);
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
      Section queue = Section.getQueueInstance(section);
      String name = section.getName();
      if (queue != null) {
        queueList.add(name);
        queueMap.put(name, queue);
      }
    }
  }

  private void loadComputers(EtomoNumber imodProcessors) {
    if (imodProcessors == null || imodProcessors.isNull()
        || !imodProcessors.isValid()) {
      return;
    }
    Section computer = Section.getComputerInstance(imodProcessors.getInt());
    if (computer != null) {
      computerList.add(LOCAL_HOST);
      computerMap.put(LOCAL_HOST, computer);
    }
  }

  private boolean loadBooleanAttribute(ReadOnlyAutodoc autodoc, String key) {
    ReadOnlyAttribute attrib = autodoc.getAttribute(key);
    if (attrib != null
        && (attrib.getValue() == null || !attrib.getValue().equals("0"))) {
      return true;
    }
    return false;
  }

  private void loadAttribute(EtomoNumber number, ReadOnlyAutodoc autodoc,
      String key1, String key2) {
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

  private String loadStringAttribute(ReadOnlyAutodoc autodoc, String key1,
      String key2) {
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

  private String[] loadStringListAttribute(ReadOnlyAutodoc autodoc,
      String key1, String key2) {
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

  /**
   * @threadsafe
   */
  public static final class Section {
    private final static int NUMBER_DEFAULT = 1;
    private final EtomoNumber cpuNumber = new EtomoNumber();

    private InterfaceType excludeInterface = null;
    private String[] users = null;
    private EtomoNumber number = new EtomoNumber();
    private String type = "";
    private String memory = "";
    private String os = "";
    private String speed = "";
    private String command = "";
    private boolean queue = false;

    /**
     * Do not call directly.
     */
    private Section() {
    }

    private static Section getComputerInstance() {
      Section instance = new Section();
      instance.init();
      return instance;
    }

    private static Section getComputerInstance(ReadOnlySection section) {
      Section instance = new Section();
      instance.init();
      instance.load(section);
      return instance;
    }

    private static Section getComputerInstance(int imodProcessor) {
      Section instance = new Section();
      instance.init();
      instance.load(imodProcessor);
      return instance;
    }

    private static Section getQueueInstance() {
      Section instance = new Section();
      instance.queue = true;
      instance.init();
      return instance;
    }

    private static Section getQueueInstance(ReadOnlySection section) {
      Section instance = new Section();
      instance.queue = true;
      instance.init();
      instance.load(section);
      return instance;
    }

    private void init() {
      number.setDisplayValue(NUMBER_DEFAULT);
      number.setDefault(NUMBER_DEFAULT);
    }

    private void load(int imodProcessor) {
      number.set(imodProcessor);
    }

    private void load(ReadOnlySection section) {
      ReadOnlyAttribute attribute = section.getAttribute("exclude-interface");
      if (attribute != null) {
        excludeInterface = InterfaceType.getInstance(attribute.getValue());
      }
      attribute = section.getAttribute("users");
      if (attribute != null) {
        String list = attribute.getValue();
        if (list != null) {
          users = list.split("\\s*,\\s*");
        }
      }
      attribute = section.getAttribute("number");
      if (attribute != null) {
        number.set(attribute.getValue());
      }
      attribute = section.getAttribute("memory");
      if (attribute != null) {
        memory = attribute.getValue();
      }
      attribute = section.getAttribute("os");
      if (attribute != null) {
        os = attribute.getValue();
      }
      attribute = section.getAttribute("speed");
      if (attribute != null) {
        speed = attribute.getValue();
      }
      attribute = section.getAttribute("type");
      if (attribute != null) {
        type = attribute.getValue();
      }
      if (queue) {
        attribute = section.getAttribute("command");
        if (attribute != null) {
          command = attribute.getValue();
        }
      }
    }

    public String getMemory() {
      return memory;
    }

    public int getNumber() {
      return number.getInt();
    }

    public String getOs() {
      return os;
    }

    public String getSpeed() {
      return speed;
    }

    public String getType() {
      return type;
    }

    public String getCommand() {
      return command;
    }

    /**
     * Use the users array to decide whether a user should be excluded.
     * @param user
     * @return
     */
    public boolean isExcludedUser(String user) {
      if (users == null || users.length == 0) {
        return false;
      }
      for (int i = 0; i < users.length; i++) {
        if (users[i].equals(user)) {
          return false;
        }
      }
      return true;
    }

    /**
     * Use the excludeInterface member variable to decide whether an interface 
     * should be excluded.
     * @param user
     * @return
     */
    public boolean isExcludedInterface(InterfaceType input) {
      if (excludeInterface == null || excludeInterface != input) {
        return false;
      }
      return true;
    }
  }
}
/**
 * <p> $Log$
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
