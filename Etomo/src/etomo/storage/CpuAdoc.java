package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import etomo.EtomoDirector;
import etomo.ManagerKey;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.storage.autodoc.SectionLocation;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.InterfaceType;
import etomo.type.UserConfiguration;
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

  private final ManagerKey managerKey;

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
  private boolean file = false;
  private boolean envVar = false;
  private boolean userConfig = false;

  private CpuAdoc(ManagerKey managerKey) {
    this.managerKey = managerKey;
    minNice.setDisplayValue(MIN_NICE_DEFAULT);
    minNice.setDefault(MIN_NICE_DEFAULT);
  }

  public static synchronized CpuAdoc getInstance(AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    return createInstance(axisID, propertyUserDir, managerKey);
  }

  private static synchronized CpuAdoc createInstance(AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    INSTANCE = new CpuAdoc(managerKey);
    INSTANCE.load(axisID, propertyUserDir);
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
    return file && queueMap.size() >= 1;
  }

  public boolean hasComputers() {
    return (file && computerMap.size() >= 1) || envVar || userConfig;
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

  private void load(AxisID axisID, String propertyUserDir) {
    ReadOnlyAutodoc autodoc = getAutodoc(axisID);
    if (autodoc == null) {
      setImodProcessors(axisID, propertyUserDir);
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
      if (computerList.size() > 0 || queueList.size() > 0) {
        file = true;
      }
      else {
        setImodProcessors(axisID, propertyUserDir);
      }
    }
  }

  private void setImodProcessors(AxisID axisID, String propertyUserDir) {
    EtomoNumber imodProcessors = new EtomoNumber();
    imodProcessors.set(EnvironmentVariable.INSTANCE.getValue(propertyUserDir,
        "IMOD_PROCESSORS", axisID, managerKey));
    if (!imodProcessors.isNull() && imodProcessors.isValid()) {
      envVar = true;
      createComputerInstance(imodProcessors.getInt());
    }
    else {
      UserConfiguration userConfiguration = EtomoDirector.INSTANCE
          .getUserConfiguration();
      if (!setUserConfig(userConfiguration.getParallelProcessing(),
          userConfiguration.getCpus())) {
        //File, envVar, and userConfig are all false but we still need to create
        //a parallel processing configuration to be used by processes which
        //require parallel processing.
        createComputerInstance(1);
      }
    }
  }

  private void createComputerInstance(int cpus) {
    Section computer = Section.getComputerInstance(cpus);
    if (computer != null) {
      computerList.add(LOCAL_HOST);
      computerMap.put(LOCAL_HOST, computer);
    }
  }

  /**
   * cpu.adoc and IMOD_PROCESSORS take precedence over userConfig, so do nothing
   * if they are set.  File and envVar are never reset while Etomo is running.
   * Set the first computer on the list to cpus if userConfig is true, set it to
   * 1 if userConfig is false (if it exists).  Set this.userConfig to
   * userConfig.  Create a computer if the list is empty and userConfig is true.
   * @param input
   * @param cpus
   */
  public boolean setUserConfig(boolean userConfig, ConstEtomoNumber cpus) {
    if (file || envVar) {
      return false;
    }
    if (!userConfig) {
      this.userConfig = false;
      if (getNumComputers() > 0) {
        getComputer(0).setNumber(1);
      }
      return false;
    }
    else {
      this.userConfig = true;
      int number = 1;
      if (!cpus.isNull() && cpus.isValid()) {
        number = cpus.getInt();
      }
      if (getNumComputers() > 0) {
        getComputer(0).setNumber(number);
      }
      else {
        createComputerInstance(number);
      }
      return true;
    }
  }

  /**
   * True if the computer information has been specified with either cpu.adoc,
   * IMOD_PROCESSORS, or .etomo.
   * @return
   */
  public boolean isAvailable() {
    return file || envVar || userConfig;
  }

  public boolean isFile() {
    return file;
  }

  public boolean isEnvVar() {
    return envVar;
  }

  private ReadOnlyAutodoc getAutodoc(AxisID axisID) {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.CPU, axisID,
          managerKey);
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

    private void setNumber(int input) {
      number.set(input);
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
