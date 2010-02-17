package etomo.storage;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.InterfaceType;
import etomo.type.UserConfiguration;
import etomo.util.EnvironmentVariable;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2010/01/11 23:57:17  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.  Node was CpuAdoc.Section.
 * <p> </p>
 */
public final class Node {
  public static final String rcsid = "$Id$";

  public static final String LOCAL_HOST_NAME = "localhost";

  /**
   * Created by Network when cpu.adoc is missing.
   */
  static Node LOCAL_HOST_INSTANCE = null;

  /**
   * Type of Node.  Default is computer.
   */
  private boolean queue = false;
  private String name = "";
  private EtomoNumber number = new EtomoNumber();
  private EtomoNumber gpu = null;
  private InterfaceType excludeInterface = null;
  private String[] userArray = null;
  private String memory = "";
  private String os = "";
  private String speed = "";
  private String type = "";
  private String command = "";//cluster only

  private Node() {
    number.setDisplayValue(1);
    number.setDefault(1);
  }

  static Node getComputerInstance() {
    return new Node();
  }

  static Node getQueueInstance() {
    Node instance = new Node();
    instance.queue = true;
    return instance;
  }

  /**
   * Create a virtual cpu.adoc consisting of one entry.  Called by Network when
   * cpu.adoc is missing.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   */
  static synchronized void createLocalInstance(BaseManager manager,
      AxisID axisID, String propertyUserDir) {
    if (LOCAL_HOST_INSTANCE != null) {
      return;
    }
    LOCAL_HOST_INSTANCE = new Node();
    LOCAL_HOST_INSTANCE.name = LOCAL_HOST_NAME;
    //See if LOCAL_INSTANCE.number should be greater then 1 and set it if necessary.
    EtomoNumber imodProcessors = new EtomoNumber();
    imodProcessors.set(EnvironmentVariable.INSTANCE.getValue(manager,
        propertyUserDir, "IMOD_PROCESSORS", axisID));
    UserConfiguration userConfiguration = EtomoDirector.INSTANCE
        .getUserConfiguration();
    if (!imodProcessors.isNull() && imodProcessors.isValid()) {
      LOCAL_HOST_INSTANCE.number.set(imodProcessors);
    }
    else {
      if (userConfiguration.isParallelProcessing()) {
        LOCAL_HOST_INSTANCE.number.set(userConfiguration.getCpus());
      }
    }
    //Set GPU processing.
    if (userConfiguration.isGpuProcessing()) {
      LOCAL_HOST_INSTANCE.gpu = new EtomoNumber();
      LOCAL_HOST_INSTANCE.gpu.setDisplayValue(1);
      LOCAL_HOST_INSTANCE.gpu.setDefault(1);
    }
  }

  void load(ReadOnlySection section) {
    name = section.getName();
    ReadOnlyAttribute attribute = section.getAttribute("exclude-interface");
    if (attribute != null) {
      excludeInterface = InterfaceType.getInstance(attribute.getValue());
    }
    attribute = section.getAttribute("users");
    if (attribute != null) {
      String list = attribute.getValue();
      if (list != null) {
        userArray = list.split("\\s*,\\s*");
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
    attribute = section.getAttribute("gpu");
    if (attribute != null) {
      gpu = new EtomoNumber();
      gpu.set(attribute.getValue());
    }
    if (queue) {
      attribute = section.getAttribute("command");
      if (attribute != null) {
        command = attribute.getValue();
      }
    }
  }

  boolean isGpu() {
    return gpu != null;
  }

  public String getName() {
    return name;
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
   * Use the users array to decide whether a user should be excluded.  If the
   * userArray exists, users not on the list are excluded.  If the userArray
   * does not exist, no users are excluded.
   * @param user
   * @return
   */
  public boolean isExcludedUser(String user) {
    if (userArray == null || userArray.length == 0) {
      return false;
    }
    for (int i = 0; i < userArray.length; i++) {
      if (userArray[i].equals(user)) {
        return false;
      }
    }
    return true;
  }
}
