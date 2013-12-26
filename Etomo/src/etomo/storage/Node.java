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
 * <p> Revision 1.4  2011/02/22 04:52:05  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2011/02/03 06:07:07  sueh
 * <p> bug# 1422 Added gpulocal.
 * <p>
 * <p> Revision 1.2  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
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
  private boolean gpu = false;
  private boolean gpuLocal = false;
  private InterfaceType excludeInterface = null;
  private String[] userArray = null;
  private String memory = "";
  private String os = "";
  private String speed = "";
  private String type = "";
  private String command = "";// cluster only
  private String[] gpuDeviceArray = null;

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
  static synchronized void createLocalInstance(final BaseManager manager,
      final AxisID axisID, final String propertyUserDir) {
    if (LOCAL_HOST_INSTANCE != null) {
      return;
    }
    LOCAL_HOST_INSTANCE = new Node();
    LOCAL_HOST_INSTANCE.name = LOCAL_HOST_NAME;
    // See if LOCAL_INSTANCE.number should be greater then 1 and set it if necessary.
    EtomoNumber imodProcessors = new EtomoNumber();
    imodProcessors.set(EnvironmentVariable.INSTANCE.getValue(manager, propertyUserDir,
        "IMOD_PROCESSORS", axisID));
    UserConfiguration userConfiguration = EtomoDirector.INSTANCE.getUserConfiguration();
    if (!imodProcessors.isNull() && imodProcessors.isValid()) {
      LOCAL_HOST_INSTANCE.number.set(imodProcessors);
    }
    else {
      if (userConfiguration.isParallelProcessing()) {
        LOCAL_HOST_INSTANCE.number.set(userConfiguration.getCpus());
      }
    }
    // Set GPU processing.
    if (userConfiguration.isGpuProcessing()) {
      LOCAL_HOST_INSTANCE.gpu = true;
    }
  }

  void load(final ReadOnlySection section) {
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
    loadGpu(section.getAttribute(CpuAdoc.GPU_KEY));
    if (queue) {
      attribute = section.getAttribute("command");
      if (attribute != null) {
        command = attribute.getValue();
      }
    }
  }

  /**
   * Handle:
   * gpu=
   * gpu.load=
   * gpu.device=
   * @param gpuAttribute
   */
  private void loadGpu(final ReadOnlyAttribute gpuAttribute) {
    gpu = false;
    gpuLocal = false;
    gpuDeviceArray = null;
    boolean error = false;
    if (gpuAttribute != null) {
      // gpu.device
      ReadOnlyAttribute attribute = gpuAttribute.getAttribute("device");
      if (attribute != null) {
        String value = attribute.getValue();
        if (value != null) {
          String[] array = value.split("\\s*,\\s*");
          if (array != null && array.length > 0) {
            gpuDeviceArray = array;
          }
          else {
            error = true;
          }
        }
        else {
          error = true;
        }
      }
      // gpu.local
      attribute = gpuAttribute.getAttribute("local");
      EtomoNumber value = new EtomoNumber();
      if (attribute != null) {
        value.set(attribute.getValue());
        if (!value.isNull() && value.gt(0)) {
          gpuLocal = true;
        }
        else {
          error = true;
        }
      }
      if (!error) {
        gpu = true;
      }
    }
  }

  /**
   * Returns true if the name member variable matches the output of the
   * "hostname" command or the output stripped of everything from the first "."
   * on.
   * @param manager
   * @param axisID
   * @param propertyUserDir
   * @return
   */
  public boolean isLocalHost(final BaseManager manager, final AxisID axisID,
      final String propertyUserDir) {
    String localHostName = Network.getLocalHostName(manager, axisID, propertyUserDir);
    if (localHostName ==null) {
      return false;
    }
    if (name.equals(localHostName)) {
      return true;
    }
    // Local host does not match. Try removing everything from the local host name
    // starting with the first ".".
    int index = localHostName.indexOf('.');
    if (index != -1) {
      localHostName = localHostName.substring(0, index);
      return name.equals(localHostName);
    }
    return false;
  }

  public boolean isGpu() {
    return gpu;
  }

  public boolean isGpuLocal() {
    return gpuLocal;
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

  public boolean isMemoryEmpty() {
    return memory == null || memory.matches("\\s*");
  }

  public String getMemory() {
    return memory;
  }

  private void setNumber(final int input) {
    number.set(input);
  }

  public int getNumber() {
    return number.getInt();
  }

  public int getGpuNumber() {
    if (!gpu || gpuDeviceArray == null) {
      return 1;
    }
    return gpuDeviceArray.length;
  }
  
  public String[] getGpuDeviceArray() {
    return gpuDeviceArray;
  }

  public boolean isOsEmpty() {
    return os == null || os.matches("\\s*");
  }

  public String getOs() {
    return os;
  }

  public boolean isSpeedEmpty() {
    return speed == null || speed.matches("\\s*");
  }

  public String getSpeed() {
    return speed;
  }

  public boolean isTypeEmpty() {
    return type == null || type.matches("\\s*");
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
  public boolean isExcludedUser(final String user) {
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
