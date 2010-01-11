package etomo.storage;

import etomo.EtomoDirector;
import etomo.ManagerKey;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.EnvironmentVariable;

/**
 * <p>Description: Represents the queues and computers available in the local
 * network.  Uses the cpu.adoc file, the IMOD_PROCESSORS environment variable,
 * and the Settings dialog (saved to the .etomo file).</p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
public final class Network {
  public static final String rcsid = "$Id$";

  private Network() {
  }

  /**
   * Gets the host name from the current host computer.
   * Calls hostname.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static String getLocalHostName(AxisID axisID, String propertyUserDir,
      ManagerKey managerKey) {
    SystemProgram hostname = new SystemProgram(propertyUserDir,
        new String[] { "hostname" }, axisID, managerKey);
    hostname.run();
    String[] stdout = hostname.getStdOutput();
    if (stdout == null || stdout.length < 1) {
      return null;
    }
    return stdout[0];
  }

  /**
   * Parallel processing is set outside of Etomo if the cpu.adoc exists, or if
   * the IMOD_PROCESSORS environment variable is set.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static boolean isParallelProcessingSetExternally(AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    if (CpuAdoc.INSTANCE.exists(axisID, propertyUserDir, managerKey)) {
      return true;
    }
    return EnvironmentVariable.INSTANCE.exists(propertyUserDir,
        "IMOD_PROCESSORS", axisID, managerKey);
  }

  /**
   * Parallel processing is enabled if the cpu.adoc contains one or more
   * Computer or Queue sections, or if the IMOD_PROCESSORS environment variable
   * is set, or if parallel processing was turned on in the Settings dialog.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static boolean isParallelProcessingEnabled(AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    if (!CpuAdoc.INSTANCE.isComputerListEmpty(axisID, propertyUserDir,
        managerKey)
        || !CpuAdoc.INSTANCE.isQueueListEmpty(axisID, propertyUserDir,
            managerKey)) {
      return true;
    }
    EtomoNumber imodProcessors = new EtomoNumber();
    imodProcessors.set(EnvironmentVariable.INSTANCE.getValue(propertyUserDir,
        "IMOD_PROCESSORS", axisID, managerKey));
    if (!imodProcessors.isNull() && imodProcessors.isValid()) {
      return true;
    }
    return EtomoDirector.INSTANCE.getUserConfiguration().isParallelProcessing();
  }

  /**
   * GPU processing is set outside of Etomo if the cpu.adoc exists.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static boolean isGpuProcessingSetExternally(AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    return CpuAdoc.INSTANCE.exists(axisID, propertyUserDir, managerKey);
  }

  /**
   * Returns true if GPU processing is enabled in the cpu.adoc section for the
   * current computer.  If cpu.adoc is not in use, returns true if GPU
   * processing has been turned on in the Settings dialog.  Otherwise returns
   * false.
   * Side effect:  May create Node.LOCAL_HOST_INSTANCE.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static boolean isLocalHostGpuProcessingEnabled(AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    Node localHost;
    if (CpuAdoc.INSTANCE.exists(axisID, propertyUserDir, managerKey)) {
      localHost = CpuAdoc.INSTANCE.getLocalHostComputer(axisID,
          propertyUserDir, managerKey);
    }
    else {
      if (Node.LOCAL_HOST_INSTANCE == null) {
        Node.createLocalInstance(axisID, propertyUserDir, managerKey);
      }
      localHost = Node.LOCAL_HOST_INSTANCE;
    }
    if (localHost == null) {
      return false;
    }
    //Get gpu from the section in cpu.adoc belonging to the current computer.
    return localHost.isGpu();
  }

  /**
   * If cpu.adoc is in use, returns true if it contains 1 or more Computer
   * sections.  If cpu.adoc is not in use, then check the IMOD_PROCESSORS
   * environment and then the parallel processing checkbox the Settings dialog. 
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static boolean hasComputers(AxisID axisID, String propertyUserDir,
      ManagerKey managerKey) {
    if (CpuAdoc.INSTANCE.exists(axisID, propertyUserDir, managerKey)) {
      return !CpuAdoc.INSTANCE.isComputerListEmpty(axisID, propertyUserDir,
          managerKey);
    }
    if (EnvironmentVariable.INSTANCE.exists(propertyUserDir, "IMOD_PROCESSORS",
        axisID, managerKey)
        || EtomoDirector.INSTANCE.getUserConfiguration().isParallelProcessing()) {
      return true;
    }
    return false;
  }

  /**
   * Returns the number of Computer sections in cpu.adoc.  If cpu.adoc is not
   * in use, returns 1.  Will be 1 even when isParallelProcessingEnabled()
   * returns false.  This allows processes that require parallel processing to
   * be used when parallel processing wasn't set up.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static int getNumComputers(AxisID axisID, String propertyUserDir,
      ManagerKey managerKey) {
    if (CpuAdoc.INSTANCE.exists(axisID, propertyUserDir, managerKey)) {
      return CpuAdoc.INSTANCE.getComputerListSize(axisID, propertyUserDir,
          managerKey);
    }
    else {
      //Count Node.LOCAL_INSTANCE if cpu.adoc is missing or has no computers.
      return 1;
    }
  }

  /**
   * Returns a Computer section from cpu.adoc by index, or returns
   * Node.LOCAL_HOST_INSTANCE if cpu.adoc is not in use and the index is 0.
   * Otherwise returns null.
   * Side effect:  may create Node.LOCAL_HOST_INSTANCE.
   * @param index
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static Node getComputer(int index, AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    if (CpuAdoc.INSTANCE.exists(axisID, propertyUserDir, managerKey)) {
      return CpuAdoc.INSTANCE.getComputer(index, axisID, propertyUserDir,
          managerKey);
    }
    if (index == 0) {
      if (Node.LOCAL_HOST_INSTANCE == null) {
        Node.createLocalInstance(axisID, propertyUserDir, managerKey);
      }
      return Node.LOCAL_HOST_INSTANCE;
    }
    return null;
  }

  /**
   * Returns true if there are Queue sections in the cpu.adoc.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static boolean hasQueues(AxisID axisID, String propertyUserDir,
      ManagerKey managerKey) {
    return !CpuAdoc.INSTANCE.isQueueListEmpty(axisID, propertyUserDir,
        managerKey);
  }

  /**
   * Gets the number of Queue sections in cpu.adoc.
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static int getNumQueues(AxisID axisID, String propertyUserDir,
      ManagerKey managerKey) {
    return CpuAdoc.INSTANCE.getQueueListSize(axisID, propertyUserDir,
        managerKey);
  }

  /**
   * Gets a Queue section from cpu.adoc by name, or returns null.
   * @param name
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static Node getQueue(String name, AxisID axisID,
      String propertyUserDir, ManagerKey managerKey) {
    return CpuAdoc.INSTANCE.getQueue(name, axisID, propertyUserDir, managerKey);
  }

  /**
   * Gets a Queue section from cpu.adoc by index, or returns null.
   * @param index
   * @param axisID
   * @param propertyUserDir
   * @param managerKey
   * @return
   */
  public static Node getQueue(int index, AxisID axisID, String propertyUserDir,
      ManagerKey managerKey) {
    return CpuAdoc.INSTANCE
        .getQueue(index, axisID, propertyUserDir, managerKey);
  }
}
