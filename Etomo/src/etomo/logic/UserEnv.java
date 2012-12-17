package etomo.logic;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.Network;
import etomo.type.AxisID;

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
public final class UserEnv {
  public static final String rcsid = "$Id:$";

  public static boolean isParallelProcessing(final BaseManager manager,
      final AxisID axisID, final String propertyUserDir) {
    return EtomoDirector.INSTANCE.getArguments().isCpus()
        || (Network.isParallelProcessingEnabled(manager, axisID, propertyUserDir) && !EtomoDirector.INSTANCE
            .getUserConfiguration().getNoParallelProcessing());
  }

  public static boolean isParallelProcessingEnabled(final BaseManager manager,
      final AxisID axisID, final String propertyUserDir) {
    return EtomoDirector.INSTANCE.getArguments().isCpus()
        || Network.isParallelProcessingEnabled(manager, axisID, propertyUserDir);
  }

  public static boolean isGpuProcessing(final BaseManager manager, final AxisID axisID,
      final String propertyUserDir) {
    return EtomoDirector.INSTANCE.getArguments().isGpus()
        || (isGpuProcessingEnabled(manager, axisID, propertyUserDir) && EtomoDirector.INSTANCE
            .getUserConfiguration().getGpuProcessingDefault());
  }

  public static boolean isGpuProcessingEnabled(final BaseManager manager,
      final AxisID axisID, final String propertyUserDir) {
    return Network.isNonLocalHostGpuProcessingEnabled(manager, axisID, propertyUserDir)
        || Network.isLocalHostGpuProcessingEnabled(manager, axisID, propertyUserDir);
  }
}
