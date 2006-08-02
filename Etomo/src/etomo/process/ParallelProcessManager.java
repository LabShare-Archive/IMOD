package etomo.process;

import etomo.BaseManager;
import etomo.ParallelManager;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
*/
public final class ParallelProcessManager extends BaseProcessManager {
  public static  final String  rcsid =  "$Id$";
  
  private final ParallelManager manager;
  
  public ParallelProcessManager(ParallelManager manager) {
    super(manager);
    this.manager = manager;
  }
  
  protected void errorProcess(BackgroundProcess process) {
  }
  
  protected void errorProcess(ComScriptProcess process) {
  }
  
  protected BaseManager getManager() {
    return manager;
  }
  
  protected void postProcess(ComScriptProcess script) {
  }
  
  protected void postProcess(InteractiveSystemProgram program) {
  }
  
  protected void errorProcess(ReconnectProcess script) {
  }

  protected void postProcess(ReconnectProcess script) {
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/06/05 16:27:25  sueh
* <p> bug# 766 Added manager to the base class.
* <p>
* <p> Revision 1.1  2006/03/20 17:52:03  sueh
* <p> bug# 835 A process manager for ParallelManager.
* <p> </p>
*/