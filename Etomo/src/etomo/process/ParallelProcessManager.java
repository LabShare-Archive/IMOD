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
    super();
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
}
/**
* <p> $Log$ </p>
*/