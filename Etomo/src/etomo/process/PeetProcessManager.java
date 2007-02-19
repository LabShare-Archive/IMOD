package etomo.process;

import etomo.PeetManager;

/**
* <p>Description: </p>
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
* <p> $Log$ </p>
*/
public final class PeetProcessManager extends BaseProcessManager {
  public static  final String  rcsid =  "$Id$";
  
  private final PeetManager manager;
  
  public PeetProcessManager(PeetManager manager) {
    super(manager);
    this.manager = manager;
  }
  
  protected void errorProcess(BackgroundProcess process) {
  }
  
  protected void errorProcess(ComScriptProcess process) {
  }
  
  protected void errorProcess(ReconnectProcess script) {
  }
  
  protected void postProcess(ComScriptProcess script) {
  }
  
  protected void postProcess(InteractiveSystemProgram program) {
  }
  
  protected void postProcess(ReconnectProcess script) {
  }
}
