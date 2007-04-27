package etomo.process;

import etomo.PeetManager;
import etomo.comscript.PeetParserParam;
import etomo.type.AxisID;
import etomo.type.ProcessName;

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
* <p> $Log$
* <p> Revision 1.2  2007/04/26 02:43:34  sueh
* <p> bug# 964 Added prmParser.
* <p>
* <p> Revision 1.1  2007/02/19 21:53:04  sueh
* <p> bug# 964 Process manager for PEET interface.
* <p> </p>
*/
public final class PeetProcessManager extends BaseProcessManager {
  public static  final String  rcsid =  "$Id$";
  
  private final PeetManager manager;
  
  public PeetProcessManager(PeetManager manager) {
    super(manager);
    this.manager = manager;
  }
  
  public String peetParser(PeetParserParam param) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param
        .getCommand(), AxisID.ONLY, null, ProcessName.PEET_PARSER);
    return backgroundProcess.getName();
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
