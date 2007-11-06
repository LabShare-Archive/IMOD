package etomo.comscript;

import etomo.type.ProcessName;

/**
* <p>Description: Generic interface for params used by classes which implement
* ParallelDialog.
* 
* Implemented by:
* ProcesschunksParam</p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public interface ParallelParam {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * @return the process that the parallel process will run
   */
  public ProcessName getProcessName();
  public CommandMode getSubcommandMode();
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/09/16 17:19:06  sueh
* <p> bug# 532 A generic interface for ProcesschunksParam.  Allows
* <p> ApplicationManager to have only one function which sets parameters in
* <p> processchunks.  This is done by using ParallelDialog to represent the
* <p> dialog.
* <p> </p>
*/