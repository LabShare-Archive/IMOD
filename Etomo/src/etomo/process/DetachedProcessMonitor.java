package etomo.process;

import etomo.storage.LogFile;

/**
* <p>Description: </p>
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

interface DetachedProcessMonitor extends ProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  public boolean isProcessRunning();
  public String getProcessOutputFileName()throws LogFile.LockException;
  public void setProcess(SystemProcessInterface process);
}
/**
 * Old log:
 * BackgroundProcessMonitor.java,v $
 * <p> $Revision 1.5  2005/08/30 18:35:11  sueh
 * <p> $bug# 532 interface for monitors used by BackgroundProcess.  Currently
 * <p> $implemented by ProcesschunksProcessMonitor.
 * <p> $
 * <p> $Revision 1.4  2005/07/26 17:36:50  sueh
 * <p> $bug# 701 Changed BackgroundProcessMonitor to extend ProcessMonitor
 * <p> $so that a class that implements BackgroundProcessMonitor can be
 * <p> $passed to ComScriptProcess.
 * <p> $
 * <p> $Revision 1.3  2005/04/25 20:43:27  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 1.2  2004/08/23 23:32:42  sueh
 * <p> $bug# 508 changed setKilled(boolean) to kill().  changed isDone()
 * <p> $to isSuccessful
 * <p> $
 * <p> $Revision 1.1  2004/08/19 01:47:30  sueh
 * <p> $bug# 508 Generic interface for CombineProcessMonitor.
 * <p> $$ </p>
 */
/**
* <p> $Log$
* <p> Revision 1.4  2006/10/11 10:08:39  sueh
* <p> bug# 931 Added delete functionality to LogFile - changed BackupException to
* <p> FileException.
* <p>
* <p> Revision 1.3  2006/01/31 20:44:33  sueh
* <p> bug# 521 Added the process to combine monitor.  This allows the last
* <p> ProcessResultDisplay used by the monitor to be assigned to the process.
* <p>
* <p> Revision 1.2  2005/11/19 02:24:09  sueh
* <p> bug# 744 Removing BackgroundComScriptMonitor.  Using
* <p> DetachedProcessMonitor with both DetachedProcess and
* <p> BackgroundComScriptProcess.  BackgroundComScriptProcess is also
* <p> detached.  Detached monitors don't wait for interrupts so they need
* <p> isProcessRunning().  The other functions in
* <p> BackgroundComScriptMonitor are unnecessary.  Added
* <p> setProcessOutputFileName() to DetachedProcessMonitor.
* <p>
* <p> Revision 1.1  2005/11/14 21:25:43  sueh
* <p> bug# 744 An interface for monitors used with DetachedProcess.
* <p> </p>
*/