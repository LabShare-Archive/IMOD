/*
 * Created on May 22, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package etomo.process;

/**
 * @author rickg
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public interface SystemProcessInterface {
  public String[] getStdOutput();
  public String[] getStdError();
  public boolean isStarted();
  public boolean isDone();
  public String getShellProcessID();
  public void notifyKill();
}
